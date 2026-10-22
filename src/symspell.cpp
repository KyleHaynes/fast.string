// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "codepoint_snapshot.h"
#include "metric_dispatch.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// fuzzy_within(): every table string within k edits of each query, found
// with SymSpell's symmetric-delete index (Wolf Garbe). Each distinct table
// string contributes the strings obtained by deleting up to k characters
// from its first `prefix_length` characters; a query's own deletions look
// those up, and every candidate is verified with the exact distance.
//
// Two strings within k edits (Levenshtein, OSA or Damerau-Levenshtein)
// always share a deletion variant of their prefixes: each edit removes at
// most one character of either prefix from their longest common
// subsequence. So the prefix shortens the index without losing matches.

namespace {

typedef std::uint32_t cp_t;

struct Sequence {
    const cp_t* data;
    int size;
};

// Code points (or bytes) of each element, NA as size -1.
class SequenceTable {
public:
    SequenceTable(const StringVector& x, bool use_bytes, const char* name)
        : bytes_(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8) {
        const std::size_t n = bytes_.size();
        seqs_.resize(n);
        if (!use_bytes) {
            points_.reset(new CodepointSnapshot(x, name));
            for (std::size_t i = 0; i < n; ++i) {
                const CodepointView& v = points_->data()[i];
                seqs_[i] = v.is_na() ? Sequence{nullptr, -1}
                                     : Sequence{v.data, static_cast<int>(v.size)};
            }
        } else {
            std::size_t total = 0;
            for (std::size_t i = 0; i < n; ++i)
                if (!bytes_[i].is_na()) total += bytes_[i].size;
            storage_.resize(total);
            std::size_t at = 0;
            for (std::size_t i = 0; i < n; ++i) {
                const StringView& v = bytes_[i];
                if (v.is_na()) {
                    seqs_[i] = Sequence{nullptr, -1};
                    continue;
                }
                for (std::size_t k = 0; k < v.size; ++k)
                    storage_[at + k] = static_cast<unsigned char>(v.data[k]);
                seqs_[i] = Sequence{storage_.data() + at, static_cast<int>(v.size)};
                at += v.size;
            }
        }
    }

    const Sequence& operator[](std::size_t i) const { return seqs_[i]; }
    std::size_t size() const { return seqs_.size(); }
    const StringSnapshot& bytes() const { return bytes_; }

private:
    StringSnapshot bytes_;
    std::unique_ptr<CodepointSnapshot> points_;
    std::vector<cp_t> storage_;
    std::vector<Sequence> seqs_;
};

inline std::uint64_t mix64(std::uint64_t x) {
    x ^= x >> 33;
    x *= 0xff51afd7ed558ccdULL;
    x ^= x >> 33;
    x *= 0xc4ceb9fe1a85ec53ULL;
    x ^= x >> 33;
    return x;
}

inline std::uint64_t hash_sequence(const cp_t* s, int n) {
    std::uint64_t h = 0x9E3779B97F4A7C15ULL ^ static_cast<std::uint64_t>(n);
    for (int i = 0; i < n; ++i) h = mix64(h ^ (s[i] + 0x9E3779B97F4A7C15ULL + (h << 6)));
    return h;
}

// Calls emit(hash) once for every distinct string obtained by deleting up
// to k characters (none included) from s[0, n).
template <typename Emit>
void deletion_variants(const cp_t* s, int n, int k,
                       std::unordered_set<std::uint64_t>& seen,
                       std::vector<cp_t>& work, Emit emit) {
    seen.clear();
    const std::uint64_t own = hash_sequence(s, n);
    seen.insert(own);
    emit(own);
    if (k == 0 || n == 0) return;
    // Breadth first over deletion depth; `level` holds the variants of the
    // current depth, back to back.
    std::vector<cp_t> level(s, s + n), next;
    int length = n;
    for (int depth = 1; depth <= k && length > 0; ++depth) {
        next.clear();
        const std::size_t count = level.size() / static_cast<std::size_t>(length);
        for (std::size_t v = 0; v < count; ++v) {
            const cp_t* word = level.data() + v * static_cast<std::size_t>(length);
            for (int i = 0; i < length; ++i) {
                work.assign(word, word + i);
                work.insert(work.end(), word + i + 1, word + length);
                const std::uint64_t h = hash_sequence(work.data(), length - 1);
                if (!seen.insert(h).second) continue;
                emit(h);
                next.insert(next.end(), work.begin(), work.end());
            }
        }
        level.swap(next);
        --length;
    }
}

struct Hit {
    std::uint32_t query;     // distinct query id
    std::uint32_t target;    // distinct table id
    std::uint32_t distance;
};

int verify_distance(MetricMethod method, const Sequence& a, const Sequence& b,
                    int k, DamerauWorkspace& workspace) {
    switch (method) {
    case MetricMethod::levenshtein:
        return sequence_bounded_levenshtein_distance(a.data, a.size, b.data,
                                                     b.size, k);
    case MetricMethod::osa:
        return sequence_osa_distance(a.data, a.size, b.data, b.size);
    default:
        return damerau_levenshtein_codepoints(a.data, a.size, b.data, b.size,
                                              workspace);
    }
}

struct IndexWorker : public Worker {
    const SequenceTable& table;
    const std::vector<std::size_t>& distinct;  // distinct table rows
    int k;
    int prefix;
    std::vector<std::vector<std::uint64_t>>& chunks;  // per chunk entries
    std::size_t chunk_size;

    IndexWorker(const SequenceTable& table_, const std::vector<std::size_t>& distinct_,
                int k_, int prefix_, std::vector<std::vector<std::uint64_t>>& chunks_,
                std::size_t chunk_size_)
        : table(table_), distinct(distinct_), k(k_), prefix(prefix_),
          chunks(chunks_), chunk_size(chunk_size_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::unordered_set<std::uint64_t> seen;
        std::vector<cp_t> work;
        for (std::size_t c = begin; c < end; ++c) {
            std::vector<std::uint64_t>& out = chunks[c];
            const std::size_t stop = (std::min)(distinct.size(), (c + 1) * chunk_size);
            for (std::size_t d = c * chunk_size; d < stop; ++d) {
                const Sequence& s = table[distinct[d]];
                const int n = (std::min)(s.size, prefix);
                const std::uint64_t id = static_cast<std::uint64_t>(d);
                deletion_variants(s.data, n, k, seen, work,
                                  [&out, id](std::uint64_t h) {
                                      out.push_back((h & 0xFFFFFFFF00000000ULL) | id);
                                  });
            }
        }
    }
};

struct QueryWorker : public Worker {
    const SequenceTable& queries;
    const std::vector<std::size_t>& query_rows;   // distinct query rows
    const SequenceTable& table;
    const std::vector<std::size_t>& table_rows;   // distinct table rows
    const std::vector<std::uint64_t>& index;      // sorted entries
    MetricMethod method;
    int k;
    int prefix;
    std::vector<std::vector<Hit>>& hits;
    std::size_t chunk_size;

    QueryWorker(const SequenceTable& queries_, const std::vector<std::size_t>& query_rows_,
                const SequenceTable& table_, const std::vector<std::size_t>& table_rows_,
                const std::vector<std::uint64_t>& index_, MetricMethod method_, int k_,
                int prefix_, std::vector<std::vector<Hit>>& hits_, std::size_t chunk_size_)
        : queries(queries_), query_rows(query_rows_), table(table_),
          table_rows(table_rows_), index(index_), method(method_), k(k_),
          prefix(prefix_), hits(hits_), chunk_size(chunk_size_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::unordered_set<std::uint64_t> seen;
        std::vector<cp_t> work;
        std::vector<std::uint32_t> stamp(table_rows.size(), 0);
        std::uint32_t current = 0;
        DamerauWorkspace workspace;
        std::vector<std::uint32_t> candidates;
        for (std::size_t c = begin; c < end; ++c) {
            std::vector<Hit>& out = hits[c];
            const std::size_t stop = (std::min)(query_rows.size(), (c + 1) * chunk_size);
            for (std::size_t q = c * chunk_size; q < stop; ++q) {
                const Sequence& s = queries[query_rows[q]];
                ++current;
                candidates.clear();
                const int n = (std::min)(s.size, prefix);
                deletion_variants(
                    s.data, n, k, seen, work,
                    [&](std::uint64_t h) {
                        const std::uint64_t key = h & 0xFFFFFFFF00000000ULL;
                        auto it = std::lower_bound(index.begin(), index.end(), key);
                        for (; it != index.end() && (*it & 0xFFFFFFFF00000000ULL) == key; ++it) {
                            const std::uint32_t id = static_cast<std::uint32_t>(*it & 0xFFFFFFFFULL);
                            if (stamp[id] == current) continue;
                            stamp[id] = current;
                            candidates.push_back(id);
                        }
                    });
                for (std::uint32_t id : candidates) {
                    const Sequence& t = table[table_rows[id]];
                    const int gap = s.size > t.size ? s.size - t.size : t.size - s.size;
                    if (gap > k) continue;
                    const int d = verify_distance(method, s, t, k, workspace);
                    if (d >= 0 && d <= k)
                        out.push_back(Hit{static_cast<std::uint32_t>(q), id,
                                          static_cast<std::uint32_t>(d)});
                }
            }
        }
    }
};

// Distinct non-NA elements of a sequence table: the representative rows, and
// for each distinct id the list of all rows equal to it.
void distinct_rows(const SequenceTable& t, std::vector<std::size_t>& rows,
                   std::vector<std::vector<std::size_t>>& members) {
    struct Key {
        const cp_t* data;
        int size;
        bool operator==(const Key& o) const {
            return size == o.size &&
                (size == 0 || std::memcmp(data, o.data, sizeof(cp_t) * size) == 0);
        }
    };
    struct KeyHash {
        std::size_t operator()(const Key& k) const {
            return static_cast<std::size_t>(hash_sequence(k.data, k.size));
        }
    };
    std::unordered_map<Key, std::size_t, KeyHash> seen;
    seen.reserve(t.size());
    for (std::size_t i = 0; i < t.size(); ++i) {
        const Sequence& s = t[i];
        if (s.size < 0) continue;
        auto ins = seen.emplace(Key{s.data, s.size}, rows.size());
        if (ins.second) {
            rows.push_back(i);
            members.emplace_back();
        }
        members[ins.first->second].push_back(i);
    }
}

}  // namespace

// [[Rcpp::export]]
List fast_fuzzy_within_impl(const StringVector& x, const StringVector& table,
                            int max_distance, int method, int prefix_length,
                            bool use_bytes, int nthreads) {
    if (table.size() >= (std::numeric_limits<std::uint32_t>::max)() ||
        x.size() >= (std::numeric_limits<std::uint32_t>::max)())
        stop("`x` and `table` must have fewer than 2^32 elements.");
    const SequenceTable queries(x, use_bytes, "x"), targets(table, use_bytes, "table");
    std::vector<std::size_t> query_rows, table_rows;
    std::vector<std::vector<std::size_t>> query_members, table_members;
    distinct_rows(queries, query_rows, query_members);
    distinct_rows(targets, table_rows, table_members);

    // Index: (high 32 bits of the variant hash, distinct table id), sorted.
    const std::size_t chunk = 512;
    const std::size_t index_chunks = (table_rows.size() + chunk - 1) / chunk;
    std::vector<std::vector<std::uint64_t>> parts(index_chunks);
    IndexWorker indexer(targets, table_rows, max_distance, prefix_length, parts, chunk);
    dispatch_for(0, index_chunks, indexer, table_rows.size() * 8, 20000, nthreads);
    std::size_t total = 0;
    for (const auto& p : parts) total += p.size();
    std::vector<std::uint64_t> index;
    index.reserve(total);
    for (auto& p : parts) {
        index.insert(index.end(), p.begin(), p.end());
        std::vector<std::uint64_t>().swap(p);
    }
    std::sort(index.begin(), index.end());

    const std::size_t query_chunks = (query_rows.size() + chunk - 1) / chunk;
    std::vector<std::vector<Hit>> hits(query_chunks);
    QueryWorker searcher(queries, query_rows, targets, table_rows, index,
                         static_cast<MetricMethod>(method), max_distance,
                         prefix_length, hits, chunk);
    // Each task allocates one stamp per distinct table string, so keep the
    // tasks few.
    dispatch_for(0, query_chunks, searcher, query_rows.size() * 64, 2000,
                 nthreads, (std::max)(std::size_t(1), query_chunks / 64));

    // Expand distinct queries and targets to rows, in (query, distance,
    // table) order.
    std::vector<Hit> all;
    for (auto& h : hits) all.insert(all.end(), h.begin(), h.end());
    std::size_t rows = 0;
    for (const Hit& h : all)
        rows += query_members[h.query].size() * table_members[h.target].size();
    if (rows > static_cast<std::size_t>((std::numeric_limits<int>::max)()))
        stop("Too many matches to return.");
    struct Row { std::size_t query, target; std::uint32_t distance; };
    std::vector<Row> out;
    out.reserve(rows);
    for (const Hit& h : all)
        for (std::size_t qi : query_members[h.query])
            for (std::size_t ti : table_members[h.target])
                out.push_back(Row{qi, ti, h.distance});
    std::sort(out.begin(), out.end(), [](const Row& a, const Row& b) {
        if (a.query != b.query) return a.query < b.query;
        if (a.distance != b.distance) return a.distance < b.distance;
        return a.target < b.target;
    });
    IntegerVector qi(out.size()), ti(out.size()), dist(out.size());
    for (std::size_t r = 0; r < out.size(); ++r) {
        qi[r] = static_cast<int>(out[r].query + 1);
        ti[r] = static_cast<int>(out[r].target + 1);
        dist[r] = static_cast<int>(out[r].distance);
    }
    return List::create(_["query_index"] = qi, _["table_index"] = ti,
                        _["distance"] = dist);
}
