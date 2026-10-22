// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "codepoint_snapshot.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// tfidf_top_n(): cosine similarity of TF-IDF weighted character n-gram
// vectors, as in string_grouper / sparse_dot_topn (ING) and PolyFuzz's
// default model. The IDF comes from the table alone, with scikit-learn's
// smoothing, idf = ln((1 + N) / (1 + df)) + 1; term frequencies are raw
// counts and vectors are L2-normalised. The table is indexed by n-gram
// (an inverted index), so each query only meets the table entries it
// shares an n-gram with.

namespace {

typedef std::uint32_t cp_t;

// A string's characters: code points, or bytes.
class Texts {
public:
    Texts(const StringVector& x, bool use_bytes, const char* name)
        : bytes_(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8),
          use_bytes_(use_bytes) {
        if (!use_bytes) points_.reset(new CodepointSnapshot(x, name));
    }

    bool missing(std::size_t i) const { return bytes_[i].is_na(); }
    std::size_t size() const { return bytes_.size(); }

    const cp_t* get(std::size_t i, std::vector<cp_t>& buffer, int& n) const {
        if (!use_bytes_) {
            n = static_cast<int>(points_->data()[i].size);
            return points_->data()[i].data;
        }
        const StringView& v = bytes_[i];
        buffer.resize(v.size);
        for (std::size_t k = 0; k < v.size; ++k)
            buffer[k] = static_cast<unsigned char>(v.data[k]);
        n = static_cast<int>(v.size);
        return buffer.data();
    }

private:
    StringSnapshot bytes_;
    std::unique_ptr<CodepointSnapshot> points_;
    bool use_bytes_;
};

// n-gram keys. Up to three characters pack exactly into 64 bits (shorter
// grams padded with a value no character takes); longer ones are byte
// strings of their code units.
struct GramKeys {
    int q;

    bool packed() const { return q <= 3; }

    std::uint64_t pack(const cp_t* s, int n) const {
        std::uint64_t key = 0;
        for (int i = 0; i < 3; ++i) {
            const std::uint64_t c = i < n ? s[i] : 0x1FFFFFu;
            key |= c << (21 * i);
        }
        return key;
    }

    std::string text(const cp_t* s, int n) const {
        return std::string(reinterpret_cast<const char*>(s),
                           sizeof(cp_t) * static_cast<std::size_t>(n));
    }

    // Calls f(gram, length) for each n-gram of s; a non-empty string
    // shorter than q is one gram.
    template <typename F>
    void each(const cp_t* s, int n, F f) const {
        if (n == 0) return;
        if (n < q) {
            f(s, n);
            return;
        }
        for (int i = 0; i + q <= n; ++i) f(s + i, q);
    }
};

class Vocabulary {
public:
    explicit Vocabulary(GramKeys keys) : keys_(keys) {}

    int intern(const cp_t* g, int n) {
        if (keys_.packed()) {
            auto ins = packed_.emplace(keys_.pack(g, n), static_cast<int>(packed_.size() + text_.size()));
            return ins.first->second;
        }
        auto ins = text_.emplace(keys_.text(g, n), static_cast<int>(packed_.size() + text_.size()));
        return ins.first->second;
    }

    // -1 when the gram never occurs in the table.
    int find(const cp_t* g, int n) const {
        if (keys_.packed()) {
            auto it = packed_.find(keys_.pack(g, n));
            return it == packed_.end() ? -1 : it->second;
        }
        auto it = text_.find(keys_.text(g, n));
        return it == text_.end() ? -1 : it->second;
    }

    std::size_t size() const { return packed_.size() + text_.size(); }

private:
    GramKeys keys_;
    std::unordered_map<std::uint64_t, int> packed_;
    std::unordered_map<std::string, int> text_;
};

struct Term {
    int id;  // vocabulary id, or -1 for a gram the table lacks
    double weight;
};

// The L2-normalised TF-IDF vector of one string.
void tfidf_vector(const cp_t* s, int n, const GramKeys& keys,
                  const Vocabulary& vocabulary, const std::vector<double>& idf,
                  double unseen_idf, std::vector<Term>& out,
                  std::vector<std::pair<int, std::uint64_t>>& scratch) {
    // Count each gram: known grams by id, unknown ones by packed key or
    // hash of their text (only their count matters, for the norm).
    scratch.clear();
    keys.each(s, n, [&](const cp_t* g, int len) {
        const int id = vocabulary.find(g, len);
        std::uint64_t key;
        if (id >= 0) {
            key = static_cast<std::uint64_t>(id);
        } else if (keys.packed()) {
            key = keys.pack(g, len);
        } else {
            key = std::hash<std::string>()(keys.text(g, len));
        }
        scratch.push_back(std::make_pair(id, key));
    });
    std::sort(scratch.begin(), scratch.end());
    out.clear();
    double norm = 0.0;
    for (std::size_t i = 0; i < scratch.size();) {
        std::size_t j = i;
        while (j < scratch.size() && scratch[j] == scratch[i]) ++j;
        const double tf = static_cast<double>(j - i);
        const int id = scratch[i].first;
        const double w = tf * (id >= 0 ? idf[static_cast<std::size_t>(id)] : unseen_idf);
        norm += w * w;
        out.push_back(Term{id, w});
        i = j;
    }
    if (norm > 0) {
        const double scale = 1.0 / std::sqrt(norm);
        for (Term& t : out) t.weight *= scale;
    }
}

struct Posting {
    std::uint32_t row;
    double weight;
};

struct Candidate {
    double score;
    std::uint32_t row;
};

struct QueryWorker : public Worker {
    const Texts& queries;
    const GramKeys& keys;
    const Vocabulary& vocabulary;
    const std::vector<double>& idf;
    double unseen_idf;
    const std::vector<std::size_t>& offsets;  // postings of gram id g
    const std::vector<Posting>& postings;
    std::size_t table_size;
    std::size_t top_n;
    double min_score;
    int* index_out;
    double* score_out;
    std::size_t query_count;

    QueryWorker(const Texts& queries_, const GramKeys& keys_,
                const Vocabulary& vocabulary_, const std::vector<double>& idf_,
                double unseen_idf_, const std::vector<std::size_t>& offsets_,
                const std::vector<Posting>& postings_, std::size_t table_size_,
                std::size_t top_n_, double min_score_, int* index_out_,
                double* score_out_, std::size_t query_count_)
        : queries(queries_), keys(keys_), vocabulary(vocabulary_), idf(idf_),
          unseen_idf(unseen_idf_), offsets(offsets_), postings(postings_),
          table_size(table_size_), top_n(top_n_), min_score(min_score_),
          index_out(index_out_), score_out(score_out_),
          query_count(query_count_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<double> accumulator(table_size, 0.0);
        std::vector<std::uint32_t> touched;
        std::vector<Term> terms;
        std::vector<std::pair<int, std::uint64_t>> scratch;
        std::vector<cp_t> buffer;
        std::vector<Candidate> best;
        for (std::size_t q = begin; q < end; ++q) {
            if (queries.missing(q)) continue;
            int n;
            const cp_t* s = queries.get(q, buffer, n);
            tfidf_vector(s, n, keys, vocabulary, idf, unseen_idf, terms, scratch);
            touched.clear();
            for (const Term& t : terms) {
                if (t.id < 0) continue;
                const std::size_t g = static_cast<std::size_t>(t.id);
                for (std::size_t p = offsets[g]; p < offsets[g + 1]; ++p) {
                    const Posting& post = postings[p];
                    if (accumulator[post.row] == 0.0) touched.push_back(post.row);
                    accumulator[post.row] += t.weight * post.weight;
                }
            }
            best.clear();
            for (std::uint32_t row : touched) {
                // Rounding can push a perfect match just past 1.
                const double score = (std::min)(1.0, accumulator[row]);
                accumulator[row] = 0.0;
                if (score > 0 && score >= min_score) best.push_back(Candidate{score, row});
            }
            auto better = [](const Candidate& a, const Candidate& b) {
                return a.score > b.score || (a.score == b.score && a.row < b.row);
            };
            if (best.size() > top_n) {
                std::partial_sort(best.begin(), best.begin() + top_n, best.end(), better);
                best.resize(top_n);
            } else {
                std::sort(best.begin(), best.end(), better);
            }
            for (std::size_t r = 0; r < best.size(); ++r) {
                index_out[q + r * query_count] = static_cast<int>(best[r].row) + 1;
                score_out[q + r * query_count] = best[r].score;
            }
        }
    }
};

}  // namespace

// [[Rcpp::export]]
List fast_tfidf_top_n_impl(const StringVector& x, const StringVector& table,
                           int top_n, int q, double min_score, bool use_bytes,
                           int nthreads) {
    if (table.size() >= (std::numeric_limits<std::int32_t>::max)())
        stop("`table` is too long.");
    const Texts queries(x, use_bytes, "x"), targets(table, use_bytes, "table");
    const GramKeys keys{q};
    Vocabulary vocabulary(keys);

    // Pass 1: intern every table gram, per row (id, count).
    const std::size_t rows = targets.size();
    std::vector<std::vector<std::pair<int, int>>> row_terms(rows);
    std::vector<int> df;
    std::size_t documents = 0;
    std::vector<cp_t> buffer;
    std::vector<int> ids;
    for (std::size_t i = 0; i < rows; ++i) {
        if (targets.missing(i)) continue;
        ++documents;
        int n;
        const cp_t* s = targets.get(i, buffer, n);
        ids.clear();
        keys.each(s, n, [&](const cp_t* g, int len) { ids.push_back(vocabulary.intern(g, len)); });
        std::sort(ids.begin(), ids.end());
        for (std::size_t a = 0; a < ids.size();) {
            std::size_t b = a;
            while (b < ids.size() && ids[b] == ids[a]) ++b;
            if (static_cast<std::size_t>(ids[a]) >= df.size()) df.resize(ids[a] + 1, 0);
            ++df[static_cast<std::size_t>(ids[a])];
            row_terms[i].push_back(std::make_pair(ids[a], static_cast<int>(b - a)));
            a = b;
        }
    }
    const std::size_t grams = vocabulary.size();
    df.resize(grams, 0);
    const double total = static_cast<double>(documents);
    std::vector<double> idf(grams);
    for (std::size_t g = 0; g < grams; ++g)
        idf[g] = std::log((1.0 + total) / (1.0 + df[g])) + 1.0;
    const double unseen_idf = std::log(1.0 + total) + 1.0;

    // Pass 2: normalised weights into postings grouped by gram.
    std::vector<std::size_t> offsets(grams + 1, 0);
    for (std::size_t i = 0; i < rows; ++i)
        for (const auto& t : row_terms[i]) ++offsets[static_cast<std::size_t>(t.first) + 1];
    for (std::size_t g = 0; g < grams; ++g) offsets[g + 1] += offsets[g];
    std::vector<Posting> postings(offsets[grams]);
    {
        std::vector<std::size_t> fill(offsets.begin(), offsets.end() - 1);
        for (std::size_t i = 0; i < rows; ++i) {
            double norm = 0.0;
            for (const auto& t : row_terms[i]) {
                const double w = t.second * idf[static_cast<std::size_t>(t.first)];
                norm += w * w;
            }
            const double scale = norm > 0 ? 1.0 / std::sqrt(norm) : 0.0;
            for (const auto& t : row_terms[i]) {
                const std::size_t g = static_cast<std::size_t>(t.first);
                postings[fill[g]++] = Posting{
                    static_cast<std::uint32_t>(i),
                    t.second * idf[g] * scale};
            }
            std::vector<std::pair<int, int>>().swap(row_terms[i]);
        }
    }

    const std::size_t query_count = queries.size();
    IntegerMatrix index(x.size(), top_n);
    NumericMatrix score(x.size(), top_n);
    std::fill(INTEGER(index), INTEGER(index) + index.size(), NA_INTEGER);
    std::fill(REAL(score), REAL(score) + score.size(), NA_REAL);
    QueryWorker worker(queries, keys, vocabulary, idf, unseen_idf, offsets,
                       postings, rows, static_cast<std::size_t>(top_n), min_score,
                       INTEGER(index), REAL(score), query_count);
    // Each task allocates an accumulator over the table, so keep tasks few.
    dispatch_for(0, query_count, worker, query_count * 256, 2000, nthreads,
                 (std::max)(std::size_t(64), query_count / 64));
    return List::create(_["index"] = index, _["score"] = score);
}
