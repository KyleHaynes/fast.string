// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <unordered_map>
#include <vector>
#include "codepoint_snapshot.h"
#include "metric_dispatch.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

namespace {

struct Candidate {
    double score;
    int index;
};

static inline bool candidate_better(const Candidate& left,
                                    const Candidate& right) {
    return left.score > right.score ||
        (left.score == right.score && left.index < right.index);
}

// The highest Jaro-Winkler score two strings of these lengths could reach:
// every character of the shorter one matched, no transpositions, and the
// longest possible common prefix. A candidate whose bound cannot beat the
// cutoff is skipped without being scored.
static inline double jaro_winkler_bound(int la, int lb, double p) {
    if (la == 0 || lb == 0) return la == lb ? 1.0 : 0.0;
    const int shorter = (std::min)(la, lb);
    const int longer = (std::max)(la, lb);
    const double jaro =
        (2.0 + static_cast<double>(shorter) / static_cast<double>(longer)) / 3.0;
    return jaro + (std::min)(4, shorter) * p * (1.0 - jaro);
}

// Byte strings as hash-map keys, pointing into a snapshot that outlives the
// map.
struct TextKey {
    const char* data;
    std::size_t size;

    bool operator==(const TextKey& other) const {
        return size == other.size &&
            (size == 0 || std::memcmp(data, other.data, size) == 0);
    }
};

struct TextKeyHash {
    std::size_t operator()(const TextKey& key) const {
        std::uint64_t hash = 1469598103934665603ULL;  // FNV-1a
        for (std::size_t i = 0; i < key.size; ++i) {
            hash ^= static_cast<unsigned char>(key.data[i]);
            hash *= 1099511628211ULL;
        }
        return static_cast<std::size_t>(hash);
    }
};

typedef std::unordered_map<TextKey, int, TextKeyHash> TextIndex;

struct FuzzyLookupWorker : public Worker {
    const StringView* query_bytes;
    const StringView* table_bytes;
    const CodepointView* query_codepoints;
    const CodepointView* table_codepoints;
    // Distinct queries to score; duplicates are copied afterwards.
    const std::size_t* queries;
    std::size_t query_count;
    std::size_t table_count;
    MetricMethod method;
    double p;
    double min_score;
    int max_distance;
    bool match_na;
    int first_table_na;
    bool use_bytes;
    std::size_t top_n;
    // For top_n == 1: the first table position of every distinct string.
    // An identical string is the only way to score 1, so it is the answer.
    const TextIndex* exact;
    int* indices;
    double* scores;

    FuzzyLookupWorker(
            const StringView* query_bytes_, const StringView* table_bytes_,
            const CodepointView* query_codepoints_,
            const CodepointView* table_codepoints_,
            const std::size_t* queries_,
            std::size_t query_count_, std::size_t table_count_,
            MetricMethod method_, double p_, double min_score_,
            int max_distance_, bool match_na_, int first_table_na_,
            bool use_bytes_, std::size_t top_n_, const TextIndex* exact_,
            int* indices_, double* scores_)
        : query_bytes(query_bytes_), table_bytes(table_bytes_),
          query_codepoints(query_codepoints_),
          table_codepoints(table_codepoints_), queries(queries_),
          query_count(query_count_), table_count(table_count_),
          method(method_), p(p_), min_score(min_score_),
          max_distance(max_distance_), match_na(match_na_),
          first_table_na(first_table_na_), use_bytes(use_bytes_),
          top_n(top_n_), exact(exact_), indices(indices_), scores(scores_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<Candidate> best;
        best.reserve(top_n + 1);
        DamerauWorkspace workspace;
        JaroScratch jaro_scratch;
        for (std::size_t k = begin; k < end; ++k) {
            const std::size_t query = queries[k];
            best.clear();
            if (query_bytes[query].is_na()) {
                if (match_na && first_table_na >= 0) {
                    best.push_back(Candidate{1.0, first_table_na});
                }
                write_result(query, best);
                continue;
            }
            if (exact) {
                const auto hit = exact->find(TextKey{
                    query_bytes[query].data, query_bytes[query].size
                });
                if (hit != exact->end()) {
                    best.push_back(Candidate{1.0, hit->second});
                    write_result(query, best);
                    continue;
                }
            }

            for (std::size_t choice = 0; choice < table_count; ++choice) {
                if (table_bytes[choice].is_na()) continue;
                const bool byte_path = use_bytes ||
                    (query_codepoints[query].ascii &&
                     table_codepoints[choice].ascii);
                const int length_query = byte_path
                    ? static_cast<int>(query_bytes[query].size)
                    : static_cast<int>(query_codepoints[query].size);
                const int length_choice = byte_path
                    ? static_cast<int>(table_bytes[choice].size)
                    : static_cast<int>(table_codepoints[choice].size);
                double required_score = min_score;
                if (best.size() == top_n)
                    required_score = std::max(required_score, best.back().score);
                double score;
                if (method == MetricMethod::jaro_winkler) {
                    // Later candidates lose ties, so a bound equal to a full
                    // list's last score cannot get in either. The margin
                    // keeps a rounding difference from skipping a candidate
                    // that does reach the cutoff.
                    const double bound =
                        jaro_winkler_bound(length_query, length_choice, p);
                    if (bound + 1e-12 < min_score ||
                        (best.size() == top_n &&
                         bound + 1e-12 <= best.back().score))
                        continue;
                    score = byte_path
                        ? jaro_winkler_sim(
                            query_bytes[query].data, length_query,
                            table_bytes[choice].data, length_choice, p,
                            jaro_scratch
                        )
                        : sequence_jaro_winkler_similarity(
                            query_codepoints[query].data, length_query,
                            table_codepoints[choice].data, length_choice, p
                        );
                } else {
                    int cutoff = max_distance;
                    const int denominator = std::max(length_query, length_choice);
                    const int score_cutoff = denominator == 0
                        ? 0
                        : static_cast<int>(std::floor(
                            (1.0 - required_score) * denominator + 1e-12
                        ));
                    cutoff = cutoff < 0
                        ? score_cutoff
                        : std::min(cutoff, score_cutoff);
                    if (std::abs(length_query - length_choice) > cutoff)
                        continue;

                    int distance;
                    if (method == MetricMethod::levenshtein) {
                        distance = byte_path
                            ? sequence_bounded_levenshtein_distance(
                                query_bytes[query].data, length_query,
                                table_bytes[choice].data, length_choice, cutoff
                            )
                            : sequence_bounded_levenshtein_distance(
                                query_codepoints[query].data, length_query,
                                table_codepoints[choice].data, length_choice,
                                cutoff
                            );
                        if (distance > cutoff) continue;
                    } else if (byte_path) {
                        distance = metric_distance_bytes(
                            method, query_bytes[query], table_bytes[choice]
                        );
                    } else {
                        distance = metric_distance_codepoints_with_workspace(
                            method, query_codepoints[query],
                            table_codepoints[choice], workspace
                        );
                    }
                    if (distance < 0 || distance > cutoff) continue;
                    score = normalized_edit_similarity(
                        distance, length_query, length_choice
                    );
                }

                if (score < min_score) continue;
                const Candidate candidate{
                    score, static_cast<int>(choice + 1)
                };
                const auto position = std::lower_bound(
                    best.begin(), best.end(), candidate,
                    [](const Candidate& existing, const Candidate& value) {
                        return candidate_better(existing, value);
                    }
                );
                if (position == best.end() && best.size() == top_n) continue;
                best.insert(position, candidate);
                if (best.size() > top_n) best.pop_back();
                if (top_n == 1 && score == 1.0) break;
            }
            write_result(query, best);
        }
    }

    void write_result(std::size_t query,
                      const std::vector<Candidate>& best) const {
        for (std::size_t rank = 0; rank < best.size(); ++rank) {
            const std::size_t cell = query + rank * query_count;
            indices[cell] = best[rank].index;
            scores[cell] = best[rank].score;
        }
    }
};

} // namespace

// [[Rcpp::export]]
List fast_fuzzy_top_n_impl(const StringVector& x,
                           const StringVector& table,
                           int method, int top_n, double p,
                           double min_score, int max_distance,
                           bool match_na, int nthreads,
                           bool use_bytes) {
    if (table.size() > (std::numeric_limits<int>::max)())
        stop("`table` is too long for integer match indices.");
    const MetricMethod selected = static_cast<MetricMethod>(method);
    if (selected < MetricMethod::jaro_winkler ||
        selected > MetricMethod::damerau_levenshtein)
        stop("Invalid fuzzy matching method.");

    const std::size_t query_count = static_cast<std::size_t>(x.size());
    const std::size_t table_count = static_cast<std::size_t>(table.size());
    // Without use_bytes the byte views are UTF-8 (latin1 translated), so two
    // strings are byte-identical exactly when their code points are.
    const SnapshotText text =
        use_bytes ? SnapshotText::raw : SnapshotText::utf8;
    StringSnapshot query_bytes(x, text), table_bytes(table, text);
    std::unique_ptr<CodepointSnapshot> query_codepoints, table_codepoints;
    if (!use_bytes) {
        query_codepoints.reset(new CodepointSnapshot(x, "x"));
        table_codepoints.reset(new CodepointSnapshot(table, "table"));
    }

    int first_table_na = -1;
    if (match_na) {
        for (std::size_t i = 0; i < table_count; ++i) {
            if (table_bytes[i].is_na()) {
                first_table_na = static_cast<int>(i + 1);
                break;
            }
        }
    }

    // Score each distinct query once. `representative[i]` is the first
    // query equal to query i (all NA queries share one).
    std::vector<std::size_t> representative(query_count);
    std::vector<std::size_t> distinct;
    distinct.reserve(query_count);
    {
        TextIndex seen;
        seen.reserve(query_count);
        std::size_t first_na = query_count;
        for (std::size_t i = 0; i < query_count; ++i) {
            const StringView& value = query_bytes[i];
            if (value.is_na()) {
                if (first_na == query_count) {
                    first_na = i;
                    distinct.push_back(i);
                }
                representative[i] = first_na;
                continue;
            }
            const auto inserted = seen.emplace(
                TextKey{value.data, value.size}, static_cast<int>(i)
            );
            if (inserted.second) distinct.push_back(i);
            representative[i] = static_cast<std::size_t>(inserted.first->second);
        }
    }

    std::unique_ptr<TextIndex> exact;
    if (top_n == 1) {
        exact.reset(new TextIndex());
        exact->reserve(table_count);
        for (std::size_t i = 0; i < table_count; ++i) {
            const StringView& value = table_bytes[i];
            if (!value.is_na())
                exact->emplace(TextKey{value.data, value.size},
                               static_cast<int>(i + 1));
        }
    }

    IntegerMatrix indices(x.size(), top_n);
    NumericMatrix scores(x.size(), top_n);
    std::fill(INTEGER(indices), INTEGER(indices) + indices.size(), NA_INTEGER);
    std::fill(REAL(scores), REAL(scores) + scores.size(), NA_REAL);
    FuzzyLookupWorker worker(
        query_bytes.data(), table_bytes.data(),
        use_bytes ? nullptr : query_codepoints->data(),
        use_bytes ? nullptr : table_codepoints->data(),
        distinct.data(), query_count, table_count, selected, p, min_score,
        max_distance, match_na, first_table_na, use_bytes,
        static_cast<std::size_t>(top_n), exact.get(),
        INTEGER(indices), REAL(scores)
    );

    std::size_t cells = distinct.size();
    if (table_count != 0 &&
        distinct.size() <= (std::numeric_limits<std::size_t>::max)() / table_count)
        cells = distinct.size() * table_count;
    else if (table_count != 0)
        cells = (std::numeric_limits<std::size_t>::max)();
    dispatch_for(
        0, distinct.size(), worker,
        estimated_matrix_string_work(query_bytes, table_bytes, cells),
        10000, nthreads, 1
    );

    int* index_cells = INTEGER(indices);
    double* score_cells = REAL(scores);
    for (std::size_t i = 0; i < query_count; ++i) {
        const std::size_t source = representative[i];
        if (source == i) continue;
        for (int rank = 0; rank < top_n; ++rank) {
            const std::size_t offset =
                static_cast<std::size_t>(rank) * query_count;
            index_cells[i + offset] = index_cells[source + offset];
            score_cells[i + offset] = score_cells[source + offset];
        }
    }
    return List::create(_["index"] = indices, _["score"] = scores);
}
