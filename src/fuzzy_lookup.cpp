// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
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

struct FuzzyLookupWorker : public Worker {
    const StringView* query_bytes;
    const StringView* table_bytes;
    const CodepointView* query_codepoints;
    const CodepointView* table_codepoints;
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
    int* indices;
    double* scores;

    FuzzyLookupWorker(
            const StringView* query_bytes_, const StringView* table_bytes_,
            const CodepointView* query_codepoints_,
            const CodepointView* table_codepoints_,
            std::size_t query_count_, std::size_t table_count_,
            MetricMethod method_, double p_, double min_score_,
            int max_distance_, bool match_na_, int first_table_na_,
            bool use_bytes_, std::size_t top_n_,
            int* indices_, double* scores_)
        : query_bytes(query_bytes_), table_bytes(table_bytes_),
          query_codepoints(query_codepoints_),
          table_codepoints(table_codepoints_),
          query_count(query_count_), table_count(table_count_),
          method(method_), p(p_), min_score(min_score_),
          max_distance(max_distance_), match_na(match_na_),
          first_table_na(first_table_na_), use_bytes(use_bytes_),
          top_n(top_n_), indices(indices_), scores(scores_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<Candidate> best;
        best.reserve(top_n + 1);
        std::vector<int> matrix;
        std::unordered_map<char, int> byte_last_row;
        std::unordered_map<std::uint32_t, int> codepoint_last_row;
        for (std::size_t query = begin; query < end; ++query) {
            best.clear();
            if (query_bytes[query].is_na()) {
                if (match_na && first_table_na >= 0) {
                    best.push_back(Candidate{1.0, first_table_na});
                }
                write_result(query, best);
                continue;
            }

            for (std::size_t choice = 0; choice < table_count; ++choice) {
                if (table_bytes[choice].is_na()) continue;
                const bool byte_path = use_bytes ||
                    (query_codepoints[query].ascii &&
                     table_codepoints[choice].ascii);
                double score;
                if (method == MetricMethod::jaro_winkler) {
                    score = byte_path
                        ? jaro_winkler_sim(
                            query_bytes[query].data,
                            static_cast<int>(query_bytes[query].size),
                            table_bytes[choice].data,
                            static_cast<int>(table_bytes[choice].size), p
                        )
                        : sequence_jaro_winkler_similarity(
                            query_codepoints[query].data,
                            static_cast<int>(query_codepoints[query].size),
                            table_codepoints[choice].data,
                            static_cast<int>(table_codepoints[choice].size), p
                        );
                } else {
                    const int length_query = byte_path
                        ? static_cast<int>(query_bytes[query].size)
                        : static_cast<int>(query_codepoints[query].size);
                    const int length_choice = byte_path
                        ? static_cast<int>(table_bytes[choice].size)
                        : static_cast<int>(table_codepoints[choice].size);
                    int cutoff = max_distance;
                    double required_score = min_score;
                    if (best.size() == top_n)
                        required_score = std::max(required_score, best.back().score);
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
                        distance = metric_distance_bytes_with_workspace(
                            method, query_bytes[query], table_bytes[choice],
                            matrix, byte_last_row
                        );
                    } else {
                        distance = metric_distance_codepoints_with_workspace(
                            method, query_codepoints[query],
                            table_codepoints[choice], matrix,
                            codepoint_last_row
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
    StringSnapshot query_bytes(x), table_bytes(table);
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

    IntegerMatrix indices(x.size(), top_n);
    NumericMatrix scores(x.size(), top_n);
    std::fill(INTEGER(indices), INTEGER(indices) + indices.size(), NA_INTEGER);
    std::fill(REAL(scores), REAL(scores) + scores.size(), NA_REAL);
    FuzzyLookupWorker worker(
        query_bytes.data(), table_bytes.data(),
        use_bytes ? nullptr : query_codepoints->data(),
        use_bytes ? nullptr : table_codepoints->data(),
        query_count, table_count, selected, p, min_score, max_distance,
        match_na, first_table_na, use_bytes,
        static_cast<std::size_t>(top_n), INTEGER(indices), REAL(scores)
    );

    std::size_t cells = query_count;
    if (table_count != 0 &&
        query_count <= (std::numeric_limits<std::size_t>::max)() / table_count)
        cells = query_count * table_count;
    else if (table_count != 0)
        cells = (std::numeric_limits<std::size_t>::max)();
    dispatch_for(
        0, query_count, worker,
        estimated_matrix_string_work(query_bytes, table_bytes, cells),
        10000, nthreads, 1
    );
    return List::create(_["index"] = indices, _["score"] = scores);
}
