// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <vector>
#include "parallel_dispatch.h"
#include "rapidfuzz_core.h"
#include "string_snapshot.h"
#include "text_results.h"
using namespace Rcpp;
using namespace RcppParallel;

// R entry points for the RapidFuzz ports in rapidfuzz_core.h: the pairwise
// scorers (cpdist), partial_ratio_alignment, utils.default_process, and the
// process module's cdist and extract.

namespace {

RfScorer scorer_from_id(int id) {
    if (id < 0 || id >= RF_SCORER_COUNT) stop("Unknown fuzz scorer.");
    return static_cast<RfScorer>(id);
}

// ---------------------------------------------------------------------------
// Pairwise scores
// ---------------------------------------------------------------------------

struct FuzzPairWorker : public Worker {
    const StringView* a;
    const StringView* b;
    RfScorer scorer;
    bool full_process;
    double score_cutoff;
    double* out;

    FuzzPairWorker(const StringView* a_, const StringView* b_, RfScorer scorer_,
                   bool full_process_, double score_cutoff_, double* out_)
        : a(a_), b(b_), scorer(scorer_), full_process(full_process_),
          score_cutoff(score_cutoff_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        RfScratch scratch;
        std::vector<rf_char> buffer_a, buffer_b;
        for (std::size_t i = begin; i < end; ++i) {
            if (a[i].is_na() || b[i].is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            const RfText x = rf_prepare(a[i].data, a[i].size, full_process, buffer_a);
            const RfText y = rf_prepare(b[i].data, b[i].size, full_process, buffer_b);
            out[i] = rf_score(scorer, x, y, score_cutoff, scratch);
        }
    }
};

struct FuzzAlignmentWorker : public Worker {
    const StringView* a;
    const StringView* b;
    bool full_process;
    double score_cutoff;
    double* score;
    int* src_start;
    int* src_end;
    int* dest_start;
    int* dest_end;

    FuzzAlignmentWorker(const StringView* a_, const StringView* b_,
                        bool full_process_, double score_cutoff_,
                        double* score_, int* src_start_, int* src_end_,
                        int* dest_start_, int* dest_end_)
        : a(a_), b(b_), full_process(full_process_),
          score_cutoff(score_cutoff_), score(score_), src_start(src_start_),
          src_end(src_end_), dest_start(dest_start_), dest_end(dest_end_) {}

    void operator()(std::size_t begin, std::size_t end) {
        RfScratch scratch;
        std::vector<rf_char> buffer_a, buffer_b;
        for (std::size_t i = begin; i < end; ++i) {
            if (a[i].is_na() || b[i].is_na()) {
                score[i] = NA_REAL;
                src_start[i] = src_end[i] = dest_start[i] = dest_end[i] =
                    NA_INTEGER;
                continue;
            }
            const RfText x = rf_prepare(a[i].data, a[i].size, full_process, buffer_a);
            const RfText y = rf_prepare(b[i].data, b[i].size, full_process, buffer_b);
            const RfAlignment alignment = rf_partial_ratio_alignment(
                x.data, x.size, y.data, y.size, score_cutoff, scratch
            );
            // RapidFuzz returns None below the cutoff: score 0, no positions.
            if (alignment.score < score_cutoff) {
                score[i] = 0.0;
                src_start[i] = src_end[i] = dest_start[i] = dest_end[i] =
                    NA_INTEGER;
                continue;
            }
            // 1-based starts and inclusive ends, for substr().
            score[i] = alignment.score;
            src_start[i] = alignment.src_start + 1;
            src_end[i] = alignment.src_end;
            dest_start[i] = alignment.dest_start + 1;
            dest_end[i] = alignment.dest_end;
        }
    }
};

// ---------------------------------------------------------------------------
// utils.default_process
// ---------------------------------------------------------------------------

static void encode_utf8(const RfText& text, std::string& out) {
    out.clear();
    for (int i = 0; i < text.size; ++i) {
        const rf_char c = text.data[i];
        if (c < 0x80) {
            out.push_back(static_cast<char>(c));
        } else if (c < 0x800) {
            out.push_back(static_cast<char>(0xC0 | (c >> 6)));
            out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
        } else if (c < 0x10000) {
            out.push_back(static_cast<char>(0xE0 | (c >> 12)));
            out.push_back(static_cast<char>(0x80 | ((c >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
        } else {
            out.push_back(static_cast<char>(0xF0 | (c >> 18)));
            out.push_back(static_cast<char>(0x80 | ((c >> 12) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | ((c >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
        }
    }
}

struct DefaultProcessWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    DefaultProcessWorker(const StringView* strings_,
                         const std::vector<TextWorkChunk>& chunks_,
                         std::vector<TextResult>& results_, TextArenas& arenas_)
        : strings(strings_), chunks(chunks_), results(results_),
          arenas(arenas_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<rf_char> points;
        std::string utf8;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t row = chunks[chunk].begin; row < chunks[chunk].end;
                 ++row) {
                const StringView& view = strings[row];
                if (view.is_na()) continue;
                const RfText text = rf_prepare(view.data, view.size, true, points);
                encode_utf8(text, utf8);
                store_text_output(row, chunk, view, utf8.data(), utf8.size(),
                                  results, arenas);
            }
        }
    }
};

// ---------------------------------------------------------------------------
// Prepared corpora for cdist and extract
// ---------------------------------------------------------------------------

// Every string of a vector decoded (and optionally default_process-ed) once
// into one arena, so that the all-pairs functions do not redo it for every
// pair. A string's slot is sized by its byte count, which bounds its number
// of code points.
class FuzzCorpus {
public:
    FuzzCorpus(const StringVector& x, bool full_process, int nthreads)
        : snapshot_(x, SnapshotText::utf8), size_(snapshot_.size()),
          offsets_(size_), lengths_(size_), wide_(size_) {
        std::size_t total = 0;
        for (std::size_t i = 0; i < size_; ++i) {
            offsets_[i] = total;
            if (!snapshot_[i].is_na()) total += snapshot_[i].size;
        }
        arena_.reset(new rf_char[total == 0 ? 1 : total]);
        PrepareWorker worker(snapshot_.data(), full_process, arena_.get(),
                             offsets_.data(), lengths_.data(), wide_.data());
        dispatch_for(0, size_, worker, total / 4 + size_, 100000, nthreads, 256);
    }

    std::size_t size() const { return size_; }
    bool is_na(std::size_t i) const { return snapshot_[i].is_na(); }
    RfText text(std::size_t i) const {
        return RfText{arena_.get() + offsets_[i], lengths_[i], wide_[i] != 0};
    }
    const StringSnapshot& snapshot() const { return snapshot_; }

private:
    struct PrepareWorker : public Worker {
        const StringView* strings;
        bool full_process;
        rf_char* arena;
        const std::size_t* offsets;
        int* lengths;
        std::uint8_t* wide;

        PrepareWorker(const StringView* strings_, bool full_process_,
                      rf_char* arena_, const std::size_t* offsets_,
                      int* lengths_, std::uint8_t* wide_)
            : strings(strings_), full_process(full_process_), arena(arena_),
              offsets(offsets_), lengths(lengths_), wide(wide_) {}

        void operator()(std::size_t begin, std::size_t end) {
            for (std::size_t i = begin; i < end; ++i) {
                lengths[i] = 0;
                wide[i] = 0;
                if (strings[i].is_na()) continue;
                bool is_wide = false;
                lengths[i] = rf_prepare_into(strings[i].data, strings[i].size,
                                             full_process, arena + offsets[i],
                                             is_wide);
                wide[i] = is_wide;
            }
        }
    };

    StringSnapshot snapshot_;
    std::size_t size_;
    std::vector<std::size_t> offsets_;
    std::vector<int> lengths_;
    std::vector<std::uint8_t> wide_;
    std::unique_ptr<rf_char[]> arena_;
};

// ---------------------------------------------------------------------------
// process.cdist
// ---------------------------------------------------------------------------

// Column-major cells of an x-by-y score matrix.
struct FuzzMatrixWorker : public Worker {
    const FuzzCorpus& x;
    const FuzzCorpus& y;
    RfScorer scorer;
    double score_cutoff;
    double* out;

    FuzzMatrixWorker(const FuzzCorpus& x_, const FuzzCorpus& y_,
                     RfScorer scorer_, double score_cutoff_, double* out_)
        : x(x_), y(y_), scorer(scorer_), score_cutoff(score_cutoff_),
          out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end) return;
        RfScratch scratch;
        const std::size_t rows = x.size();
        std::size_t j = begin / rows;
        std::size_t i = begin - j * rows;
        for (std::size_t cell = begin; cell < end; ++cell) {
            out[cell] = x.is_na(i) || y.is_na(j)
                ? NA_REAL
                : rf_score(scorer, x.text(i), y.text(j), score_cutoff, scratch);
            if (++i == rows) {
                i = 0;
                ++j;
            }
        }
    }
};

// cdist(x, x): every scorer is symmetric, so each row computes the cells
// from the diagonal rightwards and mirrors them. The diagonal is computed
// too, since a string need not score 100 against itself (an empty one under
// QRatio or WRatio, a blank one under token_set_ratio).
struct FuzzSymmetricMatrixWorker : public Worker {
    const FuzzCorpus& x;
    RfScorer scorer;
    double score_cutoff;
    double* out;

    FuzzSymmetricMatrixWorker(const FuzzCorpus& x_, RfScorer scorer_,
                              double score_cutoff_, double* out_)
        : x(x_), scorer(scorer_), score_cutoff(score_cutoff_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        RfScratch scratch;
        const std::size_t n = x.size();
        for (std::size_t row = begin; row < end; ++row) {
            for (std::size_t column = row; column < n; ++column) {
                const double value = x.is_na(row) || x.is_na(column)
                    ? NA_REAL
                    : rf_score(scorer, x.text(row), x.text(column),
                               score_cutoff, scratch);
                out[row + column * n] = value;
                out[column + row * n] = value;
            }
        }
    }
};

// ---------------------------------------------------------------------------
// process.extract / extractOne
// ---------------------------------------------------------------------------

struct ExtractCandidate {
    double score;
    int index;
};

// One task scores one query against one contiguous chunk of the choices and
// keeps the best `limit` (all when limit is 0), ordered by score and then by
// position, as RapidFuzz orders ties. With few queries the choices are split
// into several chunks so that a single query still uses every thread.
struct FuzzExtractWorker : public Worker {
    const FuzzCorpus& queries;
    const FuzzCorpus& choices;
    RfScorer scorer;
    double score_cutoff;
    std::size_t limit;
    std::size_t chunks_per_query;
    std::size_t chunk_size;
    std::vector<std::vector<ExtractCandidate>>& found;

    FuzzExtractWorker(const FuzzCorpus& queries_, const FuzzCorpus& choices_,
                      RfScorer scorer_, double score_cutoff_,
                      std::size_t limit_, std::size_t chunks_per_query_,
                      std::size_t chunk_size_,
                      std::vector<std::vector<ExtractCandidate>>& found_)
        : queries(queries_), choices(choices_), scorer(scorer_),
          score_cutoff(score_cutoff_), limit(limit_),
          chunks_per_query(chunks_per_query_), chunk_size(chunk_size_),
          found(found_) {}

    void operator()(std::size_t begin, std::size_t end) {
        RfScratch scratch;
        for (std::size_t task = begin; task < end; ++task) {
            std::vector<ExtractCandidate>& best = found[task];
            const std::size_t query = task / chunks_per_query;
            if (queries.is_na(query)) continue;
            const RfText text = queries.text(query);
            const std::size_t first = (task % chunks_per_query) * chunk_size;
            const std::size_t last = (std::min)(choices.size(), first + chunk_size);
            for (std::size_t choice = first; choice < last; ++choice) {
                if (choices.is_na(choice)) continue;
                // Once the list is full a newcomer must beat its last entry
                // (it loses ties, coming later), so that score becomes the
                // cutoff the scorer can prune with.
                const bool full = limit != 0 && best.size() == limit;
                const double cutoff = full
                    ? (std::max)(score_cutoff, best.back().score)
                    : score_cutoff;
                const double score = rf_score(scorer, text, choices.text(choice),
                                              cutoff, scratch);
                if (score < score_cutoff) continue;
                const ExtractCandidate candidate{score, static_cast<int>(choice)};
                if (limit == 0) {
                    best.push_back(candidate);
                    continue;
                }
                if (full) {
                    if (!(score > best.back().score)) continue;
                    best.pop_back();
                }
                const auto position = std::find_if(
                    best.begin(), best.end(),
                    [score](const ExtractCandidate& entry) {
                        return entry.score < score;
                    });
                best.insert(position, candidate);
                // extractOne stops at a perfect score: later ones only tie.
                if (limit == 1 && score == 100) break;
            }
            if (limit == 0) {
                std::stable_sort(best.begin(), best.end(),
                                 [](const ExtractCandidate& left,
                                    const ExtractCandidate& right) {
                                     return left.score > right.score;
                                 });
            }
        }
    }
};

} // namespace

// [[Rcpp::export]]
NumericVector fast_fuzz_score_impl(const StringVector& a, const StringVector& b,
                                   int scorer, bool full_process,
                                   double score_cutoff, int nthreads) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    const RfScorer kind = scorer_from_id(scorer);
    const std::size_t n = static_cast<std::size_t>(a.size());
    NumericVector result = no_init(a.size());
    StringSnapshot a_snapshot(a, SnapshotText::utf8);
    StringSnapshot b_snapshot(b, SnapshotText::utf8);
    FuzzPairWorker worker(a_snapshot.data(), b_snapshot.data(), kind,
                          full_process, score_cutoff, REAL(result));
    dispatch_for(0, n, worker,
                 estimated_pairwise_string_work(a_snapshot, b_snapshot),
                 1000, nthreads);
    return result;
}

// [[Rcpp::export]]
List fast_fuzz_alignment_impl(const StringVector& a, const StringVector& b,
                              bool full_process, double score_cutoff,
                              int nthreads) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    const std::size_t n = static_cast<std::size_t>(a.size());
    NumericVector score = no_init(a.size());
    IntegerVector src_start = no_init(a.size());
    IntegerVector src_end = no_init(a.size());
    IntegerVector dest_start = no_init(a.size());
    IntegerVector dest_end = no_init(a.size());
    StringSnapshot a_snapshot(a, SnapshotText::utf8);
    StringSnapshot b_snapshot(b, SnapshotText::utf8);
    FuzzAlignmentWorker worker(
        a_snapshot.data(), b_snapshot.data(), full_process, score_cutoff,
        REAL(score), INTEGER(src_start), INTEGER(src_end), INTEGER(dest_start),
        INTEGER(dest_end)
    );
    dispatch_for(0, n, worker,
                 estimated_pairwise_string_work(a_snapshot, b_snapshot),
                 1000, nthreads);
    return List::create(
        _["score"] = score, _["src_start"] = src_start, _["src_end"] = src_end,
        _["dest_start"] = dest_start, _["dest_end"] = dest_end
    );
}

// [[Rcpp::export]]
CharacterVector fast_fuzz_default_process_impl(const StringVector& x,
                                               int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t estimated_work = saturating_string_work_add(
        snapshot_bytes_as_size(snapshot),
        saturating_string_work_multiply(snapshot.size(), 16)
    );
    const std::size_t parallel_threshold = 300000;
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 16
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());
    DefaultProcessWorker worker(snapshot.data(), chunks, results, arenas);
    dispatch_for(0, chunks.size(), worker, estimated_work, parallel_threshold,
                 work_plan.dispatch_threads, work_plan.grain_size);
    return finalize_text_results(snapshot, results, arenas);
}

// [[Rcpp::export]]
NumericMatrix fast_fuzz_cdist_impl(const StringVector& x, const StringVector& y,
                                   int scorer, bool full_process,
                                   double score_cutoff, int nthreads) {
    const RfScorer kind = scorer_from_id(scorer);
    const std::size_t rows = static_cast<std::size_t>(x.size());
    const std::size_t columns = static_cast<std::size_t>(y.size());
    if (columns != 0 &&
        rows > (std::numeric_limits<std::size_t>::max)() / columns)
        stop("Requested pairwise matrix is too large.");
    const std::size_t cells = rows * columns;
    const bool symmetric = static_cast<SEXP>(x) == static_cast<SEXP>(y);

    FuzzCorpus x_corpus(x, full_process, nthreads);
    std::unique_ptr<FuzzCorpus> y_owned;
    if (!symmetric) y_owned.reset(new FuzzCorpus(y, full_process, nthreads));
    const FuzzCorpus& y_corpus = symmetric ? x_corpus : *y_owned;

    NumericMatrix result(x.size(), y.size());
    const std::size_t work = estimated_matrix_string_work(
        x_corpus.snapshot(), y_corpus.snapshot(), cells
    );
    if (symmetric) {
        FuzzSymmetricMatrixWorker worker(x_corpus, kind, score_cutoff,
                                         REAL(result));
        dispatch_for(0, rows, worker, work / 2 + work % 2, 10000, nthreads, 1);
        return result;
    }
    FuzzMatrixWorker worker(x_corpus, y_corpus, kind, score_cutoff, REAL(result));
    dispatch_for(0, cells, worker, work, 10000, nthreads, 1024);
    return result;
}

// [[Rcpp::export]]
List fast_fuzz_extract_impl(const StringVector& x, const StringVector& choices,
                            int scorer, int limit, double score_cutoff,
                            bool full_process, int nthreads) {
    const RfScorer kind = scorer_from_id(scorer);
    if (limit < 0) stop("`limit` must be non-negative.");
    FuzzCorpus queries(x, full_process, nthreads);
    FuzzCorpus table(choices, full_process, nthreads);
    const std::size_t nq = queries.size();
    const std::size_t nc = table.size();

    // Aim for ~128 tasks, but never chunks of fewer than ~1024 choices.
    const std::size_t by_queries = nq == 0 ? 1 : (128 + nq - 1) / nq;
    const std::size_t by_choices = (nc + 1023) / 1024;
    const std::size_t chunks_per_query =
        (std::max)(std::size_t(1), (std::min)(by_queries, by_choices));
    const std::size_t chunk_size = nc == 0
        ? 1
        : (nc + chunks_per_query - 1) / chunks_per_query;
    const std::size_t tasks = nq * chunks_per_query;

    std::vector<std::vector<ExtractCandidate>> found(tasks);
    FuzzExtractWorker worker(queries, table, kind, score_cutoff,
                             static_cast<std::size_t>(limit), chunks_per_query,
                             chunk_size, found);
    const std::size_t cells = saturating_string_work_multiply(nq, nc);
    dispatch_for(0, tasks, worker, cells, 10000, nthreads, 1);

    // Merge each query's chunks: concatenated in chunk order, a stable sort
    // by score keeps tied choices in position order.
    std::vector<ExtractCandidate> merged;
    std::vector<int> out_query, out_choice, out_rank;
    std::vector<double> out_score;
    for (std::size_t query = 0; query < nq; ++query) {
        merged.clear();
        for (std::size_t k = 0; k < chunks_per_query; ++k) {
            const std::vector<ExtractCandidate>& part =
                found[query * chunks_per_query + k];
            merged.insert(merged.end(), part.begin(), part.end());
        }
        if (chunks_per_query > 1) {
            std::stable_sort(merged.begin(), merged.end(),
                             [](const ExtractCandidate& left,
                                const ExtractCandidate& right) {
                                 return left.score > right.score;
                             });
        }
        std::size_t keep = merged.size();
        if (limit != 0) keep = (std::min)(keep, static_cast<std::size_t>(limit));
        for (std::size_t r = 0; r < keep; ++r) {
            out_query.push_back(static_cast<int>(query) + 1);
            out_choice.push_back(merged[r].index + 1);
            out_score.push_back(merged[r].score);
            out_rank.push_back(static_cast<int>(r) + 1);
        }
    }
    return List::create(
        _["query_index"] = wrap(out_query), _["choice_index"] = wrap(out_choice),
        _["score"] = wrap(out_score), _["rank"] = wrap(out_rank)
    );
}
