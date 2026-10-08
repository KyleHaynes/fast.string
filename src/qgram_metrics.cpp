// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
#include "codepoint_snapshot.h"
#include "parallel_dispatch.h"
#include "qgram_core.h"
#include "pairwise_worker.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

struct QCtx { int q; double alpha; double beta; };

static double sim_jaccard(const char* a, int la, const char* b, int lb, const QCtx& c) {
    return qgram_jaccard_sim(a, la, b, lb, c.q);
}
static double sim_dice(const char* a, int la, const char* b, int lb, const QCtx& c) {
    return qgram_dice_sim(a, la, b, lb, c.q);
}
static double sim_tversky(const char* a, int la, const char* b, int lb, const QCtx& c) {
    return qgram_tversky_sim(a, la, b, lb, c.q, c.alpha, c.beta);
}
static double sim_cosine(const char* a, int la, const char* b, int lb, const QCtx& c) {
    return qgram_cosine_sim(a, la, b, lb, c.q);
}

namespace {

constexpr std::size_t PREPARED_MIN_CELLS = 4096;
constexpr std::size_t PREPARED_MAX_BYTES =
    static_cast<std::size_t>(128) * 1024 * 1024;
constexpr long double PREPARED_MIN_REUSE = 1.5L;
constexpr std::size_t MATRIX_GRAIN = 1024;

struct PackedSlice {
    std::size_t offset;
    std::size_t size;
    bool is_na;
    // One id per distinct CHARSXP, i.e. per distinct string: lets the
    // workers score identical strings 1 even when neither has a q-gram.
    std::size_t id;
};

struct UniquePackedString {
    const char* data;
    int size;
    std::size_t upper_grams;
};

struct PackedQgramArena {
    std::vector<uint64_t> keys;
    std::vector<PackedSlice> a;
    std::vector<PackedSlice> b;
};

static inline std::size_t upper_gram_count(const StringView& view, int q) {
    if (view.is_na() || view.size < static_cast<std::size_t>(q)) return 0;
    return view.size - static_cast<std::size_t>(q) + 1;
}

static inline bool add_within(std::size_t& total, std::size_t value,
                              std::size_t limit) {
    if (value > limit - total) return false;
    total += value;
    return true;
}

// Build a single flattened arena for both inputs. CHARSXP identity is stable
// for the duration of the call, so duplicate strings share one sorted key set
// even when they occur on both sides of the matrix.
static bool prepare_packed_qgrams(const StringSnapshot& a_snapshot,
                                  const StringSnapshot& b_snapshot,
                                  int q,
                                  std::size_t cells,
                                  bool deduplicate,
                                  PackedQgramArena& arena) {
    if (q > 8 || cells < PREPARED_MIN_CELLS) return false;

    const std::size_t na = a_snapshot.size();
    const std::size_t nb = b_snapshot.size();
    const std::size_t missing_id = (std::numeric_limits<std::size_t>::max)();
    if (nb > (std::numeric_limits<std::size_t>::max)() - na) return false;
    const std::size_t n_slices = na + nb;
    constexpr std::size_t PER_INPUT_METADATA =
        sizeof(PackedSlice) + sizeof(std::size_t);
    if (n_slices > PREPARED_MAX_BYTES / PER_INPUT_METADATA) return false;
    std::size_t estimated_bytes = n_slices * PER_INPUT_METADATA;

    std::unordered_map<SEXP, std::size_t> ids;
    std::vector<UniquePackedString> unique;
    std::vector<std::size_t> a_ids(na, missing_id), b_ids(nb, missing_id);

    std::size_t total_upper_grams = 0;
    std::size_t max_scratch_grams = 0;
    long double current_build_work = 0.0L;
    // Map node/bucket storage, UniquePackedString, and unique_slices entry.
    constexpr std::size_t UNIQUE_OVERHEAD = 128;

    auto add_input = [&](const StringSnapshot& snapshot,
                         std::vector<std::size_t>& side_ids,
                         std::size_t reuse_count) -> bool {
        for (std::size_t i = 0; i < snapshot.size(); ++i) {
            const StringView& view = snapshot[i];
            if (view.is_na()) continue;

            const std::size_t grams = upper_gram_count(view, q);
            current_build_work += static_cast<long double>(grams) *
                                  static_cast<long double>(reuse_count);

            SEXP charsxp = snapshot.charsxp(i);
            auto found = ids.find(charsxp);
            if (found != ids.end()) {
                side_ids[i] = found->second;
                continue;
            }

            const std::size_t scratch_growth =
                grams > max_scratch_grams ? grams - max_scratch_grams : 0;
            if (grams > (std::numeric_limits<std::size_t>::max)() -
                            scratch_growth)
                return false;
            const std::size_t key_growth = grams + scratch_growth;
            if (key_growth > (PREPARED_MAX_BYTES - estimated_bytes) /
                                 sizeof(uint64_t))
                return false;
            estimated_bytes += key_growth * sizeof(uint64_t);
            max_scratch_grams = (std::max)(max_scratch_grams, grams);
            if (!add_within(estimated_bytes, UNIQUE_OVERHEAD,
                            PREPARED_MAX_BYTES))
                return false;

            const std::size_t id = unique.size();
            ids.emplace(charsxp, id);
            side_ids[i] = id;
            unique.push_back(UniquePackedString{
                view.data, static_cast<int>(view.size), grams
            });
            if (!add_within(total_upper_grams, grams,
                            (std::numeric_limits<std::size_t>::max)()))
                return false;
        }
        return true;
    };

    if (!add_input(a_snapshot, a_ids, nb) ||
        !add_input(b_snapshot, b_ids, na))
        return false;

    const long double prepared_build_work =
        static_cast<long double>(total_upper_grams);
    if (prepared_build_work > 0.0L &&
        current_build_work < PREPARED_MIN_REUSE * prepared_build_work)
        return false;

    arena.keys.clear();
    arena.a.resize(na);
    arena.b.resize(nb);
    arena.keys.reserve(total_upper_grams);

    std::vector<PackedSlice> unique_slices(unique.size());
    std::vector<uint64_t> scratch;
    for (std::size_t id = 0; id < unique.size(); ++id) {
        const UniquePackedString& value = unique[id];
        if (deduplicate)
            qgram_keys_packed(value.data, value.size, q, scratch);
        else
            qgram_keys_packed_all(value.data, value.size, q, scratch);
        PackedSlice slice{arena.keys.size(), scratch.size(), false, id};
        arena.keys.insert(arena.keys.end(), scratch.begin(), scratch.end());
        unique_slices[id] = slice;
    }

    const PackedSlice missing{0, 0, true, 0};
    for (std::size_t i = 0; i < na; ++i)
        arena.a[i] = a_ids[i] == missing_id ? missing : unique_slices[a_ids[i]];
    for (std::size_t j = 0; j < nb; ++j)
        arena.b[j] = b_ids[j] == missing_id ? missing : unique_slices[b_ids[j]];

    return true;
}

using PreparedScore = double (*)(const QgramOverlap&, const QCtx&);

static double prepared_jaccard(const QgramOverlap& overlap, const QCtx&) {
    return qgram_jaccard_from_overlap(overlap);
}
static double prepared_dice(const QgramOverlap& overlap, const QCtx&) {
    return qgram_dice_from_overlap(overlap);
}
static double prepared_tversky(const QgramOverlap& overlap, const QCtx& ctx) {
    return qgram_tversky_from_overlap(overlap, ctx.alpha, ctx.beta);
}

// Scores one prepared cell. Different strings with no q-grams score 0;
// the slice id identifies the same string.
template <PreparedScore Score>
struct SetOverlapCell {
    const uint64_t* keys;
    QCtx ctx;

    double operator()(const PackedSlice& as, const PackedSlice& bs) const {
        if (as.is_na || bs.is_na) return NA_REAL;
        if (as.size == 0 && bs.size == 0) return as.id == bs.id ? 1.0 : 0.0;
        const std::size_t inter = (as.size == 0 || bs.size == 0) ? 0 :
            sorted_intersection_size(keys + as.offset, as.size,
                                     keys + bs.offset, bs.size);
        return Score(QgramOverlap{as.size, bs.size, inter}, ctx);
    }
};

struct CosineCell {
    const uint64_t* keys;

    double operator()(const PackedSlice& as, const PackedSlice& bs) const {
        if (as.is_na || bs.is_na) return NA_REAL;
        if (as.size == 0 && bs.size == 0) return as.id == bs.id ? 1.0 : 0.0;
        if (as.size == 0 || bs.size == 0) return 0.0;
        return qgram_cosine_from_frequency(qgram_frequency_overlap(
            keys + as.offset, as.size, keys + bs.offset, bs.size
        ));
    }
};

template <typename Cell>
struct PreparedMatrixWorker : public Worker {
    const PackedSlice* a;
    const PackedSlice* b;
    std::size_t na;
    Cell cell;
    double* out;

    PreparedMatrixWorker(const PackedQgramArena& arena, std::size_t na_,
                         const Cell& cell_, double* out_)
        : a(arena.a.data()), b(arena.b.data()), na(na_), cell(cell_),
          out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end || na == 0) return;
        std::size_t j = begin / na;
        std::size_t i = begin - j * na;
        while (begin < end) {
            const std::size_t run = (std::min)(end - begin, na - i);
            const PackedSlice& bs = b[j];
            for (std::size_t k = 0; k < run; ++k)
                out[begin + k] = cell(a[i + k], bs);
            begin += run;
            ++j;
            i = 0;
        }
    }
};

// m(x, x) for a symmetric score: each row computes only the cells right of
// the diagonal and mirrors them (the diagonal is a string against itself:
// 1, or NA). Rows never write the same cell.
template <typename Cell>
struct PreparedSymmetricWorker : public Worker {
    const PackedSlice* slices;
    std::size_t size;
    Cell cell;
    double* out;

    PreparedSymmetricWorker(const PackedQgramArena& arena, std::size_t size_,
                            const Cell& cell_, double* out_)
        : slices(arena.a.data()), size(size_), cell(cell_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t row = begin; row < end; ++row) {
            out[row + row * size] = slices[row].is_na ? NA_REAL : 1.0;
            for (std::size_t column = row + 1; column < size; ++column) {
                const double value = cell(slices[row], slices[column]);
                out[row + column * size] = value;
                out[column + row * size] = value;
            }
        }
    }
};

// Shared driver: prepares packed q-grams once (q <= 8, enough cells), then
// fills the matrix with `cell`; returns false to fall back to the generic
// pairwise matrix. `symmetric` means b is the same vector as a and the
// score is symmetric.
template <typename Cell>
static bool run_prepared_matrix(const StringVector& a, const StringVector& b,
                                int q, bool deduplicate, bool symmetric,
                                int nthreads, Cell cell_template,
                                NumericMatrix& result) {
    const std::size_t na = static_cast<std::size_t>(a.size());
    const std::size_t nb = static_cast<std::size_t>(b.size());
    const std::size_t cells = na * nb;
    if (q > 8 || cells < PREPARED_MIN_CELLS) return false;
    StringSnapshot a_snapshot(a), b_snapshot(b);
    PackedQgramArena arena;
    if (!prepare_packed_qgrams(a_snapshot, b_snapshot, q, cells, deduplicate,
                               arena))
        return false;
    result = NumericMatrix(a.size(), b.size());
    Cell cell = cell_template;
    cell.keys = arena.keys.data();
    const std::size_t work =
        estimated_matrix_string_work(a_snapshot, b_snapshot, cells);
    if (symmetric) {
        PreparedSymmetricWorker<Cell> worker(arena, na, cell, REAL(result));
        dispatch_for(0, na, worker, work / 2 + work % 2, 10000, nthreads, 1);
    } else {
        PreparedMatrixWorker<Cell> worker(arena, na, cell, REAL(result));
        dispatch_for(0, cells, worker, work, 10000, nthreads, MATRIX_GRAIN);
    }
    return true;
}

template <double (*PairFn)(const char*, int, const char*, int, const QCtx&),
          PreparedScore Score>
static NumericMatrix run_qgram_matrix(const StringVector& a,
                                      const StringVector& b,
                                      const QCtx& ctx,
                                      int nthreads,
                                      bool symmetric_score = true) {
    const std::size_t na = static_cast<std::size_t>(a.size());
    const std::size_t nb = static_cast<std::size_t>(b.size());
    if (na != 0 && nb > (std::numeric_limits<std::size_t>::max)() / na)
        stop("Requested matrix is too large.");
    const bool symmetric = symmetric_score &&
        static_cast<SEXP>(a) == static_cast<SEXP>(b);
    NumericMatrix result;
    if (run_prepared_matrix(a, b, ctx.q, true, symmetric, nthreads,
                            SetOverlapCell<Score>{nullptr, ctx}, result))
        return result;
    return run_pairwise_matrix<QCtx, PairFn>(a, b, ctx, nthreads);
}

static NumericMatrix run_qgram_cosine_matrix(const StringVector& a,
                                              const StringVector& b,
                                              const QCtx& ctx,
                                              int nthreads) {
    const std::size_t na = static_cast<std::size_t>(a.size());
    const std::size_t nb = static_cast<std::size_t>(b.size());
    if (na != 0 && nb > (std::numeric_limits<std::size_t>::max)() / na)
        stop("Requested matrix is too large.");
    const bool symmetric = static_cast<SEXP>(a) == static_cast<SEXP>(b);
    NumericMatrix result;
    if (run_prepared_matrix(a, b, ctx.q, false, symmetric, nthreads,
                            CosineCell{nullptr}, result))
        return result;
    return run_pairwise_matrix<QCtx, sim_cosine>(a, b, ctx, nthreads);
}

// ---------------------------------------------------------------------------
// use_bytes = FALSE: q-grams of UTF-8 code points. ASCII pairs, whose bytes
// are their code points, keep the byte functions above.
// ---------------------------------------------------------------------------

enum class QgramKind { jaccard, dice, tversky, cosine };

template <QgramKind Kind>
static inline double qgram_bytes_score(const StringView& a, const StringView& b,
                                       const QCtx& ctx) {
    const int la = static_cast<int>(a.size);
    const int lb = static_cast<int>(b.size);
    switch (Kind) {
    case QgramKind::jaccard: return sim_jaccard(a.data, la, b.data, lb, ctx);
    case QgramKind::dice: return sim_dice(a.data, la, b.data, lb, ctx);
    case QgramKind::tversky: return sim_tversky(a.data, la, b.data, lb, ctx);
    case QgramKind::cosine: return sim_cosine(a.data, la, b.data, lb, ctx);
    }
    return NA_REAL;
}

template <QgramKind Kind>
static inline double qgram_codepoint_score(const CodepointView& a,
                                           const CodepointView& b,
                                           const QCtx& ctx,
                                           CodepointQgramScratch& scratch) {
    const int la = static_cast<int>(a.size);
    const int lb = static_cast<int>(b.size);
    switch (Kind) {
    case QgramKind::jaccard:
        return qgram_jaccard_sim_codepoints(a.data, la, b.data, lb, ctx.q, scratch);
    case QgramKind::dice:
        return qgram_dice_sim_codepoints(a.data, la, b.data, lb, ctx.q, scratch);
    case QgramKind::tversky:
        return qgram_tversky_sim_codepoints(a.data, la, b.data, lb, ctx.q,
                                            ctx.alpha, ctx.beta, scratch);
    case QgramKind::cosine:
        return qgram_cosine_sim_codepoints(a.data, la, b.data, lb, ctx.q, scratch);
    }
    return NA_REAL;
}

// Scores pair `i` of the pairwise form, or cell (i, j) of the matrix form.
template <QgramKind Kind>
struct CodepointQgramScorer {
    const StringView* a_bytes;
    const StringView* b_bytes;
    const CodepointView* a_points;
    const CodepointView* b_points;
    QCtx ctx;

    double operator()(std::size_t i, std::size_t j,
                      CodepointQgramScratch& scratch) const {
        if (a_bytes[i].is_na() || b_bytes[j].is_na()) return NA_REAL;
        if (a_points[i].ascii && b_points[j].ascii)
            return qgram_bytes_score<Kind>(a_bytes[i], b_bytes[j], ctx);
        return qgram_codepoint_score<Kind>(a_points[i], b_points[j], ctx, scratch);
    }
};

template <QgramKind Kind>
struct CodepointQgramPairWorker : public Worker {
    CodepointQgramScorer<Kind> score;
    RVector<double> out;

    CodepointQgramPairWorker(const CodepointQgramScorer<Kind>& score_,
                             NumericVector& out_)
        : score(score_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        CodepointQgramScratch scratch;
        for (std::size_t i = begin; i < end; ++i) out[i] = score(i, i, scratch);
    }
};

template <QgramKind Kind>
struct CodepointQgramMatrixWorker : public Worker {
    CodepointQgramScorer<Kind> score;
    std::size_t rows;
    double* out;

    CodepointQgramMatrixWorker(const CodepointQgramScorer<Kind>& score_,
                               std::size_t rows_, double* out_)
        : score(score_), rows(rows_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end) return;
        CodepointQgramScratch scratch;
        std::size_t j = begin / rows;
        std::size_t i = begin - j * rows;
        for (std::size_t cell = begin; cell < end; ++cell) {
            out[cell] = score(i, j, scratch);
            if (++i == rows) {
                i = 0;
                ++j;
            }
        }
    }
};

template <QgramKind Kind>
static NumericVector run_codepoint_qgram(const StringVector& a,
                                         const StringVector& b,
                                         const QCtx& ctx, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const StringSnapshot a_bytes(a), b_bytes(b);
    const CodepointSnapshot a_points(a, "a"), b_points(b, "b");
    NumericVector result = no_init(a.size());
    CodepointQgramPairWorker<Kind> worker(
        CodepointQgramScorer<Kind>{a_bytes.data(), b_bytes.data(),
                                   a_points.data(), b_points.data(), ctx},
        result
    );
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(a_bytes, b_bytes), 1000,
                 nthreads);
    return result;
}

template <QgramKind Kind>
static NumericMatrix run_codepoint_qgram_matrix(const StringVector& a,
                                                const StringVector& b,
                                                const QCtx& ctx, int nthreads) {
    const std::size_t na = static_cast<std::size_t>(a.size());
    const std::size_t nb = static_cast<std::size_t>(b.size());
    if (na != 0 && nb > (std::numeric_limits<std::size_t>::max)() / na)
        stop("Requested matrix is too large.");
    const StringSnapshot a_bytes(a), b_bytes(b);
    const CodepointSnapshot a_points(a, "a"), b_points(b, "b");
    NumericMatrix result(a.size(), b.size());
    CodepointQgramMatrixWorker<Kind> worker(
        CodepointQgramScorer<Kind>{a_bytes.data(), b_bytes.data(),
                                   a_points.data(), b_points.data(), ctx},
        na, REAL(result)
    );
    dispatch_for(0, na * nb, worker,
                 estimated_matrix_string_work(a_bytes, b_bytes, na * nb),
                 10000, nthreads, MATRIX_GRAIN);
    return result;
}

} // namespace

// [[Rcpp::export]]
NumericVector fast_jaccard_impl(const StringVector& a, const StringVector& b,
                                int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 1.0, 1.0};
    if (!use_bytes) return run_codepoint_qgram<QgramKind::jaccard>(a, b, ctx, nthreads);
    return run_pairwise<QCtx, sim_jaccard>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericMatrix fast_jaccard_matrix_impl(const StringVector& a, const StringVector& b,
                                       int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 1.0, 1.0};
    if (!use_bytes)
        return run_codepoint_qgram_matrix<QgramKind::jaccard>(a, b, ctx, nthreads);
    return run_qgram_matrix<sim_jaccard, prepared_jaccard>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_dice_impl(const StringVector& a, const StringVector& b,
                             int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 0.5, 0.5};
    if (!use_bytes) return run_codepoint_qgram<QgramKind::dice>(a, b, ctx, nthreads);
    return run_pairwise<QCtx, sim_dice>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericMatrix fast_dice_matrix_impl(const StringVector& a, const StringVector& b,
                                    int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 0.5, 0.5};
    if (!use_bytes)
        return run_codepoint_qgram_matrix<QgramKind::dice>(a, b, ctx, nthreads);
    return run_qgram_matrix<sim_dice, prepared_dice>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_tversky_impl(const StringVector& a, const StringVector& b,
                                int q, double alpha, double beta,
                                int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, alpha, beta};
    if (!use_bytes) return run_codepoint_qgram<QgramKind::tversky>(a, b, ctx, nthreads);
    return run_pairwise<QCtx, sim_tversky>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericMatrix fast_tversky_matrix_impl(const StringVector& a, const StringVector& b,
                                       int q, double alpha, double beta,
                                       int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, alpha, beta};
    if (!use_bytes)
        return run_codepoint_qgram_matrix<QgramKind::tversky>(a, b, ctx, nthreads);
    // Not mirrored even when alpha == beta: the denominator adds the two
    // leftover terms in a fixed order, so swapping a and b can change the
    // last bit, and m(x, x) should equal m(x, copy of x) exactly.
    return run_qgram_matrix<sim_tversky, prepared_tversky>(
        a, b, ctx, nthreads, false
    );
}

// [[Rcpp::export]]
NumericVector fast_cosine_impl(const StringVector& a, const StringVector& b,
                               int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 0.0, 0.0};
    if (!use_bytes) return run_codepoint_qgram<QgramKind::cosine>(a, b, ctx, nthreads);
    return run_pairwise<QCtx, sim_cosine>(a, b, ctx, nthreads);
}

// [[Rcpp::export]]
NumericMatrix fast_cosine_matrix_impl(const StringVector& a,
                                      const StringVector& b,
                                      int q, int nthreads, bool use_bytes) {
    if (q < 1) stop("`q` must be >= 1.");
    const QCtx ctx{q, 0.0, 0.0};
    if (!use_bytes)
        return run_codepoint_qgram_matrix<QgramKind::cosine>(a, b, ctx, nthreads);
    return run_qgram_cosine_matrix(a, b, ctx, nthreads);
}
