// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>
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
static bool prepare_packed_qgrams(const StringVector& a,
                                  const StringVector& b,
                                  const StringSnapshot& a_snapshot,
                                  const StringSnapshot& b_snapshot,
                                  int q,
                                  std::size_t cells,
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

    auto add_input = [&](const StringVector& input,
                         const StringSnapshot& snapshot,
                         std::vector<std::size_t>& side_ids,
                         std::size_t reuse_count) -> bool {
        for (std::size_t i = 0; i < snapshot.size(); ++i) {
            const StringView& view = snapshot[i];
            if (view.is_na()) continue;

            const std::size_t grams = upper_gram_count(view, q);
            current_build_work += static_cast<long double>(grams) *
                                  static_cast<long double>(reuse_count);

            SEXP charsxp = STRING_ELT(input, static_cast<R_xlen_t>(i));
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

    if (!add_input(a, a_snapshot, a_ids, nb) ||
        !add_input(b, b_snapshot, b_ids, na))
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
        qgram_keys_packed(value.data, value.size, q, scratch);
        PackedSlice slice{arena.keys.size(), scratch.size(), false};
        arena.keys.insert(arena.keys.end(), scratch.begin(), scratch.end());
        unique_slices[id] = slice;
    }

    const PackedSlice missing{0, 0, true};
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

template <PreparedScore Score>
struct PreparedQgramMatrixWorker : public Worker {
    const uint64_t* keys;
    const PackedSlice* a;
    const PackedSlice* b;
    std::size_t na;
    QCtx ctx;
    double* out;

    PreparedQgramMatrixWorker(const PackedQgramArena& arena,
                              std::size_t na, const QCtx& ctx, double* out)
        : keys(arena.keys.data()), a(arena.a.data()), b(arena.b.data()),
          na(na), ctx(ctx), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end || na == 0) return;
        std::size_t j = begin / na;
        std::size_t i = begin - j * na;

        while (begin < end) {
            const std::size_t run = (std::min)(end - begin, na - i);
            const PackedSlice& bs = b[j];
            for (std::size_t k = 0; k < run; ++k) {
                const PackedSlice& as = a[i + k];
                if (as.is_na || bs.is_na) {
                    out[begin + k] = NA_REAL;
                    continue;
                }

                const std::size_t inter =
                    (as.size == 0 || bs.size == 0) ? 0 :
                    sorted_intersection_size(
                        keys + as.offset, as.size,
                        keys + bs.offset, bs.size
                    );
                out[begin + k] = Score(
                    QgramOverlap{as.size, bs.size, inter}, ctx
                );
            }
            begin += run;
            ++j;
            i = 0;
        }
    }
};

template <double (*PairFn)(const char*, int, const char*, int, const QCtx&),
          PreparedScore Score>
static NumericMatrix run_qgram_matrix(const StringVector& a,
                                      const StringVector& b,
                                      const QCtx& ctx,
                                      int nthreads) {
    const std::size_t na = static_cast<std::size_t>(a.size());
    const std::size_t nb = static_cast<std::size_t>(b.size());
    if (na != 0 && nb > (std::numeric_limits<std::size_t>::max)() / na)
        stop("Requested matrix is too large.");
    const std::size_t cells = na * nb;

    if (ctx.q <= 8 && cells >= PREPARED_MIN_CELLS) {
        StringSnapshot a_snapshot(a), b_snapshot(b);
        PackedQgramArena arena;
        if (prepare_packed_qgrams(
                a, b, a_snapshot, b_snapshot, ctx.q, cells, arena)) {
            NumericMatrix result(a.size(), b.size());
            PreparedQgramMatrixWorker<Score> worker(
                arena, na, ctx, REAL(result)
            );
            dispatch_for(
                0, cells, worker, cells, 10000, nthreads, MATRIX_GRAIN
            );
            return result;
        }
    }

    return run_pairwise_matrix<QCtx, PairFn>(a, b, ctx, nthreads);
}

} // namespace

// [[Rcpp::export]]
NumericVector fast_jaccard_impl(const StringVector& a, const StringVector& b,
                                int q, int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_jaccard>(
        a, b, QCtx{q, 1.0, 1.0}, nthreads
    );
}

// [[Rcpp::export]]
NumericMatrix fast_jaccard_matrix_impl(const StringVector& a, const StringVector& b,
                                       int q, int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_qgram_matrix<sim_jaccard, prepared_jaccard>(
        a, b, QCtx{q, 1.0, 1.0}, nthreads
    );
}

// [[Rcpp::export]]
NumericVector fast_dice_impl(const StringVector& a, const StringVector& b,
                             int q, int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_dice>(
        a, b, QCtx{q, 0.5, 0.5}, nthreads
    );
}

// [[Rcpp::export]]
NumericMatrix fast_dice_matrix_impl(const StringVector& a, const StringVector& b,
                                    int q, int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_qgram_matrix<sim_dice, prepared_dice>(
        a, b, QCtx{q, 0.5, 0.5}, nthreads
    );
}

// [[Rcpp::export]]
NumericVector fast_tversky_impl(const StringVector& a, const StringVector& b,
                                int q, double alpha, double beta,
                                int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_tversky>(
        a, b, QCtx{q, alpha, beta}, nthreads
    );
}

// [[Rcpp::export]]
NumericMatrix fast_tversky_matrix_impl(const StringVector& a, const StringVector& b,
                                       int q, double alpha, double beta,
                                       int nthreads) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_qgram_matrix<sim_tversky, prepared_tversky>(
        a, b, QCtx{q, alpha, beta}, nthreads
    );
}
