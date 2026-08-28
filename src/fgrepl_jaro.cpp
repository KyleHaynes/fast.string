// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <limits>
#include <memory>
#include <string>
#include <vector>
#include "codepoint_snapshot.h"
#include "jaro_winkler_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
#include "unicode_metric_core.h"
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Pairwise worker: jw(a[i], b[i]) for each i
// ---------------------------------------------------------------------------

struct JaroWinklerWorker : public Worker {
    const StringView* a;
    const StringView* b;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    double p;
    bool use_bytes;
    RVector<double> out;

    JaroWinklerWorker(const StringView* a, const StringView* b,
                      const CodepointView* a_codepoints,
                      const CodepointView* b_codepoints,
                      double p, bool use_bytes, NumericVector& out)
        : a(a), b(b), a_codepoints(a_codepoints),
          b_codepoints(b_codepoints), p(p), use_bytes(use_bytes), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& ai = a[i];
            const StringView& bi = b[i];
            if (ai.is_na() || bi.is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            out[i] = use_bytes ||
                (a_codepoints[i].ascii && b_codepoints[i].ascii)
                ? jaro_winkler_sim(
                    ai.data, static_cast<int>(ai.size),
                    bi.data, static_cast<int>(bi.size), p
                )
                : sequence_jaro_winkler_similarity(
                    a_codepoints[i].data,
                    static_cast<int>(a_codepoints[i].size),
                    b_codepoints[i].data,
                    static_cast<int>(b_codepoints[i].size), p
                );
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_jaro_winkler_impl(const StringVector& a,
                                      const StringVector& b,
                                      double p,
                                      int nthreads,
                                      bool use_bytes) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    const R_xlen_t n = a.size();
    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    NumericVector result(n);
    JaroWinklerWorker worker(
        a_snapshot.data(), b_snapshot.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        p, use_bytes, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        estimated_pairwise_string_work(a_snapshot, b_snapshot),
        1000, nthreads
    );
    return result;
}

// ---------------------------------------------------------------------------
// Matrix worker: n_a × n_b matrix of all pairwise scores.
// Column-major layout: element [i,j] at offset i + j*n_a.
// Threads own contiguous, non-overlapping output ranges.
// ---------------------------------------------------------------------------

struct JaroWinklerMatrixWorker : public Worker {
    const StringView* a;
    const StringView* b;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    double p;
    bool use_bytes;
    std::size_t na;
    double* out_ptr;

    JaroWinklerMatrixWorker(const StringView* a, const StringView* b,
                            const CodepointView* a_codepoints,
                            const CodepointView* b_codepoints,
                            double p, bool use_bytes,
                            std::size_t na, double* out_ptr)
        : a(a), b(b), a_codepoints(a_codepoints),
          b_codepoints(b_codepoints), p(p), use_bytes(use_bytes),
          na(na), out_ptr(out_ptr) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end) return;
        std::size_t j = begin / na;
        std::size_t i = begin - j * na;
        for (std::size_t cell = begin; cell < end; ++cell) {
            const StringView& ai = a[i];
            const StringView& bj = b[j];
            if (ai.is_na() || bj.is_na()) {
                out_ptr[cell] = NA_REAL;
            } else {
                out_ptr[cell] = use_bytes ||
                    (a_codepoints[i].ascii && b_codepoints[j].ascii)
                    ? jaro_winkler_sim(
                        ai.data, static_cast<int>(ai.size),
                        bj.data, static_cast<int>(bj.size), p
                    )
                    : sequence_jaro_winkler_similarity(
                        a_codepoints[i].data,
                        static_cast<int>(a_codepoints[i].size),
                        b_codepoints[j].data,
                        static_cast<int>(b_codepoints[j].size), p
                    );
            }
            if (++i == na) {
                i = 0;
                ++j;
            }
        }
    }
};

struct JaroWinklerSymmetricMatrixWorker : public Worker {
    const StringView* bytes;
    const CodepointView* codepoints;
    double p;
    bool use_bytes;
    std::size_t size;
    double* out_ptr;

    JaroWinklerSymmetricMatrixWorker(
            const StringView* bytes_, const CodepointView* codepoints_,
            double p_, bool use_bytes_, std::size_t size_, double* out_ptr_)
        : bytes(bytes_), codepoints(codepoints_), p(p_),
          use_bytes(use_bytes_), size(size_), out_ptr(out_ptr_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t row = begin; row < end; ++row) {
            if (bytes[row].is_na()) {
                out_ptr[row + row * size] = NA_REAL;
            } else {
                out_ptr[row + row * size] = 1.0;
            }
            for (std::size_t column = row + 1; column < size; ++column) {
                double score;
                if (bytes[row].is_na() || bytes[column].is_na()) {
                    score = NA_REAL;
                } else {
                    score = use_bytes ||
                        (codepoints[row].ascii && codepoints[column].ascii)
                        ? jaro_winkler_sim(
                            bytes[row].data,
                            static_cast<int>(bytes[row].size),
                            bytes[column].data,
                            static_cast<int>(bytes[column].size), p
                        )
                        : sequence_jaro_winkler_similarity(
                            codepoints[row].data,
                            static_cast<int>(codepoints[row].size),
                            codepoints[column].data,
                            static_cast<int>(codepoints[column].size), p
                        );
                }
                out_ptr[row + column * size] = score;
                out_ptr[column + row * size] = score;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix fast_jaro_winkler_matrix_impl(const StringVector& a,
                                             const StringVector& b,
                                             double p,
                                             int nthreads,
                                             bool use_bytes) {
    const R_xlen_t na = a.size();
    const R_xlen_t nb = b.size();
    const std::size_t na_size = static_cast<std::size_t>(na);
    const std::size_t nb_size = static_cast<std::size_t>(nb);
    if (nb_size != 0 &&
        na_size > (std::numeric_limits<std::size_t>::max)() / nb_size)
        stop("Requested Jaro-Winkler matrix is too large.");
    const std::size_t cells = na_size * nb_size;
    const bool symmetric = static_cast<SEXP>(a) == static_cast<SEXP>(b);

    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    NumericMatrix result(na, nb);
    if (symmetric) {
        JaroWinklerSymmetricMatrixWorker worker(
            a_snapshot.data(),
            use_bytes ? nullptr : a_codepoints->data(),
            p, use_bytes, na_size, REAL(result)
        );
        const std::size_t estimated_work =
            estimated_matrix_string_work(a_snapshot, a_snapshot, cells);
        dispatch_for(
            0, na_size, worker,
            estimated_work / 2 + estimated_work % 2,
            10000, nthreads, 1
        );
        return result;
    }
    JaroWinklerMatrixWorker worker(
        a_snapshot.data(), b_snapshot.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        p, use_bytes, na_size, REAL(result)
    );
    dispatch_for(
        0, cells, worker,
        estimated_matrix_string_work(a_snapshot, b_snapshot, cells),
        10000, nthreads, 1024
    );
    return result;
}
