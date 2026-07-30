// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <limits>
#include <string>
#include <vector>
#include "jaro_winkler_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Pairwise worker: jw(a[i], b[i]) for each i
// ---------------------------------------------------------------------------

struct JaroWinklerWorker : public Worker {
    const StringView* a;
    const StringView* b;
    double p;
    RVector<double> out;

    JaroWinklerWorker(const StringView* a, const StringView* b,
                      double p, NumericVector& out)
        : a(a), b(b), p(p), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& ai = a[i];
            const StringView& bi = b[i];
            if (ai.is_na() || bi.is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            out[i] = jaro_winkler_sim(
                ai.data, static_cast<int>(ai.size),
                bi.data, static_cast<int>(bi.size), p
            );
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_jaro_winkler_impl(const StringVector& a,
                                      const StringVector& b,
                                      double p,
                                      int nthreads) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    const R_xlen_t n = a.size();
    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    NumericVector result(n);
    JaroWinklerWorker worker(
        a_snapshot.data(), b_snapshot.data(), p, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        static_cast<std::size_t>(n), 1000, nthreads
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
    double p;
    std::size_t na;
    double* out_ptr;

    JaroWinklerMatrixWorker(const StringView* a, const StringView* b,
                            double p, std::size_t na, double* out_ptr)
        : a(a), b(b), p(p), na(na), out_ptr(out_ptr) {}

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
                out_ptr[cell] = jaro_winkler_sim(
                    ai.data, static_cast<int>(ai.size),
                    bj.data, static_cast<int>(bj.size), p
                );
            }
            if (++i == na) {
                i = 0;
                ++j;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix fast_jaro_winkler_matrix_impl(const StringVector& a,
                                             const StringVector& b,
                                             double p,
                                             int nthreads) {
    const R_xlen_t na = a.size();
    const R_xlen_t nb = b.size();
    const std::size_t na_size = static_cast<std::size_t>(na);
    const std::size_t nb_size = static_cast<std::size_t>(nb);
    if (nb_size != 0 &&
        na_size > (std::numeric_limits<std::size_t>::max)() / nb_size)
        stop("Requested Jaro-Winkler matrix is too large.");
    const std::size_t cells = na_size * nb_size;

    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    NumericMatrix result(na, nb);
    JaroWinklerMatrixWorker worker(
        a_snapshot.data(), b_snapshot.data(), p, na_size, REAL(result)
    );
    dispatch_for(0, cells, worker, cells, 10000, nthreads, 1024);
    return result;
}
