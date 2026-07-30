#ifndef FAST_STRING_PAIRWISE_WORKER_H
#define FAST_STRING_PAIRWISE_WORKER_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <cstddef>
#include <limits>
#include "parallel_dispatch.h"
#include "string_snapshot.h"

// Shared parallel pairwise / all-pairs-matrix dispatch for the package's
// string-metric functions. Each metric plugs in as a free function
// `double fn(const char*, int, const char*, int, const Ctx&)` (Ctx carries
// whatever per-call parameters the metric needs -- a prefix factor, a
// q-gram length, alpha/beta weights, or nothing at all) and gets NA-aware
// pairwise comparison and an n x m similarity/distance matrix, both
// parallelised across CPU cores via Intel TBB (RcppParallel) once the
// workload is large enough to be worth the thread hand-off. This replaces
// what used to be a hand-rolled Worker struct per metric (jaro_winkler,
// jaccard, fuzz_*, ...) with one template, instantiated per metric.

// Use as Ctx for metrics that take no extra parameters (Levenshtein,
// Damerau-Levenshtein, Hamming, the fuzz_* family).
struct NoCtx {};

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
struct PairwiseWorker : public RcppParallel::Worker {
    const StringView* a;
    const StringView* b;
    Ctx ctx;
    RcppParallel::RVector<double> out;

    PairwiseWorker(const StringView* a, const StringView* b,
                   const Ctx& ctx, Rcpp::NumericVector& out)
        : a(a), b(b), ctx(ctx), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& ai = a[i];
            const StringView& bi = b[i];
            if (ai.is_na() || bi.is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            out[i] = Fn(ai.data, static_cast<int>(ai.size),
                        bi.data, static_cast<int>(bi.size), ctx);
        }
    }
};

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
Rcpp::NumericVector run_pairwise(const Rcpp::StringVector& a, const Rcpp::StringVector& b,
                                  const Ctx& ctx, int nthreads = -1,
                                  std::size_t parallel_threshold = 1000) {
    if (a.size() != b.size()) Rcpp::stop("`a` and `b` must have the same length.");
    const R_xlen_t n = a.size();
    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    Rcpp::NumericVector result(n);
    PairwiseWorker<Ctx, Fn> worker(
        a_snapshot.data(), b_snapshot.data(), ctx, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        static_cast<std::size_t>(n), parallel_threshold, nthreads
    );
    return result;
}

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
struct MatrixWorker : public RcppParallel::Worker {
    const StringView* a;
    const StringView* b;
    Ctx ctx;
    std::size_t na;
    double* out_ptr;

    MatrixWorker(const StringView* a, const StringView* b,
                 const Ctx& ctx, std::size_t na, double* out_ptr)
        : a(a), b(b), ctx(ctx), na(na), out_ptr(out_ptr) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end) return;
        std::size_t j = begin / na;
        std::size_t i = begin - j * na;
        for (std::size_t cell = begin; cell < end; ++cell) {
            const StringView& ai = a[i];
            const StringView& bj = b[j];
            out_ptr[cell] = (ai.is_na() || bj.is_na())
                ? NA_REAL
                : Fn(ai.data, static_cast<int>(ai.size),
                     bj.data, static_cast<int>(bj.size), ctx);
            if (++i == na) {
                i = 0;
                ++j;
            }
        }
    }
};

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
Rcpp::NumericMatrix run_pairwise_matrix(const Rcpp::StringVector& a, const Rcpp::StringVector& b,
                                         const Ctx& ctx, int nthreads = -1,
                                         std::size_t parallel_threshold = 10000) {
    const R_xlen_t na = a.size();
    const R_xlen_t nb = b.size();
    const std::size_t na_size = static_cast<std::size_t>(na);
    const std::size_t nb_size = static_cast<std::size_t>(nb);
    if (nb_size != 0 &&
        na_size > (std::numeric_limits<std::size_t>::max)() / nb_size)
        Rcpp::stop("Requested pairwise matrix is too large.");
    const std::size_t cells = na_size * nb_size;

    StringSnapshot a_snapshot(a);
    StringSnapshot b_snapshot(b);
    Rcpp::NumericMatrix result(na, nb);
    MatrixWorker<Ctx, Fn> worker(
        a_snapshot.data(), b_snapshot.data(), ctx, na_size, REAL(result)
    );
    dispatch_for(
        0, cells, worker, cells, parallel_threshold, nthreads, 1024
    );
    return result;
}

#endif // FAST_STRING_PAIRWISE_WORKER_H
