#ifndef FAST_STRING_PAIRWISE_WORKER_H
#define FAST_STRING_PAIRWISE_WORKER_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <cstddef>

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
    SEXP a_sexp;
    SEXP b_sexp;
    Ctx ctx;
    RcppParallel::RVector<double> out;

    PairwiseWorker(SEXP a, SEXP b, const Ctx& ctx, Rcpp::NumericVector& out)
        : a_sexp(a), b_sexp(b), ctx(ctx), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) { out[i] = NA_REAL; continue; }
            out[i] = Fn(CHAR(ea), LENGTH(ea), CHAR(eb), LENGTH(eb), ctx);
        }
    }
};

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
Rcpp::NumericVector run_pairwise(const Rcpp::StringVector& a, const Rcpp::StringVector& b,
                                  const Ctx& ctx, std::size_t parallel_threshold = 1000) {
    if (a.size() != b.size()) Rcpp::stop("`a` and `b` must have the same length.");
    R_xlen_t n = a.size();
    Rcpp::NumericVector result(n);
    PairwiseWorker<Ctx, Fn> worker(a, b, ctx, result);
    if ((std::size_t)n >= parallel_threshold) RcppParallel::parallelFor(0, (std::size_t)n, worker);
    else worker(0, (std::size_t)n);
    return result;
}

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
struct MatrixWorker : public RcppParallel::Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    Ctx ctx;
    R_xlen_t na;
    R_xlen_t nb;
    double* out_ptr;

    MatrixWorker(SEXP a, SEXP b, const Ctx& ctx, R_xlen_t na, R_xlen_t nb, double* out_ptr)
        : a_sexp(a), b_sexp(b), ctx(ctx), na(na), nb(nb), out_ptr(out_ptr) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            for (R_xlen_t j = 0; j < nb; ++j) {
                SEXP eb = STRING_ELT(b_sexp, j);
                double v = (ea == NA_STRING || eb == NA_STRING)
                    ? NA_REAL
                    : Fn(CHAR(ea), LENGTH(ea), CHAR(eb), LENGTH(eb), ctx);
                out_ptr[i + (std::size_t)j * (std::size_t)na] = v;
            }
        }
    }
};

template <typename Ctx, double (*Fn)(const char*, int, const char*, int, const Ctx&)>
Rcpp::NumericMatrix run_pairwise_matrix(const Rcpp::StringVector& a, const Rcpp::StringVector& b,
                                         const Ctx& ctx, std::size_t parallel_threshold = 10000) {
    R_xlen_t na = a.size(), nb = b.size();
    Rcpp::NumericMatrix result(na, nb);
    MatrixWorker<Ctx, Fn> worker(a, b, ctx, na, nb, REAL(result));
    if ((std::size_t)na * (std::size_t)nb >= parallel_threshold)
        RcppParallel::parallelFor(0, (std::size_t)na, worker);
    else
        worker(0, (std::size_t)na);
    return result;
}

#endif // FAST_STRING_PAIRWISE_WORKER_H
