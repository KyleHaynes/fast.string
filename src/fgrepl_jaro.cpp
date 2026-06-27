// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include "jaro_winkler_core.h"
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Pairwise worker: jw(a[i], b[i]) for each i
// ---------------------------------------------------------------------------

struct JaroWinklerWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    double p;
    RVector<double> out;

    JaroWinklerWorker(SEXP a, SEXP b, double p, NumericVector& out)
        : a_sexp(a), b_sexp(b), p(p), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) {
                out[i] = NA_REAL; continue;
            }
            out[i] = jaro_winkler_sim(CHAR(ea), LENGTH(ea),
                                       CHAR(eb), LENGTH(eb), p);
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_jaro_winkler_impl(const StringVector& a,
                                      const StringVector& b,
                                      double p) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    R_xlen_t n = a.size();
    NumericVector result(n);
    JaroWinklerWorker worker(a, b, p, result);
    if (n >= 1000) parallelFor(0, (std::size_t)n, worker);
    else           worker(0, (std::size_t)n);
    return result;
}

// ---------------------------------------------------------------------------
// Matrix worker: n_a × n_b matrix of all pairwise scores.
// Column-major layout: element [i,j] at offset i + j*n_a.
// Threads own non-overlapping row ranges — writing to distinct offsets is safe.
// ---------------------------------------------------------------------------

struct JaroWinklerMatrixWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    double p;
    R_xlen_t na;
    R_xlen_t nb;
    double* out_ptr;

    JaroWinklerMatrixWorker(SEXP a, SEXP b, double p,
                              R_xlen_t na, R_xlen_t nb, double* out_ptr)
        : a_sexp(a), b_sexp(b), p(p), na(na), nb(nb), out_ptr(out_ptr) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            for (R_xlen_t j = 0; j < nb; ++j) {
                SEXP eb = STRING_ELT(b_sexp, j);
                double sim;
                if (ea == NA_STRING || eb == NA_STRING)
                    sim = NA_REAL;
                else
                    sim = jaro_winkler_sim(CHAR(ea), LENGTH(ea),
                                           CHAR(eb), LENGTH(eb), p);
                out_ptr[i + (std::size_t)j * (std::size_t)na] = sim;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix fast_jaro_winkler_matrix_impl(const StringVector& a,
                                             const StringVector& b,
                                             double p) {
    R_xlen_t na = a.size(), nb = b.size();
    NumericMatrix result(na, nb);
    JaroWinklerMatrixWorker worker(a, b, p, na, nb, REAL(result));
    if ((std::size_t)na * (std::size_t)nb >= 10000)
        parallelFor(0, (std::size_t)na, worker);
    else
        worker(0, (std::size_t)na);
    return result;
}
