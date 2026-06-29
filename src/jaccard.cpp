// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include "jaccard_core.h"
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Pairwise worker: jaccard(a[i], b[i]) for each i
// ---------------------------------------------------------------------------

struct JaccardWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    int q;
    RVector<double> out;

    JaccardWorker(SEXP a, SEXP b, int q, NumericVector& out)
        : a_sexp(a), b_sexp(b), q(q), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) {
                out[i] = NA_REAL; continue;
            }
            out[i] = qgram_jaccard_sim(CHAR(ea), LENGTH(ea),
                                        CHAR(eb), LENGTH(eb), q);
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_jaccard_impl(const StringVector& a,
                                 const StringVector& b,
                                 int q) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    if (q < 1) stop("`q` must be >= 1.");
    R_xlen_t n = a.size();
    NumericVector result(n);
    JaccardWorker worker(a, b, q, result);
    if (n >= 1000) parallelFor(0, (std::size_t)n, worker);
    else           worker(0, (std::size_t)n);
    return result;
}

// ---------------------------------------------------------------------------
// Matrix worker: n_a × n_b matrix of all pairwise scores.
// ---------------------------------------------------------------------------

struct JaccardMatrixWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    int q;
    R_xlen_t na;
    R_xlen_t nb;
    double* out_ptr;

    JaccardMatrixWorker(SEXP a, SEXP b, int q,
                         R_xlen_t na, R_xlen_t nb, double* out_ptr)
        : a_sexp(a), b_sexp(b), q(q), na(na), nb(nb), out_ptr(out_ptr) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            for (R_xlen_t j = 0; j < nb; ++j) {
                SEXP eb = STRING_ELT(b_sexp, j);
                double sim;
                if (ea == NA_STRING || eb == NA_STRING)
                    sim = NA_REAL;
                else
                    sim = qgram_jaccard_sim(CHAR(ea), LENGTH(ea),
                                             CHAR(eb), LENGTH(eb), q);
                out_ptr[i + (std::size_t)j * (std::size_t)na] = sim;
            }
        }
    }
};

// [[Rcpp::export]]
NumericMatrix fast_jaccard_matrix_impl(const StringVector& a,
                                        const StringVector& b,
                                        int q) {
    if (q < 1) stop("`q` must be >= 1.");
    R_xlen_t na = a.size(), nb = b.size();
    NumericMatrix result(na, nb);
    JaccardMatrixWorker worker(a, b, q, na, nb, REAL(result));
    if ((std::size_t)na * (std::size_t)nb >= 10000)
        parallelFor(0, (std::size_t)na, worker);
    else
        worker(0, (std::size_t)na);
    return result;
}
