// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include "ratcliff_obershelp_core.h"
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Generic pairwise worker parametrised on a (const char*,int,const char*,int)
// -> double similarity function, returning a 0-100 score like fuzzywuzzy.
// ---------------------------------------------------------------------------

template <double (*SimFn)(const char*, int, const char*, int)>
struct FuzzWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    RVector<double> out;

    FuzzWorker(SEXP a, SEXP b, NumericVector& out)
        : a_sexp(a), b_sexp(b), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) {
                out[i] = NA_REAL; continue;
            }
            out[i] = 100.0 * SimFn(CHAR(ea), LENGTH(ea), CHAR(eb), LENGTH(eb));
        }
    }
};

template <double (*SimFn)(const char*, int, const char*, int)>
static NumericVector run_fuzz(const StringVector& a, const StringVector& b) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    R_xlen_t n = a.size();
    NumericVector result(n);
    FuzzWorker<SimFn> worker(a, b, result);
    if (n >= 1000) parallelFor(0, (std::size_t)n, worker);
    else           worker(0, (std::size_t)n);
    return result;
}

static double sim_ratio(const char* a, int la, const char* b, int lb) {
    return ro_ratio(a, la, b, lb);
}
static double sim_partial_ratio(const char* a, int la, const char* b, int lb) {
    return ro_partial_ratio(a, la, b, lb);
}
static double sim_token_sort_ratio(const char* a, int la, const char* b, int lb) {
    std::string sa = ro_sorted_token_string(a, la);
    std::string sb = ro_sorted_token_string(b, lb);
    return ro_ratio(sa.data(), (int)sa.size(), sb.data(), (int)sb.size());
}
static double sim_token_set_ratio(const char* a, int la, const char* b, int lb) {
    return ro_token_set_ratio(a, la, b, lb);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_fuzz<sim_ratio>(a, b);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_partial_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_fuzz<sim_partial_ratio>(a, b);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_sort_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_fuzz<sim_token_sort_ratio>(a, b);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_set_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_fuzz<sim_token_set_ratio>(a, b);
}
