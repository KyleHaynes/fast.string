// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include "levenshtein_core.h"
#include "pairwise_worker.h"
using namespace Rcpp;

static double sim_levenshtein(const char* a, int la, const char* b, int lb, const NoCtx&) {
    return (double)levenshtein_distance(a, la, b, lb);
}
static double sim_damerau(const char* a, int la, const char* b, int lb, const NoCtx&) {
    return (double)damerau_levenshtein_distance(a, la, b, lb);
}
static double sim_hamming(const char* a, int la, const char* b, int lb, const NoCtx&) {
    int d = hamming_distance(a, la, b, lb);
    return d < 0 ? R_PosInf : (double)d; // stringdist's method = "hamming" convention
}

// [[Rcpp::export]]
NumericVector fast_levenshtein_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_levenshtein>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericMatrix fast_levenshtein_matrix_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise_matrix<NoCtx, sim_levenshtein>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericVector fast_damerau_levenshtein_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_damerau>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericMatrix fast_damerau_levenshtein_matrix_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise_matrix<NoCtx, sim_damerau>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericVector fast_hamming_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_hamming>(a, b, NoCtx{});
}
