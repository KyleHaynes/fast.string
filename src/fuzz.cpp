// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include "ratcliff_obershelp_core.h"
#include "pairwise_worker.h"
using namespace Rcpp;

static double sim_ratio(const char* a, int la, const char* b, int lb, const NoCtx&) {
    return 100.0 * ro_ratio(a, la, b, lb);
}
static double sim_partial_ratio(const char* a, int la, const char* b, int lb, const NoCtx&) {
    return 100.0 * ro_partial_ratio(a, la, b, lb);
}
static double sim_token_sort_ratio(const char* a, int la, const char* b, int lb, const NoCtx&) {
    std::string sa = ro_sorted_token_string(a, la);
    std::string sb = ro_sorted_token_string(b, lb);
    return 100.0 * ro_ratio(sa.data(), (int)sa.size(), sb.data(), (int)sb.size());
}
static double sim_token_set_ratio(const char* a, int la, const char* b, int lb, const NoCtx&) {
    return 100.0 * ro_token_set_ratio(a, la, b, lb);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_ratio>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericVector fast_fuzz_partial_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_partial_ratio>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_sort_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_token_sort_ratio>(a, b, NoCtx{});
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_set_ratio_impl(const StringVector& a, const StringVector& b) {
    return run_pairwise<NoCtx, sim_token_set_ratio>(a, b, NoCtx{});
}
