// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include "qgram_core.h"
#include "pairwise_worker.h"
using namespace Rcpp;

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

// [[Rcpp::export]]
NumericVector fast_jaccard_impl(const StringVector& a, const StringVector& b, int q) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_jaccard>(a, b, QCtx{q, 1.0, 1.0});
}

// [[Rcpp::export]]
NumericMatrix fast_jaccard_matrix_impl(const StringVector& a, const StringVector& b, int q) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise_matrix<QCtx, sim_jaccard>(a, b, QCtx{q, 1.0, 1.0});
}

// [[Rcpp::export]]
NumericVector fast_dice_impl(const StringVector& a, const StringVector& b, int q) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_dice>(a, b, QCtx{q, 0.5, 0.5});
}

// [[Rcpp::export]]
NumericMatrix fast_dice_matrix_impl(const StringVector& a, const StringVector& b, int q) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise_matrix<QCtx, sim_dice>(a, b, QCtx{q, 0.5, 0.5});
}

// [[Rcpp::export]]
NumericVector fast_tversky_impl(const StringVector& a, const StringVector& b,
                                 int q, double alpha, double beta) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise<QCtx, sim_tversky>(a, b, QCtx{q, alpha, beta});
}

// [[Rcpp::export]]
NumericMatrix fast_tversky_matrix_impl(const StringVector& a, const StringVector& b,
                                        int q, double alpha, double beta) {
    if (q < 1) stop("`q` must be >= 1.");
    return run_pairwise_matrix<QCtx, sim_tversky>(a, b, QCtx{q, alpha, beta});
}
