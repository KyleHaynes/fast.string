// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <string>
#include <vector>
#include "jaro_winkler_core.h"
using namespace Rcpp;
using namespace RcppParallel;

namespace {

// Reused per-thread across rows so the per-call cost is just a handful of
// vector resizes (capacity sticks after the first few rows), not fresh heap
// allocations every row.
struct TokenScratch {
    std::vector<const char*> ptr_a, ptr_b;
    std::vector<int> len_a, len_b;
    std::vector<double> sim;
    std::vector<char> used_a, used_b;
    std::string collapsed_a, collapsed_b;
};

inline void tokenize(const char* s, int n, std::vector<const char*>& ptrs, std::vector<int>& lens) {
    ptrs.clear(); lens.clear();
    int i = 0;
    while (i < n) {
        while (i < n && s[i] == ' ') ++i;
        int start = i;
        while (i < n && s[i] != ' ') ++i;
        if (i > start) { ptrs.push_back(s + start); lens.push_back(i - start); }
    }
}

// Greedy token assignment: repeatedly pick the highest-similarity unused
// (a-token, b-token) pair. Equivalent to sorting all m*k pairs descending and
// taking the first available at each step, just without materialising/
// sorting the full pair list — for the handful of tokens in a name/address
// (m, k almost always <= ~5) this is a few dozen comparisons, not a
// performance concern.
double token_alignment_score(TokenScratch& sc, double p, double extra_penalty) {
    int mi = (int)sc.ptr_a.size();
    int ki = (int)sc.ptr_b.size();
    int want = std::min(mi, ki);
    if (want == 0) return 0.0;

    sc.sim.assign((std::size_t)mi * (std::size_t)ki, 0.0);
    for (int r = 0; r < mi; ++r)
        for (int c = 0; c < ki; ++c)
            sc.sim[(std::size_t)r * ki + c] =
                jaro_winkler_sim(sc.ptr_a[r], sc.len_a[r], sc.ptr_b[c], sc.len_b[c], p);

    sc.used_a.assign(mi, 0);
    sc.used_b.assign(ki, 0);
    double total = 0.0;
    for (int step = 0; step < want; ++step) {
        int best_r = -1, best_c = -1;
        double best_val = -1.0;
        for (int r = 0; r < mi; ++r) {
            if (sc.used_a[r]) continue;
            const double* row = &sc.sim[(std::size_t)r * ki];
            for (int c = 0; c < ki; ++c) {
                if (sc.used_b[c]) continue;
                if (row[c] > best_val) { best_val = row[c]; best_r = r; best_c = c; }
            }
        }
        sc.used_a[best_r] = 1; sc.used_b[best_c] = 1;
        total += best_val;
    }

    if (ISNA(extra_penalty)) return total / std::max(mi, ki);
    int n_extra = std::max(mi, ki) - want;
    double score = total / want - extra_penalty * n_extra;
    return score < 0.0 ? 0.0 : score;
}

// Both sides with token boundaries removed, compared as one string — catches
// matches that differ only in punctuation/whitespace placement (e.g. one
// token vs two) which token alignment alone would under-score.
double collapsed_score(TokenScratch& sc, double p) {
    sc.collapsed_a.clear();
    for (std::size_t i = 0; i < sc.ptr_a.size(); ++i) sc.collapsed_a.append(sc.ptr_a[i], sc.len_a[i]);
    sc.collapsed_b.clear();
    for (std::size_t i = 0; i < sc.ptr_b.size(); ++i) sc.collapsed_b.append(sc.ptr_b[i], sc.len_b[i]);
    return jaro_winkler_sim(sc.collapsed_a.data(), (int)sc.collapsed_a.size(),
                             sc.collapsed_b.data(), (int)sc.collapsed_b.size(), p);
}

struct JaroWinklerTokensWorker : public Worker {
    SEXP a_sexp;
    SEXP b_sexp;
    double p;
    double extra_penalty;
    RVector<double> out;

    JaroWinklerTokensWorker(SEXP a, SEXP b, double p, double extra_penalty, NumericVector& out)
        : a_sexp(a), b_sexp(b), p(p), extra_penalty(extra_penalty), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        TokenScratch sc;
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) { out[i] = NA_REAL; continue; }

            tokenize(CHAR(ea), LENGTH(ea), sc.ptr_a, sc.len_a);
            tokenize(CHAR(eb), LENGTH(eb), sc.ptr_b, sc.len_b);

            double cscore = collapsed_score(sc, p);
            double tscore = token_alignment_score(sc, p, extra_penalty);
            out[i] = std::max(cscore, tscore);
        }
    }
};

} // namespace

// [[Rcpp::export]]
NumericVector fast_jaro_winkler_tokens_impl(const StringVector& a,
                                             const StringVector& b,
                                             double p,
                                             double extra_penalty) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    R_xlen_t n = a.size();
    NumericVector result(n);
    JaroWinklerTokensWorker worker(a, b, p, extra_penalty, result);
    if (n >= 1000) parallelFor(0, (std::size_t)n, worker);
    else           worker(0, (std::size_t)n);
    return result;
}
