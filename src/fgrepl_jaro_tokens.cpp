// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>
#include "jaro_winkler_core.h"
using namespace Rcpp;
using namespace RcppParallel;

#if defined(_MSC_VER)
#include <intrin.h>
static inline int popcount32(uint32_t x) { return (int)__popcnt(x); }
#else
static inline int popcount32(uint32_t x) { return __builtin_popcount(x); }
#endif

namespace {

// Bitmask of original-token indices is used to track which tokens a
// candidate (single or contracted) consumes, so contraction safely caps out
// at 32 tokens per side — already far beyond any real name/address field.
constexpr int MASK_CAP = 32;
// Generating all C(n,2) pairwise contractions is only worth it for the
// handful of tokens in a name/address; beyond this, candidates fall back to
// singles-only (still correct, just without the contraction rescue) so a
// pathological long field can't blow up the per-row cost.
constexpr int PAIR_CAP = 12;
// A cyclic rotation only ever makes sense between equal-length candidates,
// and is only tried up to this length — well past any real name/address
// token/contraction, and matching the bit-parallel Jaro fast path's own
// length cutoff in jaro_winkler_core.h.
constexpr int ROTATION_LEN_CAP = 64;

struct Candidate {
    const char* ptr;
    int len;
    uint32_t mask;
};

// Plain Jaro-Winkler can't see past its matching window, so a token that's
// "the same two words stuck together in the opposite order" — e.g.
// "JOHNKYLE" vs "KYLEJOHN" — scores badly: the block swap moves every
// character further than the window tolerates. Equal-length candidates also
// try every cyclic rotation of `a` against `b` and keep the best score,
// which catches exactly that case (the rotation that undoes the swap is an
// exact match) without needing to know where the original word boundary
// was. `rot_buf` is reused across calls; sized to `la` each time, never
// reallocated mid-loop since it's filled by clear()+append() per rotation.
inline double jaro_winkler_sim_best(const char* a, int la, const char* b, int lb,
                                     double p, std::string& rot_buf) {
    double best = jaro_winkler_sim(a, la, b, lb, p);
    if (la != lb || la < 2 || la > ROTATION_LEN_CAP) return best;
    for (int r = 1; r < la; ++r) {
        rot_buf.clear();
        rot_buf.append(a + r, (std::size_t)(la - r));
        rot_buf.append(a, (std::size_t)r);
        double s = jaro_winkler_sim(rot_buf.data(), la, b, lb, p);
        if (s > best) best = s;
    }
    return best;
}

// Reused per-thread across rows so the per-call cost is just a handful of
// vector resizes (capacity sticks after the first few rows), not fresh heap
// allocations every row.
struct TokenScratch {
    std::vector<const char*> ptr_a, ptr_b;
    std::vector<int> len_a, len_b;
    std::vector<double> sim;
    std::vector<char> used_a, used_b;
    std::string collapsed_a, collapsed_b;
    std::vector<Candidate> cand_a, cand_b;
    std::vector<std::string> bufs_a, bufs_b;
    std::vector<int> order;
    std::string rot_buf;
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

// Candidates for one side: every single token, plus (when there are 2..
// PAIR_CAP tokens) every pair of tokens concatenated in their original
// left-to-right order — not just adjacent pairs, so "Kyle Haynes" still
// forms from tokens 0 and 2 of "Kyle John Haynes" with "John" in between.
// Buffers are sized exactly up front so no reallocation happens after their
// addresses are taken (vector<string> growth would otherwise relocate the
// short-string-optimised contents and dangle the pointers below).
inline void build_candidates(const std::vector<const char*>& ptrs, const std::vector<int>& lens,
                              std::vector<Candidate>& cands, std::vector<std::string>& bufs) {
    int n = (int)ptrs.size();
    cands.clear();
    bufs.clear();
    if (n == 0 || n > MASK_CAP) return;

    std::size_t n_pairs = (n >= 2 && n <= PAIR_CAP) ? (std::size_t)n * (n - 1) / 2 : 0;
    cands.reserve((std::size_t)n + n_pairs);
    for (int i = 0; i < n; ++i)
        cands.push_back(Candidate{ptrs[i], lens[i], 1u << (unsigned)i});

    if (n_pairs == 0) return;
    bufs.reserve(n_pairs);
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            bufs.emplace_back();
            std::string& buf = bufs.back();
            buf.reserve((std::size_t)lens[i] + (std::size_t)lens[j]);
            buf.append(ptrs[i], lens[i]);
            buf.append(ptrs[j], lens[j]);
        }
    }
    std::size_t k = 0;
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) {
            cands.push_back(Candidate{bufs[k].data(), (int)bufs[k].size(), (1u << (unsigned)i) | (1u << (unsigned)j)});
            ++k;
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

// Same greedy idea as token_alignment_score(), but each side's candidate
// pool also includes every pairwise contraction (build_candidates()), and a
// candidate is only eligible while none of the original-token indices it
// consumes have already been claimed by an earlier (higher-scoring) pick.
// "Effective" token counts for the max(...)/extra_penalty arithmetic count
// each accepted match once (whether it consumed one token or two) plus any
// genuinely leftover, unconsumed tokens — so when a contraction accounts for
// every token on both sides, there's nothing left to penalise.
double token_alignment_score_contractions(TokenScratch& sc, double p, double extra_penalty) {
    int mi = (int)sc.ptr_a.size();
    int ki = (int)sc.ptr_b.size();
    if (mi == 0 || ki == 0) return 0.0;
    if (mi > MASK_CAP || ki > MASK_CAP)
        return token_alignment_score(sc, p, extra_penalty);

    build_candidates(sc.ptr_a, sc.len_a, sc.cand_a, sc.bufs_a);
    build_candidates(sc.ptr_b, sc.len_b, sc.cand_b, sc.bufs_b);
    int na = (int)sc.cand_a.size();
    int nb = (int)sc.cand_b.size();

    sc.sim.assign((std::size_t)na * (std::size_t)nb, 0.0);
    for (int r = 0; r < na; ++r) {
        // Rotation tolerance only applies between two genuine, untouched
        // original tokens (single-bit mask) — never to a contraction
        // candidate, where the left-to-right order of the tokens it fused
        // is exactly what must stay meaningful (it's what tells "Kyle
        // Haynes" apart from the wrong-order "Haynes Kyle" contraction).
        bool a_single = (sc.cand_a[r].mask & (sc.cand_a[r].mask - 1)) == 0;
        for (int c = 0; c < nb; ++c) {
            bool b_single = (sc.cand_b[c].mask & (sc.cand_b[c].mask - 1)) == 0;
            sc.sim[(std::size_t)r * nb + c] =
                (a_single && b_single)
                    ? jaro_winkler_sim_best(sc.cand_a[r].ptr, sc.cand_a[r].len,
                                            sc.cand_b[c].ptr, sc.cand_b[c].len, p, sc.rot_buf)
                    : jaro_winkler_sim(sc.cand_a[r].ptr, sc.cand_a[r].len,
                                       sc.cand_b[c].ptr, sc.cand_b[c].len, p);
        }
    }

    sc.order.resize((std::size_t)na * (std::size_t)nb);
    for (std::size_t idx = 0; idx < sc.order.size(); ++idx) sc.order[idx] = (int)idx;
    std::sort(sc.order.begin(), sc.order.end(),
              [&](int x, int y) { return sc.sim[(std::size_t)x] > sc.sim[(std::size_t)y]; });

    uint32_t consumed_a = 0, consumed_b = 0;
    double total = 0.0;
    int n_matched = 0;
    for (int idx : sc.order) {
        uint32_t ma = sc.cand_a[idx / nb].mask;
        uint32_t mb = sc.cand_b[idx % nb].mask;
        if ((consumed_a & ma) || (consumed_b & mb)) continue;
        consumed_a |= ma; consumed_b |= mb;
        total += sc.sim[(std::size_t)idx];
        ++n_matched;
    }
    if (n_matched == 0) return 0.0;

    uint32_t full_a = (mi == 32) ? 0xFFFFFFFFu : ((1u << (unsigned)mi) - 1u);
    uint32_t full_b = (ki == 32) ? 0xFFFFFFFFu : ((1u << (unsigned)ki) - 1u);
    int leftover_a = popcount32(full_a & ~consumed_a);
    int leftover_b = popcount32(full_b & ~consumed_b);
    int effective_mi = n_matched + leftover_a;
    int effective_ki = n_matched + leftover_b;

    if (ISNA(extra_penalty)) return total / std::max(effective_mi, effective_ki);
    int n_extra = std::max(effective_mi, effective_ki) - n_matched;
    double score = total / n_matched - extra_penalty * n_extra;
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
    bool contractions;
    RVector<double> out;

    JaroWinklerTokensWorker(SEXP a, SEXP b, double p, double extra_penalty, bool contractions, NumericVector& out)
        : a_sexp(a), b_sexp(b), p(p), extra_penalty(extra_penalty), contractions(contractions), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        TokenScratch sc;
        for (std::size_t i = begin; i < end; ++i) {
            SEXP ea = STRING_ELT(a_sexp, i);
            SEXP eb = STRING_ELT(b_sexp, i);
            if (ea == NA_STRING || eb == NA_STRING) { out[i] = NA_REAL; continue; }

            tokenize(CHAR(ea), LENGTH(ea), sc.ptr_a, sc.len_a);
            tokenize(CHAR(eb), LENGTH(eb), sc.ptr_b, sc.len_b);

            double cscore = collapsed_score(sc, p);
            double tscore = contractions ? token_alignment_score_contractions(sc, p, extra_penalty)
                                          : token_alignment_score(sc, p, extra_penalty);
            out[i] = std::max(cscore, tscore);
        }
    }
};

} // namespace

// [[Rcpp::export]]
NumericVector fast_jaro_winkler_tokens_impl(const StringVector& a,
                                             const StringVector& b,
                                             double p,
                                             double extra_penalty,
                                             bool contractions) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");
    R_xlen_t n = a.size();
    NumericVector result(n);
    JaroWinklerTokensWorker worker(a, b, p, extra_penalty, contractions, result);
    if (n >= 1000) parallelFor(0, (std::size_t)n, worker);
    else           worker(0, (std::size_t)n);
    return result;
}
