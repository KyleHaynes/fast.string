// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cstring>
#include <cstdint>
#if defined(_MSC_VER)
#include <intrin.h>
static inline int ctz64(uint64_t x) { unsigned long idx; _BitScanForward64(&idx, x); return (int)idx; }
#else
static inline int ctz64(uint64_t x) { return __builtin_ctzll(x); }
#endif
using namespace Rcpp;
using namespace RcppParallel;

// Bit-parallel Jaro similarity for strings up to 64 bytes each (covers the
// overwhelming majority of names/addresses in data-linkage workloads).
//
// The scalar algorithm below, for each i in s1, linearly scans the matching
// window [lo,hi] in s2 for the first unclaimed position j with s2[j]==s1[i].
// That's exactly "the lowest set bit in (positions of s1[i] in s2) AND
// (window mask) AND NOT (already-claimed mask)" — so with one 64-bit
// bitmask per byte value giving s2's occurrence positions, the O(window)
// linear scan becomes a handful of bitwise ops plus a count-trailing-zeros,
// turning the O(l1 * window) double loop into O(l1 + l2).
//
// char_mask[] is thread_local so it's zeroed once per worker thread rather
// than once per call; only the (<=64) distinct bytes actually touched this
// call are cleared afterwards, so per-call setup/teardown stays O(l2), not
// O(256).
static double jaro_sim_bitparallel(const char* s1, int l1, const char* s2, int l2) {
    int match_range = std::max(l1, l2) / 2 - 1;
    if (match_range < 0) match_range = 0;

    static thread_local uint64_t char_mask[256];
    static thread_local bool char_mask_ready = false;
    if (!char_mask_ready) { std::memset(char_mask, 0, sizeof(char_mask)); char_mask_ready = true; }

    unsigned char touched[64];
    int n_touched = 0;
    for (int j = 0; j < l2; ++j) {
        unsigned char c = (unsigned char)s2[j];
        if (char_mask[c] == 0) touched[n_touched++] = c;
        char_mask[c] |= (1ULL << j);
    }

    uint64_t used_mask = 0;   // claimed positions in s2
    uint64_t s1_matched = 0;  // which positions in s1 matched
    int matches = 0;

    for (int i = 0; i < l1; ++i) {
        int lo = std::max(0, i - match_range);
        int hi = std::min(l2 - 1, i + match_range);
        uint64_t hi_mask = (hi == 63) ? ~0ULL : ((1ULL << (hi + 1)) - 1);
        uint64_t window  = hi_mask & ~((1ULL << lo) - 1);
        uint64_t cand = char_mask[(unsigned char)s1[i]] & window & ~used_mask;
        if (cand) {
            int j = ctz64(cand); // lowest set bit == leftmost match, same as the scalar scan order
            used_mask  |= (1ULL << j);
            s1_matched |= (1ULL << i);
            ++matches;
        }
    }

    for (int k = 0; k < n_touched; ++k) char_mask[touched[k]] = 0;

    if (matches == 0) return 0.0;

    int t = 0, k = 0;
    for (int i = 0; i < l1; ++i) {
        if (!((s1_matched >> i) & 1ULL)) continue;
        while (k < l2 && !((used_mask >> k) & 1ULL)) ++k;
        if (k < l2 && s1[i] != s2[k]) ++t;
        ++k;
    }
    double m = (double)matches;
    return (m / l1 + m / l2 + (m - t / 2.0) / m) / 3.0;
}

// Jaro similarity. Stack-allocated boolean arrays for strings up to 256 bytes
// (covers >99% of names/addresses in data-linkage workloads). Heap fallback
// for longer strings — allocated and freed per call (rare path).
static double jaro_sim(const char* s1, int l1, const char* s2, int l2) {
    if (l1 == 0 && l2 == 0) return 1.0;
    if (l1 == 0 || l2 == 0) return 0.0;

    int match_range = std::max(l1, l2) / 2 - 1;
    if (match_range < 0) match_range = 0;

    const int STACK_LIM = 256;
    bool s1_stack[STACK_LIM], s2_stack[STACK_LIM];
    bool* s1m = (l1 <= STACK_LIM) ? s1_stack : new bool[l1];
    bool* s2m = (l2 <= STACK_LIM) ? s2_stack : new bool[l2];
    std::memset(s1m, 0, (std::size_t)l1);
    std::memset(s2m, 0, (std::size_t)l2);

    int matches = 0;
    for (int i = 0; i < l1; ++i) {
        int lo = std::max(0, i - match_range);
        int hi = std::min(l2 - 1, i + match_range);
        for (int j = lo; j <= hi; ++j) {
            if (!s2m[j] && s1[i] == s2[j]) {
                s1m[i] = true; s2m[j] = true; ++matches; break;
            }
        }
    }

    double sim;
    if (matches == 0) {
        sim = 0.0;
    } else {
        int t = 0, k = 0;
        for (int i = 0; i < l1; ++i) {
            if (!s1m[i]) continue;
            while (k < l2 && !s2m[k]) ++k;
            if (k < l2 && s1[i] != s2[k]) ++t;
            ++k;
        }
        double m = (double)matches;
        sim = (m / l1 + m / l2 + (m - t / 2.0) / m) / 3.0;
    }

    if (l1 > STACK_LIM) delete[] s1m;
    if (l2 > STACK_LIM) delete[] s2m;
    return sim;
}

// Jaro-Winkler similarity. p = prefix scaling factor (standard default: 0.1).
static inline double jaro_winkler_sim(const char* s1, int l1,
                                       const char* s2, int l2, double p) {
    if (l1 == 0 && l2 == 0) return 1.0;
    if (l1 == 0 || l2 == 0) return 0.0;
    double j = (l1 <= 64 && l2 <= 64) ? jaro_sim_bitparallel(s1, l1, s2, l2)
                                       : jaro_sim(s1, l1, s2, l2);
    if (j == 0.0) return 0.0;
    int prefix = 0;
    int maxp = std::min(4, std::min(l1, l2));
    while (prefix < maxp && s1[prefix] == s2[prefix]) ++prefix;
    return j + prefix * p * (1.0 - j);
}

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
