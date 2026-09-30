#ifndef FAST_STRING_JARO_WINKLER_CORE_H
#define FAST_STRING_JARO_WINKLER_CORE_H

#include <algorithm>
#include <cstring>
#include <cstdint>
#if defined(_MSC_VER)
#include <intrin.h>
static inline int fastjw_ctz64(uint64_t x) { unsigned long idx; _BitScanForward64(&idx, x); return (int)idx; }
#else
static inline int fastjw_ctz64(uint64_t x) { return __builtin_ctzll(x); }
#endif

// Bit-parallel Jaro similarity for strings up to 64 bytes each (covers the
// overwhelming majority of names/addresses in data-linkage workloads).
//
// The scalar algorithm below, for each i in s1, linearly scans the matching
// window [lo,hi] in s2 for the first unclaimed position j with s2[j]==s1[i].
// That's exactly "the lowest set bit in (positions of s1[i] in s2) AND
// (window mask) AND NOT (already-claimed mask)" — so with one 64-bit
// bitmask per byte value giving s2's occurrence positions, the O(window)
// linear scan becomes a handful of bitwise ops that isolate that lowest bit,
// turning the O(l1 * window) double loop into O(l1 + l2).
//
// The 256-entry mask table lives in a caller-owned JaroScratch, zeroed once
// per worker range rather than once per call; each call clears only the
// entries of the (<= 64) bytes it set, so per-call setup/teardown stays
// O(l2), not O(256). It is deliberately not thread_local: MinGW compiles
// thread_local to emulated TLS, and the __emutls_get_address() lookup (a
// locked pthread_getspecific() in winpthreads) on every call cost more than
// the whole Jaro computation for short names.
struct JaroScratch {
    uint64_t char_mask[256];

    JaroScratch() { std::memset(char_mask, 0, sizeof(char_mask)); }
};

static inline double jaro_sim_bitparallel(const char* s1, int l1,
                                          const char* s2, int l2,
                                          JaroScratch& scratch) {
    int match_range = std::max(l1, l2) / 2 - 1;
    if (match_range < 0) match_range = 0;

    uint64_t* char_mask = scratch.char_mask;
    for (int j = 0; j < l2; ++j)
        char_mask[(unsigned char)s2[j]] |= (1ULL << j);

    uint64_t used_mask = 0;   // claimed positions in s2
    uint64_t s1_matched = 0;  // which positions in s1 matched
    int matches = 0;

    for (int i = 0; i < l1; ++i) {
        int lo = std::max(0, i - match_range);
        int hi = std::min(l2 - 1, i + match_range);
        uint64_t hi_mask = (hi == 63) ? ~0ULL : ((1ULL << (hi + 1)) - 1);
        uint64_t window  = hi_mask & ~((1ULL << lo) - 1);
        uint64_t cand = char_mask[(unsigned char)s1[i]] & window & ~used_mask;
        // Claim the lowest set bit (the leftmost match, same as the scalar
        // scan order) without branching: whether a character matches is
        // close to a coin flip for unrelated strings, so a branch here
        // mispredicts often. cand & -cand is 0 when there is no match.
        const uint64_t found = (uint64_t)(cand != 0);
        used_mask  |= cand & (0 - cand);
        s1_matched |= found << i;
        matches += (int)found;
    }

    for (int j = 0; j < l2; ++j) char_mask[(unsigned char)s2[j]] = 0;

    if (matches == 0) return 0.0;

    // Transpositions: the k-th matched character of s1 against the k-th
    // claimed character of s2, walking both bitsets lowest bit first.
    int t = 0;
    for (uint64_t am = s1_matched, bm = used_mask; am != 0;
         am &= am - 1, bm &= bm - 1)
        t += s1[fastjw_ctz64(am)] != s2[fastjw_ctz64(bm)];
    double m = (double)matches;
    return (m / l1 + m / l2 + (m - t / 2.0) / m) / 3.0;
}

// Jaro similarity. Stack-allocated boolean arrays for strings up to 256 bytes
// (covers >99% of names/addresses in data-linkage workloads). Heap fallback
// for longer strings — allocated and freed per call (rare path).
static inline double jaro_sim(const char* s1, int l1, const char* s2, int l2) {
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
// Workers create one JaroScratch per range and pass it to every call.
static inline double jaro_winkler_sim(const char* s1, int l1,
                                       const char* s2, int l2, double p,
                                       JaroScratch& scratch) {
    if (l1 == 0 && l2 == 0) return 1.0;
    if (l1 == 0 || l2 == 0) return 0.0;
    if (l1 == l2 &&
        (s1 == s2 || std::memcmp(s1, s2, static_cast<std::size_t>(l1)) == 0))
        return 1.0;
    double j = (l1 <= 64 && l2 <= 64)
        ? jaro_sim_bitparallel(s1, l1, s2, l2, scratch)
        : jaro_sim(s1, l1, s2, l2);
    if (j == 0.0) return 0.0;
    int prefix = 0;
    int maxp = std::min(4, std::min(l1, l2));
    while (prefix < maxp && s1[prefix] == s2[prefix]) ++prefix;
    return j + prefix * p * (1.0 - j);
}

// One-off form that zeroes a fresh scratch table per call.
static inline double jaro_winkler_sim(const char* s1, int l1,
                                       const char* s2, int l2, double p) {
    JaroScratch scratch;
    return jaro_winkler_sim(s1, l1, s2, l2, p, scratch);
}

#endif // FAST_STRING_JARO_WINKLER_CORE_H
