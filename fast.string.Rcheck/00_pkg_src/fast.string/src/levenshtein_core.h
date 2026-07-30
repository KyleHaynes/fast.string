#ifndef FAST_STRING_LEVENSHTEIN_CORE_H
#define FAST_STRING_LEVENSHTEIN_CORE_H

#include <algorithm>
#include <array>
#include <cstdint>
#include <vector>

// Myers (1999) bit-vector edit distance: scans the longer string ("text")
// one byte at a time against a 64-bit bitmask built from the shorter string
// ("pattern", must be <= 64 bytes), turning what's normally an O(l1*l2) DP
// table into O(l1) word ops -- the same family of trick as this package's
// existing bit-parallel Jaro (jaro_winkler_core.h), just for edit distance
// instead. `pat`/`lp` is the masked side (lp <= 64), `txt`/`lt` is scanned
// with no length limit.
static inline int myers_levenshtein_64(const char* txt, int lt, const char* pat, int lp) {
    std::array<uint64_t, 256> peq{};
    for (int j = 0; j < lp; ++j) peq[(unsigned char)pat[j]] |= (1ULL << j);

    uint64_t pv = ~0ULL;
    uint64_t mv = 0;
    int score = lp;
    uint64_t last = 1ULL << (lp - 1);

    for (int i = 0; i < lt; ++i) {
        uint64_t eq = peq[(unsigned char)txt[i]];
        uint64_t xv = eq | mv;
        uint64_t xh = (((eq & pv) + pv) ^ pv) | eq;
        uint64_t ph = mv | ~(xh | pv);
        uint64_t mh = pv & xh;
        if (ph & last) ++score;
        else if (mh & last) --score;
        ph = (ph << 1) | 1ULL;
        pv = (mh << 1) | ~(xv | ph);
        mv = ph & xv;
    }
    return score;
}

// O(l1 * l2) time, O(min(l1, l2)) space DP fallback for pairs where neither
// string fits the 64-bit bit-parallel path. Stack-allocated for the common
// case, heap fallback for longer strings -- mirrors jaro_sim()'s STACK_LIM
// pattern in jaro_winkler_core.h. Deliberately *not* a thread_local scratch
// buffer: thread_local non-POD locals (std::vector, std::string) crash when
// first touched inside an RcppParallel/TBB worker thread on this toolchain,
// since those threads aren't created through the CRT path MinGW's
// thread_local destructor registration relies on. jaro_winkler_core.h's own
// thread_local usage is safe only because it's a POD array with no
// constructor/destructor to run.
static inline int levenshtein_dp(const char* s1, int l1, const char* s2, int l2) {
    if (l1 < l2) { std::swap(s1, s2); std::swap(l1, l2); } // keep the shorter row in memory
    // Besides handling the valid empty-row case without allocating scratch
    // space, this makes the non-negative row bound explicit to GCC.  Without
    // it, -Wmaybe-uninitialized cannot prove that the initialization loop
    // reaches row[l2], even though public callers only supply string lengths.
    if (l2 <= 0) return l1;
    const int STACK_LIM = 256;
    int row_stack[STACK_LIM + 1];
    int* row = (l2 <= STACK_LIM) ? row_stack : new int[(std::size_t)l2 + 1];
    for (int j = 0; j <= l2; ++j) row[j] = j;
    for (int i = 1; i <= l1; ++i) {
        int prev_diag = row[0];
        row[0] = i;
        for (int j = 1; j <= l2; ++j) {
            int tmp = row[j];
            int cost = (s1[i - 1] == s2[j - 1]) ? 0 : 1;
            row[j] = std::min({row[j] + 1, row[j - 1] + 1, prev_diag + cost});
            prev_diag = tmp;
        }
    }
    int result = row[l2];
    if (l2 > STACK_LIM) delete[] row;
    return result;
}

static inline int levenshtein_distance(const char* s1, int l1, const char* s2, int l2) {
    // Matching edges cannot contribute to edit cost. Removing them first is
    // especially valuable for records that share long prefixes/suffixes and
    // can also move the remaining problem back under Myers' 64-byte limit.
    while (l1 > 0 && l2 > 0 && *s1 == *s2) {
        ++s1;
        ++s2;
        --l1;
        --l2;
    }
    while (l1 > 0 && l2 > 0 && s1[l1 - 1] == s2[l2 - 1]) {
        --l1;
        --l2;
    }
    if (l1 == 0) return l2;
    if (l2 == 0) return l1;
    const char* pat; int lp;
    const char* txt; int lt;
    if (l1 <= l2) { pat = s1; lp = l1; txt = s2; lt = l2; }
    else          { pat = s2; lp = l2; txt = s1; lt = l1; }
    if (lp <= 64) return myers_levenshtein_64(txt, lt, pat, lp);
    return levenshtein_dp(s1, l1, s2, l2);
}

// Restricted edit distance (a.k.a. Optimal String Alignment / OSA): like
// Levenshtein but adjacent-transposition is also a single-cost edit, with
// the OSA restriction that no substring is edited more than once (so it's
// not a true metric -- same tradeoff stringdist's method = "dl" makes).
// O(l1 * l2) time, O(l2) space via three rolling rows (transposition needs
// the row two back, not just one).
static inline int damerau_levenshtein_distance(const char* s1, int l1, const char* s2, int l2) {
    if (l1 == 0) return l2;
    if (l2 == 0) return l1;
    // OSA distance is symmetric, so use the shorter input as the rolling-row
    // dimension without changing the recurrence or observable result.
    if (l1 < l2) {
        std::swap(s1, s2);
        std::swap(l1, l2);
    }
    // Stack-allocated for the common case, heap fallback for longer strings;
    // see levenshtein_dp() above for why these are plain locals, not
    // thread_local. row0 = i-2, row1 = i-1, row2 = current.
    const int STACK_LIM = 256;
    int row0_stack[STACK_LIM + 1], row1_stack[STACK_LIM + 1], row2_stack[STACK_LIM + 1];
    bool heap = l2 > STACK_LIM;
    int* row0 = heap ? new int[(std::size_t)l2 + 1] : row0_stack;
    int* row1 = heap ? new int[(std::size_t)l2 + 1] : row1_stack;
    int* row2 = heap ? new int[(std::size_t)l2 + 1] : row2_stack;
    for (int j = 0; j <= l2; ++j) { row0[j] = 0; row1[j] = j; }

    for (int i = 1; i <= l1; ++i) {
        row2[0] = i;
        for (int j = 1; j <= l2; ++j) {
            int cost = (s1[i - 1] == s2[j - 1]) ? 0 : 1;
            int best = std::min({row1[j] + 1, row2[j - 1] + 1, row1[j - 1] + cost});
            if (i > 1 && j > 1 && s1[i - 1] == s2[j - 2] && s1[i - 2] == s2[j - 1])
                best = std::min(best, row0[j - 2] + 1);
            row2[j] = best;
        }
        std::swap(row0, row1);
        std::swap(row1, row2);
    }
    int result = row1[l2];
    if (heap) { delete[] row0; delete[] row1; delete[] row2; }
    return result;
}

// Hamming distance: defined only for equal-length strings. Returns -1 for
// mismatched lengths; callers map that to Inf (matching stringdist's
// method = "hamming" convention) or NA as appropriate.
static inline int hamming_distance(const char* s1, int l1, const char* s2, int l2) {
    if (l1 != l2) return -1;
    int d = 0;
    for (int i = 0; i < l1; ++i) d += (s1[i] != s2[i]);
    return d;
}

#endif // FAST_STRING_LEVENSHTEIN_CORE_H
