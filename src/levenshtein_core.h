#ifndef FAST_STRING_LEVENSHTEIN_CORE_H
#define FAST_STRING_LEVENSHTEIN_CORE_H

#include <algorithm>
#include <array>
#include <cstddef>
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
// thread_local destructor registration relies on. Even POD thread_local
// data is slow in per-pair code there, since MinGW emulates TLS with a
// function call per access; see JaroScratch in jaro_winkler_core.h.
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

// Hyyro (2003) bit-vector OSA distance: Myers' recurrence above plus one
// extra term, TR, that marks cells reachable by an adjacent transposition
// (current text byte matches pattern position p, previous text byte matches
// p - 1, and the diagonal two steps back was not already a match). Same
// O(lt) word ops and same `pat`/`lp <= 64` contract as myers_levenshtein_64.
static inline int hyyro_osa_64(const char* txt, int lt, const char* pat, int lp) {
    std::array<uint64_t, 256> peq{};
    for (int j = 0; j < lp; ++j) peq[(unsigned char)pat[j]] |= (1ULL << j);

    uint64_t vp = ~0ULL;
    uint64_t vn = 0;
    uint64_t d0 = 0;
    uint64_t previous_eq = 0;
    int score = lp;
    const uint64_t last = 1ULL << (lp - 1);

    for (int i = 0; i < lt; ++i) {
        const uint64_t eq = peq[(unsigned char)txt[i]];
        const uint64_t tr = (((~d0) & eq) << 1) & previous_eq;
        d0 = (((eq & vp) + vp) ^ vp) | eq | vn | tr;
        uint64_t hp = vn | ~(d0 | vp);
        uint64_t hn = d0 & vp;
        if (hp & last) ++score;
        else if (hn & last) --score;
        hp = (hp << 1) | 1ULL;
        hn = hn << 1;
        vp = hn | ~(d0 | hp);
        vn = hp & d0;
        previous_eq = eq;
    }
    return score;
}

// Restricted edit distance (a.k.a. Optimal String Alignment / OSA): like
// Levenshtein but adjacent-transposition is also a single-cost edit, with
// the OSA restriction that no substring is edited more than once (so it's
// not a true metric -- same tradeoff stringdist's method = "osa" makes).
// O(l1 * l2) time, O(l2) space via three rolling rows (transposition needs
// the row two back, not just one). Fallback for pairs where neither string
// fits hyyro_osa_64().
static inline int osa_dp(const char* s1, int l1, const char* s2, int l2) {
    if (l1 == 0) return l2;
    if (l2 == 0) return l1;
    // OSA distance is symmetric, so use the shorter input as the rolling-row
    // dimension without changing the recurrence or observable result.
    if (l1 < l2) {
        std::swap(s1, s2);
        std::swap(l1, l2);
    }
    // Keep the valid empty-row case explicit after pointer/length swapping so
    // GCC can prove that row1[l2] is initialized without clearing the stack
    // buffers. Public callers only supply non-negative string lengths.
    if (l2 <= 0) return l1;
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

// Strips matching edges (an optimal OSA alignment always keeps them, just as
// for Levenshtein) and then dispatches like levenshtein_distance().
static inline int osa_distance(const char* s1, int l1, const char* s2, int l2) {
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
    if (l1 <= l2 && l1 <= 64) return hyyro_osa_64(s2, l2, s1, l1);
    if (l2 < l1 && l2 <= 64) return hyyro_osa_64(s1, l1, s2, l2);
    return osa_dp(s1, l1, s2, l2);
}

// Unrestricted (Lowrance-Wagner) Damerau-Levenshtein in O(lb) space, after
// Zhao & Sahni (2019). A transposition spanning i - k rows and j - l columns
// costs (i - k) + (j - l) - 1, which a run of substitutions and indels
// already matches unless one of the two spans is 1, so only those two cases
// are kept: FR[j] remembers row k - 1's value at column j - 2 from the last
// row whose symbol matched b[j - 1], and `t` remembers row i - 2's value at
// column l - 1 from the last match in the current row.
//
// `a`/`b` are symbol ids that index `last_row`, which must hold -1 for every
// id that can occur in `b` (ids that occur only in `b` are never written).
// `rows` is scratch space for 3 * (lb + 2) ints.
template <typename Id>
static inline int damerau_levenshtein_linear(const Id* a, int la,
                                             const Id* b, int lb,
                                             int* last_row, int* rows) {
    const int unreachable = (std::max)(la, lb) + 1;
    const std::size_t width = static_cast<std::size_t>(lb) + 2;
    int* r_base = rows;
    int* r1_base = rows + width;
    int* fr_base = rows + 2 * width;
    r_base[0] = unreachable;
    for (int j = 0; j <= lb; ++j) r_base[j + 1] = j;
    std::fill(r1_base, r1_base + width, unreachable);
    std::fill(fr_base, fr_base + width, unreachable);
    // Offset by one so column -1 (read as R1[j - 2] at j = 1) is in bounds.
    int* r = r_base + 1;
    int* r1 = r1_base + 1;
    int* fr = fr_base + 1;

    for (int i = 1; i <= la; ++i) {
        std::swap(r, r1);
        const Id ai = a[i - 1];
        int last_match_column = -1;
        int row_i2_before = r[0];
        int t = unreachable;
        r[0] = i;
        for (int j = 1; j <= lb; ++j) {
            const Id bj = b[j - 1];
            int best = (std::min)(r1[j - 1] + (ai != bj),
                                  (std::min)(r[j - 1], r1[j]) + 1);
            if (ai == bj) {
                last_match_column = j;
                fr[j] = r1[j - 2];
                t = row_i2_before;
            } else {
                const int k = last_row[bj];
                if (j - last_match_column == 1)
                    best = (std::min)(best, fr[j] + (i - k));
                else if (i - k == 1)
                    best = (std::min)(best, t + (j - last_match_column));
            }
            row_i2_before = r[j];
            r[j] = best;
        }
        last_row[ai] = i;
    }
    return r[lb];
}

static inline int damerau_levenshtein_distance(const char* s1, int l1, const char* s2, int l2) {
    // Matching edges are kept by an optimal alignment here too.
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
    // Symmetric, so keep the shorter string as the row dimension.
    if (l1 < l2) {
        std::swap(s1, s2);
        std::swap(l1, l2);
    }
    // Stack-allocated for the common case, heap fallback for longer strings;
    // see levenshtein_dp() above for why these are plain locals.
    const int STACK_LIM = 256;
    int last_row[256];
    std::fill(last_row, last_row + 256, -1);
    int rows_stack[3 * (STACK_LIM + 2)];
    int* rows = l2 <= STACK_LIM
        ? rows_stack
        : new int[3 * (static_cast<std::size_t>(l2) + 2)];
    const int result = damerau_levenshtein_linear(
        reinterpret_cast<const unsigned char*>(s1), l1,
        reinterpret_cast<const unsigned char*>(s2), l2, last_row, rows
    );
    if (l2 > STACK_LIM) delete[] rows;
    return result;
}

// Reusable scratch space for code-point Damerau-Levenshtein, where the
// alphabet is too large for a flat last-row table: symbols are remapped to
// dense ids (index into the sorted distinct symbols of `a`; symbols absent
// from `a` share the id `alphabet.size()`).
struct DamerauWorkspace {
    std::vector<std::uint32_t> alphabet;
    std::vector<std::uint32_t> a_ids;
    std::vector<std::uint32_t> b_ids;
    std::vector<int> last_row;
    std::vector<int> rows;
};

static inline int damerau_levenshtein_codepoints(const std::uint32_t* a, int la,
                                                 const std::uint32_t* b, int lb,
                                                 DamerauWorkspace& ws) {
    while (la > 0 && lb > 0 && *a == *b) {
        ++a;
        ++b;
        --la;
        --lb;
    }
    while (la > 0 && lb > 0 && a[la - 1] == b[lb - 1]) {
        --la;
        --lb;
    }
    if (la == 0) return lb;
    if (lb == 0) return la;
    if (la < lb) {
        std::swap(a, b);
        std::swap(la, lb);
    }
    ws.alphabet.assign(a, a + la);
    std::sort(ws.alphabet.begin(), ws.alphabet.end());
    ws.alphabet.erase(std::unique(ws.alphabet.begin(), ws.alphabet.end()),
                      ws.alphabet.end());
    const std::uint32_t absent = static_cast<std::uint32_t>(ws.alphabet.size());
    const auto id_of = [&](std::uint32_t symbol) -> std::uint32_t {
        const auto it = std::lower_bound(ws.alphabet.begin(), ws.alphabet.end(), symbol);
        return it != ws.alphabet.end() && *it == symbol
            ? static_cast<std::uint32_t>(it - ws.alphabet.begin())
            : absent;
    };
    ws.a_ids.resize(static_cast<std::size_t>(la));
    ws.b_ids.resize(static_cast<std::size_t>(lb));
    for (int i = 0; i < la; ++i) ws.a_ids[static_cast<std::size_t>(i)] = id_of(a[i]);
    for (int j = 0; j < lb; ++j) ws.b_ids[static_cast<std::size_t>(j)] = id_of(b[j]);
    ws.last_row.assign(static_cast<std::size_t>(absent) + 1, -1);
    const std::size_t needed = 3 * (static_cast<std::size_t>(lb) + 2);
    if (ws.rows.size() < needed) ws.rows.resize(needed);
    return damerau_levenshtein_linear(
        ws.a_ids.data(), la, ws.b_ids.data(), lb,
        ws.last_row.data(), ws.rows.data()
    );
}

// Hamming distance: defined only for equal-length strings. Returns -1 for
// mismatched lengths; callers map that to Inf (matching stringdist's
// method = "hamming" convention) or NA as appropriate.
static inline int hamming_distance(const char* s1, int l1, const char* s2, int l2) {
    if (l1 != l2) return -1;
    if (s1 == s2) return 0;
    int d = 0;
    for (int i = 0; i < l1; ++i) d += (s1[i] != s2[i]);
    return d;
}

#endif // FAST_STRING_LEVENSHTEIN_CORE_H
