#ifndef FAST_STRING_RATCLIFF_OBERSHELP_CORE_H
#define FAST_STRING_RATCLIFF_OBERSHELP_CORE_H

#include <algorithm>
#include <array>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>

// Ratcliff/Obershelp matching-blocks algorithm — the same algorithm behind
// Python's difflib.SequenceMatcher, which is in turn what fuzzywuzzy's
// fuzz.ratio()/partial_ratio()/token_*_ratio() are built on (fuzzywuzzy only
// adds Levenshtein as a faster *substitute* when python-Levenshtein happens
// to be installed; the algorithm being ported here is the always-available
// reference behaviour). Recursively finds the longest matching block, then
// recurses on the unmatched left/right remainders, same as
// difflib.SequenceMatcher.get_matching_blocks(). No junk/autojunk handling —
// difflib's own autojunk heuristic is a no-op below 200 elements anyway,
// comfortably covering the short strings (names/addresses) this package
// targets.

struct ROBlock { int a, b, size; };

// Bucket b's byte positions directly by byte value (0-255) -- exact and
// hash-free, unlike difflib's b2j dict keyed by arbitrary characters.
struct ROIndex {
    std::array<std::vector<int>, 256> pos;
    ROIndex(const char* b, int lb) {
        for (int j = 0; j < lb; ++j) pos[(unsigned char)b[j]].push_back(j);
    }
};

// Longest matching block within a[alo,ahi) vs b[blo,bhi), mirroring
// difflib.SequenceMatcher.find_longest_match. `len_at`/`new_len_at` are
// caller-owned scratch buffers (size >= lb+1) reused across calls to avoid
// reallocating on every recursive step.
static inline ROBlock ro_find_longest_match(const char* a, int alo, int ahi,
                                             const char* b, int blo, int bhi,
                                             const ROIndex& idx,
                                             std::vector<int>& len_at,
                                             std::vector<int>& new_len_at) {
    int besti = alo, bestj = blo, bestsize = 0;
    int n = bhi - blo + 1;
    std::fill(len_at.begin(), len_at.begin() + n, 0);

    for (int i = alo; i < ahi; ++i) {
        std::fill(new_len_at.begin(), new_len_at.begin() + n, 0);
        const std::vector<int>& js = idx.pos[(unsigned char)a[i]];
        auto it = std::lower_bound(js.begin(), js.end(), blo);
        for (; it != js.end() && *it < bhi; ++it) {
            int j = *it;
            int p = j - blo + 1;       // index of j in [0, n)
            int k = len_at[p - 1] + 1; // len_at[p-1] == j2len.get(j-1, 0)
            new_len_at[p] = k;
            if (k > bestsize) { besti = i - k + 1; bestj = j - k + 1; bestsize = k; }
        }
        len_at.swap(new_len_at);
    }
    return ROBlock{besti, bestj, bestsize};
}

// All matching blocks between a[0,la) and b[0,lb), sorted by (a, b) position,
// same contract as difflib.SequenceMatcher.get_matching_blocks() (minus the
// dummy zero-size terminal block difflib appends, which callers don't need
// here since we always sum or scan, never index off the end).
static inline std::vector<ROBlock> ro_matching_blocks(const char* a, int la,
                                                       const char* b, int lb) {
    std::vector<ROBlock> blocks;
    if (la == 0 || lb == 0) return blocks;

    ROIndex idx(b, lb);
    std::vector<int> len_at((std::size_t)lb + 1, 0);
    std::vector<int> new_len_at((std::size_t)lb + 1, 0);

    struct Range { int alo, ahi, blo, bhi; };
    std::vector<Range> stack;
    stack.push_back({0, la, 0, lb});

    while (!stack.empty()) {
        Range r = stack.back();
        stack.pop_back();
        ROBlock m = ro_find_longest_match(a, r.alo, r.ahi, b, r.blo, r.bhi,
                                           idx, len_at, new_len_at);
        if (m.size > 0) {
            blocks.push_back(m);
            if (r.alo < m.a && r.blo < m.b)
                stack.push_back({r.alo, m.a, r.blo, m.b});
            if (m.a + m.size < r.ahi && m.b + m.size < r.bhi)
                stack.push_back({m.a + m.size, r.ahi, m.b + m.size, r.bhi});
        }
    }
    std::sort(blocks.begin(), blocks.end(), [](const ROBlock& x, const ROBlock& y) {
        return x.a < y.a || (x.a == y.a && x.b < y.b);
    });
    return blocks;
}

static inline int ro_total_matched(const char* a, int la, const char* b, int lb) {
    int m = 0;
    for (const ROBlock& blk : ro_matching_blocks(a, la, b, lb)) m += blk.size;
    return m;
}

// difflib SequenceMatcher.ratio(): 2*M / T, T = len(a) + len(b).
static inline double ro_ratio(const char* a, int la, const char* b, int lb) {
    if (la == 0 && lb == 0) return 1.0;
    if (la == lb &&
        (a == b || std::memcmp(a, b, static_cast<std::size_t>(la)) == 0))
        return 1.0;
    int m = ro_total_matched(a, la, b, lb);
    return (2.0 * m) / (double)(la + lb);
}

// fuzzywuzzy fuzz.partial_ratio(): align the shorter string against every
// matching block's offset into the longer one, take the best full ratio()
// over those alignments.
static inline double ro_partial_ratio(const char* s1, int l1, const char* s2, int l2) {
    if (l1 == l2 &&
        (l1 == 0 || s1 == s2 ||
         std::memcmp(s1, s2, static_cast<std::size_t>(l1)) == 0))
        return 1.0;
    const char* shorter; int ls;
    const char* longer; int ll;
    if (l1 <= l2) { shorter = s1; ls = l1; longer = s2; ll = l2; }
    else          { shorter = s2; ls = l2; longer = s1; ll = l1; }
    if (ls == 0) return (ll == 0) ? 1.0 : 0.0;

    std::vector<ROBlock> blocks = ro_matching_blocks(shorter, ls, longer, ll);
    if (blocks.empty()) return 0.0;

    double best = 0.0;
    for (const ROBlock& blk : blocks) {
        int long_start = std::max(blk.b - blk.a, 0);
        int long_end = std::min(long_start + ls, ll);
        int sub_len = long_end - long_start;
        double r = ro_ratio(shorter, ls, longer + long_start, sub_len);
        if (r > 0.995) return 1.0;
        if (r > best) best = r;
    }
    return best;
}

// ---------------------------------------------------------------------------
// Tokenisation helpers shared by token_sort_ratio / token_set_ratio.
// ---------------------------------------------------------------------------

static inline std::vector<std::pair<const char*, int>> ro_tokenize(const char* s, int n) {
    std::vector<std::pair<const char*, int>> toks;
    int i = 0;
    while (i < n) {
        while (i < n && std::isspace((unsigned char)s[i])) ++i;
        int start = i;
        while (i < n && !std::isspace((unsigned char)s[i])) ++i;
        if (i > start) toks.emplace_back(s + start, i - start);
    }
    return toks;
}

// fuzzywuzzy token_sort_ratio's "_process_and_sort": split on whitespace,
// sort tokens lexicographically, rejoin with a single space.
static inline std::string ro_sorted_token_string(const char* s, int n) {
    std::vector<std::pair<const char*, int>> toks = ro_tokenize(s, n);
    std::sort(toks.begin(), toks.end(), [](const auto& x, const auto& y) {
        int cmp = std::memcmp(x.first, y.first, (std::size_t)std::min(x.second, y.second));
        if (cmp != 0) return cmp < 0;
        return x.second < y.second;
    });
    std::string out;
    for (std::size_t i = 0; i < toks.size(); ++i) {
        if (i) out.push_back(' ');
        out.append(toks[i].first, (std::size_t)toks[i].second);
    }
    return out;
}

static inline std::vector<std::string> ro_unique_sorted_tokens(const char* s, int n) {
    std::vector<std::pair<const char*, int>> toks = ro_tokenize(s, n);
    std::vector<std::string> v;
    v.reserve(toks.size());
    for (const auto& t : toks) v.emplace_back(t.first, (std::size_t)t.second);
    std::sort(v.begin(), v.end());
    v.erase(std::unique(v.begin(), v.end()), v.end());
    return v;
}

static inline std::string ro_join(const std::vector<std::string>& a,
                                   const std::vector<std::string>& b) {
    std::string out;
    for (const std::string& s : a) { if (!out.empty()) out.push_back(' '); out += s; }
    for (const std::string& s : b) { if (!out.empty()) out.push_back(' '); out += s; }
    return out;
}

// fuzzywuzzy fuzz.token_set_ratio() (the non-partial variant: ratio_func =
// ratio): split into token sets, build the "intersection", "intersection +
// A-only", "intersection + B-only" strings, take the best pairwise ratio().
static inline double ro_token_set_ratio(const char* a, int la, const char* b, int lb) {
    std::vector<std::string> A = ro_unique_sorted_tokens(a, la);
    std::vector<std::string> B = ro_unique_sorted_tokens(b, lb);

    std::vector<std::string> inter, only_a, only_b;
    std::size_t i = 0, j = 0;
    while (i < A.size() && j < B.size()) {
        if (A[i] == B[j]) { inter.push_back(A[i]); ++i; ++j; }
        else if (A[i] < B[j]) only_a.push_back(A[i++]);
        else only_b.push_back(B[j++]);
    }
    while (i < A.size()) only_a.push_back(A[i++]);
    while (j < B.size()) only_b.push_back(B[j++]);

    std::string t0 = ro_join(inter, {});
    std::string t1 = ro_join(inter, only_a);
    std::string t2 = ro_join(inter, only_b);

    double r01 = ro_ratio(t0.data(), (int)t0.size(), t1.data(), (int)t1.size());
    double r02 = ro_ratio(t0.data(), (int)t0.size(), t2.data(), (int)t2.size());
    double r12 = ro_ratio(t1.data(), (int)t1.size(), t2.data(), (int)t2.size());
    return std::max({r01, r02, r12});
}

#endif // FAST_STRING_RATCLIFF_OBERSHELP_CORE_H
