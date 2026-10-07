#ifndef FAST_STRING_RATCLIFF_OBERSHELP_CORE_H
#define FAST_STRING_RATCLIFF_OBERSHELP_CORE_H

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <utility>
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

// Positions of every symbol of b, ascending. Bytes are bucketed directly by
// value -- exact and hash-free, unlike difflib's b2j dict keyed by arbitrary
// characters; code points (used for non-ASCII text, so that a multi-byte
// character counts once, as in Python) go through a sorted table.
struct ROIndexBytes {
    std::array<std::vector<int>, 256> pos;
    ROIndexBytes(const char* b, int lb) {
        for (int j = 0; j < lb; ++j) pos[(unsigned char)b[j]].push_back(j);
    }
    const std::vector<int>& at(char c) const { return pos[(unsigned char)c]; }
};

struct ROIndexCodepoints {
    std::vector<std::pair<std::uint32_t, std::vector<int>>> entries;
    std::vector<int> none;
    ROIndexCodepoints(const std::uint32_t* b, int lb) {
        std::vector<std::pair<std::uint32_t, int>> order;
        order.reserve(static_cast<std::size_t>(lb));
        for (int j = 0; j < lb; ++j) order.emplace_back(b[j], j);
        std::sort(order.begin(), order.end());
        for (const auto& item : order) {
            if (entries.empty() || entries.back().first != item.first)
                entries.emplace_back(item.first, std::vector<int>());
            entries.back().second.push_back(item.second);
        }
    }
    const std::vector<int>& at(std::uint32_t c) const {
        const auto it = std::lower_bound(
            entries.begin(), entries.end(), c,
            [](const std::pair<std::uint32_t, std::vector<int>>& entry,
               std::uint32_t value) { return entry.first < value; }
        );
        return it != entries.end() && it->first == c ? it->second : none;
    }
};

template <typename Sym> struct ROIndexFor;
template <> struct ROIndexFor<char> { typedef ROIndexBytes type; };
template <> struct ROIndexFor<std::uint32_t> { typedef ROIndexCodepoints type; };

// Longest matching block within a[alo,ahi) vs b[blo,bhi), mirroring
// difflib.SequenceMatcher.find_longest_match. `len_at`/`new_len_at` are
// caller-owned scratch buffers (size >= lb+1) reused across calls to avoid
// reallocating on every recursive step.
template <typename Sym, typename Index>
static inline ROBlock ro_find_longest_match(const Sym* a, int alo, int ahi,
                                             int blo, int bhi,
                                             const Index& idx,
                                             std::vector<int>& len_at,
                                             std::vector<int>& new_len_at) {
    int besti = alo, bestj = blo, bestsize = 0;
    int n = bhi - blo + 1;
    std::fill(len_at.begin(), len_at.begin() + n, 0);

    for (int i = alo; i < ahi; ++i) {
        std::fill(new_len_at.begin(), new_len_at.begin() + n, 0);
        const std::vector<int>& js = idx.at(a[i]);
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
// same contract as difflib.SequenceMatcher.get_matching_blocks() minus the
// zero-size terminal block difflib appends (ro_partial_ratio() adds it back).
template <typename Sym>
static inline std::vector<ROBlock> ro_matching_blocks(const Sym* a, int la,
                                                       const Sym* b, int lb) {
    std::vector<ROBlock> blocks;
    if (la == 0 || lb == 0) return blocks;

    const typename ROIndexFor<Sym>::type idx(b, lb);
    std::vector<int> len_at((std::size_t)lb + 1, 0);
    std::vector<int> new_len_at((std::size_t)lb + 1, 0);

    struct Range { int alo, ahi, blo, bhi; };
    std::vector<Range> stack;
    stack.push_back({0, la, 0, lb});

    while (!stack.empty()) {
        Range r = stack.back();
        stack.pop_back();
        ROBlock m = ro_find_longest_match(a, r.alo, r.ahi, r.blo, r.bhi,
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

template <typename Sym>
static inline int ro_total_matched(const Sym* a, int la, const Sym* b, int lb) {
    int m = 0;
    for (const ROBlock& blk : ro_matching_blocks(a, la, b, lb)) m += blk.size;
    return m;
}

template <typename Sym>
static inline bool ro_equal(const Sym* a, int la, const Sym* b, int lb) {
    return la == lb && (la == 0 || a == b || std::equal(a, a + la, b));
}

// difflib SequenceMatcher.ratio(): 2*M / T, T = len(a) + len(b).
template <typename Sym>
static inline double ro_ratio(const Sym* a, int la, const Sym* b, int lb) {
    if (ro_equal(a, la, b, lb)) return 1.0;
    int m = ro_total_matched(a, la, b, lb);
    return (2.0 * m) / (double)(la + lb);
}

// fuzzywuzzy fuzz.partial_ratio(): align the shorter string against every
// matching block's offset into the longer one, take the best full ratio()
// over those alignments.
template <typename Sym>
static inline double ro_partial_ratio(const Sym* s1, int l1, const Sym* s2, int l2) {
    if (ro_equal(s1, l1, s2, l2)) return 1.0;
    const Sym* shorter; int ls;
    const Sym* longer; int ll;
    if (l1 <= l2) { shorter = s1; ls = l1; longer = s2; ll = l2; }
    else          { shorter = s2; ls = l2; longer = s1; ll = l1; }
    if (ls == 0) return (ll == 0) ? 1.0 : 0.0;

    std::vector<ROBlock> blocks = ro_matching_blocks(shorter, ls, longer, ll);
    // difflib ends get_matching_blocks() with a (len(a), len(b), 0) block, and
    // fuzzywuzzy scores its window too: the shorter string aligned with the
    // end of the longer one.
    blocks.push_back(ROBlock{ls, ll, 0});

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

// The ASCII characters Python's str.split() splits on. (The locale's
// isspace() could also match bytes inside UTF-8 characters.)
static inline bool ro_is_space(char c) {
    const unsigned char u = static_cast<unsigned char>(c);
    return u == ' ' || (u >= '\t' && u <= '\r') || (u >= 0x1C && u <= 0x1F);
}

static inline std::vector<std::pair<const char*, int>> ro_tokenize(const char* s, int n) {
    std::vector<std::pair<const char*, int>> toks;
    int i = 0;
    while (i < n) {
        while (i < n && ro_is_space(s[i])) ++i;
        int start = i;
        while (i < n && !ro_is_space(s[i])) ++i;
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
// A-only", "intersection + B-only" strings, take the best pairwise
// `ratio(t_i, t_j)` of those std::strings.
template <typename RatioFn>
static inline double ro_token_set_ratio(const char* a, int la, const char* b, int lb,
                                        RatioFn ratio) {
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

    return std::max({ratio(t0, t1), ratio(t0, t2), ratio(t1, t2)});
}

#endif // FAST_STRING_RATCLIFF_OBERSHELP_CORE_H
