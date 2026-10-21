#ifndef FAST_STRING_RAPIDFUZZ_CORE_H
#define FAST_STRING_RAPIDFUZZ_CORE_H

// Ports of the RapidFuzz string scorers (https://github.com/rapidfuzz/RapidFuzz,
// MIT licence, copyright Max Bachmann; its fuzz module grew out of
// fuzzywuzzy, copyright Adam Cohen). They follow rapidfuzz-cpp's
// fuzz_impl.hpp step for step -- including the order in which
// partial_ratio visits windows and how score cutoffs travel between the
// scorers -- so scores and alignments match the Python package.
//
// Text is a sequence of code points, the way RapidFuzz sees a Python str.
// Nothing here touches the R API: all of it runs on worker threads, with
// every buffer in a caller-owned RfScratch (per-pair heap allocation
// serialises TBB workers on the CRT heap lock).

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <utility>
#include <vector>
#include "rapidfuzz_unicode.h"

typedef std::uint32_t rf_char;

struct RfText {
    const rf_char* data;
    int size;
    // Whether any code point is >= U+0100. Python then stores the string
    // with two or four bytes per character, and RapidFuzz's whitespace test
    // also accepts U+0085, U+00A0 and the Unicode spaces; in a one-byte
    // string only the ASCII ones split words.
    bool wide;
};

static inline int rf_popcount64(std::uint64_t x) {
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_popcountll(x);
#else
    x = x - ((x >> 1) & 0x5555555555555555ULL);
    x = (x & 0x3333333333333333ULL) + ((x >> 2) & 0x3333333333333333ULL);
    x = (x + (x >> 4)) & 0x0F0F0F0F0F0F0F0FULL;
    return static_cast<int>((x * 0x0101010101010101ULL) >> 56);
#endif
}

// ---------------------------------------------------------------------------
// Text preparation: UTF-8 decoding and utils.default_process.
// ---------------------------------------------------------------------------

// utils.default_process for one code point: alphanumerics are lowercased,
// everything else becomes a space (see rapidfuzz_unicode.h).
static inline rf_char rf_default_process_char(rf_char c) {
    if (c < 256) return rf_latin1_process[c];
    std::size_t lo = 0, hi = rf_process_run_count;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        if (rf_process_runs[mid].last < c) lo = mid + 1;
        else hi = mid;
    }
    if (lo == rf_process_run_count || rf_process_runs[lo].first > c)
        return 0x20;
    const RfProcessRun& run = rf_process_runs[lo];
    switch (run.kind) {
    case 1: return static_cast<rf_char>(static_cast<std::int64_t>(c) + run.delta);
    case 2: return (c & 1u) == 0 ? c + 1 : c;
    case 3: return (c & 1u) == 1 ? c + 1 : c;
    default: return 0x20;
    }
}

// Decodes s[0, n) into out (room for n code points) and returns the count.
// With `process`, applies default_process: map every code point, then trim
// spaces at both ends. A byte that does not start a valid UTF-8 sequence
// stands for itself, mapped past the Unicode range so it only equals itself.
static inline int rf_prepare_into(const char* s, std::size_t n, bool process,
                                  rf_char* out, bool& wide) {
    const unsigned char* u = reinterpret_cast<const unsigned char*>(s);
    std::size_t count = 0;
    std::size_t i = 0;
    while (i < n) {
        const unsigned char c = u[i];
        if (c < 0x80) {
            out[count++] = c;
            ++i;
            continue;
        }
        const std::size_t width =
            (c >> 5) == 0x6 ? 2 : (c >> 4) == 0xE ? 3 : (c >> 3) == 0x1E ? 4 : 0;
        bool valid = width != 0 && i + width <= n;
        for (std::size_t k = 1; valid && k < width; ++k)
            valid = (u[i + k] & 0xC0) == 0x80;
        if (!valid) {
            out[count++] = 0x110000u + c;
            ++i;
            continue;
        }
        rf_char point = c & (0x7F >> width);
        for (std::size_t k = 1; k < width; ++k)
            point = (point << 6) | (u[i + k] & 0x3F);
        out[count++] = point;
        i += width;
    }

    std::size_t begin = 0;
    if (process) {
        for (std::size_t k = 0; k < count; ++k)
            out[k] = rf_default_process_char(out[k]);
        while (count > 0 && out[count - 1] == 0x20) --count;
        while (begin < count && out[begin] == 0x20) ++begin;
        if (begin != 0) {
            std::memmove(out, out + begin, (count - begin) * sizeof(rf_char));
            count -= begin;
        }
    }
    wide = false;
    for (std::size_t k = 0; k < count; ++k) {
        if (out[k] >= 256) {
            wide = true;
            break;
        }
    }
    return static_cast<int>(count);
}

static inline RfText rf_prepare(const char* s, std::size_t n, bool process,
                                std::vector<rf_char>& buffer) {
    if (buffer.size() < n) buffer.resize(n);
    bool wide = false;
    const int size = rf_prepare_into(s, n, process, buffer.data(), wide);
    return RfText{buffer.data(), size, wide};
}

// ---------------------------------------------------------------------------
// Bit-parallel LCS (the core of RapidFuzz's Indel distance).
// ---------------------------------------------------------------------------

// Pattern-match bit vectors of one string, as rapidfuzz's
// BlockPatternMatchVector: for each code point, the positions that hold it,
// in 64-bit words. Code points below 256 index a table directly; it is kept
// all-zero between uses, so building and clearing cost O(n) rather than
// O(256 * words). Rarer code points are found in a sorted list.
class RfPattern {
public:
    int words = 0;

    void build(const rf_char* s, int n) {
        s_ = s;
        n_ = n;
        words = (n + 63) / 64;
        const std::size_t need = 256u * static_cast<std::size_t>(words);
        if (latin_.size() < need) latin_.resize(need, 0);
        if (zero_.size() < static_cast<std::size_t>(words))
            zero_.resize(static_cast<std::size_t>(words), 0);
        wide_keys_.clear();
        for (int i = 0; i < n; ++i) {
            const rf_char c = s[i];
            if (c < 256) {
                latin_[c * words + i / 64] |= 1ULL << (i % 64);
                latin_set_[c] = 1;
            } else {
                wide_keys_.push_back(c);
            }
        }
        if (wide_keys_.empty()) return;
        std::sort(wide_keys_.begin(), wide_keys_.end());
        wide_keys_.erase(std::unique(wide_keys_.begin(), wide_keys_.end()),
                         wide_keys_.end());
        wide_rows_.assign(wide_keys_.size() * static_cast<std::size_t>(words), 0);
        for (int i = 0; i < n; ++i) {
            const rf_char c = s[i];
            if (c < 256) continue;
            const std::size_t key = static_cast<std::size_t>(
                std::lower_bound(wide_keys_.begin(), wide_keys_.end(), c) -
                wide_keys_.begin());
            wide_rows_[key * words + i / 64] |= 1ULL << (i % 64);
        }
    }

    void clear() {
        for (int i = 0; i < n_; ++i) {
            const rf_char c = s_[i];
            if (c < 256) {
                latin_[c * words + i / 64] = 0;
                latin_set_[c] = 0;
            }
        }
        wide_keys_.clear();
        n_ = 0;
    }

    const std::uint64_t* row(rf_char c) const {
        if (c < 256) return &latin_[c * words];
        if (wide_keys_.empty()) return zero_.data();
        const auto it =
            std::lower_bound(wide_keys_.begin(), wide_keys_.end(), c);
        if (it == wide_keys_.end() || *it != c) return zero_.data();
        return &wide_rows_[static_cast<std::size_t>(it - wide_keys_.begin()) *
                           words];
    }

    bool contains(rf_char c) const {
        if (c < 256) return latin_set_[c] != 0;
        return std::binary_search(wide_keys_.begin(), wide_keys_.end(), c);
    }

private:
    const rf_char* s_ = nullptr;
    int n_ = 0;
    std::vector<std::uint64_t> latin_;
    std::uint8_t latin_set_[256] = {};
    std::vector<rf_char> wide_keys_;
    std::vector<std::uint64_t> wide_rows_;
    std::vector<std::uint64_t> zero_;
};

// Length of the longest common subsequence of the pattern (n1 code points)
// and s2: Hyyro's bit-parallel recurrence, O(n2 * words). Bits above n1 in
// the last word pick up carries but never feed back into lower bits, so
// they are only masked off when counting.
static inline int rf_lcs_with(const RfPattern& pm, int n1, const rf_char* s2,
                              int n2, std::vector<std::uint64_t>& state) {
    if (n1 == 0 || n2 == 0) return 0;
    const int words = pm.words;
    if (words == 1) {
        std::uint64_t S = ~0ULL;
        for (int j = 0; j < n2; ++j) {
            const std::uint64_t u = S & pm.row(s2[j])[0];
            S = (S + u) | (S - u);
        }
        const std::uint64_t mask = n1 == 64 ? ~0ULL : (1ULL << n1) - 1;
        return rf_popcount64(~S & mask);
    }
    state.assign(static_cast<std::size_t>(words), ~0ULL);
    std::uint64_t* S = state.data();
    for (int j = 0; j < n2; ++j) {
        const std::uint64_t* M = pm.row(s2[j]);
        std::uint64_t carry = 0;
        for (int w = 0; w < words; ++w) {
            const std::uint64_t Sw = S[w];
            const std::uint64_t u = Sw & M[w];
            const std::uint64_t x = Sw + u;
            const std::uint64_t sum = x + carry;
            carry = static_cast<std::uint64_t>(x < Sw) |
                    static_cast<std::uint64_t>(sum < x);
            S[w] = sum | (Sw - u);
        }
    }
    int lcs = 0;
    for (int w = 0; w < words; ++w) {
        std::uint64_t bits = ~S[w];
        if (w == words - 1 && (n1 % 64) != 0)
            bits &= (1ULL << (n1 % 64)) - 1;
        lcs += rf_popcount64(bits);
    }
    return lcs;
}

struct RfToken {
    const rf_char* data;
    int size;
};

// Every buffer the scorers need, reused from pair to pair by one worker.
struct RfScratch {
    RfPattern pm;
    std::vector<std::uint64_t> state;
    std::vector<std::size_t> window_scores;
    std::vector<std::pair<std::size_t, std::size_t>> windows, new_windows;
    std::vector<RfToken> tokens_a, tokens_b, unique_a, unique_b;
    std::vector<RfToken> inter, diff_ab, diff_ba;
    std::vector<rf_char> joined_a, joined_b, joined_ab, joined_ba;
};

// LCS of two strings, after stripping their common prefix and suffix (which
// is always part of an LCS); the shorter remainder becomes the pattern.
static inline int rf_lcs_pair(const rf_char* a, int la, const rf_char* b,
                              int lb, RfScratch& sc) {
    int prefix = 0;
    while (prefix < la && prefix < lb && a[prefix] == b[prefix]) ++prefix;
    a += prefix;
    b += prefix;
    la -= prefix;
    lb -= prefix;
    int suffix = 0;
    while (suffix < la && suffix < lb &&
           a[la - 1 - suffix] == b[lb - 1 - suffix])
        ++suffix;
    la -= suffix;
    lb -= suffix;
    if (la == 0 || lb == 0) return prefix + suffix;
    if (la > lb) {
        std::swap(a, b);
        std::swap(la, lb);
    }
    sc.pm.build(a, la);
    const int lcs = rf_lcs_with(sc.pm, la, b, lb, sc.state);
    sc.pm.clear();
    return prefix + suffix + lcs;
}

static inline int rf_indel_distance(const rf_char* a, int la, const rf_char* b,
                                    int lb, RfScratch& sc) {
    return la + lb - 2 * rf_lcs_pair(a, la, b, lb, sc);
}

// ---------------------------------------------------------------------------
// Score arithmetic, written exactly as rapidfuzz-cpp evaluates it.
// ---------------------------------------------------------------------------

// detail::NormSim_to_NormDist
static inline double rf_norm_sim_to_norm_dist(double score_cutoff) {
    return (std::min)(1.0, 1.0 - score_cutoff + 0.00001);
}

// Indel::normalized_similarity on a 0-1 scale, from a known distance: 0
// when the similarity is below score_cutoff.
static inline double rf_indel_norm_sim(int dist, int maximum,
                                       double score_cutoff) {
    const double cutoff_dist = rf_norm_sim_to_norm_dist(score_cutoff);
    double norm_dist = maximum != 0
        ? static_cast<double>(dist) / static_cast<double>(maximum)
        : 0.0;
    if (norm_dist > cutoff_dist) norm_dist = 1.0;
    const double norm_sim = 1.0 - norm_dist;
    return norm_sim >= score_cutoff ? norm_sim : 0.0;
}

// fuzz_detail::norm_distance
static inline double rf_norm_distance(std::size_t dist, std::size_t lensum,
                                      double score_cutoff) {
    const double score = lensum > 0
        ? 100.0 - 100.0 * static_cast<double>(dist) / static_cast<double>(lensum)
        : 100.0;
    return score >= score_cutoff ? score : 0.0;
}

// fuzz_detail::score_cutoff_to_distance
static inline std::size_t rf_score_cutoff_to_distance(double score_cutoff,
                                                      std::size_t lensum) {
    return static_cast<std::size_t>(
        std::ceil(static_cast<double>(lensum) * (1.0 - score_cutoff / 100)));
}

// ---------------------------------------------------------------------------
// fuzz.ratio and fuzz.partial_ratio
// ---------------------------------------------------------------------------

static inline double rf_ratio(const rf_char* a, int la, const rf_char* b,
                              int lb, double score_cutoff, RfScratch& sc) {
    const int maximum = la + lb;
    const double cutoff = score_cutoff / 100;
    // The length difference alone already costs that many edits; when even
    // that best case is below the cutoff the answer is 0.
    if (cutoff > 0 && maximum > 0) {
        const int lower = la > lb ? la - lb : lb - la;
        if (1.0 - static_cast<double>(lower) / static_cast<double>(maximum) <
            cutoff)
            return 0.0;
    }
    const int dist = rf_indel_distance(a, la, b, lb, sc);
    return rf_indel_norm_sim(dist, maximum, cutoff) * 100;
}

// CachedRatio::similarity: ratio against the string sc.pm was built from.
static inline double rf_cached_ratio(const RfPattern& pm, int len1,
                                     const rf_char* s2, int len2,
                                     double score_cutoff,
                                     std::vector<std::uint64_t>& state) {
    const int maximum = len1 + len2;
    const int lcs = rf_lcs_with(pm, len1, s2, len2, state);
    return rf_indel_norm_sim(maximum - 2 * lcs, maximum, score_cutoff / 100) *
           100;
}

// Positions are 0-based and end-exclusive, as in RapidFuzz's ScoreAlignment.
struct RfAlignment {
    double score;
    int src_start, src_end, dest_start, dest_end;
};

// fuzz_detail::partial_ratio_impl. sc.pm must hold the pattern of the
// needle, of length len1 <= len2. Full-length windows of s2 are searched by
// bisection: sliding a window one place changes the Indel distance by at
// most 2, so a range whose end scores cannot enclose a better window is
// skipped. Then the windows hanging off either end are tried.
static inline RfAlignment rf_partial_ratio_impl(int len1, const rf_char* s2,
                                                int len2, double score_cutoff,
                                                RfScratch& sc) {
    RfAlignment res{0.0, 0, len1, 0, len1};
    const RfPattern& pm = sc.pm;
    const std::size_t none = (std::numeric_limits<std::size_t>::max)();

    if (len2 > len1) {
        const std::size_t maximum = static_cast<std::size_t>(len1) * 2;
        const double norm_cutoff_sim =
            rf_norm_sim_to_norm_dist(score_cutoff / 100);
        std::size_t cutoff_dist = static_cast<std::size_t>(
            std::ceil(static_cast<double>(maximum) * norm_cutoff_sim));
        std::size_t best_dist = none;
        std::vector<std::size_t>& scores = sc.window_scores;
        scores.assign(static_cast<std::size_t>(len2 - len1), none);
        std::vector<std::pair<std::size_t, std::size_t>>& windows = sc.windows;
        std::vector<std::pair<std::size_t, std::size_t>>& new_windows =
            sc.new_windows;
        windows.assign(1, std::make_pair(std::size_t(0),
                                         static_cast<std::size_t>(len2 - len1 - 1)));
        new_windows.clear();

        while (!windows.empty()) {
            for (const auto& window : windows) {
                const std::size_t ends[2] = {window.first, window.second};
                for (const std::size_t start : ends) {
                    if (scores[start] != none) continue;
                    scores[start] = maximum - 2 * static_cast<std::size_t>(
                        rf_lcs_with(pm, len1, s2 + start, len1, sc.state));
                    if (scores[start] < cutoff_dist) {
                        cutoff_dist = best_dist = scores[start];
                        res.dest_start = static_cast<int>(start);
                        res.dest_end = static_cast<int>(start) + len1;
                        if (best_dist == 0) {
                            res.score = 100;
                            return res;
                        }
                    }
                }

                const std::size_t cell_diff = window.second - window.first;
                if (cell_diff == 1) continue;

                // The lowest distance any window in between could reach.
                const std::size_t first = scores[window.first];
                const std::size_t second = scores[window.second];
                const std::size_t known_edits =
                    first > second ? first - second : second - first;
                const std::size_t max_score_improvement =
                    (cell_diff - known_edits / 2) / 2 * 2;
                const std::ptrdiff_t min_score =
                    static_cast<std::ptrdiff_t>((std::min)(first, second)) -
                    static_cast<std::ptrdiff_t>(max_score_improvement);
                if (min_score < static_cast<std::ptrdiff_t>(cutoff_dist)) {
                    const std::size_t center = cell_diff / 2;
                    new_windows.emplace_back(window.first, window.first + center);
                    new_windows.emplace_back(window.first + center, window.second);
                }
            }
            std::swap(windows, new_windows);
            new_windows.clear();
        }

        if (best_dist != none) {
            double score = 1.0 - (static_cast<double>(best_dist) /
                                  static_cast<double>(maximum));
            score *= 100;
            if (score >= score_cutoff) score_cutoff = res.score = score;
        }
    }

    for (int i = 1; i < len1; ++i) {
        if (!pm.contains(s2[i - 1])) continue;
        const double ls_ratio =
            rf_cached_ratio(pm, len1, s2, i, score_cutoff, sc.state);
        if (ls_ratio > res.score) {
            score_cutoff = res.score = ls_ratio;
            res.dest_start = 0;
            res.dest_end = i;
            if (res.score == 100.0) return res;
        }
    }

    for (int i = len2 - len1; i < len2; ++i) {
        if (!pm.contains(s2[i])) continue;
        const double ls_ratio =
            rf_cached_ratio(pm, len1, s2 + i, len2 - i, score_cutoff, sc.state);
        if (ls_ratio > res.score) {
            score_cutoff = res.score = ls_ratio;
            res.dest_start = i;
            res.dest_end = len2;
            if (res.score == 100.0) return res;
        }
    }
    return res;
}

// fuzz::partial_ratio_alignment. "src" positions index s1, "dest" ones s2.
static inline RfAlignment rf_partial_ratio_alignment(const rf_char* s1, int len1,
                                                     const rf_char* s2, int len2,
                                                     double score_cutoff,
                                                     RfScratch& sc) {
    if (len1 > len2) {
        RfAlignment result =
            rf_partial_ratio_alignment(s2, len2, s1, len1, score_cutoff, sc);
        std::swap(result.src_start, result.dest_start);
        std::swap(result.src_end, result.dest_end);
        return result;
    }
    if (score_cutoff > 100) return RfAlignment{0.0, 0, len1, 0, len1};
    if (len1 == 0 || len2 == 0)
        return RfAlignment{len1 == len2 ? 100.0 : 0.0, 0, len1, 0, len1};

    sc.pm.build(s1, len1);
    RfAlignment alignment = rf_partial_ratio_impl(len1, s2, len2, score_cutoff, sc);
    sc.pm.clear();
    if (alignment.score != 100 && len1 == len2) {
        score_cutoff = (std::max)(score_cutoff, alignment.score);
        sc.pm.build(s2, len2);
        RfAlignment alignment2 =
            rf_partial_ratio_impl(len2, s1, len1, score_cutoff, sc);
        sc.pm.clear();
        if (alignment2.score > alignment.score) {
            std::swap(alignment2.src_start, alignment2.dest_start);
            std::swap(alignment2.src_end, alignment2.dest_end);
            return alignment2;
        }
    }
    return alignment;
}

static inline double rf_partial_ratio(const rf_char* s1, int len1,
                                      const rf_char* s2, int len2,
                                      double score_cutoff, RfScratch& sc) {
    return rf_partial_ratio_alignment(s1, len1, s2, len2, score_cutoff, sc).score;
}

// ---------------------------------------------------------------------------
// Tokens: detail::sorted_split, set_decomposition and join.
// ---------------------------------------------------------------------------

// detail::is_space: the characters with bidirectional type WS, B or S or
// category Zs. One-byte (all code points < 256) strings only test the ASCII
// ones.
static inline bool rf_is_space(rf_char c, bool wide) {
    switch (c) {
    case 0x0009: case 0x000A: case 0x000B: case 0x000C: case 0x000D:
    case 0x001C: case 0x001D: case 0x001E: case 0x001F: case 0x0020:
        return true;
    default:
        break;
    }
    if (!wide) return false;
    switch (c) {
    case 0x0085: case 0x00A0: case 0x1680:
    case 0x2000: case 0x2001: case 0x2002: case 0x2003: case 0x2004:
    case 0x2005: case 0x2006: case 0x2007: case 0x2008: case 0x2009:
    case 0x200A: case 0x2028: case 0x2029: case 0x202F: case 0x205F:
    case 0x3000:
        return true;
    default:
        return false;
    }
}

static inline bool rf_token_less(const RfToken& x, const RfToken& y) {
    return std::lexicographical_compare(x.data, x.data + x.size,
                                        y.data, y.data + y.size);
}

static inline bool rf_token_equal(const RfToken& x, const RfToken& y) {
    return x.size == y.size && std::equal(x.data, x.data + x.size, y.data);
}

// Whitespace-separated words of s, sorted by code point.
static inline void rf_sorted_split(const RfText& s, std::vector<RfToken>& out) {
    out.clear();
    int i = 0;
    while (i < s.size) {
        while (i < s.size && rf_is_space(s.data[i], s.wide)) ++i;
        const int start = i;
        while (i < s.size && !rf_is_space(s.data[i], s.wide)) ++i;
        if (i > start) out.push_back(RfToken{s.data + start, i - start});
    }
    std::sort(out.begin(), out.end(), rf_token_less);
}

// Words joined by single spaces (SplittedSentenceView::join).
static inline void rf_join(const std::vector<RfToken>& tokens,
                           std::vector<rf_char>& out) {
    out.clear();
    for (std::size_t i = 0; i < tokens.size(); ++i) {
        if (i) out.push_back(0x20);
        out.insert(out.end(), tokens[i].data, tokens[i].data + tokens[i].size);
    }
}

// Length of the joined string (SplittedSentenceView::length).
static inline std::size_t rf_joined_length(const std::vector<RfToken>& tokens) {
    if (tokens.empty()) return 0;
    std::size_t length = tokens.size() - 1;
    for (const RfToken& token : tokens)
        length += static_cast<std::size_t>(token.size);
    return length;
}

// detail::set_decomposition of sc.tokens_a and sc.tokens_b (both sorted):
// after removing duplicates, the words they share and those only in one.
// RapidFuzz searches linearly; a merge of the sorted lists gives the same
// three lists in the same order.
static inline void rf_set_decomposition(RfScratch& sc) {
    sc.unique_a.assign(sc.tokens_a.begin(), sc.tokens_a.end());
    sc.unique_a.erase(std::unique(sc.unique_a.begin(), sc.unique_a.end(),
                                  rf_token_equal),
                      sc.unique_a.end());
    sc.unique_b.assign(sc.tokens_b.begin(), sc.tokens_b.end());
    sc.unique_b.erase(std::unique(sc.unique_b.begin(), sc.unique_b.end(),
                                  rf_token_equal),
                      sc.unique_b.end());
    sc.inter.clear();
    sc.diff_ab.clear();
    sc.diff_ba.clear();
    std::size_t i = 0, j = 0;
    const std::vector<RfToken>& A = sc.unique_a;
    const std::vector<RfToken>& B = sc.unique_b;
    while (i < A.size() && j < B.size()) {
        if (rf_token_equal(A[i], B[j])) {
            sc.inter.push_back(A[i]);
            ++i;
            ++j;
        } else if (rf_token_less(A[i], B[j])) {
            sc.diff_ab.push_back(A[i++]);
        } else {
            sc.diff_ba.push_back(B[j++]);
        }
    }
    while (i < A.size()) sc.diff_ab.push_back(A[i++]);
    while (j < B.size()) sc.diff_ba.push_back(B[j++]);
}

// The three ratios token_set_ratio and token_ratio take the best of, from
// the decomposition in sc: diff_ab vs diff_ba (each preceded by the shared
// words, which cost nothing), and the shared words alone against each side.
static inline double rf_token_set_from_decomposition(double score_cutoff,
                                                     double result,
                                                     RfScratch& sc) {
    rf_join(sc.diff_ab, sc.joined_ab);
    rf_join(sc.diff_ba, sc.joined_ba);
    const std::size_t ab_len = sc.joined_ab.size();
    const std::size_t ba_len = sc.joined_ba.size();
    const std::size_t sect_len = rf_joined_length(sc.inter);

    const std::size_t sect_ab_len = sect_len + (sect_len != 0) + ab_len;
    const std::size_t sect_ba_len = sect_len + (sect_len != 0) + ba_len;

    const std::size_t cutoff_distance =
        rf_score_cutoff_to_distance(score_cutoff, sect_ab_len + sect_ba_len);
    const std::size_t length_gap = ab_len > ba_len ? ab_len - ba_len : ba_len - ab_len;
    if (length_gap <= cutoff_distance) {
        const std::size_t dist = static_cast<std::size_t>(rf_indel_distance(
            sc.joined_ab.data(), static_cast<int>(ab_len),
            sc.joined_ba.data(), static_cast<int>(ba_len), sc));
        if (dist <= cutoff_distance)
            result = (std::max)(result, rf_norm_distance(
                dist, sect_ab_len + sect_ba_len, score_cutoff));
    }

    if (!sect_len) return result;

    const std::size_t sect_ab_dist = (sect_len != 0) + ab_len;
    const double sect_ab_ratio =
        rf_norm_distance(sect_ab_dist, sect_len + sect_ab_len, score_cutoff);
    const std::size_t sect_ba_dist = (sect_len != 0) + ba_len;
    const double sect_ba_ratio =
        rf_norm_distance(sect_ba_dist, sect_len + sect_ba_len, score_cutoff);
    return (std::max)({result, sect_ab_ratio, sect_ba_ratio});
}

// ---------------------------------------------------------------------------
// The token scorers, WRatio and QRatio.
// ---------------------------------------------------------------------------

static inline double rf_ratio_text(const RfText& a, const RfText& b,
                                   double score_cutoff, RfScratch& sc) {
    return rf_ratio(a.data, a.size, b.data, b.size, score_cutoff, sc);
}

static inline double rf_partial_ratio_text(const RfText& a, const RfText& b,
                                           double score_cutoff, RfScratch& sc) {
    return rf_partial_ratio(a.data, a.size, b.data, b.size, score_cutoff, sc);
}

static inline double rf_token_sort_ratio(const RfText& a, const RfText& b,
                                         double score_cutoff, RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    rf_join(sc.tokens_a, sc.joined_a);
    rf_join(sc.tokens_b, sc.joined_b);
    return rf_ratio(sc.joined_a.data(), static_cast<int>(sc.joined_a.size()),
                    sc.joined_b.data(), static_cast<int>(sc.joined_b.size()),
                    score_cutoff, sc);
}

static inline double rf_partial_token_sort_ratio(const RfText& a,
                                                 const RfText& b,
                                                 double score_cutoff,
                                                 RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    rf_join(sc.tokens_a, sc.joined_a);
    rf_join(sc.tokens_b, sc.joined_b);
    return rf_partial_ratio(
        sc.joined_a.data(), static_cast<int>(sc.joined_a.size()),
        sc.joined_b.data(), static_cast<int>(sc.joined_b.size()),
        score_cutoff, sc);
}

static inline double rf_token_set_ratio(const RfText& a, const RfText& b,
                                        double score_cutoff, RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    // fuzzywuzzy returns 0 here, so RapidFuzz does too (RapidFuzz#110).
    if (sc.tokens_a.empty() || sc.tokens_b.empty()) return 0;
    rf_set_decomposition(sc);
    // One sentence is part of the other.
    if (!sc.inter.empty() && (sc.diff_ab.empty() || sc.diff_ba.empty()))
        return 100;
    return rf_token_set_from_decomposition(score_cutoff, 0.0, sc);
}

static inline double rf_partial_token_set_ratio(const RfText& a,
                                                const RfText& b,
                                                double score_cutoff,
                                                RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    if (sc.tokens_a.empty() || sc.tokens_b.empty()) return 0;
    rf_set_decomposition(sc);
    // A word common to both is a perfect partial match.
    if (!sc.inter.empty()) return 100;
    rf_join(sc.diff_ab, sc.joined_ab);
    rf_join(sc.diff_ba, sc.joined_ba);
    return rf_partial_ratio(
        sc.joined_ab.data(), static_cast<int>(sc.joined_ab.size()),
        sc.joined_ba.data(), static_cast<int>(sc.joined_ba.size()),
        score_cutoff, sc);
}

// fuzz::token_ratio: the best of token_sort_ratio and token_set_ratio,
// sharing the tokenisation (unlike token_set_ratio it has no empty-input
// rule, so two empty strings score 100 through the sorted ratio).
static inline double rf_token_ratio(const RfText& a, const RfText& b,
                                    double score_cutoff, RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    rf_set_decomposition(sc);
    if (!sc.inter.empty() && (sc.diff_ab.empty() || sc.diff_ba.empty()))
        return 100;
    rf_join(sc.tokens_a, sc.joined_a);
    rf_join(sc.tokens_b, sc.joined_b);
    const double result = rf_ratio(
        sc.joined_a.data(), static_cast<int>(sc.joined_a.size()),
        sc.joined_b.data(), static_cast<int>(sc.joined_b.size()),
        score_cutoff, sc);
    return rf_token_set_from_decomposition(score_cutoff, result, sc);
}

// fuzz::partial_token_ratio: the best of partial_token_sort_ratio and
// partial_token_set_ratio, sharing the tokenisation.
static inline double rf_partial_token_ratio(const RfText& a, const RfText& b,
                                            double score_cutoff, RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    rf_sorted_split(a, sc.tokens_a);
    rf_sorted_split(b, sc.tokens_b);
    rf_set_decomposition(sc);
    if (!sc.inter.empty()) return 100;

    rf_join(sc.tokens_a, sc.joined_a);
    rf_join(sc.tokens_b, sc.joined_b);
    const double result = rf_partial_ratio(
        sc.joined_a.data(), static_cast<int>(sc.joined_a.size()),
        sc.joined_b.data(), static_cast<int>(sc.joined_b.size()),
        score_cutoff, sc);
    // Without duplicate words the sets are the sorted lists: same answer.
    if (sc.tokens_a.size() == sc.diff_ab.size() &&
        sc.tokens_b.size() == sc.diff_ba.size())
        return result;

    score_cutoff = (std::max)(score_cutoff, result);
    rf_join(sc.diff_ab, sc.joined_ab);
    rf_join(sc.diff_ba, sc.joined_ba);
    return (std::max)(result, rf_partial_ratio(
        sc.joined_ab.data(), static_cast<int>(sc.joined_ab.size()),
        sc.joined_ba.data(), static_cast<int>(sc.joined_ba.size()),
        score_cutoff, sc));
}

static inline double rf_wratio(const RfText& a, const RfText& b,
                               double score_cutoff, RfScratch& sc) {
    if (score_cutoff > 100) return 0;
    const double UNBASE_SCALE = 0.95;
    const int len1 = a.size;
    const int len2 = b.size;
    // fuzzywuzzy returns 0 here, so RapidFuzz does too (RapidFuzz#110).
    if (!len1 || !len2) return 0;

    const double len_ratio = len1 > len2
        ? static_cast<double>(len1) / static_cast<double>(len2)
        : static_cast<double>(len2) / static_cast<double>(len1);

    double end_ratio = rf_ratio_text(a, b, score_cutoff, sc);

    if (len_ratio < 1.5) {
        score_cutoff = (std::max)(score_cutoff, end_ratio) / UNBASE_SCALE;
        return (std::max)(end_ratio,
                          rf_token_ratio(a, b, score_cutoff, sc) * UNBASE_SCALE);
    }

    const double PARTIAL_SCALE = len_ratio <= 8.0 ? 0.9 : 0.6;

    score_cutoff = (std::max)(score_cutoff, end_ratio) / PARTIAL_SCALE;
    end_ratio = (std::max)(
        end_ratio, rf_partial_ratio_text(a, b, score_cutoff, sc) * PARTIAL_SCALE);

    score_cutoff = (std::max)(score_cutoff, end_ratio) / UNBASE_SCALE;
    return (std::max)(end_ratio, rf_partial_token_ratio(a, b, score_cutoff, sc) *
                                     UNBASE_SCALE * PARTIAL_SCALE);
}

static inline double rf_qratio(const RfText& a, const RfText& b,
                               double score_cutoff, RfScratch& sc) {
    // fuzzywuzzy returns 0 here, so RapidFuzz does too (RapidFuzz#110).
    if (!a.size || !b.size) return 0;
    return rf_ratio_text(a, b, score_cutoff, sc);
}

// Scorer ids shared with R (.fuzz_scorers in R/fuzz.R lists them in order).
enum class RfScorer : int {
    ratio = 0,
    partial_ratio = 1,
    token_sort_ratio = 2,
    token_set_ratio = 3,
    token_ratio = 4,
    partial_token_sort_ratio = 5,
    partial_token_set_ratio = 6,
    partial_token_ratio = 7,
    wratio = 8,
    qratio = 9
};

static const int RF_SCORER_COUNT = 10;

static inline double rf_score(RfScorer scorer, const RfText& a, const RfText& b,
                              double score_cutoff, RfScratch& sc) {
    switch (scorer) {
    case RfScorer::ratio: return rf_ratio_text(a, b, score_cutoff, sc);
    case RfScorer::partial_ratio: return rf_partial_ratio_text(a, b, score_cutoff, sc);
    case RfScorer::token_sort_ratio: return rf_token_sort_ratio(a, b, score_cutoff, sc);
    case RfScorer::token_set_ratio: return rf_token_set_ratio(a, b, score_cutoff, sc);
    case RfScorer::token_ratio: return rf_token_ratio(a, b, score_cutoff, sc);
    case RfScorer::partial_token_sort_ratio:
        return rf_partial_token_sort_ratio(a, b, score_cutoff, sc);
    case RfScorer::partial_token_set_ratio:
        return rf_partial_token_set_ratio(a, b, score_cutoff, sc);
    case RfScorer::partial_token_ratio:
        return rf_partial_token_ratio(a, b, score_cutoff, sc);
    case RfScorer::wratio: return rf_wratio(a, b, score_cutoff, sc);
    case RfScorer::qratio: return rf_qratio(a, b, score_cutoff, sc);
    }
    return 0.0;
}

#endif // FAST_STRING_RAPIDFUZZ_CORE_H
