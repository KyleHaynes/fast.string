#ifndef FAST_STRING_QGRAM_CORE_H
#define FAST_STRING_QGRAM_CORE_H

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

// Shared machinery for the package's q-gram set-overlap metrics: Jaccard
// index, Sorensen-Dice coefficient, and the Tversky index that generalises
// both. All three reduce to the same triple -- |A|, |B|, |A ∩ B| -- of a
// string's set of distinct length-q byte substrings, so qgram_overlap()
// computes that once and jaccard_sim/dice_sim/tversky_sim just combine it
// differently.
//
// For q <= 8 each q-gram packs exactly into a uint64_t (one byte per shift),
// so the q-gram set is built and deduplicated as plain sorted integers —
// no string allocation and no hashing/collisions, just memory comparisons —
// which is the speed edge over stringdist's generic R-level qgram
// tabulation for any q. q > 8 falls back to an exact (still hash-free)
// sorted-string path, since q-grams that long are rare for the short
// name/address strings this package targets.

static inline void qgram_keys_packed(const char* s, int n, int q,
                                      std::vector<uint64_t>& out) {
    out.clear();
    if (n < q) return;
    out.reserve((std::size_t)(n - q + 1));
    uint64_t key = 0;
    uint64_t mask = (q == 8) ? ~0ULL : ((1ULL << (8 * q)) - 1);
    for (int i = 0; i < q - 1; ++i) key = (key << 8) | (unsigned char)s[i];
    for (int i = q - 1; i < n; ++i) {
        key = ((key << 8) | (unsigned char)s[i]) & mask;
        out.push_back(key);
    }
    std::sort(out.begin(), out.end());
    out.erase(std::unique(out.begin(), out.end()), out.end());
}

static inline void qgram_keys_strings(const char* s, int n, int q,
                                       std::vector<std::string>& out) {
    out.clear();
    if (n < q) return;
    out.reserve((std::size_t)(n - q + 1));
    for (int i = 0; i + q <= n; ++i) out.emplace_back(s + i, (std::size_t)q);
    std::sort(out.begin(), out.end());
    out.erase(std::unique(out.begin(), out.end()), out.end());
}

template <typename T>
static inline std::size_t sorted_intersection_size(const T* a, std::size_t na,
                                                    const T* b, std::size_t nb) {
    std::size_t i = 0, j = 0, inter = 0;
    while (i < na && j < nb) {
        if (a[i] == b[j]) { ++inter; ++i; ++j; }
        else if (a[i] < b[j]) ++i;
        else ++j;
    }
    return inter;
}

template <typename T>
static inline std::size_t sorted_intersection_size(const std::vector<T>& a,
                                                    const std::vector<T>& b) {
    return sorted_intersection_size(a.data(), a.size(), b.data(), b.size());
}

// Sizes of the two q-gram sets plus their intersection size -- everything
// downstream (Jaccard/Dice/Tversky) needs.
struct QgramOverlap { std::size_t size_a, size_b, inter; };

static inline double qgram_jaccard_from_overlap(const QgramOverlap& o) {
    if (o.size_a == 0 && o.size_b == 0) return 1.0;
    std::size_t uni = o.size_a + o.size_b - o.inter;
    return uni == 0 ? 1.0 : (double)o.inter / (double)uni;
}

static inline double qgram_dice_from_overlap(const QgramOverlap& o) {
    if (o.size_a == 0 && o.size_b == 0) return 1.0;
    std::size_t denom = o.size_a + o.size_b;
    return denom == 0 ? 1.0 : (2.0 * (double)o.inter) / (double)denom;
}

static inline double qgram_tversky_from_overlap(const QgramOverlap& o,
                                                 double alpha, double beta) {
    if (o.size_a == 0 && o.size_b == 0) return 1.0;
    double denom = (double)o.inter + alpha * (double)(o.size_a - o.inter)
                                    + beta  * (double)(o.size_b - o.inter);
    return denom == 0 ? 1.0 : (double)o.inter / denom;
}

// Deliberately plain (non-thread_local) locals: thread_local non-POD
// objects (std::vector, std::string) crash the first time they're touched
// inside an RcppParallel/TBB worker thread on this toolchain -- see the
// comment above levenshtein_dp() in levenshtein_core.h for the full story.
static inline QgramOverlap qgram_overlap(const char* s1, int l1,
                                          const char* s2, int l2, int q) {
    if (q <= 8) {
        std::vector<uint64_t> a, b;
        qgram_keys_packed(s1, l1, q, a);
        qgram_keys_packed(s2, l2, q, b);
        return QgramOverlap{a.size(), b.size(), sorted_intersection_size(a, b)};
    } else {
        std::vector<std::string> a, b;
        qgram_keys_strings(s1, l1, q, a);
        qgram_keys_strings(s2, l2, q, b);
        return QgramOverlap{a.size(), b.size(), sorted_intersection_size(a, b)};
    }
}

static inline double qgram_jaccard_sim(const char* s1, int l1, const char* s2, int l2, int q) {
    return qgram_jaccard_from_overlap(qgram_overlap(s1, l1, s2, l2, q));
}

static inline double qgram_dice_sim(const char* s1, int l1, const char* s2, int l2, int q) {
    return qgram_dice_from_overlap(qgram_overlap(s1, l1, s2, l2, q));
}

// Tversky index: generalises Jaccard (alpha = beta = 1) and Dice
// (alpha = beta = 0.5) by weighting how much each side's leftover
// (non-shared) q-grams count against the score.
static inline double qgram_tversky_sim(const char* s1, int l1, const char* s2, int l2,
                                        int q, double alpha, double beta) {
    return qgram_tversky_from_overlap(
        qgram_overlap(s1, l1, s2, l2, q), alpha, beta
    );
}

#endif // FAST_STRING_QGRAM_CORE_H
