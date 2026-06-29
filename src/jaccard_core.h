#ifndef FAST_STRING_JACCARD_CORE_H
#define FAST_STRING_JACCARD_CORE_H

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

// Q-gram Jaccard similarity: |set(A) ∩ set(B)| / |set(A) ∪ set(B)|, where
// set(A) is the set of distinct length-q byte substrings ("q-grams") of A.
//
// For q <= 8 each q-gram packs exactly into a uint64_t (one byte per shift),
// so the q-gram set is built and deduplicated as plain sorted integers —
// no string allocation and no hashing/collisions, just memory comparisons
// stringdist pays for by going through R-level qgram tabulation generically
// for any q. q > 8 falls back to an exact (still hash-free) sorted-string
// path, since q-grams that long are rare for the short name/address strings
// this package targets.

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
static inline double jaccard_from_sorted(const std::vector<T>& a,
                                          const std::vector<T>& b) {
    if (a.empty() && b.empty()) return 1.0;
    std::size_t i = 0, j = 0, inter = 0;
    while (i < a.size() && j < b.size()) {
        if (a[i] == b[j]) { ++inter; ++i; ++j; }
        else if (a[i] < b[j]) ++i;
        else ++j;
    }
    std::size_t uni = a.size() + b.size() - inter;
    return uni == 0 ? 1.0 : (double)inter / (double)uni;
}

static inline double qgram_jaccard_sim(const char* s1, int l1,
                                        const char* s2, int l2, int q) {
    if (q <= 8) {
        thread_local std::vector<uint64_t> a, b;
        qgram_keys_packed(s1, l1, q, a);
        qgram_keys_packed(s2, l2, q, b);
        return jaccard_from_sorted(a, b);
    } else {
        thread_local std::vector<std::string> a, b;
        qgram_keys_strings(s1, l1, q, a);
        qgram_keys_strings(s2, l2, q, b);
        return jaccard_from_sorted(a, b);
    }
}

#endif // FAST_STRING_JACCARD_CORE_H
