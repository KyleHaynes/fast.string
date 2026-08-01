#ifndef FAST_STRING_UNICODE_METRIC_CORE_H
#define FAST_STRING_UNICODE_METRIC_CORE_H

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>

template <typename Symbol>
static inline int sequence_levenshtein_distance(const Symbol* a, int la,
                                                const Symbol* b, int lb) {
    if (la < lb) {
        std::swap(a, b);
        std::swap(la, lb);
    }
    if (lb == 0) return la;
    std::vector<int> row(static_cast<std::size_t>(lb) + 1);
    for (int j = 0; j <= lb; ++j) row[static_cast<std::size_t>(j)] = j;
    for (int i = 1; i <= la; ++i) {
        int previous_diagonal = row[0];
        row[0] = i;
        for (int j = 1; j <= lb; ++j) {
            const int previous = row[static_cast<std::size_t>(j)];
            const int cost = a[i - 1] == b[j - 1] ? 0 : 1;
            row[static_cast<std::size_t>(j)] = std::min({
                row[static_cast<std::size_t>(j)] + 1,
                row[static_cast<std::size_t>(j - 1)] + 1,
                previous_diagonal + cost
            });
            previous_diagonal = previous;
        }
    }
    return row[static_cast<std::size_t>(lb)];
}

template <typename Symbol>
static inline int sequence_osa_distance(const Symbol* a, int la,
                                        const Symbol* b, int lb) {
    if (la < lb) {
        std::swap(a, b);
        std::swap(la, lb);
    }
    if (lb == 0) return la;
    std::vector<int> row0(static_cast<std::size_t>(lb) + 1, 0);
    std::vector<int> row1(static_cast<std::size_t>(lb) + 1);
    std::vector<int> row2(static_cast<std::size_t>(lb) + 1);
    for (int j = 0; j <= lb; ++j) row1[static_cast<std::size_t>(j)] = j;
    for (int i = 1; i <= la; ++i) {
        row2[0] = i;
        for (int j = 1; j <= lb; ++j) {
            const int cost = a[i - 1] == b[j - 1] ? 0 : 1;
            int best = std::min({
                row1[static_cast<std::size_t>(j)] + 1,
                row2[static_cast<std::size_t>(j - 1)] + 1,
                row1[static_cast<std::size_t>(j - 1)] + cost
            });
            if (i > 1 && j > 1 && a[i - 1] == b[j - 2] &&
                a[i - 2] == b[j - 1]) {
                best = std::min(
                    best, row0[static_cast<std::size_t>(j - 2)] + 1
                );
            }
            row2[static_cast<std::size_t>(j)] = best;
        }
        row0.swap(row1);
        row1.swap(row2);
    }
    return row1[static_cast<std::size_t>(lb)];
}

// Lowrance-Wagner unrestricted Damerau-Levenshtein. Unlike OSA, a
// substring may participate in more than one edit.
template <typename Symbol>
static inline int sequence_damerau_levenshtein_distance_with_workspace(
        const Symbol* a, int la, const Symbol* b, int lb,
        std::vector<int>& matrix,
        std::unordered_map<Symbol, int>& last_row) {
    if (la == 0) return lb;
    if (lb == 0) return la;
    const int sentinel = la + lb;
    const std::size_t columns = static_cast<std::size_t>(lb) + 2;
    matrix.assign((static_cast<std::size_t>(la) + 2) * columns, 0);
    const auto at = [&](int i, int j) -> int& {
        return matrix[static_cast<std::size_t>(i) * columns +
                      static_cast<std::size_t>(j)];
    };
    at(0, 0) = sentinel;
    for (int i = 0; i <= la; ++i) {
        at(i + 1, 1) = i;
        at(i + 1, 0) = sentinel;
    }
    for (int j = 0; j <= lb; ++j) {
        at(1, j + 1) = j;
        at(0, j + 1) = sentinel;
    }

    last_row.clear();
    last_row.reserve(static_cast<std::size_t>(la + lb));
    for (int i = 1; i <= la; ++i) {
        int last_match_column = 0;
        for (int j = 1; j <= lb; ++j) {
            const auto found = last_row.find(b[j - 1]);
            const int matching_row = found == last_row.end()
                ? 0
                : found->second;
            const int matching_column = last_match_column;
            int cost = 1;
            if (a[i - 1] == b[j - 1]) {
                cost = 0;
                last_match_column = j;
            }
            at(i + 1, j + 1) = std::min({
                at(i, j) + cost,
                at(i + 1, j) + 1,
                at(i, j + 1) + 1,
                at(matching_row, matching_column) +
                    (i - matching_row - 1) + 1 +
                    (j - matching_column - 1)
            });
        }
        last_row[a[i - 1]] = i;
    }
    return at(la + 1, lb + 1);
}

template <typename Symbol>
static inline int sequence_damerau_levenshtein_distance(
        const Symbol* a, int la, const Symbol* b, int lb) {
    std::vector<int> matrix;
    std::unordered_map<Symbol, int> last_row;
    return sequence_damerau_levenshtein_distance_with_workspace(
        a, la, b, lb, matrix, last_row
    );
}

template <typename Symbol>
static inline int sequence_bounded_levenshtein_distance(
        const Symbol* a, int la, const Symbol* b, int lb, int cutoff) {
    if (cutoff < 0) return cutoff + 1;
    if (std::abs(la - lb) > cutoff) return cutoff + 1;
    if (la < lb) {
        std::swap(a, b);
        std::swap(la, lb);
    }
    if (lb == 0) return la <= cutoff ? la : cutoff + 1;
    const int outside = cutoff + 1;
    std::vector<int> previous(static_cast<std::size_t>(lb) + 1, outside);
    std::vector<int> current(static_cast<std::size_t>(lb) + 1, outside);
    for (int j = 0; j <= std::min(lb, cutoff); ++j)
        previous[static_cast<std::size_t>(j)] = j;

    for (int i = 1; i <= la; ++i) {
        const int first = std::max(1, i - cutoff);
        const int last = std::min(lb, i + cutoff);
        current[0] = i <= cutoff ? i : outside;
        if (first > 1) current[static_cast<std::size_t>(first - 1)] = outside;
        int row_minimum = outside;
        for (int j = first; j <= last; ++j) {
            const int cost = a[i - 1] == b[j - 1] ? 0 : 1;
            const int value = std::min({
                previous[static_cast<std::size_t>(j)] + 1,
                current[static_cast<std::size_t>(j - 1)] + 1,
                previous[static_cast<std::size_t>(j - 1)] + cost
            });
            current[static_cast<std::size_t>(j)] = std::min(value, outside);
            row_minimum = std::min(row_minimum, value);
        }
        if (last < lb) current[static_cast<std::size_t>(last + 1)] = outside;
        if (row_minimum > cutoff) return outside;
        previous.swap(current);
    }
    const int result = previous[static_cast<std::size_t>(lb)];
    return result <= cutoff ? result : outside;
}

template <typename Symbol>
static inline double sequence_jaro_similarity(const Symbol* a, int la,
                                               const Symbol* b, int lb) {
    if (la == 0 && lb == 0) return 1.0;
    if (la == 0 || lb == 0) return 0.0;
    const int range = std::max(0, std::max(la, lb) / 2 - 1);
    std::vector<unsigned char> matched_a(static_cast<std::size_t>(la), 0);
    std::vector<unsigned char> matched_b(static_cast<std::size_t>(lb), 0);
    int matches = 0;
    for (int i = 0; i < la; ++i) {
        const int first = std::max(0, i - range);
        const int last = std::min(lb - 1, i + range);
        for (int j = first; j <= last; ++j) {
            if (!matched_b[static_cast<std::size_t>(j)] && a[i] == b[j]) {
                matched_a[static_cast<std::size_t>(i)] = 1;
                matched_b[static_cast<std::size_t>(j)] = 1;
                ++matches;
                break;
            }
        }
    }
    if (matches == 0) return 0.0;
    int transpositions = 0;
    int j = 0;
    for (int i = 0; i < la; ++i) {
        if (!matched_a[static_cast<std::size_t>(i)]) continue;
        while (j < lb && !matched_b[static_cast<std::size_t>(j)]) ++j;
        if (j < lb && a[i] != b[j]) ++transpositions;
        ++j;
    }
    const double count = static_cast<double>(matches);
    return (count / la + count / lb +
            (count - transpositions / 2.0) / count) / 3.0;
}

template <typename Symbol>
static inline double sequence_jaro_winkler_similarity(
        const Symbol* a, int la, const Symbol* b, int lb, double p) {
    const double jaro = sequence_jaro_similarity(a, la, b, lb);
    if (jaro == 0.0) return 0.0;
    int prefix = 0;
    const int limit = std::min(4, std::min(la, lb));
    while (prefix < limit && a[prefix] == b[prefix]) ++prefix;
    return jaro + prefix * p * (1.0 - jaro);
}

static inline double normalized_edit_similarity(int distance,
                                                int length_a,
                                                int length_b) {
    const int denominator = std::max(length_a, length_b);
    if (denominator == 0) return 1.0;
    return std::max(0.0, 1.0 -
        static_cast<double>(distance) / static_cast<double>(denominator));
}

#endif // FAST_STRING_UNICODE_METRIC_CORE_H
