#ifndef FAST_STRING_METRIC_DISPATCH_H
#define FAST_STRING_METRIC_DISPATCH_H

#include <algorithm>
#include <limits>
#include "codepoint_snapshot.h"
#include "jaro_winkler_core.h"
#include "levenshtein_core.h"
#include "string_snapshot.h"
#include "unicode_metric_core.h"

enum class MetricMethod : int {
    jaro_winkler = 0,
    levenshtein = 1,
    osa = 2,
    damerau_levenshtein = 3,
    hamming = 4
};

static inline bool metric_is_edit(MetricMethod method) {
    return method != MetricMethod::jaro_winkler &&
           method != MetricMethod::hamming;
}

static inline int metric_distance_bytes(MetricMethod method,
                                        const StringView& a,
                                        const StringView& b) {
    const int la = static_cast<int>(a.size);
    const int lb = static_cast<int>(b.size);
    switch (method) {
    case MetricMethod::levenshtein:
        return levenshtein_distance(a.data, la, b.data, lb);
    case MetricMethod::osa:
        return damerau_levenshtein_distance(a.data, la, b.data, lb);
    case MetricMethod::damerau_levenshtein:
        return sequence_damerau_levenshtein_distance(
            a.data, la, b.data, lb
        );
    case MetricMethod::hamming:
        return hamming_distance(a.data, la, b.data, lb);
    default:
        return -1;
    }
}

static inline int metric_distance_codepoints(MetricMethod method,
                                             const CodepointView& a,
                                             const CodepointView& b) {
    const int la = static_cast<int>(a.size);
    const int lb = static_cast<int>(b.size);
    switch (method) {
    case MetricMethod::levenshtein:
        return sequence_levenshtein_distance(a.data, la, b.data, lb);
    case MetricMethod::osa:
        return sequence_osa_distance(a.data, la, b.data, lb);
    case MetricMethod::damerau_levenshtein:
        return sequence_damerau_levenshtein_distance(
            a.data, la, b.data, lb
        );
    case MetricMethod::hamming:
        if (la != lb) return -1;
        {
            int distance = 0;
            for (int i = 0; i < la; ++i)
                distance += a.data[i] != b.data[i];
            return distance;
        }
    default:
        return -1;
    }
}

static inline int metric_distance_bytes_with_workspace(
        MetricMethod method, const StringView& a, const StringView& b,
        std::vector<int>& matrix,
        std::unordered_map<char, int>& last_row) {
    if (method != MetricMethod::damerau_levenshtein)
        return metric_distance_bytes(method, a, b);
    return sequence_damerau_levenshtein_distance_with_workspace(
        a.data, static_cast<int>(a.size),
        b.data, static_cast<int>(b.size), matrix, last_row
    );
}

static inline int metric_distance_codepoints_with_workspace(
        MetricMethod method, const CodepointView& a, const CodepointView& b,
        std::vector<int>& matrix,
        std::unordered_map<std::uint32_t, int>& last_row) {
    if (method != MetricMethod::damerau_levenshtein)
        return metric_distance_codepoints(method, a, b);
    return sequence_damerau_levenshtein_distance_with_workspace(
        a.data, static_cast<int>(a.size),
        b.data, static_cast<int>(b.size), matrix, last_row
    );
}

static inline double metric_similarity_bytes(MetricMethod method,
                                             const StringView& a,
                                             const StringView& b,
                                             double p) {
    if (method == MetricMethod::jaro_winkler) {
        return jaro_winkler_sim(
            a.data, static_cast<int>(a.size),
            b.data, static_cast<int>(b.size), p
        );
    }
    const int distance = metric_distance_bytes(method, a, b);
    if (distance < 0) return 0.0;
    return normalized_edit_similarity(
        distance, static_cast<int>(a.size), static_cast<int>(b.size)
    );
}

static inline double metric_similarity_codepoints(MetricMethod method,
                                                  const CodepointView& a,
                                                  const CodepointView& b,
                                                  double p) {
    if (method == MetricMethod::jaro_winkler) {
        return sequence_jaro_winkler_similarity(
            a.data, static_cast<int>(a.size),
            b.data, static_cast<int>(b.size), p
        );
    }
    const int distance = metric_distance_codepoints(method, a, b);
    if (distance < 0) return 0.0;
    return normalized_edit_similarity(
        distance, static_cast<int>(a.size), static_cast<int>(b.size)
    );
}

#endif // FAST_STRING_METRIC_DISPATCH_H
