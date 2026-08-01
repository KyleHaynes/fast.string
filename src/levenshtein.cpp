// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <limits>
#include <memory>
#include "codepoint_snapshot.h"
#include "metric_dispatch.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

namespace {

struct DistanceWorker : public Worker {
    const StringView* a_bytes;
    const StringView* b_bytes;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    MetricMethod method;
    bool use_bytes;
    RVector<double> output;

    DistanceWorker(const StringView* a_bytes_, const StringView* b_bytes_,
                   const CodepointView* a_codepoints_,
                   const CodepointView* b_codepoints_, MetricMethod method_,
                   bool use_bytes_, NumericVector& output_)
        : a_bytes(a_bytes_), b_bytes(b_bytes_),
          a_codepoints(a_codepoints_), b_codepoints(b_codepoints_),
          method(method_), use_bytes(use_bytes_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<int> matrix;
        std::unordered_map<char, int> byte_last_row;
        std::unordered_map<std::uint32_t, int> codepoint_last_row;
        for (std::size_t i = begin; i < end; ++i) {
            if (a_bytes[i].is_na() || b_bytes[i].is_na()) {
                output[i] = NA_REAL;
                continue;
            }
            int distance;
            if (use_bytes ||
                (a_codepoints[i].ascii && b_codepoints[i].ascii)) {
                distance = metric_distance_bytes_with_workspace(
                    method, a_bytes[i], b_bytes[i], matrix, byte_last_row
                );
            } else {
                distance = metric_distance_codepoints_with_workspace(
                    method, a_codepoints[i], b_codepoints[i],
                    matrix, codepoint_last_row
                );
            }
            output[i] = distance < 0
                ? (std::numeric_limits<double>::infinity)()
                : static_cast<double>(distance);
        }
    }
};

struct DistanceMatrixWorker : public Worker {
    const StringView* a_bytes;
    const StringView* b_bytes;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    MetricMethod method;
    bool use_bytes;
    std::size_t rows;
    double* output;

    DistanceMatrixWorker(
            const StringView* a_bytes_, const StringView* b_bytes_,
            const CodepointView* a_codepoints_,
            const CodepointView* b_codepoints_, MetricMethod method_,
            bool use_bytes_, std::size_t rows_, double* output_)
        : a_bytes(a_bytes_), b_bytes(b_bytes_),
          a_codepoints(a_codepoints_), b_codepoints(b_codepoints_),
          method(method_), use_bytes(use_bytes_), rows(rows_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (begin >= end) return;
        std::vector<int> matrix;
        std::unordered_map<char, int> byte_last_row;
        std::unordered_map<std::uint32_t, int> codepoint_last_row;
        std::size_t column = begin / rows;
        std::size_t row = begin - column * rows;
        for (std::size_t cell = begin; cell < end; ++cell) {
            if (a_bytes[row].is_na() || b_bytes[column].is_na()) {
                output[cell] = NA_REAL;
            } else {
                int distance;
                if (use_bytes ||
                    (a_codepoints[row].ascii &&
                     b_codepoints[column].ascii)) {
                    distance = metric_distance_bytes_with_workspace(
                        method, a_bytes[row], b_bytes[column],
                        matrix, byte_last_row
                    );
                } else {
                    distance = metric_distance_codepoints_with_workspace(
                        method, a_codepoints[row], b_codepoints[column],
                        matrix, codepoint_last_row
                    );
                }
                output[cell] = distance < 0
                    ? (std::numeric_limits<double>::infinity)()
                    : static_cast<double>(distance);
            }
            if (++row == rows) {
                row = 0;
                ++column;
            }
        }
    }
};

struct SimilarityWorker : public Worker {
    const StringView* a_bytes;
    const StringView* b_bytes;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    MetricMethod method;
    bool use_bytes;
    RVector<double> output;

    SimilarityWorker(const StringView* a_bytes_, const StringView* b_bytes_,
                     const CodepointView* a_codepoints_,
                     const CodepointView* b_codepoints_, MetricMethod method_,
                     bool use_bytes_, NumericVector& output_)
        : a_bytes(a_bytes_), b_bytes(b_bytes_),
          a_codepoints(a_codepoints_), b_codepoints(b_codepoints_),
          method(method_), use_bytes(use_bytes_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<int> matrix;
        std::unordered_map<char, int> byte_last_row;
        std::unordered_map<std::uint32_t, int> codepoint_last_row;
        for (std::size_t i = begin; i < end; ++i) {
            if (a_bytes[i].is_na() || b_bytes[i].is_na()) {
                output[i] = NA_REAL;
                continue;
            }
            const bool byte_path = use_bytes ||
                (a_codepoints[i].ascii && b_codepoints[i].ascii);
            const int distance = byte_path
                ? metric_distance_bytes_with_workspace(
                    method, a_bytes[i], b_bytes[i], matrix, byte_last_row
                )
                : metric_distance_codepoints_with_workspace(
                    method, a_codepoints[i], b_codepoints[i],
                    matrix, codepoint_last_row
                );
            const int length_a = byte_path
                ? static_cast<int>(a_bytes[i].size)
                : static_cast<int>(a_codepoints[i].size);
            const int length_b = byte_path
                ? static_cast<int>(b_bytes[i].size)
                : static_cast<int>(b_codepoints[i].size);
            output[i] = normalized_edit_similarity(
                distance, length_a, length_b
            );
        }
    }
};

struct WithinWorker : public Worker {
    const StringView* a_bytes;
    const StringView* b_bytes;
    const CodepointView* a_codepoints;
    const CodepointView* b_codepoints;
    bool use_bytes;
    int cutoff;
    RVector<int> output;

    WithinWorker(const StringView* a_bytes_, const StringView* b_bytes_,
                 const CodepointView* a_codepoints_,
                 const CodepointView* b_codepoints_, bool use_bytes_,
                 int cutoff_, LogicalVector& output_)
        : a_bytes(a_bytes_), b_bytes(b_bytes_),
          a_codepoints(a_codepoints_), b_codepoints(b_codepoints_),
          use_bytes(use_bytes_), cutoff(cutoff_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            if (a_bytes[i].is_na() || b_bytes[i].is_na()) {
                output[i] = NA_LOGICAL;
                continue;
            }
            const int distance = use_bytes ||
                (a_codepoints[i].ascii && b_codepoints[i].ascii)
                ? sequence_bounded_levenshtein_distance(
                    a_bytes[i].data, static_cast<int>(a_bytes[i].size),
                    b_bytes[i].data, static_cast<int>(b_bytes[i].size), cutoff
                )
                : sequence_bounded_levenshtein_distance(
                    a_codepoints[i].data,
                    static_cast<int>(a_codepoints[i].size),
                    b_codepoints[i].data,
                    static_cast<int>(b_codepoints[i].size), cutoff
                );
            output[i] = distance <= cutoff;
        }
    }
};

static NumericVector run_distance(const StringVector& a,
                                  const StringVector& b,
                                  MetricMethod method, int nthreads,
                                  bool use_bytes) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    StringSnapshot a_bytes(a), b_bytes(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    NumericVector result(a.size());
    DistanceWorker worker(
        a_bytes.data(), b_bytes.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        method, use_bytes, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(a.size()), worker,
        estimated_pairwise_string_work(a_bytes, b_bytes), 1000, nthreads
    );
    return result;
}

static NumericMatrix run_distance_matrix(const StringVector& a,
                                         const StringVector& b,
                                         MetricMethod method, int nthreads,
                                         bool use_bytes) {
    const std::size_t rows = static_cast<std::size_t>(a.size());
    const std::size_t columns = static_cast<std::size_t>(b.size());
    if (columns != 0 &&
        rows > (std::numeric_limits<std::size_t>::max)() / columns)
        stop("Requested pairwise matrix is too large.");
    const std::size_t cells = rows * columns;
    StringSnapshot a_bytes(a), b_bytes(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    NumericMatrix result(a.size(), b.size());
    DistanceMatrixWorker worker(
        a_bytes.data(), b_bytes.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        method, use_bytes, rows, REAL(result)
    );
    dispatch_for(
        0, cells, worker,
        estimated_matrix_string_work(a_bytes, b_bytes, cells),
        10000, nthreads, 1024
    );
    return result;
}

} // namespace

// [[Rcpp::export]]
NumericVector fast_levenshtein_impl(const StringVector& a,
                                    const StringVector& b,
                                    int nthreads, bool use_bytes) {
    return run_distance(a, b, MetricMethod::levenshtein, nthreads, use_bytes);
}

// [[Rcpp::export]]
NumericMatrix fast_levenshtein_matrix_impl(const StringVector& a,
                                           const StringVector& b,
                                           int nthreads, bool use_bytes) {
    return run_distance_matrix(
        a, b, MetricMethod::levenshtein, nthreads, use_bytes
    );
}

// [[Rcpp::export]]
NumericVector fast_osa_distance_impl(const StringVector& a,
                                     const StringVector& b,
                                     int nthreads, bool use_bytes) {
    return run_distance(a, b, MetricMethod::osa, nthreads, use_bytes);
}

// [[Rcpp::export]]
NumericMatrix fast_osa_distance_matrix_impl(const StringVector& a,
                                            const StringVector& b,
                                            int nthreads, bool use_bytes) {
    return run_distance_matrix(a, b, MetricMethod::osa, nthreads, use_bytes);
}

// [[Rcpp::export]]
NumericVector fast_damerau_levenshtein_impl(const StringVector& a,
                                            const StringVector& b,
                                            int nthreads, bool use_bytes) {
    return run_distance(
        a, b, MetricMethod::damerau_levenshtein, nthreads, use_bytes
    );
}

// [[Rcpp::export]]
NumericMatrix fast_damerau_levenshtein_matrix_impl(
        const StringVector& a, const StringVector& b,
        int nthreads, bool use_bytes) {
    return run_distance_matrix(
        a, b, MetricMethod::damerau_levenshtein, nthreads, use_bytes
    );
}

// [[Rcpp::export]]
NumericVector fast_hamming_impl(const StringVector& a,
                                const StringVector& b,
                                int nthreads, bool use_bytes) {
    return run_distance(a, b, MetricMethod::hamming, nthreads, use_bytes);
}

// [[Rcpp::export]]
NumericVector fast_edit_similarity_impl(const StringVector& a,
                                        const StringVector& b,
                                        int method, int nthreads,
                                        bool use_bytes) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const MetricMethod selected = static_cast<MetricMethod>(method);
    if (!metric_is_edit(selected)) stop("Invalid edit-distance method.");
    StringSnapshot a_bytes(a), b_bytes(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    NumericVector result(a.size());
    SimilarityWorker worker(
        a_bytes.data(), b_bytes.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        selected, use_bytes, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(a.size()), worker,
        estimated_pairwise_string_work(a_bytes, b_bytes), 1000, nthreads
    );
    return result;
}

// [[Rcpp::export]]
LogicalVector fast_levenshtein_within_impl(const StringVector& a,
                                           const StringVector& b,
                                           int max_distance,
                                           int nthreads,
                                           bool use_bytes) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    StringSnapshot a_bytes(a), b_bytes(b);
    std::unique_ptr<CodepointSnapshot> a_codepoints, b_codepoints;
    if (!use_bytes) {
        a_codepoints.reset(new CodepointSnapshot(a, "a"));
        b_codepoints.reset(new CodepointSnapshot(b, "b"));
    }
    LogicalVector result(a.size());
    WithinWorker worker(
        a_bytes.data(), b_bytes.data(),
        use_bytes ? nullptr : a_codepoints->data(),
        use_bytes ? nullptr : b_codepoints->data(),
        use_bytes, max_distance, result
    );
    dispatch_for(
        0, static_cast<std::size_t>(a.size()), worker,
        estimated_pairwise_string_work(a_bytes, b_bytes), 1000, nthreads
    );
    return result;
}
