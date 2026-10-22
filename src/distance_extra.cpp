// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "codepoint_snapshot.h"
#include "parallel_dispatch.h"
#include "rapidfuzz_core.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// The rest of RapidFuzz's distance module: LCSseq, Indel, Prefix and
// Postfix, weighted Levenshtein (Wagner-Fischer), and Levenshtein editops /
// opcodes with RapidFuzz's choice among equally short alignments.

namespace {

// One string as code units: code points, or bytes with use_bytes. Workers
// copy into a per-thread buffer, so the metrics see one representation.
struct SequenceSource {
    const StringView* bytes;
    const CodepointView* points;  // null with use_bytes

    bool missing(std::size_t i) const { return bytes[i].is_na(); }

    const rf_char* get(std::size_t i, std::vector<rf_char>& buffer,
                       int& size) const {
        if (points) {
            size = static_cast<int>(points[i].size);
            return points[i].data;
        }
        const StringView& v = bytes[i];
        if (buffer.size() < v.size) buffer.resize(v.size);
        for (std::size_t k = 0; k < v.size; ++k)
            buffer[k] = static_cast<unsigned char>(v.data[k]);
        size = static_cast<int>(v.size);
        return buffer.data();
    }
};

// Holds the snapshots behind a SequenceSource.
struct SequenceInput {
    StringSnapshot bytes;
    std::unique_ptr<CodepointSnapshot> points;

    SequenceInput(const StringVector& x, bool use_bytes, const char* name)
        : bytes(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8) {
        if (!use_bytes) points.reset(new CodepointSnapshot(x, name));
    }

    SequenceSource source() const {
        return SequenceSource{bytes.data(), points ? points->data() : nullptr};
    }
};

enum class SimpleMetric : int {
    lcs_distance = 0, lcs_similarity, indel_distance, indel_similarity,
    prefix_distance, prefix_similarity, postfix_distance, postfix_similarity
};

double simple_metric(SimpleMetric metric, const rf_char* a, int la,
                     const rf_char* b, int lb, RfScratch& scratch) {
    const int longest = (std::max)(la, lb);
    switch (metric) {
    case SimpleMetric::lcs_distance:
        return longest - rf_lcs_pair(a, la, b, lb, scratch);
    case SimpleMetric::lcs_similarity:
        return longest == 0
            ? 1.0
            : static_cast<double>(rf_lcs_pair(a, la, b, lb, scratch)) / longest;
    case SimpleMetric::indel_distance:
        return la + lb - 2 * rf_lcs_pair(a, la, b, lb, scratch);
    case SimpleMetric::indel_similarity:
        return la + lb == 0
            ? 1.0
            : 1.0 - static_cast<double>(la + lb - 2 * rf_lcs_pair(a, la, b, lb, scratch)) /
                        (la + lb);
    case SimpleMetric::prefix_distance:
    case SimpleMetric::prefix_similarity: {
        int k = 0;
        while (k < la && k < lb && a[k] == b[k]) ++k;
        if (metric == SimpleMetric::prefix_distance) return longest - k;
        return longest == 0 ? 1.0 : static_cast<double>(k) / longest;
    }
    case SimpleMetric::postfix_distance:
    case SimpleMetric::postfix_similarity: {
        int k = 0;
        while (k < la && k < lb && a[la - 1 - k] == b[lb - 1 - k]) ++k;
        if (metric == SimpleMetric::postfix_distance) return longest - k;
        return longest == 0 ? 1.0 : static_cast<double>(k) / longest;
    }
    }
    return NA_REAL;
}

struct SimpleMetricWorker : public Worker {
    SequenceSource a, b;
    SimpleMetric metric;
    RVector<double> out;

    SimpleMetricWorker(SequenceSource a_, SequenceSource b_,
                       SimpleMetric metric_, NumericVector& out_)
        : a(a_), b(b_), metric(metric_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        RfScratch scratch;
        std::vector<rf_char> buffer_a, buffer_b;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const rf_char* pa = a.get(i, buffer_a, la);
            const rf_char* pb = b.get(i, buffer_b, lb);
            out[i] = simple_metric(metric, pa, la, pb, lb, scratch);
        }
    }
};

// --- Weighted Levenshtein ----------------------------------------------------

struct Weights {
    double insert, remove, replace;
};

double weighted_levenshtein(const rf_char* a, int la, const rf_char* b, int lb,
                            const Weights& w, std::vector<double>& row) {
    // Common prefix and suffix cost nothing.
    while (la > 0 && lb > 0 && a[0] == b[0]) { ++a; ++b; --la; --lb; }
    while (la > 0 && lb > 0 && a[la - 1] == b[lb - 1]) { --la; --lb; }
    row.resize(static_cast<std::size_t>(lb) + 1);
    for (int j = 0; j <= lb; ++j) row[j] = j * w.insert;
    for (int i = 1; i <= la; ++i) {
        double diagonal = row[0];
        row[0] = i * w.remove;
        for (int j = 1; j <= lb; ++j) {
            const double above = row[j];
            double best = above + w.remove;
            best = (std::min)(best, row[j - 1] + w.insert);
            best = (std::min)(best, diagonal + (a[i - 1] == b[j - 1] ? 0.0 : w.replace));
            row[j] = best;
            diagonal = above;
        }
    }
    return row[static_cast<std::size_t>(lb)];
}

// RapidFuzz's levenshtein_maximum: the cost of the cheaper of deleting
// everything and inserting everything, or replacing and then inserting or
// deleting the rest.
double weighted_maximum(int la, int lb, const Weights& w) {
    double maximum = la * w.remove + lb * w.insert;
    if (la >= lb)
        maximum = (std::min)(maximum, lb * w.replace + (la - lb) * w.remove);
    else
        maximum = (std::min)(maximum, la * w.replace + (lb - la) * w.insert);
    return maximum;
}

struct WeightedWorker : public Worker {
    SequenceSource a, b;
    Weights weights;
    bool similarity;
    RVector<double> out;

    WeightedWorker(SequenceSource a_, SequenceSource b_, Weights weights_,
                   bool similarity_, NumericVector& out_)
        : a(a_), b(b_), weights(weights_), similarity(similarity_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<rf_char> buffer_a, buffer_b;
        std::vector<double> row;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const rf_char* pa = a.get(i, buffer_a, la);
            const rf_char* pb = b.get(i, buffer_b, lb);
            const double d = weighted_levenshtein(pa, la, pb, lb, weights, row);
            if (!similarity) {
                out[i] = d;
            } else {
                const double maximum = weighted_maximum(la, lb, weights);
                out[i] = maximum > 0 ? 1.0 - d / maximum : 1.0;
            }
        }
    }
};

struct WeightedMatrixWorker : public Worker {
    SequenceSource a, b;
    Weights weights;
    std::size_t rows;
    double* out;

    WeightedMatrixWorker(SequenceSource a_, SequenceSource b_, Weights weights_,
                         std::size_t rows_, double* out_)
        : a(a_), b(b_), weights(weights_), rows(rows_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<rf_char> buffer_a, buffer_b;
        std::vector<double> row;
        for (std::size_t cell = begin; cell < end; ++cell) {
            const std::size_t i = cell % rows, j = cell / rows;
            if (a.missing(i) || b.missing(j)) {
                out[cell] = NA_REAL;
                continue;
            }
            int la, lb;
            const rf_char* pa = a.get(i, buffer_a, la);
            const rf_char* pb = b.get(j, buffer_b, lb);
            out[cell] = weighted_levenshtein(pa, la, pb, lb, weights, row);
        }
    }
};

// --- Levenshtein editops (rapidfuzz-cpp levenshtein_editops) ----------------

enum EditKind : int { EDIT_EQUAL = 0, EDIT_REPLACE = 1, EDIT_INSERT = 2, EDIT_DELETE = 3 };

struct EditOp {
    int kind;
    std::size_t src;
    std::size_t dest;
};

typedef std::vector<rf_char> Seq;

std::size_t unit_levenshtein(const rf_char* a, std::size_t la, const rf_char* b,
                             std::size_t lb) {
    std::vector<std::size_t> row(lb + 1);
    for (std::size_t j = 0; j <= lb; ++j) row[j] = j;
    for (std::size_t i = 1; i <= la; ++i) {
        std::size_t diagonal = row[0];
        row[0] = i;
        for (std::size_t j = 1; j <= lb; ++j) {
            const std::size_t above = row[j];
            row[j] = (std::min)((std::min)(above, row[j - 1]) + 1,
                                diagonal + (a[i - 1] != b[j - 1]));
            diagonal = above;
        }
    }
    return row[lb];
}

// The last row of the DP of s1 (columns) against s2[0, rows): row[c] is the
// distance between s1[0, c) and s2[0, rows).
void levenshtein_last_row(const rf_char* s1, std::size_t n, const rf_char* s2,
                          std::size_t rows, std::vector<std::size_t>& row) {
    row.resize(n + 1);
    for (std::size_t c = 0; c <= n; ++c) row[c] = c;
    for (std::size_t r = 1; r <= rows; ++r) {
        std::size_t diagonal = row[0];
        row[0] = r;
        for (std::size_t c = 1; c <= n; ++c) {
            const std::size_t above = row[c];
            row[c] = (std::min)((std::min)(above, row[c - 1]) + 1,
                                diagonal + (s1[c - 1] != s2[r - 1]));
            diagonal = above;
        }
    }
}

const std::size_t EDITOPS_MAX_CELLS = std::size_t(1) << 28;

// levenshtein_align + recover_alignment on the full DP matrix. RapidFuzz
// records the horizontal deltas D[r][c] - D[r][c-1] as bit vectors (VP for
// +1, VN for -1); they are recomputed here, and the backtrace makes the same
// choices.
void align_direct(const rf_char* s1, std::size_t n, const rf_char* s2,
                  std::size_t m, std::size_t src, std::size_t dest,
                  std::vector<EditOp>& ops) {
    if (n == 0 || m == 0) {
        const std::size_t dist = n + m;
        if (dist == 0) return;
        const std::size_t first = ops.size();
        ops.resize(first + dist);
        std::size_t d = dist, col = n, row = m;
        while (col) { --d; --col; ops[first + d] = EditOp{EDIT_DELETE, col + src, row + dest}; }
        while (row) { --d; --row; ops[first + d] = EditOp{EDIT_INSERT, col + src, row + dest}; }
        return;
    }
    if (n * m > EDITOPS_MAX_CELLS)
        stop("The strings are too long for edit_ops().");
    std::vector<std::int8_t> delta(n * m);  // rows 1..m, columns 1..n
    std::vector<std::size_t> row(n + 1);
    for (std::size_t c = 0; c <= n; ++c) row[c] = c;
    for (std::size_t r = 1; r <= m; ++r) {
        std::size_t diagonal = row[0];
        row[0] = r;
        for (std::size_t c = 1; c <= n; ++c) {
            const std::size_t above = row[c];
            row[c] = (std::min)((std::min)(above, row[c - 1]) + 1,
                                diagonal + (s1[c - 1] != s2[r - 1]));
            diagonal = above;
            delta[(r - 1) * n + (c - 1)] = static_cast<std::int8_t>(
                static_cast<long long>(row[c]) - static_cast<long long>(row[c - 1]));
        }
    }
    const std::size_t dist = row[n];
    if (dist == 0) return;
    const std::size_t first = ops.size();
    ops.resize(first + dist);
    std::size_t d = dist, col = n, r = m;
    auto h = [&delta, n](std::size_t at_row, std::size_t at_col) {
        return delta[(at_row - 1) * n + (at_col - 1)];
    };
    while (r && col) {
        if (h(r, col) == 1) {
            --d;
            --col;
            ops[first + d] = EditOp{EDIT_DELETE, col + src, r + dest};
        } else {
            --r;
            if (r && h(r, col) == -1) {
                --d;
                ops[first + d] = EditOp{EDIT_INSERT, col + src, r + dest};
            } else {
                --col;
                if (s1[col] != s2[r]) {
                    --d;
                    ops[first + d] = EditOp{EDIT_REPLACE, col + src, r + dest};
                }
            }
        }
    }
    while (col) { --d; --col; ops[first + d] = EditOp{EDIT_DELETE, col + src, r + dest}; }
    while (r) { --d; --r; ops[first + d] = EditOp{EDIT_INSERT, col + src, r + dest}; }
}

// levenshtein_align_hirschberg: strip the common affix, then align directly
// when the bit matrix would be small, else split at RapidFuzz's Hirschberg
// point (the first column with the lowest total) and recurse.
void align_hirschberg(const rf_char* s1, std::size_t n, const rf_char* s2,
                      std::size_t m, std::size_t src, std::size_t dest,
                      std::size_t max, std::vector<EditOp>& ops) {
    std::size_t prefix = 0;
    while (prefix < n && prefix < m && s1[prefix] == s2[prefix]) ++prefix;
    s1 += prefix; s2 += prefix; n -= prefix; m -= prefix;
    src += prefix; dest += prefix;
    std::size_t suffix = 0;
    while (suffix < n && suffix < m && s1[n - 1 - suffix] == s2[m - 1 - suffix]) ++suffix;
    n -= suffix; m -= suffix;

    max = (std::min)(max, (std::max)(n, m));
    const std::size_t full_band = (std::min)(n, 2 * max + 1);
    const std::size_t matrix_size = 2 * full_band * m / 8;
    if (matrix_size < 1024 * 1024 || n < 65 || m < 10) {
        align_direct(s1, n, s2, m, src, dest, ops);
        return;
    }
    const std::size_t left_size = m / 2, right_size = m - left_size;
    std::vector<std::size_t> left, right;
    levenshtein_last_row(s1, n, s2, left_size, left);
    Seq r1(s1, s1 + n), r2(s2 + left_size, s2 + m);
    std::reverse(r1.begin(), r1.end());
    std::reverse(r2.begin(), r2.end());
    levenshtein_last_row(r1.data(), n, r2.data(), right_size, right);
    std::size_t best = (std::numeric_limits<std::size_t>::max)();
    std::size_t mid = 0, left_score = 0, right_score = 0;
    for (std::size_t c = 0; c <= n; ++c) {
        const std::size_t total = left[c] + right[n - c];
        if (total < best) {
            best = total;
            mid = c;
            left_score = left[c];
            right_score = right[n - c];
        }
    }
    align_hirschberg(s1, mid, s2, left_size, src, dest, left_score, ops);
    align_hirschberg(s1 + mid, n - mid, s2 + left_size, right_size, src + mid,
                     dest + left_size, right_score, ops);
}

void levenshtein_editops(const rf_char* s1, std::size_t n, const rf_char* s2,
                         std::size_t m, std::vector<EditOp>& ops) {
    ops.clear();
    std::size_t cutoff = (std::max)(n, m);
    // levenshtein_editops: with a score hint of 31, the real distance is
    // computed first when it could at most halve the work.
    if (2 * 31 < cutoff) cutoff = unit_levenshtein(s1, n, s2, m);
    align_hirschberg(s1, n, s2, m, 0, 0, cutoff, ops);
}

// Opcodes(const Editops&): runs of one edit type become blocks, and the
// gaps between them "equal" blocks.
struct OpcodeBlock {
    int kind;
    std::size_t src_begin, src_end, dest_begin, dest_end;
};

void editops_to_opcodes(const std::vector<EditOp>& ops, std::size_t src_len,
                        std::size_t dest_len, std::vector<OpcodeBlock>& out) {
    out.clear();
    std::size_t src = 0, dest = 0;
    for (std::size_t i = 0; i < ops.size();) {
        if (src < ops[i].src || dest < ops[i].dest) {
            out.push_back(OpcodeBlock{EDIT_EQUAL, src, ops[i].src, dest, ops[i].dest});
            src = ops[i].src;
            dest = ops[i].dest;
        }
        const std::size_t src_begin = src, dest_begin = dest;
        const int kind = ops[i].kind;
        do {
            if (kind == EDIT_REPLACE) { ++src; ++dest; }
            else if (kind == EDIT_INSERT) { ++dest; }
            else if (kind == EDIT_DELETE) { ++src; }
            ++i;
        } while (i < ops.size() && ops[i].kind == kind && src == ops[i].src &&
                 dest == ops[i].dest);
        out.push_back(OpcodeBlock{kind, src_begin, src, dest_begin, dest});
    }
    if (src < src_len || dest < dest_len)
        out.push_back(OpcodeBlock{EDIT_EQUAL, src, src_len, dest, dest_len});
}

}  // namespace

// [[Rcpp::export]]
NumericVector fast_simple_metric_impl(const StringVector& a,
                                      const StringVector& b, int metric,
                                      bool use_bytes, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    SimpleMetricWorker worker(ia.source(), ib.source(),
                              static_cast<SimpleMetric>(metric), out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(ia.bytes, ib.bytes), 1000,
                 nthreads);
    return out;
}

// [[Rcpp::export]]
NumericVector fast_weighted_levenshtein_impl(const StringVector& a,
                                             const StringVector& b,
                                             NumericVector weights,
                                             bool similarity, bool use_bytes,
                                             int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    WeightedWorker worker(ia.source(), ib.source(),
                          Weights{weights[0], weights[1], weights[2]},
                          similarity, out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(ia.bytes, ib.bytes), 1000,
                 nthreads);
    return out;
}

// [[Rcpp::export]]
NumericMatrix fast_weighted_levenshtein_matrix_impl(const StringVector& a,
                                                    const StringVector& b,
                                                    NumericVector weights,
                                                    bool use_bytes,
                                                    int nthreads) {
    const std::size_t rows = static_cast<std::size_t>(a.size());
    const std::size_t cols = static_cast<std::size_t>(b.size());
    if (cols != 0 && rows > (std::numeric_limits<std::size_t>::max)() / cols)
        stop("Requested pairwise matrix is too large.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    NumericMatrix out(a.size(), b.size());
    if (rows == 0 || cols == 0) return out;
    WeightedMatrixWorker worker(ia.source(), ib.source(),
                                Weights{weights[0], weights[1], weights[2]},
                                rows, REAL(out));
    dispatch_for(0, rows * cols, worker,
                 estimated_matrix_string_work(ia.bytes, ib.bytes, rows * cols),
                 10000, nthreads, 256);
    return out;
}

// [[Rcpp::export]]
List fast_edit_ops_impl(const StringVector& a, const StringVector& b,
                        bool opcodes, bool use_bytes) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    const SequenceSource sa = ia.source(), sb = ib.source();
    std::vector<int> pair, kind;
    std::vector<double> a1, a2, b1, b2;
    std::vector<rf_char> buffer_a, buffer_b;
    std::vector<EditOp> ops;
    std::vector<OpcodeBlock> blocks;
    for (R_xlen_t i = 0; i < a.size(); ++i) {
        const std::size_t k = static_cast<std::size_t>(i);
        if (sa.missing(k) || sb.missing(k)) continue;
        int la, lb;
        const rf_char* pa = sa.get(k, buffer_a, la);
        const Seq copy_a(pa, pa + la);  // get() may reuse its buffer
        const rf_char* pb = sb.get(k, buffer_b, lb);
        levenshtein_editops(copy_a.data(), copy_a.size(), pb,
                            static_cast<std::size_t>(lb), ops);
        if (!opcodes) {
            for (const EditOp& op : ops) {
                pair.push_back(static_cast<int>(i + 1));
                kind.push_back(op.kind);
                a1.push_back(static_cast<double>(op.src) + 1);
                b1.push_back(static_cast<double>(op.dest) + 1);
            }
        } else {
            editops_to_opcodes(ops, copy_a.size(), static_cast<std::size_t>(lb),
                               blocks);
            for (const OpcodeBlock& block : blocks) {
                pair.push_back(static_cast<int>(i + 1));
                kind.push_back(block.kind);
                a1.push_back(static_cast<double>(block.src_begin) + 1);
                a2.push_back(static_cast<double>(block.src_end));
                b1.push_back(static_cast<double>(block.dest_begin) + 1);
                b2.push_back(static_cast<double>(block.dest_end));
            }
        }
        Rcpp::checkUserInterrupt();
    }
    return List::create(_["pair"] = wrap(pair), _["kind"] = wrap(kind),
                        _["a1"] = wrap(a1), _["a2"] = wrap(a2),
                        _["b1"] = wrap(b1), _["b2"] = wrap(b2));
}
