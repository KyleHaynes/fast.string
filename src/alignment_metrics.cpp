// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "codepoint_snapshot.h"
#include "latin_fold.h"
#include "metric_dispatch.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
#include "text_results.h"
#include "utf8_text.h"

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

// Editex, the longest common substring, Smith-Waterman-Gotoh local
// alignment and the Monge-Elkan token similarity.

namespace {

typedef std::uint32_t cp_t;

// Pairs of strings as code-point (or byte) sequences, as in
// distance_extra.cpp.
struct Sequences {
    StringSnapshot bytes;
    std::unique_ptr<CodepointSnapshot> points;

    Sequences(const StringVector& x, bool use_bytes, const char* name)
        : bytes(x, use_bytes ? SnapshotText::raw : SnapshotText::utf8) {
        if (!use_bytes) points.reset(new CodepointSnapshot(x, name));
    }
};

struct SequenceView {
    const StringView* bytes;
    const CodepointView* points;

    explicit SequenceView(const Sequences& s)
        : bytes(s.bytes.data()), points(s.points ? s.points->data() : nullptr) {}

    bool missing(std::size_t i) const { return bytes[i].is_na(); }

    const cp_t* get(std::size_t i, std::vector<cp_t>& buffer, int& size) const {
        if (points) {
            size = static_cast<int>(points[i].size);
            return points[i].data;
        }
        const StringView& v = bytes[i];
        buffer.resize(v.size);
        for (std::size_t k = 0; k < v.size; ++k)
            buffer[k] = static_cast<unsigned char>(v.data[k]);
        size = static_cast<int>(v.size);
        return buffer.data();
    }
};

// --- Editex (Zobel & Dart, 1996) --------------------------------------------

// Letter groups: AEIOUY, BP, CKQ, DT, LR, MN, GJ, FPV, SXZ, CSZ. Bit g of
// EDITEX_GROUPS[letter] is set when the letter is in group g.
const std::uint16_t EDITEX_GROUPS[26] = {
    /*A*/ 1 << 0, /*B*/ 1 << 1, /*C*/ (1 << 2) | (1 << 9), /*D*/ 1 << 3,
    /*E*/ 1 << 0, /*F*/ 1 << 7, /*G*/ 1 << 6, /*H*/ 0, /*I*/ 1 << 0,
    /*J*/ 1 << 6, /*K*/ 1 << 2, /*L*/ 1 << 4, /*M*/ 1 << 5, /*N*/ 1 << 5,
    /*O*/ 1 << 0, /*P*/ (1 << 1) | (1 << 7), /*Q*/ 1 << 2, /*R*/ 1 << 4,
    /*S*/ (1 << 8) | (1 << 9), /*T*/ 1 << 3, /*U*/ 1 << 0, /*V*/ 1 << 7,
    /*W*/ 0, /*X*/ 1 << 8, /*Y*/ 1 << 0, /*Z*/ (1 << 8) | (1 << 9)};

inline std::uint16_t editex_group(cp_t c) {
    return c >= 'A' && c <= 'Z' ? EDITEX_GROUPS[c - 'A'] : 0;
}

// r(a, b): 0 for the same letter, 1 for two letters of one group, else 2.
inline int editex_r(cp_t a, cp_t b) {
    if (a == b) return 0;
    return (editex_group(a) & editex_group(b)) != 0 ? 1 : 2;
}

// d(a, b): as r, except that H or W after another letter costs 1.
inline int editex_d(cp_t previous, cp_t current) {
    if (previous != current && (previous == 'H' || previous == 'W')) return 1;
    return editex_r(previous, current);
}

// Upper-cases ASCII letters and folds accented Latin ones (latin_fold.h).
void editex_prepare(const StringView& value, bool use_bytes,
                    std::string& folded, std::vector<cp_t>& out) {
    const char* data = value.data;
    std::size_t size = value.size;
    if (!use_bytes && fold_latin_letters(data, size, folded)) {
        data = folded.data();
        size = folded.size();
    }
    out.clear();
    const unsigned char* s = reinterpret_cast<const unsigned char*>(data);
    std::size_t i = 0;
    while (i < size) {
        cp_t point = s[i];
        std::size_t width = 1;
        if (!use_bytes && s[i] >= 0x80) {
            width = utf8_decode(s, size, i, point);
            if (width == 0) {
                point = 0x110000u + s[i];
                width = 1;
            }
        }
        if (point >= 'a' && point <= 'z') point -= 'a' - 'A';
        out.push_back(point);
        i += width;
    }
}

// The DP of textdistance's and abydos's Editex: both strings are prefixed
// with a space, so starting a string costs d(' ', first letter) = 2.
double editex_distance(const std::vector<cp_t>& a, const std::vector<cp_t>& b,
                       std::vector<int>& row) {
    const std::size_t n = a.size(), m = b.size();
    if (n == 0 || m == 0) return 2.0 * static_cast<double>(n + m);
    row.resize(m + 1);
    row[0] = 0;
    for (std::size_t j = 1; j <= m; ++j)
        row[j] = row[j - 1] + editex_d(j == 1 ? ' ' : b[j - 2], b[j - 1]);
    for (std::size_t i = 1; i <= n; ++i) {
        const int delete_cost = editex_d(i == 1 ? ' ' : a[i - 2], a[i - 1]);
        int diagonal = row[0];
        row[0] += delete_cost;
        for (std::size_t j = 1; j <= m; ++j) {
            const int above = row[j];
            int best = above + delete_cost;
            best = (std::min)(best, row[j - 1] + editex_d(j == 1 ? ' ' : b[j - 2], b[j - 1]));
            best = (std::min)(best, diagonal + editex_r(a[i - 1], b[j - 1]));
            row[j] = best;
            diagonal = above;
        }
    }
    return static_cast<double>(row[m]);
}

struct EditexWorker : public Worker {
    const StringView* a;
    const StringView* b;
    bool use_bytes;
    bool similarity;
    RVector<double> out;

    EditexWorker(const StringView* a_, const StringView* b_, bool use_bytes_,
                 bool similarity_, NumericVector& out_)
        : a(a_), b(b_), use_bytes(use_bytes_), similarity(similarity_),
          out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string folded;
        std::vector<cp_t> sa, sb;
        std::vector<int> row;
        for (std::size_t i = begin; i < end; ++i) {
            if (a[i].is_na() || b[i].is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            editex_prepare(a[i], use_bytes, folded, sa);
            editex_prepare(b[i], use_bytes, folded, sb);
            const double d = editex_distance(sa, sb, row);
            if (!similarity) {
                out[i] = d;
            } else {
                const double maximum =
                    2.0 * static_cast<double>((std::max)(sa.size(), sb.size()));
                out[i] = maximum > 0 ? 1.0 - d / maximum : 1.0;
            }
        }
    }
};

// --- Longest common substring -------------------------------------------------

// The longest common substring of a and b as (start in a, length): the one
// that starts earliest in a, then earliest in b (difflib's
// find_longest_match without junk).
std::pair<int, int> longest_common_substring(const cp_t* a, int la,
                                             const cp_t* b, int lb,
                                             std::vector<int>& row) {
    row.assign(static_cast<std::size_t>(lb) + 1, 0);
    int best = 0, best_end = 0;
    for (int i = 1; i <= la; ++i) {
        int diagonal = 0;
        for (int j = 1; j <= lb; ++j) {
            const int above = row[j];
            row[j] = a[i - 1] == b[j - 1] ? diagonal + 1 : 0;
            if (row[j] > best) {
                best = row[j];
                best_end = i;
            }
            diagonal = above;
        }
    }
    return std::make_pair(best_end - best, best);
}

struct LcsStrWorker : public Worker {
    SequenceView a, b;
    bool similarity;
    std::vector<int>& start;  // in code units of a
    std::vector<int>& length;
    RVector<double> out;

    LcsStrWorker(SequenceView a_, SequenceView b_, bool similarity_,
                 std::vector<int>& start_, std::vector<int>& length_,
                 NumericVector& out_)
        : a(a_), b(b_), similarity(similarity_), start(start_),
          length(length_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<cp_t> buffer_a, buffer_b;
        std::vector<int> row;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                length[i] = -1;
                if (similarity) out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const cp_t* pa = a.get(i, buffer_a, la);
            const cp_t* pb = b.get(i, buffer_b, lb);
            const std::pair<int, int> hit =
                longest_common_substring(pa, la, pb, lb, row);
            start[i] = hit.first;
            length[i] = hit.second;
            if (similarity) {
                const int longest = (std::max)(la, lb);
                out[i] = longest == 0 ? 1.0
                                      : static_cast<double>(hit.second) / longest;
            }
        }
    }
};

// --- Smith-Waterman-Gotoh local alignment -----------------------------------

struct AlignScores {
    double match, mismatch, gap_open, gap_extend;
};

// Best local alignment score (Smith & Waterman 1981) with affine gaps
// (Gotoh 1982): a gap of k characters scores gap_open + (k - 1) *
// gap_extend.
double smith_waterman_score(const cp_t* a, int la, const cp_t* b, int lb,
                            const AlignScores& s, std::vector<double>& h,
                            std::vector<double>& f) {
    const double minus_inf = -std::numeric_limits<double>::infinity();
    h.assign(static_cast<std::size_t>(lb) + 1, 0.0);
    f.assign(static_cast<std::size_t>(lb) + 1, minus_inf);
    double best = 0.0;
    for (int i = 1; i <= la; ++i) {
        double diagonal = h[0];  // H[i-1][0] = 0
        double e = minus_inf;    // gap along b in row i
        double left = 0.0;       // H[i][0]
        for (int j = 1; j <= lb; ++j) {
            const double up = h[j];  // H[i-1][j]
            e = (std::max)(left + s.gap_open, e + s.gap_extend);
            f[j] = (std::max)(up + s.gap_open, f[j] + s.gap_extend);
            double value = diagonal + (a[i - 1] == b[j - 1] ? s.match : s.mismatch);
            value = (std::max)(value, e);
            value = (std::max)(value, f[j]);
            value = (std::max)(value, 0.0);
            h[j] = value;
            if (value > best) best = value;
            diagonal = up;
            left = value;
        }
    }
    return best;
}

struct SmithWatermanWorker : public Worker {
    SequenceView a, b;
    AlignScores scores;
    bool normalize;
    RVector<double> out;

    SmithWatermanWorker(SequenceView a_, SequenceView b_, AlignScores scores_,
                        bool normalize_, NumericVector& out_)
        : a(a_), b(b_), scores(scores_), normalize(normalize_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<cp_t> buffer_a, buffer_b;
        std::vector<double> h, f;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const cp_t* pa = a.get(i, buffer_a, la);
            const cp_t* pb = b.get(i, buffer_b, lb);
            if (normalize && (la == 0 || lb == 0)) {
                out[i] = la == lb ? 1.0 : 0.0;
                continue;
            }
            const double score = smith_waterman_score(pa, la, pb, lb, scores, h, f);
            out[i] = normalize
                ? score / ((std::min)(la, lb) * scores.match)
                : score;
        }
    }
};

// --- Monge-Elkan ---------------------------------------------------------------

struct Token {
    const cp_t* data;
    int size;
};

void split_tokens(const cp_t* s, int n, std::vector<Token>& out) {
    out.clear();
    int i = 0;
    while (i < n) {
        while (i < n && is_white_space(s[i])) ++i;
        const int start = i;
        while (i < n && !is_white_space(s[i])) ++i;
        if (i > start) out.push_back(Token{s + start, i - start});
    }
}

double inner_similarity(MetricMethod method, const Token& x, const Token& y,
                        double p, DamerauWorkspace& workspace) {
    if (method == MetricMethod::jaro_winkler)
        return sequence_jaro_winkler_similarity(x.data, x.size, y.data, y.size, p);
    int distance;
    switch (method) {
    case MetricMethod::levenshtein:
        distance = sequence_levenshtein_distance(x.data, x.size, y.data, y.size);
        break;
    case MetricMethod::osa:
        distance = sequence_osa_distance(x.data, x.size, y.data, y.size);
        break;
    default:
        distance = damerau_levenshtein_codepoints(x.data, x.size, y.data, y.size,
                                                  workspace);
    }
    return normalized_edit_similarity(distance, x.size, y.size);
}

// Mean over the tokens of `from` of the best similarity to a token of `to`.
double monge_elkan_directed(const std::vector<Token>& from,
                            const std::vector<Token>& to, MetricMethod method,
                            double p, DamerauWorkspace& workspace) {
    double total = 0.0;
    for (const Token& x : from) {
        double best = 0.0;
        for (const Token& y : to) {
            const double sim = inner_similarity(method, x, y, p, workspace);
            if (sim > best) best = sim;
            if (best >= 1.0) break;
        }
        total += best;
    }
    return total / static_cast<double>(from.size());
}

struct MongeElkanWorker : public Worker {
    SequenceView a, b;
    MetricMethod method;
    double p;
    bool symmetric;
    RVector<double> out;

    MongeElkanWorker(SequenceView a_, SequenceView b_, MetricMethod method_,
                     double p_, bool symmetric_, NumericVector& out_)
        : a(a_), b(b_), method(method_), p(p_), symmetric(symmetric_),
          out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<cp_t> buffer_a, buffer_b;
        std::vector<Token> ta, tb;
        DamerauWorkspace workspace;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const cp_t* pa = a.get(i, buffer_a, la);
            const cp_t* pb = b.get(i, buffer_b, lb);
            split_tokens(pa, la, ta);
            split_tokens(pb, lb, tb);
            if (ta.empty() || tb.empty()) {
                out[i] = ta.empty() && tb.empty() ? 1.0 : 0.0;
                continue;
            }
            double score = monge_elkan_directed(ta, tb, method, p, workspace);
            if (symmetric)
                score = (score + monge_elkan_directed(tb, ta, method, p, workspace)) / 2;
            out[i] = score;
        }
    }
};

}  // namespace

// [[Rcpp::export]]
NumericVector fast_editex_impl(const StringVector& a, const StringVector& b,
                               bool similarity, bool use_bytes, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SnapshotText text = use_bytes ? SnapshotText::raw : SnapshotText::utf8;
    const StringSnapshot sa(a, text), sb(b, text);
    NumericVector out = no_init(a.size());
    EditexWorker worker(sa.data(), sb.data(), use_bytes, similarity, out);
    dispatch_for(0, sa.size(), worker, estimated_pairwise_string_work(sa, sb),
                 1000, nthreads);
    return out;
}

// [[Rcpp::export]]
SEXP fast_lcs_str_impl(const StringVector& a, const StringVector& b,
                       bool similarity, bool use_bytes, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const Sequences sa(a, use_bytes, "a"), sb(b, use_bytes, "b");
    const std::size_t n = static_cast<std::size_t>(a.size());
    std::vector<int> start(n, 0), length(n, 0);
    NumericVector out(similarity ? a.size() : 0);
    LcsStrWorker worker(SequenceView(sa), SequenceView(sb), similarity, start,
                        length, out);
    dispatch_for(0, n, worker, estimated_pairwise_string_work(sa.bytes, sb.bytes),
                 1000, nthreads);
    if (similarity) return out;
    // The substring, sliced from a's UTF-8 bytes (or raw bytes).
    CharacterVector result(a.size());
    for (std::size_t i = 0; i < n; ++i) {
        if (length[i] < 0) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
            continue;
        }
        const StringView& v = sa.bytes[i];
        std::size_t from = static_cast<std::size_t>(start[i]);
        std::size_t to = from + static_cast<std::size_t>(length[i]);
        if (!use_bytes) {
            // Code point offsets to byte offsets (CodepointSnapshot decodes
            // valid UTF-8 only, one code point per sequence).
            const unsigned char* s = reinterpret_cast<const unsigned char*>(v.data);
            std::size_t byte = 0, point = 0, byte_from = 0, byte_to = 0;
            while (true) {
                if (point == from) byte_from = byte;
                if (point == to || byte >= v.size) {
                    byte_to = byte;
                    break;
                }
                cp_t cp;
                const std::size_t w = utf8_decode(s, v.size, byte, cp);
                byte += w == 0 ? 1 : w;
                ++point;
            }
            from = byte_from;
            to = byte_to;
        }
        const cetype_t encoding = sa.bytes.translated(i)
            ? CE_UTF8
            : Rf_getCharCE(sa.bytes.charsxp(i));
        SET_STRING_ELT(result, static_cast<R_xlen_t>(i),
                       make_text_charsxp(v.data + from, to - from, encoding));
    }
    return result;
}

// [[Rcpp::export]]
NumericVector fast_smith_waterman_impl(const StringVector& a,
                                       const StringVector& b, double match,
                                       double mismatch, double gap_open,
                                       double gap_extend, bool normalize,
                                       bool use_bytes, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const Sequences sa(a, use_bytes, "a"), sb(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    SmithWatermanWorker worker(SequenceView(sa), SequenceView(sb),
                               AlignScores{match, mismatch, gap_open, gap_extend},
                               normalize, out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(sa.bytes, sb.bytes), 1000,
                 nthreads);
    return out;
}

// [[Rcpp::export]]
NumericVector fast_monge_elkan_impl(const StringVector& a,
                                    const StringVector& b, int method,
                                    double p, bool symmetric, bool use_bytes,
                                    int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const Sequences sa(a, use_bytes, "a"), sb(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    MongeElkanWorker worker(SequenceView(sa), SequenceView(sb),
                            static_cast<MetricMethod>(method), p, symmetric, out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(sa.bytes, sb.bytes), 1000,
                 nthreads);
    return out;
}
