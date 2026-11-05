// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "parallel_dispatch.h"
#include "rapidfuzz_core.h"
#include "sequence_input.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// Comparators that record linkage borrows from outside the RapidFuzz family:
// Ratcliff/Obershelp gestalt matching (Python's difflib, abydos, SimMetrics),
// the bag distance of Bartolini, Ciaccia and Patella (Febrl, abydos),
// Needleman-Wunsch global alignment (SimMetrics, abydos, and every sequence
// aligner), the Eudex phonetic hash of the Rust crate of the same name, and
// the haversine great-circle distance for geocoded addresses.

namespace {

// --- Ratcliff/Obershelp -----------------------------------------------------

struct Substring {
    int i, j, len;
};

// Longest common substring, leftmost-longest, by the usual rolling-row DP.
Substring longest_common_substring(const rf_char* a, int la, const rf_char* b,
                                   int lb, std::vector<int>& row) {
    Substring best{0, 0, 0};
    if (la == 0 || lb == 0) return best;
    row.assign(static_cast<std::size_t>(lb) + 1, 0);
    for (int i = 1; i <= la; ++i) {
        int diagonal = 0;  // row[j - 1] as it was for i - 1
        for (int j = 1; j <= lb; ++j) {
            const int above = row[j];
            const int len = a[i - 1] == b[j - 1] ? diagonal + 1 : 0;
            row[j] = len;
            diagonal = above;
            if (len > best.len) best = Substring{i - len, j - len, len};
        }
    }
    return best;
}

struct Range {
    const rf_char* a;
    int la;
    const rf_char* b;
    int lb;
};

// Total length matched by gestalt pattern matching: the longest common
// substring, then the same thing again on what is left of each side of it.
// An explicit stack rather than recursion, which on pathological input is as
// deep as the shorter string is long.
std::size_t gestalt_matches(const rf_char* a, int la, const rf_char* b, int lb,
                            std::vector<int>& row, std::vector<Range>& stack) {
    std::size_t total = 0;
    stack.clear();
    stack.push_back(Range{a, la, b, lb});
    while (!stack.empty()) {
        const Range r = stack.back();
        stack.pop_back();
        if (r.la <= 0 || r.lb <= 0) continue;
        const Substring m = longest_common_substring(r.a, r.la, r.b, r.lb, row);
        if (m.len == 0) continue;
        total += static_cast<std::size_t>(m.len);
        stack.push_back(Range{r.a, m.i, r.b, m.j});
        stack.push_back(Range{r.a + m.i + m.len, r.la - m.i - m.len,
                              r.b + m.j + m.len, r.lb - m.j - m.len});
    }
    return total;
}

// --- Bag distance -----------------------------------------------------------

// max(|A - B|, |B - A|) over the multisets of characters: a lower bound on
// the Levenshtein distance that costs a sort rather than a DP matrix.
int bag_distance(const rf_char* a, int la, const rf_char* b, int lb,
                 std::vector<rf_char>& sa, std::vector<rf_char>& sb) {
    sa.assign(a, a + la);
    sb.assign(b, b + lb);
    std::sort(sa.begin(), sa.end());
    std::sort(sb.begin(), sb.end());
    std::size_t i = 0, j = 0;
    int only_a = 0, only_b = 0;
    while (i < sa.size() && j < sb.size()) {
        if (sa[i] == sb[j]) {
            ++i;
            ++j;
        } else if (sa[i] < sb[j]) {
            ++only_a;
            ++i;
        } else {
            ++only_b;
            ++j;
        }
    }
    only_a += static_cast<int>(sa.size() - i);
    only_b += static_cast<int>(sb.size() - j);
    return (std::max)(only_a, only_b);
}

// --- Metrics without parameters --------------------------------------------

enum class SeqMetric : int {
    gestalt_similarity = 0,
    gestalt_distance,
    bag_dist,
    bag_similarity
};

double seq_metric(SeqMetric metric, const rf_char* a, int la, const rf_char* b,
                  int lb, std::vector<int>& row, std::vector<Range>& stack,
                  std::vector<rf_char>& sa, std::vector<rf_char>& sb) {
    switch (metric) {
    case SeqMetric::gestalt_similarity:
    case SeqMetric::gestalt_distance: {
        const int total = la + lb;
        const double sim = total == 0
            ? 1.0
            : 2.0 * static_cast<double>(
                        gestalt_matches(a, la, b, lb, row, stack)) / total;
        return metric == SeqMetric::gestalt_similarity ? sim : 1.0 - sim;
    }
    case SeqMetric::bag_dist:
        return bag_distance(a, la, b, lb, sa, sb);
    case SeqMetric::bag_similarity: {
        const int longest = (std::max)(la, lb);
        if (longest == 0) return 1.0;
        return 1.0 - static_cast<double>(bag_distance(a, la, b, lb, sa, sb)) /
                         longest;
    }
    }
    return NA_REAL;
}

struct SeqMetricWorker : public Worker {
    SequenceSource a, b;
    SeqMetric metric;
    RVector<double> out;

    SeqMetricWorker(SequenceSource a_, SequenceSource b_, SeqMetric metric_,
                    NumericVector& out_)
        : a(a_), b(b_), metric(metric_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<rf_char> buffer_a, buffer_b, sorted_a, sorted_b;
        std::vector<int> row;
        std::vector<Range> stack;
        for (std::size_t i = begin; i < end; ++i) {
            if (a.missing(i) || b.missing(i)) {
                out[i] = NA_REAL;
                continue;
            }
            int la, lb;
            const rf_char* pa = a.get(i, buffer_a, la);
            const rf_char* pb = b.get(i, buffer_b, lb);
            out[i] = seq_metric(metric, pa, la, pb, lb, row, stack, sorted_a,
                                sorted_b);
        }
    }
};

// --- Needleman-Wunsch -------------------------------------------------------

struct AlignScores {
    double match, mismatch, gap;
};

double needleman_wunsch(const rf_char* a, int la, const rf_char* b, int lb,
                        const AlignScores& s, std::vector<double>& row) {
    row.resize(static_cast<std::size_t>(lb) + 1);
    for (int j = 0; j <= lb; ++j) row[j] = j * s.gap;
    for (int i = 1; i <= la; ++i) {
        double diagonal = row[0];
        row[0] = i * s.gap;
        for (int j = 1; j <= lb; ++j) {
            const double above = row[j];
            double best = diagonal + (a[i - 1] == b[j - 1] ? s.match : s.mismatch);
            best = (std::max)(best, above + s.gap);
            best = (std::max)(best, row[j - 1] + s.gap);
            row[j] = best;
            diagonal = above;
        }
    }
    return row[static_cast<std::size_t>(lb)];
}

// The range the score is reported against, so that it lands in 0-1. The best
// is the score the longer string gets against a copy of itself, which only
// identical strings reach; the worst is the cheaper of aligning the two as
// mismatches and of gapping both away entirely.
double needleman_wunsch_similarity(const rf_char* a, int la, const rf_char* b,
                                   int lb, const AlignScores& s,
                                   std::vector<double>& row) {
    if (la == 0 && lb == 0) return 1.0;
    const int shortest = (std::min)(la, lb);
    const int longest = (std::max)(la, lb);
    const double best = longest * s.match;
    const double worst = (std::min)(shortest * s.mismatch +
                                        (longest - shortest) * s.gap,
                                    static_cast<double>(la + lb) * s.gap);
    if (!(best > worst)) return 1.0;
    const double score = needleman_wunsch(a, la, b, lb, s, row);
    const double sim = (score - worst) / (best - worst);
    return sim < 0.0 ? 0.0 : (sim > 1.0 ? 1.0 : sim);
}

struct AlignWorker : public Worker {
    SequenceSource a, b;
    AlignScores scores;
    bool normalise;
    RVector<double> out;

    AlignWorker(SequenceSource a_, SequenceSource b_, AlignScores scores_,
                bool normalise_, NumericVector& out_)
        : a(a_), b(b_), scores(scores_), normalise(normalise_), out(out_) {}

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
            out[i] = normalise
                ? needleman_wunsch_similarity(pa, la, pb, lb, scores, row)
                : needleman_wunsch(pa, la, pb, lb, scores, row);
        }
    }
};

// --- Eudex ------------------------------------------------------------------

// The two tables of the Rust crate eudex (src/raw.rs). PHONES codes each
// letter's articulation one bit at a time -- discriminant, nasal, fricative,
// plosive, dental, liquid, labial, "hard to misspell" -- so that letters that
// sound alike differ in few bits. INJECTIVE_PHONES is the distinct table used
// for the first letter, which the hash keeps in its own top byte.
const unsigned char EUDEX_PHONES[26] = {
    0,           // a
    0x48,        // b  0b01001000
    0x0C,        // c  0b00001100
    0x18,        // d  0b00011000
    0,           // e
    0x44,        // f  0b01000100
    0x08,        // g  0b00001000
    0x04,        // h  0b00000100
    1,           // i
    0x05,        // j  0b00000101
    0x09,        // k  0b00001001
    0xA0,        // l  0b10100000
    0x02,        // m  0b00000010
    0x12,        // n  0b00010010
    0,           // o
    0x49,        // p  0b01001001
    0xA8,        // q  0b10101000
    0xA1,        // r  0b10100001
    0x14,        // s  0b00010100
    0x1D,        // t  0b00011101
    1,           // u
    0x45,        // v  0b01000101
    0x00,        // w
    0x84,        // x  0b10000100
    1,           // y
    0x94         // z  0b10010100
};

const unsigned char EUDEX_INJECTIVE_PHONES[26] = {
    0x84,        // a  0b10000100
    0x24,        // b  0b00100100
    0x06,        // c  0b00000110
    0x0C,        // d  0b00001100
    0xD8,        // e  0b11011000
    0x22,        // f  0b00100010
    0x04,        // g  0b00000100
    0x02,        // h  0b00000010
    0xF8,        // i  0b11111000
    0x03,        // j  0b00000011
    0x05,        // k  0b00000101
    0x50,        // l  0b01010000
    0x01,        // m  0b00000001
    0x09,        // n  0b00001001
    0x94,        // o  0b10010100
    0x25,        // p  0b00100101
    0x54,        // q  0b01010100
    0x51,        // r  0b01010001
    0x0A,        // s  0b00001010
    0x0E,        // t  0b00001110
    0xE0,        // u  0b11100000
    0x23,        // v  0b00100011
    0x00,        // w
    0x42,        // x  0b01000010
    0xE4,        // y  0b11100100
    0x4A         // z  0b01001010
};

// Fold to a letter index, exactly as the crate's map_first() and filter() do:
// set bit 5 (so case is dropped) and subtract 'a' with wraparound. Anything
// that does not land in 0..25 is not a letter the tables cover.
inline bool eudex_letter(unsigned char byte, unsigned char& index) {
    index = static_cast<unsigned char>((byte | 32) - 'a');
    return index < 26;
}

// eudex::Hash::new. The first letter's injective phone goes in the top byte;
// the rest are pushed into the low bytes, skipping any phone whose
// discriminant bit matches the one before it (which is how the crate
// collapses repeated and silent sounds). Eight pushes fill the hash.
std::uint64_t eudex_hash(const unsigned char* s, std::size_t n) {
    unsigned char index;
    const std::uint64_t first =
        (n > 0 && eudex_letter(s[0], index)) ? EUDEX_INJECTIVE_PHONES[index] : 0;
    std::uint64_t res = 0;
    unsigned char slot = 1;
    std::size_t b = 0;
    for (;;) {
        ++b;
        if (slot == 0 || b >= n) break;
        if (!eudex_letter(s[b], index)) continue;
        const unsigned char phone = EUDEX_PHONES[index];
        const unsigned char previous = static_cast<unsigned char>(res & 0xFF);
        if ((phone & 1) != (previous & 1)) {
            res = (res << 8) | phone;
            slot = static_cast<unsigned char>(slot << 1);
        }
    }
    return res | (first << 56);
}

inline int popcount8(unsigned char x) {
    int n = 0;
    while (x) {
        x = static_cast<unsigned char>(x & (x - 1));
        ++n;
    }
    return n;
}

// eudex::Difference::dist: the Hamming weight of each byte of the XOR, each
// weighted by where it sits, so a first-letter difference counts for much
// more than a difference in the last sound recorded.
const int EUDEX_WEIGHTS[8] = {1, 2, 3, 5, 8, 13, 21, 34};

double eudex_difference(std::uint64_t xor_value, bool graduated) {
    int total = 0;
    for (int byte = 0; byte < 8; ++byte) {
        const int bits =
            popcount8(static_cast<unsigned char>((xor_value >> (8 * byte)) & 0xFF));
        total += graduated ? bits * EUDEX_WEIGHTS[byte] : bits;
    }
    return total;
}

const char HEX_DIGITS[] = "0123456789abcdef";

struct EudexHashWorker : public Worker {
    const StringView* x;
    std::vector<std::uint64_t>& hashes;

    EudexHashWorker(const StringView* x_, std::vector<std::uint64_t>& hashes_)
        : x(x_), hashes(hashes_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            if (x[i].is_na()) continue;
            hashes[i] = eudex_hash(
                reinterpret_cast<const unsigned char*>(x[i].data), x[i].size);
        }
    }
};

struct EudexDistanceWorker : public Worker {
    const StringView* a;
    const StringView* b;
    bool graduated;
    RVector<double> out;

    EudexDistanceWorker(const StringView* a_, const StringView* b_,
                        bool graduated_, NumericVector& out_)
        : a(a_), b(b_), graduated(graduated_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            if (a[i].is_na() || b[i].is_na()) {
                out[i] = NA_REAL;
                continue;
            }
            const std::uint64_t ha = eudex_hash(
                reinterpret_cast<const unsigned char*>(a[i].data), a[i].size);
            const std::uint64_t hb = eudex_hash(
                reinterpret_cast<const unsigned char*>(b[i].data), b[i].size);
            out[i] = eudex_difference(ha ^ hb, graduated);
        }
    }
};

// --- Haversine --------------------------------------------------------------

struct GeoWorker : public Worker {
    const double* lat_1;
    const double* lon_1;
    const double* lat_2;
    const double* lon_2;
    double radius;
    RVector<double> out;

    GeoWorker(const double* lat_1_, const double* lon_1_, const double* lat_2_,
              const double* lon_2_, double radius_, NumericVector& out_)
        : lat_1(lat_1_), lon_1(lon_1_), lat_2(lat_2_), lon_2(lon_2_),
          radius(radius_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const double to_radians = 3.14159265358979323846 / 180.0;
        for (std::size_t i = begin; i < end; ++i) {
            const double y1 = lat_1[i], x1 = lon_1[i];
            const double y2 = lat_2[i], x2 = lon_2[i];
            if (ISNAN(y1) || ISNAN(x1) || ISNAN(y2) || ISNAN(x2)) {
                out[i] = NA_REAL;
                continue;
            }
            const double phi_1 = y1 * to_radians, phi_2 = y2 * to_radians;
            const double half_lat = 0.5 * (phi_2 - phi_1);
            const double half_lon = 0.5 * (x2 - x1) * to_radians;
            const double sin_lat = std::sin(half_lat);
            const double sin_lon = std::sin(half_lon);
            double h = sin_lat * sin_lat +
                       std::cos(phi_1) * std::cos(phi_2) * sin_lon * sin_lon;
            if (h > 1.0) h = 1.0;  // rounding, for antipodal points
            out[i] = 2.0 * radius * std::asin(std::sqrt(h));
        }
    }
};

}  // namespace

// [[Rcpp::export]]
NumericVector fast_seq_metric_impl(const StringVector& a, const StringVector& b,
                                   int metric, bool use_bytes, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    SeqMetricWorker worker(ia.source(), ib.source(),
                           static_cast<SeqMetric>(metric), out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(ia.bytes, ib.bytes), 1000,
                 nthreads);
    return out;
}

// [[Rcpp::export]]
NumericVector fast_needleman_wunsch_impl(const StringVector& a,
                                         const StringVector& b, double match,
                                         double mismatch, double gap,
                                         bool normalise, bool use_bytes,
                                         int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const SequenceInput ia(a, use_bytes, "a"), ib(b, use_bytes, "b");
    NumericVector out = no_init(a.size());
    AlignWorker worker(ia.source(), ib.source(),
                       AlignScores{match, mismatch, gap}, normalise, out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(ia.bytes, ib.bytes), 1000,
                 nthreads);
    return out;
}

// [[Rcpp::export]]
StringVector fast_eudex_impl(const StringVector& x, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::raw);
    const std::size_t n = static_cast<std::size_t>(x.size());
    std::vector<std::uint64_t> hashes(n, 0);
    EudexHashWorker worker(snapshot.data(), hashes);
    dispatch_for(0, n, worker, estimated_pairwise_string_work(snapshot, snapshot),
                 5000, nthreads);
    StringVector out(x.size());
    char buffer[17];
    buffer[16] = '\0';
    for (std::size_t i = 0; i < n; ++i) {
        if (snapshot[i].is_na()) {
            out[i] = NA_STRING;
            continue;
        }
        std::uint64_t h = hashes[i];
        for (int k = 15; k >= 0; --k) {
            buffer[k] = HEX_DIGITS[h & 0xF];
            h >>= 4;
        }
        out[i] = Rf_mkCharLenCE(buffer, 16, CE_UTF8);
    }
    return out;
}

// [[Rcpp::export]]
NumericVector fast_eudex_distance_impl(const StringVector& a,
                                       const StringVector& b, bool graduated,
                                       int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const StringSnapshot sa(a, SnapshotText::raw), sb(b, SnapshotText::raw);
    NumericVector out = no_init(a.size());
    EudexDistanceWorker worker(sa.data(), sb.data(), graduated, out);
    dispatch_for(0, static_cast<std::size_t>(a.size()), worker,
                 estimated_pairwise_string_work(sa, sb), 5000, nthreads);
    return out;
}

// [[Rcpp::export]]
NumericVector fast_geo_distance_impl(NumericVector lat_1, NumericVector lon_1,
                                     NumericVector lat_2, NumericVector lon_2,
                                     double radius, int nthreads) {
    const R_xlen_t n = lat_1.size();
    if (lon_1.size() != n || lat_2.size() != n || lon_2.size() != n)
        stop("The four coordinate vectors must have the same length.");
    NumericVector out = no_init(n);
    GeoWorker worker(REAL(lat_1), REAL(lon_1), REAL(lat_2), REAL(lon_2), radius,
                     out);
    dispatch_for(0, static_cast<std::size_t>(n), worker,
                 static_cast<std::size_t>(n), 10000, nthreads);
    return out;
}
