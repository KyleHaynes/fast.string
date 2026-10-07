// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cstdint>
#include <string>
#include <vector>
#include "ratcliff_obershelp_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

// fuzzywuzzy's utils.full_process(s, force_ascii = True), as used by
// QRatio, token_sort_ratio and token_set_ratio:
//   * characters U+0080-U+00FF are deleted (asciidammit),
//   * every remaining character matching (?ui)\W -- anything but a letter,
//     digit or underscore -- becomes one space (runs are not collapsed),
//   * the result is lowercased and stripped.
// Working on bytes, characters from U+0100 up are kept verbatim as word
// characters; Python would also lowercase letters among them and turn
// non-letters among them into spaces.
static inline void fuzz_full_process(const char* s, std::size_t n,
                                     std::string& out) {
    out.clear();
    out.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        const unsigned char c = static_cast<unsigned char>(s[i]);
        if (c < 0x80) {
            if (c >= 'A' && c <= 'Z')
                out.push_back(static_cast<char>(c + ('a' - 'A')));
            else if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') ||
                     c == '_')
                out.push_back(static_cast<char>(c));
            else
                out.push_back(' ');
            continue;
        }
        // U+0080-U+00FF are the two-byte sequences led by 0xC2 or 0xC3.
        if ((c == 0xC2 || c == 0xC3) && i + 1 < n &&
            (static_cast<unsigned char>(s[i + 1]) & 0xC0) == 0x80) {
            ++i;
            continue;
        }
        out.push_back(static_cast<char>(c));
    }
    std::size_t begin = 0, end = out.size();
    while (begin < end && out[begin] == ' ') ++begin;
    while (end > begin && out[end - 1] == ' ') --end;
    out.assign(out, begin, end - begin);
}

// Code points of UTF-8 text. A byte that does not start a valid sequence
// stands for itself, mapped past the Unicode range so it only equals itself.
static void decode_utf8(const char* s, int n, std::vector<std::uint32_t>& out) {
    out.clear();
    for (int i = 0; i < n;) {
        const unsigned char c = static_cast<unsigned char>(s[i]);
        int width = c < 0x80 ? 1 : (c >> 5) == 0x6 ? 2 : (c >> 4) == 0xE ? 3
            : (c >> 3) == 0x1E ? 4 : 0;
        bool valid = width != 0 && i + width <= n;
        for (int k = 1; valid && k < width; ++k)
            valid = (static_cast<unsigned char>(s[i + k]) & 0xC0) == 0x80;
        if (!valid) {
            out.push_back(0x110000u + c);
            ++i;
            continue;
        }
        std::uint32_t point = width == 1 ? c : c & (0x7F >> width);
        for (int k = 1; k < width; ++k)
            point = (point << 6) | (static_cast<unsigned char>(s[i + k]) & 0x3F);
        out.push_back(point);
        i += width;
    }
}

// difflib compares Python characters, so non-ASCII text is compared by code
// point; ASCII pairs keep the faster byte path.
struct FuzzScratch {
    std::vector<std::uint32_t> a, b;
};

template <bool Partial>
static double text_ratio(const char* a, int la, const char* b, int lb,
                         FuzzScratch& scratch) {
    if (bytes_are_ascii(a, static_cast<std::size_t>(la)) &&
        bytes_are_ascii(b, static_cast<std::size_t>(lb))) {
        return Partial ? ro_partial_ratio(a, la, b, lb) : ro_ratio(a, la, b, lb);
    }
    decode_utf8(a, la, scratch.a);
    decode_utf8(b, lb, scratch.b);
    const int na = static_cast<int>(scratch.a.size());
    const int nb = static_cast<int>(scratch.b.size());
    return Partial
        ? ro_partial_ratio(scratch.a.data(), na, scratch.b.data(), nb)
        : ro_ratio(scratch.a.data(), na, scratch.b.data(), nb);
}

// Each scorer applies the guards fuzzywuzzy's own version applies to the
// (optionally processed) strings: equal strings score 100 and an empty one
// scores 0. `processed` marks full_process input: QRatio and token_set_ratio
// then return 0 when either side processed to nothing, even for two equal
// raw strings.
enum class FuzzKind { ratio, partial, token_sort, token_set };

template <FuzzKind Kind>
static double fuzz_score(const char* a, int la, const char* b, int lb,
                         bool processed, FuzzScratch& scratch) {
    switch (Kind) {
    case FuzzKind::ratio:
        if (processed && (la == 0 || lb == 0)) return 0.0;
        return 100.0 * text_ratio<false>(a, la, b, lb, scratch);
    case FuzzKind::partial:
        if (processed && (la == 0 || lb == 0)) return 0.0;
        return 100.0 * text_ratio<true>(a, la, b, lb, scratch);
    case FuzzKind::token_sort: {
        const std::string sa = ro_sorted_token_string(a, la);
        const std::string sb = ro_sorted_token_string(b, lb);
        return 100.0 * text_ratio<false>(sa.data(), (int)sa.size(),
                                         sb.data(), (int)sb.size(), scratch);
    }
    case FuzzKind::token_set:
        if (la == 0 || lb == 0) return 0.0;
        return 100.0 * ro_token_set_ratio(
            a, la, b, lb,
            [&scratch](const std::string& x, const std::string& y) {
                return text_ratio<false>(x.data(), (int)x.size(),
                                         y.data(), (int)y.size(), scratch);
            }
        );
    }
    return NA_REAL;
}

template <FuzzKind Kind>
struct FuzzWorker : public Worker {
    const StringView* a;
    const StringView* b;
    bool full_process;
    RVector<double> out;

    FuzzWorker(const StringView* a, const StringView* b, bool full_process,
               NumericVector& out)
        : a(a), b(b), full_process(full_process), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string normalized_a, normalized_b;
        FuzzScratch scratch;
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& av = a[i];
            const StringView& bv = b[i];
            if (av.is_na() || bv.is_na()) {
                out[i] = NA_REAL;
                continue;
            }

            if (full_process) {
                fuzz_full_process(av.data, av.size, normalized_a);
                fuzz_full_process(bv.data, bv.size, normalized_b);
                out[i] = fuzz_score<Kind>(
                    normalized_a.data(), static_cast<int>(normalized_a.size()),
                    normalized_b.data(), static_cast<int>(normalized_b.size()),
                    true, scratch
                );
            } else if (av.size == bv.size &&
                       (av.data == bv.data ||
                        std::memcmp(av.data, bv.data, av.size) == 0)) {
                out[i] = 100.0;
            } else {
                out[i] = fuzz_score<Kind>(
                    av.data, static_cast<int>(av.size),
                    bv.data, static_cast<int>(bv.size), false, scratch
                );
            }
        }
    }
};

template <FuzzKind Kind>
static NumericVector run_fuzz(const StringVector& a, const StringVector& b,
                              bool full_process, int nthreads) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");

    const std::size_t n = static_cast<std::size_t>(a.size());
    NumericVector result(a.size());
    StringSnapshot a_snapshot(a, SnapshotText::utf8);
    StringSnapshot b_snapshot(b, SnapshotText::utf8);
    FuzzWorker<Kind> worker(
        a_snapshot.data(), b_snapshot.data(), full_process, result
    );
    dispatch_for(
        0, n, worker,
        estimated_pairwise_string_work(a_snapshot, b_snapshot),
        1000, nthreads
    );
    return result;
}

// [[Rcpp::export]]
NumericVector fast_fuzz_ratio_impl(const StringVector& a, const StringVector& b,
                                   bool full_process, int nthreads) {
    return run_fuzz<FuzzKind::ratio>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_partial_ratio_impl(const StringVector& a, const StringVector& b,
                                           bool full_process, int nthreads) {
    return run_fuzz<FuzzKind::partial>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_sort_ratio_impl(const StringVector& a, const StringVector& b,
                                              bool full_process, int nthreads) {
    return run_fuzz<FuzzKind::token_sort>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_set_ratio_impl(const StringVector& a, const StringVector& b,
                                             bool full_process, int nthreads) {
    return run_fuzz<FuzzKind::token_set>(a, b, full_process, nthreads);
}
