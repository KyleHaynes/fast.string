// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include "ratcliff_obershelp_core.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
using namespace Rcpp;
using namespace RcppParallel;

static double sim_ratio(const char* a, int la, const char* b, int lb) {
    return 100.0 * ro_ratio(a, la, b, lb);
}
static double sim_partial_ratio(const char* a, int la, const char* b, int lb) {
    return 100.0 * ro_partial_ratio(a, la, b, lb);
}
static double sim_token_sort_ratio(const char* a, int la, const char* b, int lb) {
    std::string sa = ro_sorted_token_string(a, la);
    std::string sb = ro_sorted_token_string(b, lb);
    return 100.0 * ro_ratio(sa.data(), (int)sa.size(), sb.data(), (int)sb.size());
}
static double sim_token_set_ratio(const char* a, int la, const char* b, int lb) {
    return 100.0 * ro_token_set_ratio(a, la, b, lb);
}

// Exact fused equivalent of:
//   tolower(gsub("[^A-Za-z0-9]+", " ", x)) |> trimws()
// The regex is deliberately ASCII-only and PCRE2 runs in byte mode, so every
// non-ASCII byte is part of a separator run.
static inline void fuzz_full_process_ascii(const char* s, std::size_t n,
                                           std::string& out) {
    out.clear();
    out.reserve(n);
    bool pending_space = false;

    for (std::size_t i = 0; i < n; ++i) {
        unsigned char c = static_cast<unsigned char>(s[i]);
        bool is_upper = c >= 'A' && c <= 'Z';
        bool is_lower = c >= 'a' && c <= 'z';
        bool is_digit = c >= '0' && c <= '9';

        if (is_upper || is_lower || is_digit) {
            if (pending_space && !out.empty()) out.push_back(' ');
            out.push_back(is_upper ? static_cast<char>(c + ('a' - 'A'))
                                   : static_cast<char>(c));
            pending_space = false;
        } else {
            pending_space = true;
        }
    }
}

using FuzzFn = double (*)(const char*, int, const char*, int);

template <FuzzFn Fn>
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
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& av = a[i];
            const StringView& bv = b[i];
            if (av.is_na() || bv.is_na()) {
                out[i] = NA_REAL;
                continue;
            }

            if (full_process) {
                fuzz_full_process_ascii(av.data, av.size, normalized_a);
                fuzz_full_process_ascii(bv.data, bv.size, normalized_b);
                out[i] = Fn(
                    normalized_a.data(), static_cast<int>(normalized_a.size()),
                    normalized_b.data(), static_cast<int>(normalized_b.size())
                );
            } else {
                out[i] = Fn(
                    av.data, static_cast<int>(av.size),
                    bv.data, static_cast<int>(bv.size)
                );
            }
        }
    }
};

template <FuzzFn Fn>
static NumericVector run_fuzz(const StringVector& a, const StringVector& b,
                              bool full_process, int nthreads) {
    if (a.size() != b.size())
        stop("`a` and `b` must have the same length.");

    const std::size_t n = static_cast<std::size_t>(a.size());
    NumericVector result(a.size());
    StringSnapshot a_snapshot(a), b_snapshot(b);
    FuzzWorker<Fn> worker(
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
    return run_fuzz<sim_ratio>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_partial_ratio_impl(const StringVector& a, const StringVector& b,
                                           bool full_process, int nthreads) {
    return run_fuzz<sim_partial_ratio>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_sort_ratio_impl(const StringVector& a, const StringVector& b,
                                              bool full_process, int nthreads) {
    return run_fuzz<sim_token_sort_ratio>(a, b, full_process, nthreads);
}

// [[Rcpp::export]]
NumericVector fast_fuzz_token_set_ratio_impl(const StringVector& a, const StringVector& b,
                                             bool full_process, int nthreads) {
    return run_fuzz<sim_token_set_ratio>(a, b, full_process, nthreads);
}
