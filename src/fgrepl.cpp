// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

// PCRE2 for regex: compiled code is immutable (thread-safe), per-thread
// match_data means zero cross-thread synchronisation.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

// RE2's StringPiece: used as a zero-copy string view for the fixed path.
#include "re2/stringpiece.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// Regex matching via PCRE2 (parallel, each thread owns its match_data)
//
// pcre2_code (compiled pattern) is immutable after pcre2_compile() — fully
// thread-safe for concurrent pcre2_match() calls.
// pcre2_match_data is not thread-safe, so each worker thread creates and
// destroys its own instance inside operator().
// ---------------------------------------------------------------------------

struct PCRE2GrepWorker : public Worker {
    SEXP x_sexp;
    pcre2_code* code;
    RVector<int> out;

    PCRE2GrepWorker(SEXP x_sexp, pcre2_code* code, IntegerVector& out)
        : x_sexp(x_sexp), code(code), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* mdata = pcre2_match_data_create_from_pattern(code, NULL);
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) {
                out[i] = NA_INTEGER;
            } else {
                const char* s = CHAR(elem);
                PCRE2_SIZE len = (PCRE2_SIZE)LENGTH(elem);
                int rc = pcre2_match(code, (PCRE2_SPTR8)s, len, 0, 0, mdata, NULL);
                out[i] = (rc >= 0) ? 1 : 0;
            }
        }
        pcre2_match_data_free(mdata);
    }
};

// [[Rcpp::export]]
LogicalVector fast_grepl_impl(const std::string& pattern,
                               const StringVector& x,
                               bool ignore_case) {
    uint32_t opts = PCRE2_DOTALL | (ignore_case ? PCRE2_CASELESS : 0);
    int errcode;
    PCRE2_SIZE erroffset;
    pcre2_code* code = pcre2_compile(
        (PCRE2_SPTR8)pattern.c_str(), pattern.size(),
        opts, &errcode, &erroffset, NULL
    );
    if (!code) {
        PCRE2_UCHAR8 msg[256];
        pcre2_get_error_message(errcode, msg, sizeof(msg));
        stop("Invalid PCRE2 pattern: %s", (const char*)msg);
    }
    // Enable JIT if available (best-effort; no error if JIT is not compiled in)
    pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);

    const R_xlen_t n = x.size();
    IntegerVector raw(n, 0);

    if (n < 10000) {
        PCRE2GrepWorker worker(x, code, raw);
        worker(0, (std::size_t)n);
    } else {
        PCRE2GrepWorker worker(x, code, raw);
        parallelFor(0, (std::size_t)n, worker);
    }

    pcre2_code_free(code);

    LogicalVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (raw[i] == NA_INTEGER) result[i] = NA_LOGICAL;
        else result[i] = raw[i] != 0;
    }
    return result;
}

// ---------------------------------------------------------------------------
// Fixed (literal) string matching — zero regex overhead, zero copy.
//
// For ignore_case=FALSE: wraps R's CHARSXP directly in re2::StringPiece
// (no heap allocation) and uses StringPiece::find() — SIMD-friendly via
// the compiler's memchr/memcmp intrinsics.
//
// For ignore_case=TRUE: must lowercase; allocates once per string in the
// worker (unavoidable without case-folding tables).
// ---------------------------------------------------------------------------

struct FixedGrepWorker : public Worker {
    SEXP x_sexp;
    re2::StringPiece needle;
    RVector<int> out;

    FixedGrepWorker(SEXP x_sexp, const std::string& pat, IntegerVector& out)
        : x_sexp(x_sexp), needle(pat), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) {
                out[i] = NA_INTEGER;
            } else {
                re2::StringPiece hay(CHAR(elem), (std::size_t)LENGTH(elem));
                out[i] = (hay.find(needle) != re2::StringPiece::npos) ? 1 : 0;
            }
        }
    }
};

struct FixedCaseWorker : public Worker {
    SEXP x_sexp;
    std::string needle_lower;
    RVector<int> out;

    FixedCaseWorker(SEXP x_sexp, const std::string& needle_lower, IntegerVector& out)
        : x_sexp(x_sexp), needle_lower(needle_lower), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) {
                out[i] = NA_INTEGER;
            } else {
                const char* s = CHAR(elem);
                std::size_t len = (std::size_t)LENGTH(elem);
                std::string lower(len, '\0');
                for (std::size_t j = 0; j < len; ++j)
                    lower[j] = (char)std::tolower((unsigned char)s[j]);
                out[i] = (lower.find(needle_lower) != std::string::npos) ? 1 : 0;
            }
        }
    }
};

// [[Rcpp::export]]
LogicalVector fast_fixed_impl(const std::string& pattern,
                               const StringVector& x,
                               bool ignore_case) {
    const R_xlen_t n = x.size();
    IntegerVector raw(n, 0);
    const bool go_parallel = (n >= 10000);

    if (ignore_case) {
        std::string needle_lower(pattern.size(), '\0');
        std::transform(pattern.begin(), pattern.end(), needle_lower.begin(),
                       [](unsigned char c) { return (char)std::tolower(c); });
        FixedCaseWorker worker(x, needle_lower, raw);
        if (go_parallel) parallelFor(0, (std::size_t)n, worker);
        else             worker(0, (std::size_t)n);
    } else {
        FixedGrepWorker worker(x, pattern, raw);
        if (go_parallel) parallelFor(0, (std::size_t)n, worker);
        else             worker(0, (std::size_t)n);
    }

    LogicalVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (raw[i] == NA_INTEGER) result[i] = NA_LOGICAL;
        else result[i] = raw[i] != 0;
    }
    return result;
}
