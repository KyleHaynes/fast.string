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
    uint32_t opts = ignore_case ? PCRE2_CASELESS : 0;
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

// ---------------------------------------------------------------------------
// String substitution — parallel workers write to std::vector<std::string>
// (no R API in threads), main thread converts to CharacterVector afterwards.
// NA elements: worker marks with empty string, main thread restores NA_STRING.
// ---------------------------------------------------------------------------

struct PCRE2SubWorker : public Worker {
    SEXP x_sexp;
    pcre2_code* code;
    const uint8_t* repl_data;
    PCRE2_SIZE repl_len;
    uint32_t sub_flags;
    std::vector<std::string>& results;

    PCRE2SubWorker(SEXP x_sexp, pcre2_code* code,
                   const uint8_t* repl_data, PCRE2_SIZE repl_len,
                   uint32_t sub_flags, std::vector<std::string>& results)
        : x_sexp(x_sexp), code(code),
          repl_data(repl_data), repl_len(repl_len),
          sub_flags(sub_flags), results(results) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* mdata = pcre2_match_data_create_from_pattern(code, NULL);
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const char* s = CHAR(elem);
            PCRE2_SIZE len = (PCRE2_SIZE)LENGTH(elem);
            PCRE2_SIZE outlen = std::max((PCRE2_SIZE)64,
                                         len * 2 + repl_len * 16 + 4);
            std::vector<uint8_t> buf(outlen + 1);
            int rc = pcre2_substitute(
                code, (PCRE2_SPTR8)s, len, 0,
                sub_flags | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH,
                mdata, NULL, repl_data, repl_len, buf.data(), &outlen);
            if (rc == PCRE2_ERROR_NOMEMORY) {
                buf.resize(outlen + 1);
                PCRE2_SIZE outlen2 = (PCRE2_SIZE)buf.size();
                pcre2_substitute(code, (PCRE2_SPTR8)s, len, 0, sub_flags,
                                 mdata, NULL, repl_data, repl_len,
                                 buf.data(), &outlen2);
                outlen = outlen2;
            }
            results[i].assign((char*)buf.data(), outlen);
        }
        pcre2_match_data_free(mdata);
    }
};

// [[Rcpp::export]]
CharacterVector fast_regex_sub_impl(const std::string& pattern,
                                     const std::string& replacement,
                                     const StringVector& x,
                                     bool ignore_case,
                                     bool global) {
    uint32_t opts = ignore_case ? PCRE2_CASELESS : 0;
    int errcode;
    PCRE2_SIZE erroffset;
    pcre2_code* code = pcre2_compile(
        (PCRE2_SPTR8)pattern.c_str(), pattern.size(),
        opts, &errcode, &erroffset, NULL);
    if (!code) {
        PCRE2_UCHAR8 msg[256];
        pcre2_get_error_message(errcode, msg, sizeof(msg));
        stop("Invalid PCRE2 pattern: %s", (const char*)msg);
    }
    pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);

    const R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    // PCRE2_SUBSTITUTE_EXTENDED enables \1-\9 backreference syntax in
    // replacement strings (same convention as base R's gsub replacement).
    uint32_t sub_flags = (global ? PCRE2_SUBSTITUTE_GLOBAL : 0)
                       | PCRE2_SUBSTITUTE_EXTENDED;

    PCRE2SubWorker worker(x, code, (const uint8_t*)replacement.c_str(),
                          (PCRE2_SIZE)replacement.size(), sub_flags, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    pcre2_code_free(code);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

struct FixedSubWorker : public Worker {
    SEXP x_sexp;
    re2::StringPiece needle;
    const std::string& replacement;
    bool global;
    std::vector<std::string>& results;

    FixedSubWorker(SEXP x_sexp, const std::string& needle_str,
                   const std::string& replacement, bool global,
                   std::vector<std::string>& results)
        : x_sexp(x_sexp), needle(needle_str),
          replacement(replacement), global(global), results(results) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const char* s = CHAR(elem);
            std::size_t len = (std::size_t)LENGTH(elem);
            std::size_t nl = needle.size();
            std::size_t pos = 0;
            std::string out;
            re2::StringPiece hay(s, len);
            std::size_t found;
            while ((found = hay.find(needle)) != re2::StringPiece::npos) {
                out.append(s + pos, found);
                out.append(replacement);
                pos += found + nl;
                hay.remove_prefix(found + nl);
                if (!global) break;
            }
            if (out.empty() && pos == 0) {
                results[i].assign(s, len);
            } else {
                out.append(s + pos, len - pos);
                results[i] = std::move(out);
            }
        }
    }
};

struct FixedCaseSubWorker : public Worker {
    SEXP x_sexp;
    std::string needle_lower;
    const std::string& replacement;
    bool global;
    std::vector<std::string>& results;

    FixedCaseSubWorker(SEXP x_sexp, const std::string& needle_lower,
                       const std::string& replacement, bool global,
                       std::vector<std::string>& results)
        : x_sexp(x_sexp), needle_lower(needle_lower),
          replacement(replacement), global(global), results(results) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const char* s = CHAR(elem);
            std::size_t len = (std::size_t)LENGTH(elem);
            std::string lower(len, '\0');
            for (std::size_t j = 0; j < len; ++j)
                lower[j] = (char)std::tolower((unsigned char)s[j]);
            std::size_t nl = needle_lower.size();
            std::size_t pos = 0;
            std::string out;
            std::size_t found;
            while ((found = lower.find(needle_lower, pos)) != std::string::npos) {
                out.append(s + pos, found - pos);
                out.append(replacement);
                pos = found + nl;
                if (!global) break;
            }
            if (out.empty() && pos == 0) {
                results[i].assign(s, len);
            } else {
                out.append(s + pos, len - pos);
                results[i] = std::move(out);
            }
        }
    }
};

// ---------------------------------------------------------------------------
// Helpers for multi-pattern substitution (used by FixedMultiSubWorker below)
// ---------------------------------------------------------------------------

// Global in-place replace of one fixed needle in s. No-op if needle is empty.
static void fixed_replace_str(std::string& s,
                               const re2::StringPiece& needle,
                               const std::string& repl) {
    std::size_t nl = needle.size();
    if (nl == 0) return;
    re2::StringPiece hay(s.data(), s.size());
    std::string out;
    bool changed = false;
    while (true) {
        std::size_t found = hay.find(needle);
        if (found == re2::StringPiece::npos) break;
        changed = true;
        out.append(hay.data(), found);
        out.append(repl);
        hay.remove_prefix(found + nl);
    }
    if (changed) {
        out.append(hay.data(), hay.size());
        s = std::move(out);
    }
}

// Case-insensitive variant. needle_lower must already be lower-cased.
static void fixed_replace_str_ci(std::string& s,
                                   const std::string& needle_lower,
                                   const std::string& repl) {
    std::size_t nl = needle_lower.size();
    if (nl == 0) return;
    std::string s_lower(s.size(), '\0');
    for (std::size_t j = 0; j < s.size(); ++j)
        s_lower[j] = (char)std::tolower((unsigned char)s[j]);
    std::size_t pos = 0;
    std::string out;
    bool changed = false;
    while (true) {
        std::size_t found = s_lower.find(needle_lower, pos);
        if (found == std::string::npos) break;
        changed = true;
        out.append(s.data() + pos, found - pos);
        out.append(repl);
        pos = found + nl;
    }
    if (changed) {
        out.append(s.data() + pos, s.size() - pos);
        s = std::move(out);
    }
}

// Single-scan leftmost-wins replace: at each position find the earliest matching
// needle; first needle in list breaks ties at the same position.
static std::string fixed_scan_replace(const char* hay, std::size_t len,
                                       const std::vector<re2::StringPiece>& needles,
                                       const std::vector<std::string>& repls) {
    std::size_t pos = 0;
    std::string result;
    bool changed = false;
    while (pos < len) {
        std::size_t best = re2::StringPiece::npos, best_idx = 0;
        re2::StringPiece sp(hay + pos, len - pos);
        for (std::size_t j = 0; j < needles.size(); ++j) {
            if (needles[j].empty()) continue;
            std::size_t f = sp.find(needles[j]);
            if (f != re2::StringPiece::npos &&
                (best == re2::StringPiece::npos || f < best)) {
                best = f; best_idx = j;
            }
        }
        if (best == re2::StringPiece::npos) break;
        changed = true;
        result.append(hay + pos, best);
        result.append(repls[best_idx]);
        pos += best + needles[best_idx].size();
    }
    if (!changed) return std::string(hay, len);
    result.append(hay + pos, len - pos);
    return result;
}

// Case-insensitive single-scan variant: lowercase haystack once, match there,
// copy from the original for non-matched segments.
static std::string fixed_scan_replace_ci(const char* hay, std::size_t len,
                                           const std::vector<std::string>& needles_lower,
                                           const std::vector<std::string>& repls) {
    std::string hay_lower(len, '\0');
    for (std::size_t j = 0; j < len; ++j)
        hay_lower[j] = (char)std::tolower((unsigned char)hay[j]);
    std::size_t pos = 0;
    std::string result;
    bool changed = false;
    while (pos < len) {
        std::size_t best = std::string::npos, best_idx = 0;
        for (std::size_t j = 0; j < needles_lower.size(); ++j) {
            if (needles_lower[j].empty()) continue;
            std::size_t f = hay_lower.find(needles_lower[j], pos);
            if (f != std::string::npos &&
                (best == std::string::npos || f < best)) {
                best = f; best_idx = j;
            }
        }
        if (best == std::string::npos) break;
        changed = true;
        result.append(hay + pos, best - pos);
        result.append(repls[best_idx]);
        pos = best + needles_lower[best_idx].size();
    }
    if (!changed) return std::string(hay, len);
    result.append(hay + pos, len - pos);
    return result;
}

// ---------------------------------------------------------------------------
// Multi-pattern fixed substitution worker
// sequential=true:  fold each pattern in order over the running result
// sequential=false: single-scan leftmost-wins across all patterns
// ---------------------------------------------------------------------------

struct FixedMultiSubWorker : public Worker {
    SEXP x_sexp;
    const std::vector<re2::StringPiece>& needles;
    const std::vector<std::string>& needles_lower;
    const std::vector<std::string>& repls;
    bool ignore_case;
    bool sequential;
    std::vector<std::string>& results;

    FixedMultiSubWorker(SEXP x_sexp,
                         const std::vector<re2::StringPiece>& needles,
                         const std::vector<std::string>& needles_lower,
                         const std::vector<std::string>& repls,
                         bool ignore_case, bool sequential,
                         std::vector<std::string>& results)
        : x_sexp(x_sexp), needles(needles), needles_lower(needles_lower),
          repls(repls), ignore_case(ignore_case), sequential(sequential),
          results(results) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const char* s = CHAR(elem);
            std::size_t len = (std::size_t)LENGTH(elem);
            if (sequential) {
                std::string cur(s, len);
                if (ignore_case) {
                    for (std::size_t j = 0; j < needles_lower.size(); ++j)
                        fixed_replace_str_ci(cur, needles_lower[j], repls[j]);
                } else {
                    for (std::size_t j = 0; j < needles.size(); ++j)
                        fixed_replace_str(cur, needles[j], repls[j]);
                }
                results[i] = std::move(cur);
            } else {
                if (ignore_case)
                    results[i] = fixed_scan_replace_ci(s, len, needles_lower, repls);
                else
                    results[i] = fixed_scan_replace(s, len, needles, repls);
            }
        }
    }
};

// ---------------------------------------------------------------------------
// Multi-pattern PCRE2 substitution worker (always chains patterns in order;
// sequential=false is a no-op for the regex path — combined-alternation with
// proper backreference-offset rewriting is deferred future work).
// ---------------------------------------------------------------------------

struct PCRE2MultiSubWorker : public Worker {
    SEXP x_sexp;
    const std::vector<pcre2_code*>& codes;
    const std::vector<std::string>& repls;
    uint32_t sub_flags;
    std::vector<std::string>& results;

    PCRE2MultiSubWorker(SEXP x_sexp,
                         const std::vector<pcre2_code*>& codes,
                         const std::vector<std::string>& repls,
                         uint32_t sub_flags,
                         std::vector<std::string>& results)
        : x_sexp(x_sexp), codes(codes), repls(repls),
          sub_flags(sub_flags), results(results) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::size_t np = codes.size();
        std::vector<pcre2_match_data*> mdatas(np);
        for (std::size_t j = 0; j < np; ++j)
            mdatas[j] = pcre2_match_data_create_from_pattern(codes[j], NULL);

        // Ping-pong buffers: output of step j feeds into step j+1 without copying.
        // After swap, cur_data still points into the buffer now owned by buf_b.
        std::vector<uint8_t> buf_a, buf_b;

        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }

            const char* cur_data = CHAR(elem);
            PCRE2_SIZE cur_len   = (PCRE2_SIZE)LENGTH(elem);

            for (std::size_t j = 0; j < np; ++j) {
                const std::string& rp = repls[j];
                PCRE2_SIZE outlen = std::max((PCRE2_SIZE)64,
                    cur_len * 2 + (PCRE2_SIZE)rp.size() * 16 + 4);
                buf_a.resize(outlen + 1);

                int rc = pcre2_substitute(
                    codes[j],
                    (PCRE2_SPTR8)cur_data, cur_len, 0,
                    sub_flags | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH,
                    mdatas[j], NULL,
                    (PCRE2_SPTR8)rp.data(), (PCRE2_SIZE)rp.size(),
                    buf_a.data(), &outlen);

                if (rc == PCRE2_ERROR_NOMEMORY) {
                    buf_a.resize(outlen + 1);
                    PCRE2_SIZE outlen2 = (PCRE2_SIZE)buf_a.size();
                    pcre2_substitute(
                        codes[j],
                        (PCRE2_SPTR8)cur_data, cur_len, 0, sub_flags,
                        mdatas[j], NULL,
                        (PCRE2_SPTR8)rp.data(), (PCRE2_SIZE)rp.size(),
                        buf_a.data(), &outlen2);
                    outlen = outlen2;
                }

                cur_data = (const char*)buf_a.data();
                cur_len  = outlen;
                std::swap(buf_a, buf_b);
                // cur_data now points into buf_b (old buf_a); buf_a is free next iter
            }

            results[i].assign(cur_data, (std::size_t)cur_len);
        }

        for (std::size_t j = 0; j < np; ++j)
            pcre2_match_data_free(mdatas[j]);
    }
};

// [[Rcpp::export]]
CharacterVector fast_fixed_gsub_all_impl(
        const StringVector& patterns,
        const StringVector& replacements,
        const StringVector& x,
        bool ignore_case, bool sequential) {
    const std::size_t np = (std::size_t)patterns.size();

    std::vector<std::string> pat_strs(np), repl_strs(np), pats_lower(np);
    for (std::size_t j = 0; j < np; ++j) {
        pat_strs[j]   = Rcpp::as<std::string>(patterns[j]);
        repl_strs[j]  = Rcpp::as<std::string>(replacements[j]);
        pats_lower[j].resize(pat_strs[j].size());
        std::transform(pat_strs[j].begin(), pat_strs[j].end(),
                       pats_lower[j].begin(),
                       [](unsigned char c){ return (char)std::tolower(c); });
    }

    std::vector<re2::StringPiece> needles(np);
    for (std::size_t j = 0; j < np; ++j)
        needles[j] = re2::StringPiece(pat_strs[j]);

    const R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);

    FixedMultiSubWorker worker(x, needles, pats_lower, repl_strs,
                                ignore_case, sequential, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

// [[Rcpp::export]]
CharacterVector fast_regex_gsub_all_impl(
        const StringVector& patterns,
        const StringVector& replacements,
        const StringVector& x,
        bool ignore_case, bool sequential) {
    (void)sequential;  // regex path always chains patterns sequentially
    const std::size_t np = (std::size_t)patterns.size();
    uint32_t opts      = ignore_case ? PCRE2_CASELESS : 0;
    uint32_t sub_flags = PCRE2_SUBSTITUTE_GLOBAL | PCRE2_SUBSTITUTE_EXTENDED;

    std::vector<pcre2_code*> codes(np, nullptr);
    std::vector<std::string> repl_strs(np);

    for (std::size_t j = 0; j < np; ++j) {
        repl_strs[j] = Rcpp::as<std::string>(replacements[j]);
        std::string pat = Rcpp::as<std::string>(patterns[j]);
        int errcode; PCRE2_SIZE erroffset;
        codes[j] = pcre2_compile(
            (PCRE2_SPTR8)pat.c_str(), pat.size(),
            opts, &errcode, &erroffset, NULL);
        if (!codes[j]) {
            for (std::size_t k = 0; k < j; ++k) pcre2_code_free(codes[k]);
            PCRE2_UCHAR8 msg[256];
            pcre2_get_error_message(errcode, msg, sizeof(msg));
            stop("Invalid PCRE2 pattern[%zu]: %s", j, (const char*)msg);
        }
        pcre2_jit_compile(codes[j], PCRE2_JIT_COMPLETE);
    }

    const R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);

    PCRE2MultiSubWorker worker(x, codes, repl_strs, sub_flags, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    for (std::size_t j = 0; j < np; ++j) pcre2_code_free(codes[j]);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

// [[Rcpp::export]]
CharacterVector fast_fixed_sub_impl(const std::string& pattern,
                                     const std::string& replacement,
                                     const StringVector& x,
                                     bool ignore_case,
                                     bool global) {
    const R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    const bool go_parallel = (n >= 10000);

    if (ignore_case) {
        std::string needle_lower(pattern.size(), '\0');
        std::transform(pattern.begin(), pattern.end(), needle_lower.begin(),
                       [](unsigned char c) { return (char)std::tolower(c); });
        FixedCaseSubWorker worker(x, needle_lower, replacement, global, results);
        if (go_parallel) parallelFor(0, (std::size_t)n, worker);
        else             worker(0, (std::size_t)n);
    } else {
        FixedSubWorker worker(x, pattern, replacement, global, results);
        if (go_parallel) parallelFor(0, (std::size_t)n, worker);
        else             worker(0, (std::size_t)n);
    }

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}
