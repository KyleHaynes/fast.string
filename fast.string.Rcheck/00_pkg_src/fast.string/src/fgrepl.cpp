// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

// PCRE2 for regex: compiled code is immutable (thread-safe), per-thread
// match_data means zero cross-thread synchronisation.
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include "fixed_search.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
#include "text_results.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <limits>

using namespace Rcpp;
using namespace RcppParallel;

static std::size_t estimated_string_work(const StringSnapshot& snapshot,
                                         std::size_t per_element = 8) {
    const std::uint64_t max_size =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    std::uint64_t work = snapshot.total_bytes();
    const std::uint64_t n = static_cast<std::uint64_t>(snapshot.size());
    if (per_element != 0 &&
        n > (max_size - (std::min)(work, max_size)) / per_element) {
        return (std::numeric_limits<std::size_t>::max)();
    }
    work = (std::min)(work, max_size) + n * per_element;
    return static_cast<std::size_t>(work);
}

// Base R does not take a second zero-length match at subject end immediately
// after a non-empty global match that already ended there. PCRE2's built-in
// global substitute does, so detect that one edge case before substitution.
// Return 1 when the terminal replacement must be removed, 0 otherwise, or a
// negative PCRE2 error code.
static int suppress_terminal_empty_match(
        pcre2_code* code,
        const char* subject,
        PCRE2_SIZE length,
        pcre2_match_data* scan_data) {
    PCRE2_SIZE offset = 0;
    while (true) {
        const int rc = pcre2_match(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
            offset, 0, scan_data, NULL
        );
        if (rc == PCRE2_ERROR_NOMATCH) return 0;
        if (rc < 0) return rc;

        PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(scan_data);
        const PCRE2_SIZE start = ovector[0];
        const PCRE2_SIZE end = ovector[1];
        if (start != end && end == length) {
            const int terminal_rc = pcre2_match(
                code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
                length, 0, scan_data, NULL
            );
            if (terminal_rc == PCRE2_ERROR_NOMATCH) return 0;
            if (terminal_rc < 0) return terminal_rc;
            ovector = pcre2_get_ovector_pointer(scan_data);
            return ovector[0] == length && ovector[1] == length ? 1 : 0;
        }
        if (start == end) {
            if (end == length) return 0;
            offset = end + 1;
        } else {
            offset = end;
        }
    }
}

// Remove the exact expansion PCRE2 appended for the terminal empty match.
// On PCRE2 versions with REPLACEMENT_ONLY this does no subject copying; older
// supported versions use one rare full-subject expansion and slice its suffix.
static int trim_terminal_empty_replacement(
        pcre2_code* code,
        const char* subject,
        PCRE2_SIZE length,
        const uint8_t* replacement,
        PCRE2_SIZE replacement_length,
        uint32_t substitute_flags,
        pcre2_match_data* terminal_data,
        std::vector<uint8_t>& output,
        PCRE2_SIZE& output_length,
        std::vector<uint8_t>& scratch) {
    const int match_rc = pcre2_match(
        code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
        length, 0, terminal_data, NULL
    );
    if (match_rc < 0) return match_rc;

    PCRE2_SIZE capacity = (std::max)(
        static_cast<PCRE2_SIZE>(64),
        replacement_length * 16 + 4
    );
#ifndef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    capacity += length;
#endif
    if (scratch.size() < static_cast<std::size_t>(capacity + 1))
        scratch.resize(static_cast<std::size_t>(capacity + 1));

    uint32_t flags =
        (substitute_flags & ~PCRE2_SUBSTITUTE_GLOBAL) |
        PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
#ifdef PCRE2_SUBSTITUTE_MATCHED
    flags |= PCRE2_SUBSTITUTE_MATCHED;
#endif
#ifdef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    flags |= PCRE2_SUBSTITUTE_REPLACEMENT_ONLY;
#endif
    PCRE2_SIZE expanded_length = capacity;
    int rc = pcre2_substitute(
        code, reinterpret_cast<PCRE2_SPTR8>(subject), length, length,
        flags, terminal_data, NULL,
        replacement, replacement_length,
        scratch.data(), &expanded_length
    );
    if (rc == PCRE2_ERROR_NOMEMORY) {
        scratch.resize(static_cast<std::size_t>(expanded_length + 1));
        const int rematch_rc = pcre2_match(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
            length, 0, terminal_data, NULL
        );
        if (rematch_rc < 0) return rematch_rc;
        PCRE2_SIZE retry_length =
            static_cast<PCRE2_SIZE>(scratch.size());
        rc = pcre2_substitute(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length, length,
            flags & ~PCRE2_SUBSTITUTE_OVERFLOW_LENGTH,
            terminal_data, NULL,
            replacement, replacement_length,
            scratch.data(), &retry_length
        );
        expanded_length = retry_length;
    }
    if (rc < 0) return rc;

    std::size_t expansion_offset = 0;
#ifndef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    if (expanded_length < length) return PCRE2_ERROR_INTERNAL;
    expansion_offset = static_cast<std::size_t>(length);
    expanded_length -= length;
#endif
    const std::size_t expansion_length =
        static_cast<std::size_t>(expanded_length);
    if (expansion_length > static_cast<std::size_t>(output_length))
        return PCRE2_ERROR_INTERNAL;
    const std::size_t suffix =
        static_cast<std::size_t>(output_length) - expansion_length;
    if (expansion_length != 0 &&
        std::memcmp(
            output.data() + suffix,
            scratch.data() + expansion_offset,
            expansion_length
        ) != 0) {
        return PCRE2_ERROR_INTERNAL;
    }
    output_length -= static_cast<PCRE2_SIZE>(expansion_length);
    return 0;
}

// ---------------------------------------------------------------------------
// Regex matching via PCRE2 (parallel, each thread owns its match_data)
//
// pcre2_code (compiled pattern) is immutable after pcre2_compile() — fully
// thread-safe for concurrent pcre2_match() calls.
// pcre2_match_data is not thread-safe, so each worker thread creates and
// destroys its own instance inside operator().
// ---------------------------------------------------------------------------

struct PCRE2GrepWorker : public Worker {
    const StringView* strings;
    pcre2_code* code;
    RVector<int> out;

    PCRE2GrepWorker(const StringView* strings, pcre2_code* code,
                    LogicalVector& out)
        : strings(strings), code(code), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        // Grep only needs the overall match. PCRE2 returns zero (still a
        // successful match) when the ovector is too small for captures.
        pcre2_match_data* mdata = pcre2_match_data_create(1, NULL);
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = NA_INTEGER;
            } else {
                int rc = pcre2_match(
                    code, reinterpret_cast<PCRE2_SPTR8>(value.data),
                    static_cast<PCRE2_SIZE>(value.size),
                    0, 0, mdata, NULL
                );
                out[i] = (rc >= 0) ? 1 : 0;
            }
        }
        pcre2_match_data_free(mdata);
    }
};

// [[Rcpp::export]]
LogicalVector fast_grepl_impl(const std::string& pattern,
                               const StringVector& x,
                               bool ignore_case,
                               int nthreads) {
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
    LogicalVector result(n, false);
    StringSnapshot snapshot(x);
    PCRE2GrepWorker worker(snapshot.data(), code, result);
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        estimated_string_work(snapshot), 250000, nthreads
    );

    pcre2_code_free(code);

    return result;
}

// ---------------------------------------------------------------------------
// Fixed (literal) string matching — zero regex overhead, zero copy.
//
// The prepared searcher selects memchr for one byte, memchr+memcmp for short
// needles, and Boyer-Moore-Horspool for longer needles. ignore_case uses one
// byte-fold table prepared on the main thread, without lowercasing haystacks.
// ---------------------------------------------------------------------------

struct FixedGrepWorker : public Worker {
    const StringView* strings;
    const PreparedFixedSearch& search;
    RVector<int> out;

    FixedGrepWorker(const StringView* strings,
                    const PreparedFixedSearch& search,
                    LogicalVector& out)
        : strings(strings), search(search), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = NA_INTEGER;
            } else {
                out[i] = search.find(value.data, value.size) != std::string::npos;
            }
        }
    }
};

// [[Rcpp::export]]
LogicalVector fast_fixed_impl(const std::string& pattern,
                               const StringVector& x,
                               bool ignore_case,
                               int nthreads) {
    const R_xlen_t n = x.size();
    LogicalVector result(n, false);
    StringSnapshot snapshot(x);
    PreparedFixedSearch search(pattern, ignore_case);
    FixedGrepWorker worker(snapshot.data(), search, result);
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        estimated_string_work(snapshot), 250000, nthreads
    );

    return result;
}

// ---------------------------------------------------------------------------
// String substitution — parallel workers write to std::vector<std::string>
// (no R API in threads), main thread converts to CharacterVector afterwards.
// NA elements: worker marks with empty string, main thread restores NA_STRING.
// ---------------------------------------------------------------------------

struct PCRE2SubWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    pcre2_code* code;
    const uint8_t* repl_data;
    PCRE2_SIZE repl_len;
    uint32_t sub_flags;
    bool can_match_empty;
    std::vector<TextResult>& results;
    TextArenas& arenas;
    std::vector<int>& errors;

    PCRE2SubWorker(const StringView* strings,
                   const std::vector<TextWorkChunk>& chunks,
                   pcre2_code* code,
                   const uint8_t* repl_data, PCRE2_SIZE repl_len,
                   uint32_t sub_flags, bool can_match_empty,
                   std::vector<TextResult>& results,
                   TextArenas& arenas,
                   std::vector<int>& errors)
        : strings(strings), chunks(chunks), code(code),
          repl_data(repl_data), repl_len(repl_len),
          sub_flags(sub_flags), can_match_empty(can_match_empty),
          results(results), arenas(arenas), errors(errors) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* mdata = pcre2_match_data_create_from_pattern(code, NULL);
        const bool global =
            (sub_flags & PCRE2_SUBSTITUTE_GLOBAL) != 0;
        pcre2_match_data* scan_data =
            global && can_match_empty
                ? pcre2_match_data_create_from_pattern(code, NULL)
                : nullptr;
        if (!mdata || (global && can_match_empty && !scan_data)) {
            for (std::size_t chunk = begin; chunk < end; ++chunk) {
                for (std::size_t i = chunks[chunk].begin;
                     i < chunks[chunk].end; ++i) {
                    errors[i] = PCRE2_ERROR_NOMEMORY;
                }
            }
            pcre2_match_data_free(mdata);
            pcre2_match_data_free(scan_data);
            return;
        }
        std::vector<uint8_t> buf, terminal_scratch;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& arena = arenas[chunk];
            arena.reserve(chunks[chunk].estimated_bytes);
            for (std::size_t i = chunks[chunk].begin;
                 i < chunks[chunk].end; ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                const PCRE2_SIZE len =
                    static_cast<PCRE2_SIZE>(value.size);

                const int match_rc = pcre2_match(
                    code, reinterpret_cast<PCRE2_SPTR8>(value.data), len,
                    0, 0, mdata, NULL
                );
                if (match_rc == PCRE2_ERROR_NOMATCH) continue;
                if (match_rc < 0) {
                    errors[i] = match_rc;
                    continue;
                }

                int trim_terminal = 0;
                if (scan_data) {
                    trim_terminal = suppress_terminal_empty_match(
                        code, value.data, len, scan_data
                    );
                    if (trim_terminal < 0) {
                        errors[i] = trim_terminal;
                        continue;
                    }
                }

                PCRE2_SIZE outlen = (std::max)(
                    static_cast<PCRE2_SIZE>(64),
                    len * 2 + repl_len * 16 + 4
                );
                if (buf.size() < static_cast<std::size_t>(outlen + 1))
                    buf.resize(static_cast<std::size_t>(outlen + 1));
                uint32_t flags =
                    sub_flags | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
#ifdef PCRE2_SUBSTITUTE_MATCHED
                flags |= PCRE2_SUBSTITUTE_MATCHED;
#endif
                int rc = pcre2_substitute(
                    code, reinterpret_cast<PCRE2_SPTR8>(value.data), len,
                    0, flags, mdata, NULL, repl_data, repl_len,
                    buf.data(), &outlen
                );
                if (rc == PCRE2_ERROR_NOMEMORY) {
                    buf.resize(static_cast<std::size_t>(outlen + 1));
                    PCRE2_SIZE retry_length =
                        static_cast<PCRE2_SIZE>(buf.size());
                    // The sizing pass may leave match_data positioned at a
                    // later global match, so retry from the beginning.
                    rc = pcre2_substitute(
                        code, reinterpret_cast<PCRE2_SPTR8>(value.data), len,
                        0, sub_flags, mdata, NULL, repl_data, repl_len,
                        buf.data(), &retry_length
                    );
                    outlen = retry_length;
                }
                if (rc < 0) {
                    errors[i] = rc;
                    continue;
                }

                if (trim_terminal == 1) {
                    rc = trim_terminal_empty_replacement(
                        code, value.data, len, repl_data, repl_len,
                        sub_flags, scan_data, buf, outlen,
                        terminal_scratch
                    );
                    if (rc < 0) {
                        errors[i] = rc;
                        continue;
                    }
                }

                store_text_output(
                    i, chunk, value,
                    reinterpret_cast<const char*>(buf.data()),
                    static_cast<std::size_t>(outlen), repl_len == 0,
                    results, arenas
                );
            }
        }
        pcre2_match_data_free(mdata);
        pcre2_match_data_free(scan_data);
    }
};

// [[Rcpp::export]]
CharacterVector fast_regex_sub_impl(const std::string& pattern,
                                     const std::string& replacement,
                                     const StringVector& x,
                                     bool ignore_case,
                                     bool global,
                                     int nthreads) {
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
    StringSnapshot snapshot(x);
    const std::size_t estimated_work =
        estimated_string_work(snapshot, 24);
    const std::size_t parallel_threshold = 350000;
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 24
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());
    std::vector<int> errors(static_cast<std::size_t>(n), 0);
    // PCRE2_SUBSTITUTE_EXTENDED enables \1-\9 backreference syntax in
    // replacement strings (same convention as base R's gsub replacement).
    uint32_t sub_flags = (global ? PCRE2_SUBSTITUTE_GLOBAL : 0)
                       | PCRE2_SUBSTITUTE_EXTENDED;
    uint32_t can_match_empty = 0;
    pcre2_pattern_info(
        code, PCRE2_INFO_MATCHEMPTY, &can_match_empty
    );

    PCRE2SubWorker worker(
        snapshot.data(), chunks, code,
        reinterpret_cast<const uint8_t*>(replacement.data()),
        static_cast<PCRE2_SIZE>(replacement.size()),
        sub_flags, can_match_empty != 0, results, arenas, errors
    );
    dispatch_for(
        0, chunks.size(), worker,
        estimated_work, parallel_threshold,
        work_plan.dispatch_threads, work_plan.grain_size
    );

    pcre2_code_free(code);

    for (R_xlen_t i = 0; i < n; ++i) {
        const int err = errors[(std::size_t)i];
        if (err == 0) continue;
        PCRE2_UCHAR8 msg[256];
        pcre2_get_error_message(err, msg, sizeof(msg));
        stop("PCRE2 substitution failed at x[%lld]: %s",
             static_cast<long long>(i + 1), (const char*)msg);
    }

    return finalize_text_results(snapshot, results, arenas);
}

struct FixedSubWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const PreparedFixedSearch& search;
    const std::string& replacement;
    bool global;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    FixedSubWorker(const StringView* strings,
                   const std::vector<TextWorkChunk>& chunks,
                   const PreparedFixedSearch& search,
                   const std::string& replacement, bool global,
                   std::vector<TextResult>& results,
                   TextArenas& arenas)
        : strings(strings), chunks(chunks), search(search),
          replacement(replacement), global(global),
          results(results), arenas(arenas) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string out;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& arena = arenas[chunk];
            arena.reserve(chunks[chunk].estimated_bytes);
            for (std::size_t i = chunks[chunk].begin;
                 i < chunks[chunk].end; ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                const std::size_t first =
                    search.find(value.data, value.size);
                if (first == std::string::npos) continue;

                const std::size_t nl = search.size();
                std::size_t pos = 0;
                std::size_t found = first;
                out.clear();
                out.reserve(value.size + replacement.size());
                while (true) {
                    out.append(value.data + pos, found - pos);
                    out.append(replacement);
                    pos = found + nl;
                    if (!global) break;
                    found = search.find(value.data, value.size, pos);
                    if (found == std::string::npos) break;
                }
                out.append(value.data + pos, value.size - pos);
                store_text_output(
                    i, chunk, value, out.data(), out.size(),
                    replacement.empty(), results, arenas
                );
            }
        }
    }
};

// ---------------------------------------------------------------------------
// Helpers for multi-pattern substitution (used by FixedMultiSubWorker below)
// ---------------------------------------------------------------------------

// Global in-place replace of one fixed needle in s. Returns whether a match
// was replaced, so sparse callers can retain the original CHARSXP.
static bool fixed_replace_str(std::string& s,
                              const PreparedFixedSearch& search,
                              const std::string& repl,
                              std::string& scratch) {
    const std::size_t nl = search.size();
    if (nl == 0) return false;
    std::size_t found = search.find(s.data(), s.size());
    if (found == std::string::npos) return false;

    scratch.clear();
    scratch.reserve(s.size() + repl.size());
    std::size_t pos = 0;
    while (true) {
        scratch.append(s.data() + pos, found - pos);
        scratch.append(repl);
        pos = found + nl;
        found = search.find(s.data(), s.size(), pos);
        if (found == std::string::npos) break;
    }
    scratch.append(s.data() + pos, s.size() - pos);
    s.swap(scratch);
    return true;
}

// Single-scan leftmost-wins replace: at each position find the earliest matching
// needle; first needle in list breaks ties at the same position.
static bool fixed_scan_replace(
        const char* hay, std::size_t len,
        const std::vector<PreparedFixedSearch>& searches,
        const std::vector<std::string>& repls,
        std::string& result) {
    std::size_t pos = 0;
    bool changed = false;
    while (pos < len) {
        std::size_t best = std::string::npos, best_idx = 0;
        for (std::size_t j = 0; j < searches.size(); ++j) {
            std::size_t f = searches[j].find(hay, len, pos);
            if (f != std::string::npos &&
                (best == std::string::npos || f < best)) {
                best = f; best_idx = j;
            }
        }
        if (best == std::string::npos) break;
        changed = true;
        result.append(hay + pos, best - pos);
        result.append(repls[best_idx]);
        pos = best + searches[best_idx].size();
    }
    if (!changed) return false;
    result.append(hay + pos, len - pos);
    return true;
}

// ---------------------------------------------------------------------------
// Multi-pattern fixed substitution worker
// sequential=true:  fold each pattern in order over the running result
// sequential=false: single-scan leftmost-wins across all patterns
// ---------------------------------------------------------------------------

struct FixedMultiSubWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const std::vector<PreparedFixedSearch>& searches;
    const std::vector<std::string>& repls;
    bool sequential;
    bool all_replacements_empty;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    FixedMultiSubWorker(
            const StringView* strings,
            const std::vector<TextWorkChunk>& chunks,
            const std::vector<PreparedFixedSearch>& searches,
            const std::vector<std::string>& repls,
            bool sequential, bool all_replacements_empty,
            std::vector<TextResult>& results,
            TextArenas& arenas)
        : strings(strings), chunks(chunks), searches(searches), repls(repls),
          sequential(sequential),
          all_replacements_empty(all_replacements_empty),
          results(results), arenas(arenas) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string current, scratch;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& arena = arenas[chunk];
            arena.reserve(chunks[chunk].estimated_bytes);
            for (std::size_t i = chunks[chunk].begin;
                 i < chunks[chunk].end; ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                if (sequential) {
                    // Avoid copying the subject unless at least one original
                    // pattern can start the replacement chain.
                    bool could_change = false;
                    for (const PreparedFixedSearch& search : searches) {
                        if (search.find(value.data, value.size) !=
                                std::string::npos) {
                            could_change = true;
                            break;
                        }
                    }
                    if (!could_change) continue;

                    current.assign(value.data, value.size);
                    for (std::size_t j = 0; j < searches.size(); ++j)
                        fixed_replace_str(
                            current, searches[j], repls[j], scratch
                        );
                } else {
                    current.clear();
                    if (!fixed_scan_replace(
                            value.data, value.size, searches, repls,
                            current)) {
                        continue;
                    }
                }
                store_text_output(
                    i, chunk, value, current.data(), current.size(),
                    all_replacements_empty, results, arenas
                );
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
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const std::vector<pcre2_code*>& codes;
    const std::vector<std::string>& repls;
    const std::vector<uint8_t>& can_match_empty;
    uint32_t sub_flags;
    bool all_replacements_empty;
    std::vector<TextResult>& results;
    TextArenas& arenas;
    std::vector<int>& errors;
    std::vector<std::size_t>& error_patterns;

    PCRE2MultiSubWorker(
            const StringView* strings,
            const std::vector<TextWorkChunk>& chunks,
            const std::vector<pcre2_code*>& codes,
            const std::vector<std::string>& repls,
            const std::vector<uint8_t>& can_match_empty,
            uint32_t sub_flags, bool all_replacements_empty,
            std::vector<TextResult>& results,
            TextArenas& arenas,
            std::vector<int>& errors,
            std::vector<std::size_t>& error_patterns)
        : strings(strings), chunks(chunks), codes(codes), repls(repls),
          can_match_empty(can_match_empty), sub_flags(sub_flags),
          all_replacements_empty(all_replacements_empty),
          results(results), arenas(arenas),
          errors(errors), error_patterns(error_patterns) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::size_t np = codes.size();
        std::vector<pcre2_match_data*> mdatas(np, nullptr);
        std::vector<pcre2_match_data*> scan_mdatas(np, nullptr);
        std::size_t allocation_error = np;
        for (std::size_t j = 0; j < np; ++j) {
            mdatas[j] = pcre2_match_data_create_from_pattern(codes[j], NULL);
            if (can_match_empty[j])
                scan_mdatas[j] =
                    pcre2_match_data_create_from_pattern(codes[j], NULL);
            if (!mdatas[j] ||
                (can_match_empty[j] && !scan_mdatas[j])) {
                allocation_error = j;
                break;
            }
        }
        if (allocation_error != np) {
            for (std::size_t chunk = begin; chunk < end; ++chunk) {
                for (std::size_t i = chunks[chunk].begin;
                     i < chunks[chunk].end; ++i) {
                    errors[i] = PCRE2_ERROR_NOMEMORY;
                    error_patterns[i] = allocation_error + 1;
                }
            }
            for (pcre2_match_data* mdata : mdatas)
                pcre2_match_data_free(mdata);
            for (pcre2_match_data* mdata : scan_mdatas)
                pcre2_match_data_free(mdata);
            return;
        }

        // Ping-pong buffers: output of step j feeds into step j+1 without copying.
        // After swap, cur_data still points into the buffer now owned by buf_b.
        std::vector<uint8_t> buf_a, buf_b, terminal_scratch;

        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& arena = arenas[chunk];
            arena.reserve(chunks[chunk].estimated_bytes);
            for (std::size_t i = chunks[chunk].begin;
                 i < chunks[chunk].end; ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;

                const char* cur_data = value.data;
                PCRE2_SIZE cur_len =
                    static_cast<PCRE2_SIZE>(value.size);
                bool any_changed = false;

                for (std::size_t j = 0; j < np; ++j) {
                    const int match_rc = pcre2_match(
                        codes[j],
                        reinterpret_cast<PCRE2_SPTR8>(cur_data),
                        cur_len, 0, 0, mdatas[j], NULL
                    );
                    if (match_rc == PCRE2_ERROR_NOMATCH) continue;
                    if (match_rc < 0) {
                        errors[i] = match_rc;
                        error_patterns[i] = j + 1;
                        break;
                    }

                    int trim_terminal = 0;
                    if (scan_mdatas[j]) {
                        trim_terminal = suppress_terminal_empty_match(
                            codes[j], cur_data, cur_len, scan_mdatas[j]
                        );
                        if (trim_terminal < 0) {
                            errors[i] = trim_terminal;
                            error_patterns[i] = j + 1;
                            break;
                        }
                    }

                    const std::string& rp = repls[j];
                    PCRE2_SIZE outlen = (std::max)(
                        static_cast<PCRE2_SIZE>(64),
                        cur_len * 2 +
                            static_cast<PCRE2_SIZE>(rp.size()) * 16 + 4
                    );
                    if (buf_a.size() <
                            static_cast<std::size_t>(outlen + 1)) {
                        buf_a.resize(static_cast<std::size_t>(outlen + 1));
                    }

                    uint32_t flags =
                        sub_flags | PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
#ifdef PCRE2_SUBSTITUTE_MATCHED
                    flags |= PCRE2_SUBSTITUTE_MATCHED;
#endif
                    int rc = pcre2_substitute(
                        codes[j],
                        reinterpret_cast<PCRE2_SPTR8>(cur_data),
                        cur_len, 0, flags,
                        mdatas[j], NULL,
                        reinterpret_cast<PCRE2_SPTR8>(rp.data()),
                        static_cast<PCRE2_SIZE>(rp.size()),
                        buf_a.data(), &outlen
                    );

                    if (rc == PCRE2_ERROR_NOMEMORY) {
                        buf_a.resize(
                            static_cast<std::size_t>(outlen + 1)
                        );
                        PCRE2_SIZE retry_length =
                            static_cast<PCRE2_SIZE>(buf_a.size());
                        // The sizing pass may advance match data, so retry
                        // from the beginning without MATCHED.
                        rc = pcre2_substitute(
                            codes[j],
                            reinterpret_cast<PCRE2_SPTR8>(cur_data),
                            cur_len, 0, sub_flags,
                            mdatas[j], NULL,
                            reinterpret_cast<PCRE2_SPTR8>(rp.data()),
                            static_cast<PCRE2_SIZE>(rp.size()),
                            buf_a.data(), &retry_length
                        );
                        outlen = retry_length;
                    }
                    if (rc < 0) {
                        errors[i] = rc;
                        error_patterns[i] = j + 1;
                        break;
                    }

                    if (trim_terminal == 1) {
                        rc = trim_terminal_empty_replacement(
                            codes[j], cur_data, cur_len,
                            reinterpret_cast<const uint8_t*>(rp.data()),
                            static_cast<PCRE2_SIZE>(rp.size()),
                            sub_flags, scan_mdatas[j], buf_a, outlen,
                            terminal_scratch
                        );
                        if (rc < 0) {
                            errors[i] = rc;
                            error_patterns[i] = j + 1;
                            break;
                        }
                    }

                    cur_data =
                        reinterpret_cast<const char*>(buf_a.data());
                    cur_len = outlen;
                    std::swap(buf_a, buf_b);
                    // cur_data now points into buf_b (old buf_a); buf_a is
                    // available for the next pattern.
                    any_changed = true;
                }

                if (errors[i] != 0 || !any_changed) continue;
                store_text_output(
                    i, chunk, value, cur_data,
                    static_cast<std::size_t>(cur_len),
                    all_replacements_empty, results, arenas
                );
            }
        }

        for (std::size_t j = 0; j < np; ++j)
            pcre2_match_data_free(mdatas[j]);
        for (std::size_t j = 0; j < np; ++j)
            pcre2_match_data_free(scan_mdatas[j]);
    }
};

// [[Rcpp::export]]
CharacterVector fast_fixed_gsub_all_impl(
        const StringVector& patterns,
        const StringVector& replacements,
        const StringVector& x,
        bool ignore_case, bool sequential,
        int nthreads) {
    const std::size_t np = (std::size_t)patterns.size();

    std::vector<PreparedFixedSearch> searches;
    searches.reserve(np);
    std::vector<std::string> repl_strs(np);
    bool all_replacements_empty = true;
    for (std::size_t j = 0; j < np; ++j) {
        const std::string pattern = Rcpp::as<std::string>(patterns[j]);
        if (pattern.empty()) stop("zero-length pattern");
        searches.emplace_back(pattern, ignore_case);
        repl_strs[j] = Rcpp::as<std::string>(replacements[j]);
        all_replacements_empty =
            all_replacements_empty && repl_strs[j].empty();
    }

    StringSnapshot snapshot(x);
    const std::size_t estimated_work =
        estimated_string_work(snapshot, 16);
    const std::size_t parallel_threshold = 300000;
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 16
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());

    FixedMultiSubWorker worker(
        snapshot.data(), chunks, searches, repl_strs, sequential,
        all_replacements_empty, results, arenas
    );
    dispatch_for(
        0, chunks.size(), worker,
        estimated_work, parallel_threshold,
        work_plan.dispatch_threads, work_plan.grain_size
    );

    return finalize_text_results(snapshot, results, arenas);
}

// [[Rcpp::export]]
CharacterVector fast_regex_gsub_all_impl(
        const StringVector& patterns,
        const StringVector& replacements,
        const StringVector& x,
        bool ignore_case, bool sequential,
        int nthreads) {
    (void)sequential;  // regex path always chains patterns sequentially
    const std::size_t np = (std::size_t)patterns.size();
    uint32_t opts      = ignore_case ? PCRE2_CASELESS : 0;
    uint32_t sub_flags = PCRE2_SUBSTITUTE_GLOBAL | PCRE2_SUBSTITUTE_EXTENDED;

    std::vector<pcre2_code*> codes(np, nullptr);
    std::vector<std::string> repl_strs(np);
    std::vector<uint8_t> can_match_empty(np, 0);
    bool all_replacements_empty = true;

    for (std::size_t j = 0; j < np; ++j) {
        repl_strs[j] = Rcpp::as<std::string>(replacements[j]);
        all_replacements_empty =
            all_replacements_empty && repl_strs[j].empty();
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
        uint32_t match_empty = 0;
        pcre2_pattern_info(
            codes[j], PCRE2_INFO_MATCHEMPTY, &match_empty
        );
        can_match_empty[j] = match_empty != 0;
    }

    const R_xlen_t n = x.size();
    StringSnapshot snapshot(x);
    const std::size_t estimated_work =
        estimated_string_work(snapshot, 32);
    const std::size_t parallel_threshold = 450000;
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 32
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());
    std::vector<int> errors(static_cast<std::size_t>(n), 0);
    std::vector<std::size_t> error_patterns(
        static_cast<std::size_t>(n), 0
    );

    PCRE2MultiSubWorker worker(
        snapshot.data(), chunks, codes, repl_strs, can_match_empty,
        sub_flags, all_replacements_empty, results, arenas, errors,
        error_patterns
    );
    dispatch_for(
        0, chunks.size(), worker,
        estimated_work, parallel_threshold,
        work_plan.dispatch_threads, work_plan.grain_size
    );

    for (std::size_t j = 0; j < np; ++j) pcre2_code_free(codes[j]);

    for (R_xlen_t i = 0; i < n; ++i) {
        const int err = errors[(std::size_t)i];
        if (err == 0) continue;
        PCRE2_UCHAR8 msg[256];
        pcre2_get_error_message(err, msg, sizeof(msg));
        stop("PCRE2 substitution failed for pattern[%zu] at x[%lld]: %s",
             error_patterns[(std::size_t)i],
             static_cast<long long>(i + 1), (const char*)msg);
    }

    return finalize_text_results(snapshot, results, arenas);
}

// [[Rcpp::export]]
CharacterVector fast_fixed_sub_impl(const std::string& pattern,
                                     const std::string& replacement,
                                     const StringVector& x,
                                     bool ignore_case,
                                     bool global,
                                     int nthreads) {
    if (pattern.empty()) stop("zero-length pattern");

    StringSnapshot snapshot(x);
    const std::size_t estimated_work =
        estimated_string_work(snapshot, 12);
    const std::size_t parallel_threshold = 300000;
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 12
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());
    PreparedFixedSearch search(pattern, ignore_case);
    FixedSubWorker worker(
        snapshot.data(), chunks, search, replacement, global,
        results, arenas
    );
    dispatch_for(
        0, chunks.size(), worker,
        estimated_work, parallel_threshold,
        work_plan.dispatch_threads, work_plan.grain_size
    );

    return finalize_text_results(snapshot, results, arenas);
}
