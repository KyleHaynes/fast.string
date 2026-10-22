#ifndef FAST_STRING_REGEX_ENGINE_H
#define FAST_STRING_REGEX_ENGINE_H

// PCRE2 pattern compilation, error reporting and work estimates shared by
// every regex entry point (fgrepl.cpp, fextract.cpp). All functions are
// static, so each translation unit gets its own copy.

#include <Rcpp.h>

#ifndef PCRE2_CODE_UNIT_WIDTH
#define PCRE2_CODE_UNIT_WIDTH 8
#endif
#include <pcre2.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

#include "string_snapshot.h"

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

// ---------------------------------------------------------------------------
// Pattern compilation shared by every regex entry point.
//
// `syntax` says which base R engine to reproduce:
//   SYNTAX_PERL      perl = TRUE: PCRE as base R configures it.
//   SYNTAX_EXTENDED  perl = FALSE: base R's default TRE engine, emulated on
//                    PCRE2 -- `.` also matches a newline, `$` only matches at
//                    the very end, a backslash inside a bracket expression is
//                    a literal, \< and \> are word boundaries, and in UTF-8
//                    mode \w, \b and [[:alpha:]]-style classes are Unicode
//                    aware. TRE's leftmost-longest choice between
//                    alternatives is not reproduced: PCRE2 takes the first
//                    alternative that matches.
//
// UTF mode is used, as in base R, when the pattern or any subject is
// non-ASCII and useBytes is off; subjects are UTF-8 by then (see
// SnapshotText::utf8). Invalid UTF-8 never matches instead of being an error
// where PCRE2 supports that (10.34+).
// ---------------------------------------------------------------------------

enum RegexSyntax { SYNTAX_PERL = 0, SYNTAX_EXTENDED = 1 };

// Rewrites a TRE (POSIX extended) pattern into equivalent PCRE2 syntax.
// `ucp`: PCRE2 runs with Unicode properties, where two POSIX classes need
// widening to match TRE's wide-character classes: [:punct:] also covers
// symbols such as the euro sign, and under `caseless` [:upper:]/[:lower:]
// match any cased letter.
static std::string tre_to_pcre2(const std::string& pattern, bool ucp,
                                bool caseless) {
    std::string out;
    out.reserve(pattern.size() + 16);
    const std::size_t n = pattern.size();
    std::size_t i = 0;
    while (i < n) {
        const char c = pattern[i];
        if (c == '\\') {
            if (i + 1 == n) {
                out.push_back(c);
                ++i;
            } else if (pattern[i + 1] == '<') {
                out.append("\\b(?=\\w)");
                i += 2;
            } else if (pattern[i + 1] == '>') {
                out.append("\\b(?<=\\w)");
                i += 2;
            } else {
                out.append(pattern, i, 2);
                i += 2;
            }
            continue;
        }
        if (c != '[') {
            out.push_back(c);
            ++i;
            continue;
        }

        // Bracket expression: copied as is, except that its backslashes are
        // literal in POSIX and so are escaped for PCRE2. A leading `]` (after
        // an optional `^`) is a member, and [:class:], [.x.] and [=x=] are
        // copied whole.
        std::size_t j = i + 1;
        std::string bracket("[");
        if (j < n && pattern[j] == '^') bracket.push_back(pattern[j++]);
        if (j < n && pattern[j] == ']') {
            bracket.append("\\]");
            ++j;
        }
        bool closed = false;
        while (j < n) {
            const char d = pattern[j];
            if (d == ']') {
                bracket.push_back(d);
                ++j;
                closed = true;
                break;
            }
            if (d == '[' && j + 1 < n &&
                (pattern[j + 1] == ':' || pattern[j + 1] == '.' ||
                 pattern[j + 1] == '=')) {
                const char delimiter[3] = {pattern[j + 1], ']', '\0'};
                const std::size_t end = pattern.find(delimiter, j + 2);
                if (end != std::string::npos) {
                    const std::string name =
                        pattern.substr(j + 2, end - j - 2);
                    if (ucp && pattern[j + 1] == ':' && name == "punct") {
                        bracket.append("[:punct:]\\p{S}");
                    } else if (ucp && caseless && pattern[j + 1] == ':' &&
                               (name == "upper" || name == "lower")) {
                        bracket.append("\\p{L&}");
                    } else {
                        bracket.append(pattern, j, end + 2 - j);
                    }
                    j = end + 2;
                    continue;
                }
            }
            if (d == '\\') bracket.append("\\\\");
            else bracket.push_back(d);
            ++j;
        }
        if (!closed) {
            // Unterminated: pass the rest through and let PCRE2 report it.
            out.append(pattern, i, std::string::npos);
            break;
        }
        out.append(bracket);
        i = j;
    }
    return out;
}

static bool is_ascii_string(const std::string& value) {
    return bytes_are_ascii(value.data(), value.size());
}

static pcre2_code* compile_regex(const std::string& pattern,
                                 bool ignore_case,
                                 int syntax,
                                 bool utf,
                                 std::string& error) {
    uint32_t options = ignore_case ? PCRE2_CASELESS : 0;
    if (utf) {
        options |= PCRE2_UTF;
#ifdef PCRE2_MATCH_INVALID_UTF
        options |= PCRE2_MATCH_INVALID_UTF;
#endif
    }
    const std::string* source = &pattern;
    std::string rewritten;
    if (syntax == SYNTAX_EXTENDED) {
        options |= PCRE2_DOTALL | PCRE2_DOLLAR_ENDONLY;
        if (utf) options |= PCRE2_UCP;
        rewritten = tre_to_pcre2(pattern, utf, ignore_case);
        source = &rewritten;
    }
    int error_code;
    PCRE2_SIZE error_offset;
    pcre2_code* code = pcre2_compile(
        reinterpret_cast<PCRE2_SPTR8>(source->data()), source->size(),
        options, &error_code, &error_offset, NULL
    );
    if (!code) {
        PCRE2_UCHAR8 message[256];
        pcre2_get_error_message(error_code, message, sizeof(message));
        error = reinterpret_cast<const char*>(message);
        return nullptr;
    }
    // JIT is best-effort: matching falls back to the interpreter without it.
    pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);
    return code;
}

static pcre2_code* compile_regex_or_stop(const std::string& pattern,
                                         bool ignore_case,
                                         int syntax,
                                         bool utf) {
    std::string error;
    pcre2_code* code = compile_regex(pattern, ignore_case, syntax, utf, error);
    if (!code) Rcpp::stop("Invalid PCRE2 pattern: %s", error.c_str());
    return code;
}

// Offset of the character after the one starting at `offset`, used to step
// past an empty match: one byte, or one UTF-8 character in UTF mode.
static inline PCRE2_SIZE step_past_empty(const char* subject,
                                         PCRE2_SIZE length,
                                         PCRE2_SIZE offset,
                                         bool utf) {
    ++offset;
    if (utf) {
        while (offset < length &&
               (static_cast<unsigned char>(subject[offset]) & 0xC0) == 0x80)
            ++offset;
    }
    return offset;
}

// Matching errors that base R reports as a warning naming the element, which
// then counts as not matching, instead of failing the whole call.
static inline bool soft_pcre2_error(int rc) {
    return rc == PCRE2_ERROR_MATCHLIMIT || rc == PCRE2_ERROR_DEPTHLIMIT ||
        rc == PCRE2_ERROR_HEAPLIMIT || rc == PCRE2_ERROR_JIT_STACKLIMIT ||
        (rc <= PCRE2_ERROR_UTF8_ERR1 && rc >= PCRE2_ERROR_UTF8_ERR21);
}

// Stops at the first hard error; warns once, naming the first element, when
// only soft errors occurred. Those elements keep their unmatched result.
static void report_pcre2_errors(const std::vector<int>& errors,
                                const char* action,
                                const std::vector<std::size_t>* patterns
                                    = nullptr) {
    std::size_t soft = 0, first_soft = 0;
    for (std::size_t i = 0; i < errors.size(); ++i) {
        const int rc = errors[i];
        if (rc == 0) continue;
        PCRE2_UCHAR8 message[256];
        pcre2_get_error_message(rc, message, sizeof(message));
        if (!soft_pcre2_error(rc)) {
            if (patterns) {
                Rcpp::stop("PCRE2 %s failed for pattern[%zu] at x[%lld]: %s",
                     action, (*patterns)[i], static_cast<long long>(i + 1),
                     reinterpret_cast<const char*>(message));
            }
            Rcpp::stop("PCRE2 %s failed at x[%lld]: %s", action,
                 static_cast<long long>(i + 1),
                 reinterpret_cast<const char*>(message));
        }
        if (soft++ == 0) first_soft = i;
    }
    if (soft == 0) return;
    PCRE2_UCHAR8 message[256];
    pcre2_get_error_message(errors[first_soft], message, sizeof(message));
    if (soft == 1) {
        Rcpp::warning("PCRE2 error '%s' for element %lld; it is treated as not matching.",
                      reinterpret_cast<const char*>(message),
                      static_cast<long long>(first_soft + 1));
    } else {
        Rcpp::warning("PCRE2 error '%s' for element %lld and %zu others; they are treated as not matching.",
                      reinterpret_cast<const char*>(message),
                      static_cast<long long>(first_soft + 1), soft - 1);
    }
}

static inline SnapshotText snapshot_text(bool use_bytes) {
    return use_bytes ? SnapshotText::raw : SnapshotText::utf8;
}

#endif // FAST_STRING_REGEX_ENGINE_H
