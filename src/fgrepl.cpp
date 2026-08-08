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

static inline void append_pcre2_replacement_literal(std::string& output,
                                                    char value) {
    if (value == '$') {
        output.append("$$");
    } else if (value == '\\') {
        output.append("\\\\");
    } else {
        output.push_back(value);
    }
}

// R replacement strings use \1..\9 backreferences, treat '$' literally,
// recognize \U/\L/\E in Perl mode, drop a trailing backslash, and otherwise
// strip a quoting backslash. PCRE2 uses $1..$9 and gives '$' plus many more
// backslash escapes special meaning. Compile once on the main thread into the
// portable PCRE2 extended-replacement dialect.
static std::string compile_pcre2_replacement(const std::string& replacement) {
    std::string output;
    output.reserve(replacement.size() * 2);
    for (std::size_t i = 0; i < replacement.size(); ++i) {
        const char value = replacement[i];
        if (value == '$') {
            output.append("$$");
            continue;
        }
        if (value != '\\') {
            output.push_back(value);
            continue;
        }
        if (i + 1 == replacement.size())
            break;

        const char escaped = replacement[++i];
        if (escaped >= '1' && escaped <= '9') {
            output.push_back('$');
            output.push_back(escaped);
        } else if (escaped == 'U' || escaped == 'L' || escaped == 'E') {
            output.push_back('\\');
            output.push_back(escaped);
        } else {
            append_pcre2_replacement_literal(output, escaped);
        }
    }
    return output;
}

// Expand exactly the match already stored in match_data and append only the
// replacement bytes. PCRE2_SUBSTITUTE_REPLACEMENT_ONLY avoids copying the
// subject on recent PCRE2 versions; the fallback slices the replacement out
// of a one-match full-subject substitution.
static int append_matched_replacement(
        pcre2_code* code,
        const char* subject,
        PCRE2_SIZE length,
        PCRE2_SIZE search_offset,
        uint32_t match_options,
        PCRE2_SIZE match_start,
        PCRE2_SIZE match_end,
        const uint8_t* replacement,
        PCRE2_SIZE replacement_length,
        uint32_t substitute_flags,
        pcre2_match_data* match_data,
        std::vector<uint8_t>& scratch,
        std::vector<uint8_t>& output) {
    PCRE2_SIZE capacity = (std::max)(
        static_cast<PCRE2_SIZE>(64),
        replacement_length * 16 + 4
    );
#ifndef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    capacity += length;
#endif
    if (scratch.size() < static_cast<std::size_t>(capacity + 1))
        scratch.resize(static_cast<std::size_t>(capacity + 1));

    uint32_t flags = (substitute_flags & ~PCRE2_SUBSTITUTE_GLOBAL) |
        PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
#ifdef PCRE2_SUBSTITUTE_MATCHED
    flags |= PCRE2_SUBSTITUTE_MATCHED;
#else
    // Re-run the same deterministic search when MATCHED is unavailable.
    flags |= match_options;
#endif
#ifdef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    flags |= PCRE2_SUBSTITUTE_REPLACEMENT_ONLY;
#endif
    PCRE2_SIZE expanded_length = capacity;
    int rc = pcre2_substitute(
        code, reinterpret_cast<PCRE2_SPTR8>(subject), length, search_offset,
        flags, match_data, NULL,
        replacement, replacement_length,
        scratch.data(), &expanded_length
    );
    if (rc == PCRE2_ERROR_NOMEMORY) {
        scratch.resize(static_cast<std::size_t>(expanded_length + 1));
        const int rematch_rc = pcre2_match(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
            search_offset, match_options, match_data, NULL
        );
        if (rematch_rc < 0) return rematch_rc;
        PCRE2_SIZE retry_length =
            static_cast<PCRE2_SIZE>(scratch.size());
        rc = pcre2_substitute(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
            search_offset,
            flags & ~PCRE2_SUBSTITUTE_OVERFLOW_LENGTH,
            match_data, NULL,
            replacement, replacement_length,
            scratch.data(), &retry_length
        );
        expanded_length = retry_length;
    }
    if (rc < 0) return rc;

    std::size_t expansion_offset = 0;
#ifndef PCRE2_SUBSTITUTE_REPLACEMENT_ONLY
    const PCRE2_SIZE matched_length = match_end - match_start;
    const PCRE2_SIZE fixed_length = length - matched_length;
    if (expanded_length < fixed_length) return PCRE2_ERROR_INTERNAL;
    expanded_length -= fixed_length;
    expansion_offset = static_cast<std::size_t>(match_start);
    const std::size_t expansion_size =
        static_cast<std::size_t>(expanded_length);
    if (expansion_offset > scratch.size() ||
        expansion_size > scratch.size() - expansion_offset) {
        return PCRE2_ERROR_INTERNAL;
    }
#endif
    output.insert(
        output.end(),
        scratch.data() + expansion_offset,
        scratch.data() + expansion_offset +
            static_cast<std::size_t>(expanded_length)
    );
    return 0;
}

struct SourceRange {
    bool source;
    std::size_t offset;
    std::size_t length;
};

static inline void append_source_range(std::vector<SourceRange>& ranges,
                                       bool source,
                                       std::size_t offset,
                                       std::size_t length) {
    if (length == 0) return;
    if (!ranges.empty() &&
        ranges.back().source == source &&
        (!source ||
         ranges.back().offset + ranges.back().length == offset)) {
        ranges.back().length += length;
    } else {
        ranges.push_back(SourceRange{source, offset, length});
    }
}

struct SourceSliceProvenance {
    bool contiguous = true;
    bool has_bytes = false;
    std::size_t offset = 0;
    std::size_t length = 0;
    // Optional reusable sink used only when a sequence of deletion-only
    // substitutions must compose exact source provenance across steps.
    std::vector<SourceRange>* transform_pieces = nullptr;

    void reset() {
        contiguous = true;
        has_bytes = false;
        offset = 0;
        length = 0;
        if (transform_pieces)
            transform_pieces->clear();
    }

    void append(PCRE2_SIZE begin, PCRE2_SIZE end) {
        if (begin == end) return;
        const std::size_t range_begin = static_cast<std::size_t>(begin);
        const std::size_t range_end = static_cast<std::size_t>(end);
        if (transform_pieces)
            append_source_range(
                *transform_pieces, true,
                range_begin, range_end - range_begin
            );
        if (!contiguous) return;
        if (!has_bytes) {
            has_bytes = true;
            offset = range_begin;
            length = range_end - range_begin;
        } else if (offset + length == range_begin) {
            length += range_end - range_begin;
        } else {
            contiguous = false;
        }
    }

    void constructed(std::size_t bytes) {
        if (bytes == 0) return;
        contiguous = false;
        if (transform_pieces)
            append_source_range(*transform_pieces, false, 0, bytes);
    }
};

// `input_map` describes the current string as a concatenation of slices from
// the original subject or constructed replacement bytes. `transform_pieces`
// describes retained input ranges and newly constructed ranges from one
// substitution. Compose them so later substitutions can remove constructed
// or disjoint intermediate pieces and restore a pure original source slice.
static void compose_source_ranges(
        const std::vector<SourceRange>& input_map,
        const std::vector<SourceRange>& transform_pieces,
        std::vector<SourceRange>& output_map) {
    output_map.clear();
    std::size_t map_index = 0;
    std::size_t map_begin = 0;

    for (const SourceRange& piece : transform_pieces) {
        if (!piece.source) {
            append_source_range(output_map, false, 0, piece.length);
            continue;
        }

        std::size_t position = piece.offset;
        const std::size_t retained_end = piece.offset + piece.length;

        while (map_index < input_map.size() &&
               map_begin + input_map[map_index].length <= position) {
            map_begin += input_map[map_index].length;
            ++map_index;
        }
        while (position < retained_end && map_index < input_map.size()) {
            const SourceRange& mapped = input_map[map_index];
            const std::size_t local_offset = position - map_begin;
            const std::size_t available = mapped.length - local_offset;
            const std::size_t take = (std::min)(
                retained_end - position, available
            );
            append_source_range(
                output_map, mapped.source,
                mapped.source ? mapped.offset + local_offset : 0,
                take
            );
            position += take;
            if (position == map_begin + mapped.length) {
                map_begin += mapped.length;
                ++map_index;
            }
        }
    }
}

static inline void store_provenanced_text(
        std::size_t row,
        std::size_t chunk,
        const StringView& source,
        const char* output,
        std::size_t output_size,
        const SourceSliceProvenance* provenance,
        std::vector<TextResult>& results,
        TextArenas& arenas) {
    if (output_size == source.size &&
        (output_size == 0 ||
         std::memcmp(output, source.data, source.size) == 0)) {
        results[row] = TextResult{};
    } else if (provenance && provenance->contiguous &&
               provenance->length == output_size) {
        results[row] = TextResult{
            TextResultKind::source_slice, 0,
            provenance->offset, provenance->length
        };
    } else {
        store_text_output(
            row, chunk, source, output, output_size,
            results, arenas
        );
    }
}

static inline void store_provenanced_text(
        std::size_t row,
        std::size_t chunk,
        const StringView& source,
        const std::vector<uint8_t>& output,
        const SourceSliceProvenance* provenance,
        std::vector<TextResult>& results,
        TextArenas& arenas) {
    store_provenanced_text(
        row, chunk, source,
        reinterpret_cast<const char*>(output.data()), output.size(),
        provenance, results, arenas
    );
}

// R resumes at the endpoint of a consuming match. If the next ordinary search
// returns an empty match exactly there, R suppresses that result and advances
// one byte; a consuming match at the same endpoint is accepted. After an
// accepted empty match, R also advances one byte. PCRE2_SUBSTITUTE_GLOBAL uses
// different empty-match adjacency rules, so this explicit loop is also used
// when exact source-slice provenance is required.
// match_data must contain the first successful match on entry.
static int base_global_substitute(
        pcre2_code* code,
        const char* subject,
        PCRE2_SIZE length,
        const uint8_t* replacement,
        PCRE2_SIZE replacement_length,
        uint32_t substitute_flags,
        bool can_match_empty,
        pcre2_match_data* match_data,
        std::vector<uint8_t>& replacement_scratch,
        std::vector<uint8_t>& output,
        SourceSliceProvenance* provenance = nullptr) {
    output.clear();
    if (provenance)
        provenance->reset();
    PCRE2_SIZE search_offset = 0;
    PCRE2_SIZE copy_offset = 0;
    uint32_t match_options = 0;

    while (true) {
        PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(match_data);
        const PCRE2_SIZE start = ovector[0];
        const PCRE2_SIZE end = ovector[1];
        if (start < copy_offset || end < start || end > length)
            return PCRE2_ERROR_INTERNAL;

        if (provenance)
            provenance->append(copy_offset, start);
        output.insert(
            output.end(),
            reinterpret_cast<const uint8_t*>(subject + copy_offset),
            reinterpret_cast<const uint8_t*>(subject + start)
        );
        if (replacement_length != 0) {
            const std::size_t before_replacement = output.size();
            const int replacement_rc = append_matched_replacement(
                code, subject, length, search_offset, match_options,
                start, end, replacement, replacement_length,
                substitute_flags, match_data, replacement_scratch, output
            );
            if (replacement_rc < 0) return replacement_rc;
            if (provenance)
                provenance->constructed(output.size() - before_replacement);
        }

        copy_offset = end;
        if (end == length) break;

        const bool accepted_empty = start == end;
        search_offset = accepted_empty ? end + 1 : end;
        match_options = 0;
        int match_rc = pcre2_match(
            code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
            search_offset, match_options, match_data, NULL
        );
        if (match_rc >= 0 && can_match_empty && !accepted_empty) {
            PCRE2_SIZE* next = pcre2_get_ovector_pointer(match_data);
            if (next[0] == end && next[1] == end) {
                search_offset = end + 1;
                match_rc = pcre2_match(
                    code, reinterpret_cast<PCRE2_SPTR8>(subject), length,
                    search_offset, 0, match_data, NULL
                );
            }
        }
        if (match_rc == PCRE2_ERROR_NOMATCH) break;
        if (match_rc < 0) return match_rc;
    }

    if (provenance)
        provenance->append(copy_offset, length);
    output.insert(
        output.end(),
        reinterpret_cast<const uint8_t*>(subject + copy_offset),
        reinterpret_cast<const uint8_t*>(subject + length)
    );
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
    std::vector<int>& errors;

    PCRE2GrepWorker(const StringView* strings, pcre2_code* code,
                    LogicalVector& out, std::vector<int>& errors)
        : strings(strings), code(code), out(out), errors(errors) {}

    void operator()(std::size_t begin, std::size_t end) {
        // Grep only needs the overall match. PCRE2 returns zero (still a
        // successful match) when the ovector is too small for captures.
        pcre2_match_data* mdata = pcre2_match_data_create(1, NULL);
        if (!mdata) {
            for (std::size_t i = begin; i < end; ++i)
                errors[i] = PCRE2_ERROR_NOMEMORY;
            return;
        }
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
                if (rc >= 0) {
                    out[i] = 1;
                } else if (rc == PCRE2_ERROR_NOMATCH) {
                    out[i] = 0;
                } else {
                    errors[i] = rc;
                }
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
    std::vector<int> errors(static_cast<std::size_t>(n), 0);
    PCRE2GrepWorker worker(snapshot.data(), code, result, errors);
    dispatch_for(
        0, static_cast<std::size_t>(n), worker,
        estimated_string_work(snapshot), 250000, nthreads
    );

    pcre2_code_free(code);
    for (std::size_t i = 0; i < errors.size(); ++i) {
        if (errors[i] == 0) continue;
        PCRE2_UCHAR8 msg[256];
        pcre2_get_error_message(errors[i], msg, sizeof(msg));
        stop(
            "PCRE2 matching failed at x[%lld]: %s",
            static_cast<long long>(i + 1),
            reinterpret_cast<const char*>(msg)
        );
    }

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
// Match counting -- continue after every non-overlapping match and return one
// integer per subject. Empty-pattern character semantics are handled in R.
// ---------------------------------------------------------------------------

static int count_pcre2_matches(pcre2_code* code,
                               const StringView& value,
                               bool /* can_match_empty */,
                               pcre2_match_data* match_data,
                               int& error) {
    const PCRE2_SIZE length = static_cast<PCRE2_SIZE>(value.size);
    PCRE2_SIZE search_offset = 0;
    int count = 0;

    while (true) {
        int rc = pcre2_match(
            code, reinterpret_cast<PCRE2_SPTR8>(value.data), length,
            search_offset, 0, match_data, NULL
        );
        if (rc == PCRE2_ERROR_NOMATCH) break;
        if (rc < 0) {
            error = rc;
            return 0;
        }

        PCRE2_SIZE* match = pcre2_get_ovector_pointer(match_data);
        const PCRE2_SIZE start = match[0];
        const PCRE2_SIZE end = match[1];
        const bool empty = start == end;

        ++count;
        if (end == length) break;
        search_offset = empty ? end + 1 : end;
        // gregexpr() advances once after an empty match and does not start a
        // fresh search exactly at the terminal boundary.
        if (search_offset >= length) break;
    }
    return count;
}

struct PCRE2CountWorker : public Worker {
    const StringView* strings;
    pcre2_code* code;
    bool can_match_empty;
    RVector<int> out;
    std::vector<int>& errors;

    PCRE2CountWorker(const StringView* strings_,
                     pcre2_code* code_,
                     bool can_match_empty_,
                     IntegerVector& out_,
                     std::vector<int>& errors_)
        : strings(strings_), code(code_),
          can_match_empty(can_match_empty_), out(out_), errors(errors_) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* match_data = pcre2_match_data_create(1, NULL);
        if (!match_data) {
            for (std::size_t i = begin; i < end; ++i)
                errors[i] = PCRE2_ERROR_NOMEMORY;
            return;
        }
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            out[i] = value.is_na()
                ? NA_INTEGER
                : count_pcre2_matches(
                    code, value, can_match_empty, match_data, errors[i]
                );
        }
        pcre2_match_data_free(match_data);
    }
};

// [[Rcpp::export]]
IntegerVector fast_regex_count_impl(const std::string& pattern,
                                    const StringVector& x,
                                    bool ignore_case,
                                    int nthreads) {
    const uint32_t options = ignore_case ? PCRE2_CASELESS : 0;
    int error_code;
    PCRE2_SIZE error_offset;
    pcre2_code* code = pcre2_compile(
        reinterpret_cast<PCRE2_SPTR8>(pattern.data()), pattern.size(),
        options, &error_code, &error_offset, NULL
    );
    if (!code) {
        PCRE2_UCHAR8 message[256];
        pcre2_get_error_message(error_code, message, sizeof(message));
        stop("Invalid PCRE2 pattern: %s", reinterpret_cast<const char*>(message));
    }
    pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);
    uint32_t can_match_empty = 0;
    pcre2_pattern_info(code, PCRE2_INFO_MATCHEMPTY, &can_match_empty);

    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    IntegerVector result(static_cast<R_xlen_t>(n));
    std::vector<int> errors(n, 0);
    PCRE2CountWorker worker(
        snapshot.data(), code, can_match_empty != 0, result, errors
    );
    dispatch_for(
        0, n, worker, estimated_string_work(snapshot), 250000, nthreads
    );
    pcre2_code_free(code);

    for (std::size_t i = 0; i < errors.size(); ++i) {
        if (errors[i] == 0) continue;
        PCRE2_UCHAR8 message[256];
        pcre2_get_error_message(errors[i], message, sizeof(message));
        stop(
            "PCRE2 matching failed at x[%lld]: %s",
            static_cast<long long>(i + 1),
            reinterpret_cast<const char*>(message)
        );
    }
    return result;
}

struct FixedCountWorker : public Worker {
    const StringView* strings;
    const PreparedFixedSearch& search;
    RVector<int> out;

    FixedCountWorker(const StringView* strings_,
                     const PreparedFixedSearch& search_,
                     IntegerVector& out_)
        : strings(strings_), search(search_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const std::size_t pattern_size = search.size();
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = NA_INTEGER;
                continue;
            }
            int count = 0;
            std::size_t offset = 0;
            while (offset <= value.size) {
                const std::size_t found = search.find(
                    value.data, value.size, offset
                );
                if (found == std::string::npos) break;
                ++count;
                offset = found + pattern_size;
            }
            out[i] = count;
        }
    }
};

// [[Rcpp::export]]
IntegerVector fast_fixed_count_impl(const std::string& pattern,
                                    const StringVector& x,
                                    bool ignore_case,
                                    int nthreads) {
    if (pattern.empty()) stop("zero-length pattern");
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    const PreparedFixedSearch search(pattern, ignore_case);
    IntegerVector result(static_cast<R_xlen_t>(n));
    FixedCountWorker worker(snapshot.data(), search, result);
    dispatch_for(
        0, n, worker, estimated_string_work(snapshot), 250000, nthreads
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
        if (!mdata) {
            for (std::size_t chunk = begin; chunk < end; ++chunk) {
                for (std::size_t i = chunks[chunk].begin;
                     i < chunks[chunk].end; ++i) {
                    errors[i] = PCRE2_ERROR_NOMEMORY;
                }
            }
            pcre2_match_data_free(mdata);
            return;
        }
        std::vector<uint8_t> buf, replacement_scratch;
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

                if (!global && repl_len == 0) {
                    PCRE2_SIZE* match = pcre2_get_ovector_pointer(mdata);
                    SourceSliceProvenance provenance;
                    provenance.append(0, match[0]);
                    provenance.append(match[1], len);
                    buf.clear();
                    buf.insert(
                        buf.end(),
                        reinterpret_cast<const uint8_t*>(value.data),
                        reinterpret_cast<const uint8_t*>(
                            value.data + match[0]
                        )
                    );
                    buf.insert(
                        buf.end(),
                        reinterpret_cast<const uint8_t*>(
                            value.data + match[1]
                        ),
                        reinterpret_cast<const uint8_t*>(value.data + len)
                    );
                    store_provenanced_text(
                        i, chunk, value, buf, &provenance,
                        results, arenas
                    );
                    continue;
                }

                if (global && (can_match_empty || repl_len == 0)) {
                    SourceSliceProvenance provenance;
                    const int rc = base_global_substitute(
                        code, value.data, len, repl_data, repl_len,
                        sub_flags, can_match_empty, mdata,
                        replacement_scratch, buf,
                        repl_len == 0 ? &provenance : nullptr
                    );
                    if (rc < 0) {
                        errors[i] = rc;
                        continue;
                    }
                    store_provenanced_text(
                        i, chunk, value, buf,
                        repl_len == 0 ? &provenance : nullptr,
                        results, arenas
                    );
                    continue;
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

                store_text_output(
                    i, chunk, value,
                    reinterpret_cast<const char*>(buf.data()),
                    static_cast<std::size_t>(outlen),
                    results, arenas
                );
            }
        }
        pcre2_match_data_free(mdata);
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
    const std::string compiled_replacement =
        compile_pcre2_replacement(replacement);
    const TextWorkPlan work_plan = make_text_work_plan(
        snapshot, estimated_work, parallel_threshold, nthreads, 24
    );
    const std::vector<TextWorkChunk>& chunks = work_plan.chunks;
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(chunks.size());
    std::vector<int> errors(static_cast<std::size_t>(n), 0);
    // The replacement was compiled from R syntax on the main thread. Extended
    // mode supplies portable case conversion for \U, \L, and \E.
    uint32_t sub_flags = (global ? PCRE2_SUBSTITUTE_GLOBAL : 0)
                       | PCRE2_SUBSTITUTE_EXTENDED
                       | PCRE2_SUBSTITUTE_UNSET_EMPTY;
    uint32_t can_match_empty = 0;
    pcre2_pattern_info(
        code, PCRE2_INFO_MATCHEMPTY, &can_match_empty
    );

    PCRE2SubWorker worker(
        snapshot.data(), chunks, code,
        reinterpret_cast<const uint8_t*>(compiled_replacement.data()),
        static_cast<PCRE2_SIZE>(compiled_replacement.size()),
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
                SourceSliceProvenance provenance;
                out.clear();
                out.reserve(value.size + replacement.size());
                while (true) {
                    provenance.append(pos, found);
                    out.append(value.data + pos, found - pos);
                    provenance.constructed(replacement.size());
                    out.append(replacement);
                    pos = found + nl;
                    if (!global) break;
                    found = search.find(value.data, value.size, pos);
                    if (found == std::string::npos) break;
                }
                provenance.append(pos, value.size);
                out.append(value.data + pos, value.size - pos);
                store_provenanced_text(
                    i, chunk, value, out.data(), out.size(),
                    replacement.empty() ? &provenance : nullptr,
                    results, arenas
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
                              std::string& scratch,
                              SourceSliceProvenance* provenance = nullptr) {
    if (provenance)
        provenance->reset();
    const std::size_t nl = search.size();
    if (nl == 0) return false;
    std::size_t found = search.find(s.data(), s.size());
    if (found == std::string::npos) return false;

    scratch.clear();
    scratch.reserve(s.size() + repl.size());
    std::size_t pos = 0;
    while (true) {
        if (provenance) {
            provenance->append(pos, found);
            provenance->constructed(repl.size());
        }
        scratch.append(s.data() + pos, found - pos);
        scratch.append(repl);
        pos = found + nl;
        found = search.find(s.data(), s.size(), pos);
        if (found == std::string::npos) break;
    }
    if (provenance)
        provenance->append(pos, s.size());
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
        std::string& result,
        SourceSliceProvenance* provenance = nullptr) {
    if (provenance)
        provenance->reset();
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
        if (provenance) {
            provenance->append(pos, best);
            provenance->constructed(repls[best_idx].size());
        }
        result.append(hay + pos, best - pos);
        result.append(repls[best_idx]);
        pos = best + searches[best_idx].size();
    }
    if (!changed) return false;
    if (provenance)
        provenance->append(pos, len);
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
    std::vector<TextResult>& results;
    TextArenas& arenas;

    FixedMultiSubWorker(
            const StringView* strings,
            const std::vector<TextWorkChunk>& chunks,
            const std::vector<PreparedFixedSearch>& searches,
            const std::vector<std::string>& repls,
            bool sequential,
            std::vector<TextResult>& results,
            TextArenas& arenas)
        : strings(strings), chunks(chunks), searches(searches), repls(repls),
          sequential(sequential),
          results(results), arenas(arenas) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string current, scratch;
        std::vector<SourceRange> source_map, step_ranges, next_source_map;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& arena = arenas[chunk];
            arena.reserve(chunks[chunk].estimated_bytes);
            for (std::size_t i = chunks[chunk].begin;
                 i < chunks[chunk].end; ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                SourceSliceProvenance provenance;
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
                    source_map.clear();
                    append_source_range(
                        source_map, true, 0, value.size
                    );
                    for (std::size_t j = 0; j < searches.size(); ++j) {
                        SourceSliceProvenance step_provenance;
                        step_provenance.transform_pieces = &step_ranges;
                        const bool changed = fixed_replace_str(
                            current, searches[j], repls[j], scratch,
                            &step_provenance
                        );
                        if (changed) {
                            compose_source_ranges(
                                source_map, step_ranges, next_source_map
                            );
                            source_map.swap(next_source_map);
                        }
                    }
                    provenance.contiguous =
                        current.empty() ||
                        (source_map.size() == 1 &&
                         source_map.front().source &&
                         source_map.front().length == current.size());
                    provenance.has_bytes = !current.empty();
                    provenance.offset =
                        current.empty() || source_map.empty()
                        ? 0
                        : source_map.front().offset;
                    provenance.length = current.size();
                } else {
                    current.clear();
                    if (!fixed_scan_replace(
                            value.data, value.size, searches, repls,
                            current, &provenance)) {
                        continue;
                    }
                }
                store_provenanced_text(
                    i, chunk, value, current.data(), current.size(),
                    &provenance,
                    results, arenas
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
            uint32_t sub_flags,
            std::vector<TextResult>& results,
            TextArenas& arenas,
            std::vector<int>& errors,
            std::vector<std::size_t>& error_patterns)
        : strings(strings), chunks(chunks), codes(codes), repls(repls),
          can_match_empty(can_match_empty), sub_flags(sub_flags),
          results(results), arenas(arenas),
          errors(errors), error_patterns(error_patterns) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::size_t np = codes.size();
        std::vector<pcre2_match_data*> mdatas(np, nullptr);
        std::size_t allocation_error = np;
        for (std::size_t j = 0; j < np; ++j) {
            mdatas[j] = pcre2_match_data_create_from_pattern(codes[j], NULL);
            if (!mdatas[j]) {
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
            return;
        }

        // Ping-pong buffers: output of step j feeds into step j+1 without copying.
        // After swap, cur_data still points into the buffer now owned by buf_b.
        std::vector<uint8_t> buf_a, buf_b, replacement_scratch;
        std::vector<SourceRange> source_map, step_ranges, next_source_map;

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
                source_map.clear();
                append_source_range(source_map, true, 0, value.size);

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

                    const std::string& rp = repls[j];
                    PCRE2_SIZE outlen = 0;
                    int rc;
                    SourceSliceProvenance step_provenance;
                    step_provenance.transform_pieces = &step_ranges;
                    rc = base_global_substitute(
                        codes[j], cur_data, cur_len,
                        reinterpret_cast<const uint8_t*>(rp.data()),
                        static_cast<PCRE2_SIZE>(rp.size()),
                        sub_flags, can_match_empty[j] != 0,
                        mdatas[j], replacement_scratch, buf_a,
                        &step_provenance
                    );
                    outlen = static_cast<PCRE2_SIZE>(buf_a.size());
                    if (rc < 0) {
                        errors[i] = rc;
                        error_patterns[i] = j + 1;
                        break;
                    }

                    compose_source_ranges(
                        source_map, step_ranges, next_source_map
                    );
                    source_map.swap(next_source_map);

                    cur_data = outlen == 0
                        ? ""
                        : reinterpret_cast<const char*>(buf_a.data());
                    cur_len = outlen;
                    std::swap(buf_a, buf_b);
                    // cur_data now points into buf_b (old buf_a); buf_a is
                    // available for the next pattern.
                    any_changed = true;
                }

                if (errors[i] != 0 || !any_changed) continue;
                if (static_cast<std::size_t>(cur_len) == value.size &&
                    (cur_len == 0 ||
                     std::memcmp(cur_data, value.data, value.size) == 0)) {
                    results[i] = TextResult{};
                } else if (cur_len == 0 ||
                           (source_map.size() == 1 &&
                            source_map.front().source &&
                            source_map.front().length ==
                                static_cast<std::size_t>(cur_len))) {
                    results[i] = TextResult{
                        TextResultKind::source_slice, 0,
                        cur_len == 0 ? 0 : source_map.front().offset,
                        static_cast<std::size_t>(cur_len)
                    };
                } else {
                    store_text_output(
                        i, chunk, value, cur_data,
                        static_cast<std::size_t>(cur_len),
                        results, arenas
                    );
                }
            }
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
        bool ignore_case, bool sequential,
        int nthreads) {
    const std::size_t np = (std::size_t)patterns.size();

    std::vector<PreparedFixedSearch> searches;
    searches.reserve(np);
    std::vector<std::string> repl_strs(np);
    for (std::size_t j = 0; j < np; ++j) {
        const std::string pattern = Rcpp::as<std::string>(patterns[j]);
        if (pattern.empty()) stop("zero-length pattern");
        searches.emplace_back(pattern, ignore_case);
        repl_strs[j] = Rcpp::as<std::string>(replacements[j]);
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
        results, arenas
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
    uint32_t sub_flags = PCRE2_SUBSTITUTE_GLOBAL |
        PCRE2_SUBSTITUTE_EXTENDED | PCRE2_SUBSTITUTE_UNSET_EMPTY;

    std::vector<pcre2_code*> codes(np, nullptr);
    std::vector<std::string> repl_strs(np);
    std::vector<uint8_t> can_match_empty(np, 0);

    for (std::size_t j = 0; j < np; ++j) {
        repl_strs[j] = compile_pcre2_replacement(
            Rcpp::as<std::string>(replacements[j])
        );
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
        sub_flags, results, arenas, errors,
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
