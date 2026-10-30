// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "fixed_search.h"
#include "parallel_dispatch.h"
#include "regex_engine.h"
#include "string_snapshot.h"
#include "text_results.h"
#include "utf8_text.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// fextract(), fextract_all(), fstrsplit(), fsplit_field() and fsquish(). All
// but the last return pieces of their input, so workers record byte offsets only and the
// main thread creates the R strings; fsquish() writes changed strings to
// per-chunk arenas like fgsub().

namespace {

// A match or field of row `i`, as a slice of its snapshot view.
inline void store_slice(std::size_t i, const StringView& value,
                        std::size_t offset, std::size_t length,
                        std::vector<TextResult>& results) {
    if (offset == 0 && length == value.size)
        results[i] = TextResult{};
    else
        results[i] = TextResult{TextResultKind::source_slice, 0, offset, length};
}

inline void store_missing(std::size_t i, std::vector<TextResult>& results) {
    results[i] = TextResult{TextResultKind::na, 0, 0, 0};
}

// The regex pieces every worker needs, prepared on the main thread.
struct RegexPlan {
    pcre2_code* code = nullptr;
    bool utf = false;
    bool extended = false;

    ~RegexPlan() {
        if (code) pcre2_code_free(code);
    }
};

inline void prepare_regex(RegexPlan& plan, const std::string& pattern,
                          bool ignore_case, int syntax, bool use_bytes,
                          const StringSnapshot& snapshot) {
    plan.utf = !use_bytes && !snapshot.any_bytes() &&
        (!is_ascii_string(pattern) || snapshot.any_non_ascii());
    plan.extended = syntax == SYNTAX_EXTENDED;
    plan.code = compile_regex_or_stop(pattern, ignore_case, syntax, plan.utf);
}

inline std::uint32_t capture_count(pcre2_code* code) {
    std::uint32_t count = 0;
    pcre2_pattern_info(code, PCRE2_INFO_CAPTURECOUNT, &count);
    return count;
}

// ---------------------------------------------------------------------------
// fextract(): the first match (or one capture group of it).
// ---------------------------------------------------------------------------

struct RegexExtractWorker : public Worker {
    const StringView* strings;
    pcre2_code* code;
    std::size_t group;
    std::vector<TextResult>& results;
    std::vector<int>& errors;

    RegexExtractWorker(const StringView* strings_, pcre2_code* code_,
                       std::size_t group_, std::vector<TextResult>& results_,
                       std::vector<int>& errors_)
        : strings(strings_), code(code_), group(group_), results(results_),
          errors(errors_) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* match_data =
            pcre2_match_data_create_from_pattern(code, NULL);
        if (!match_data) {
            for (std::size_t i = begin; i < end; ++i)
                errors[i] = PCRE2_ERROR_NOMEMORY;
            return;
        }
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) continue;
            const int rc = pcre2_match(
                code, reinterpret_cast<PCRE2_SPTR8>(value.data),
                static_cast<PCRE2_SIZE>(value.size), 0, 0, match_data, NULL
            );
            if (rc < 0) {
                if (rc != PCRE2_ERROR_NOMATCH) errors[i] = rc;
                store_missing(i, results);
                continue;
            }
            const PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(match_data);
            const PCRE2_SIZE start = ovector[2 * group];
            const PCRE2_SIZE stop = ovector[2 * group + 1];
            if (start == PCRE2_UNSET || stop == PCRE2_UNSET) {
                store_missing(i, results);
                continue;
            }
            store_slice(i, value, start, stop - start, results);
        }
        pcre2_match_data_free(match_data);
    }
};

struct FixedExtractWorker : public Worker {
    const StringView* strings;
    const PreparedFixedSearch& search;
    std::vector<TextResult>& results;

    FixedExtractWorker(const StringView* strings_,
                       const PreparedFixedSearch& search_,
                       std::vector<TextResult>& results_)
        : strings(strings_), search(search_), results(results_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) continue;
            const std::size_t found = search.find(value.data, value.size);
            if (found == std::string::npos)
                store_missing(i, results);
            else
                store_slice(i, value, found, search.size(), results);
        }
    }
};

// ---------------------------------------------------------------------------
// fextract_all(): every non-overlapping match. Spans go to one array per
// work chunk; row i owns spans [first[i], first[i] + count[i]) of its chunk.
// ---------------------------------------------------------------------------

struct Span {
    std::size_t offset;
    std::size_t length;
};

struct RegexExtractAllWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    pcre2_code* code;
    std::size_t group;
    bool utf;
    bool extended;
    std::vector<std::vector<Span>>& spans;
    std::vector<std::size_t>& first;
    std::vector<std::size_t>& count;
    std::vector<int>& errors;

    RegexExtractAllWorker(const StringView* strings_,
                          const std::vector<TextWorkChunk>& chunks_,
                          pcre2_code* code_, std::size_t group_, bool utf_,
                          bool extended_,
                          std::vector<std::vector<Span>>& spans_,
                          std::vector<std::size_t>& first_,
                          std::vector<std::size_t>& count_,
                          std::vector<int>& errors_)
        : strings(strings_), chunks(chunks_), code(code_), group(group_),
          utf(utf_), extended(extended_), spans(spans_), first(first_),
          count(count_), errors(errors_) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* match_data =
            pcre2_match_data_create_from_pattern(code, NULL);
        if (!match_data) {
            for (std::size_t chunk = begin; chunk < end; ++chunk)
                for (std::size_t i = chunks[chunk].begin;
                     i < chunks[chunk].end; ++i)
                    errors[i] = PCRE2_ERROR_NOMEMORY;
            return;
        }
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<Span>& out = spans[chunk];
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                first[i] = out.size();
                count[i] = 0;
                // TRE's gregexpr() finds nothing in "", as in fcount().
                if (value.is_na() || (extended && value.size == 0)) continue;
                const PCRE2_SIZE length = static_cast<PCRE2_SIZE>(value.size);
                PCRE2_SIZE offset = 0;
                while (true) {
                    const int rc = pcre2_match(
                        code, reinterpret_cast<PCRE2_SPTR8>(value.data),
                        length, offset, 0, match_data, NULL
                    );
                    if (rc == PCRE2_ERROR_NOMATCH) break;
                    if (rc < 0) {
                        errors[i] = rc;
                        out.resize(first[i]);
                        count[i] = 0;
                        break;
                    }
                    const PCRE2_SIZE* ovector =
                        pcre2_get_ovector_pointer(match_data);
                    const PCRE2_SIZE start = ovector[2 * group];
                    const PCRE2_SIZE stop = ovector[2 * group + 1];
                    // An unset group still counts as a match, as NA.
                    if (start == PCRE2_UNSET || stop == PCRE2_UNSET)
                        out.push_back(Span{std::string::npos, 0});
                    else
                        out.push_back(Span{start, stop - start});
                    ++count[i];
                    const PCRE2_SIZE match_end = ovector[1];
                    if (match_end == length) break;
                    offset = ovector[0] == match_end
                        ? step_past_empty(value.data, length, match_end, utf)
                        : match_end;
                    if (offset >= length) break;
                }
            }
        }
        pcre2_match_data_free(match_data);
    }
};

struct FixedExtractAllWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const PreparedFixedSearch& search;
    std::vector<std::vector<Span>>& spans;
    std::vector<std::size_t>& first;
    std::vector<std::size_t>& count;

    FixedExtractAllWorker(const StringView* strings_,
                          const std::vector<TextWorkChunk>& chunks_,
                          const PreparedFixedSearch& search_,
                          std::vector<std::vector<Span>>& spans_,
                          std::vector<std::size_t>& first_,
                          std::vector<std::size_t>& count_)
        : strings(strings_), chunks(chunks_), search(search_), spans(spans_),
          first(first_), count(count_) {}

    void operator()(std::size_t begin, std::size_t end) {
        const std::size_t width = search.size();
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<Span>& out = spans[chunk];
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                first[i] = out.size();
                count[i] = 0;
                if (value.is_na()) continue;
                std::size_t offset = 0;
                while (true) {
                    const std::size_t found =
                        search.find(value.data, value.size, offset);
                    if (found == std::string::npos) break;
                    out.push_back(Span{found, width});
                    ++count[i];
                    offset = found + width;
                }
            }
        }
    }
};

// Builds fextract_all()'s list on the main thread.
List spans_to_list(const StringSnapshot& snapshot,
                   const std::vector<TextWorkChunk>& chunks,
                   const std::vector<std::vector<Span>>& spans,
                   const std::vector<std::size_t>& first,
                   const std::vector<std::size_t>& count) {
    const std::size_t n = snapshot.size();
    List output(static_cast<R_xlen_t>(n));
    for (std::size_t chunk = 0; chunk < chunks.size(); ++chunk) {
        const std::vector<Span>& chunk_spans = spans[chunk];
        for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end; ++i) {
            const StringView& value = snapshot[i];
            if (value.is_na()) {
                output[static_cast<R_xlen_t>(i)] =
                    CharacterVector::create(NA_STRING);
                continue;
            }
            // As finalize_text_results() marks a slice of its source.
            const cetype_t encoding = snapshot.translated(i)
                ? CE_UTF8
                : Rf_getCharCE(snapshot.charsxp(i));
            CharacterVector matches(static_cast<R_xlen_t>(count[i]));
            for (std::size_t k = 0; k < count[i]; ++k) {
                const Span& span = chunk_spans[first[i] + k];
                if (span.offset == std::string::npos) {
                    SET_STRING_ELT(matches, static_cast<R_xlen_t>(k), NA_STRING);
                } else {
                    SET_STRING_ELT(
                        matches, static_cast<R_xlen_t>(k),
                        make_text_charsxp(value.data + span.offset,
                                          span.length, encoding)
                    );
                }
            }
            output[static_cast<R_xlen_t>(i)] = matches;
        }
    }
    return output;
}

// ---------------------------------------------------------------------------
// fsplit_field(): one field of each string split at a separator. Fields are
// numbered from 1, or from -1 for the last; empty fields count.
// ---------------------------------------------------------------------------
// fstrsplit(): every field of each string, cut as base::strsplit() cuts them.
// A trailing empty field is dropped, a leading one is kept, and an empty
// `split` cuts between characters.
// ---------------------------------------------------------------------------

struct SplitAllWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const PreparedFixedSearch* search;  // fixed separator, or
    pcre2_code* code;                   // a regex that cannot match "", or
    bool characters;                    // split = "": one piece per character
    bool utf8;
    std::vector<std::vector<Span>>& spans;
    std::vector<std::size_t>& first;
    std::vector<std::size_t>& count;
    std::vector<int>& errors;

    SplitAllWorker(const StringView* strings_,
                   const std::vector<TextWorkChunk>& chunks_,
                   const PreparedFixedSearch* search_, pcre2_code* code_,
                   bool characters_, bool utf8_,
                   std::vector<std::vector<Span>>& spans_,
                   std::vector<std::size_t>& first_,
                   std::vector<std::size_t>& count_,
                   std::vector<int>& errors_)
        : strings(strings_), chunks(chunks_), search(search_), code(code_),
          characters(characters_), utf8(utf8_), spans(spans_), first(first_),
          count(count_), errors(errors_) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* match_data = nullptr;
        if (code) {
            match_data = pcre2_match_data_create_from_pattern(code, NULL);
            if (!match_data) {
                for (std::size_t chunk = begin; chunk < end; ++chunk) {
                    for (std::size_t i = chunks[chunk].begin;
                         i < chunks[chunk].end; ++i)
                        errors[i] = PCRE2_ERROR_NOMEMORY;
                }
                return;
            }
        }
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<Span>& out = spans[chunk];
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                first[i] = out.size();
                count[i] = 0;
                if (value.is_na()) continue;
                if (characters) {
                    split_characters(value, out);
                } else if (!split_separators(value, match_data, out,
                                             errors[i])) {
                    out.resize(first[i]);
                    continue;
                }
                count[i] = out.size() - first[i];
            }
        }
        if (match_data) pcre2_match_data_free(match_data);
    }

    void split_characters(const StringView& value,
                          std::vector<Span>& out) const {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        std::size_t k = 0;
        while (k < value.size) {
            std::size_t width = 1;
            if (utf8 && s[k] >= 0x80) {
                std::uint32_t point;
                const std::size_t decoded =
                    utf8_decode(s, value.size, k, point);
                if (decoded != 0) width = decoded;
            }
            out.push_back(Span{k, width});
            k += width;
        }
    }

    // Records one span per field. Returns false on a PCRE2 error.
    bool split_separators(const StringView& value,
                          pcre2_match_data* match_data,
                          std::vector<Span>& out, int& error) const {
        std::size_t start = 0;
        std::size_t offset = 0;
        while (offset <= value.size) {
            std::size_t found = 0;
            std::size_t separator = 0;
            if (search) {
                found = search->find(value.data, value.size, offset);
                if (found == std::string::npos) break;
                separator = search->size();
            } else {
                const int rc = pcre2_match(
                    code, reinterpret_cast<PCRE2_SPTR8>(value.data),
                    static_cast<PCRE2_SIZE>(value.size),
                    static_cast<PCRE2_SIZE>(offset), 0, match_data, NULL
                );
                if (rc == PCRE2_ERROR_NOMATCH) break;
                if (rc < 0) {
                    error = rc;
                    return false;
                }
                const PCRE2_SIZE* ovector =
                    pcre2_get_ovector_pointer(match_data);
                found = ovector[0];
                separator = ovector[1] - ovector[0];
            }
            out.push_back(Span{start, found - start});
            start = found + separator;
            offset = start;
        }
        // The field after the last separator, unless it is empty: base R
        // drops a trailing empty field but keeps a leading one.
        if (start < value.size)
            out.push_back(Span{start, value.size - start});
        return true;
    }
};

// One row per string and one column per field, short rows padded with NA.
SEXP spans_to_matrix(const StringSnapshot& snapshot,
                     const std::vector<TextWorkChunk>& chunks,
                     const std::vector<std::vector<Span>>& spans,
                     const std::vector<std::size_t>& first,
                     const std::vector<std::size_t>& count) {
    const std::size_t n = snapshot.size();
    std::size_t columns = 0;
    for (std::size_t i = 0; i < n; ++i) columns = (std::max)(columns, count[i]);
    const std::size_t limit =
        static_cast<std::size_t>((std::numeric_limits<R_xlen_t>::max)());
    if (columns != 0 && n > limit / columns)
        stop("The split result is too large for a matrix.");
    CharacterVector output(static_cast<R_xlen_t>(n * columns));
    for (R_xlen_t k = 0; k < output.size(); ++k)
        SET_STRING_ELT(output, k, NA_STRING);
    for (std::size_t chunk = 0; chunk < chunks.size(); ++chunk) {
        const std::vector<Span>& chunk_spans = spans[chunk];
        for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end; ++i) {
            const StringView& value = snapshot[i];
            if (value.is_na()) continue;
            const cetype_t encoding = snapshot.translated(i)
                ? CE_UTF8
                : Rf_getCharCE(snapshot.charsxp(i));
            for (std::size_t k = 0; k < count[i]; ++k) {
                const Span& span = chunk_spans[first[i] + k];
                SET_STRING_ELT(
                    output, static_cast<R_xlen_t>(i + k * n),
                    make_text_charsxp(value.data + span.offset, span.length,
                                      encoding)
                );
            }
        }
    }
    output.attr("dim") = Dimension(n, columns);
    return output;
}

// ---------------------------------------------------------------------------

struct SplitFieldWorker : public Worker {
    const StringView* strings;
    const PreparedFixedSearch* search;  // fixed separator, or
    pcre2_code* code;                   // a regex that cannot match ""
    long long field;
    std::vector<TextResult>& results;
    std::vector<int>& errors;

    SplitFieldWorker(const StringView* strings_,
                     const PreparedFixedSearch* search_, pcre2_code* code_,
                     long long field_, std::vector<TextResult>& results_,
                     std::vector<int>& errors_)
        : strings(strings_), search(search_), code(code_), field(field_),
          results(results_), errors(errors_) {}

    void operator()(std::size_t begin, std::size_t end) {
        pcre2_match_data* match_data = nullptr;
        if (code) {
            match_data = pcre2_match_data_create_from_pattern(code, NULL);
            if (!match_data) {
                for (std::size_t i = begin; i < end; ++i)
                    errors[i] = PCRE2_ERROR_NOMEMORY;
                return;
            }
        }
        std::vector<Span> separators;
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) continue;
            separators.clear();
            // Positive fields stop scanning once the field is complete.
            const std::size_t needed = field > 0
                ? static_cast<std::size_t>(field)
                : (std::numeric_limits<std::size_t>::max)();
            std::size_t offset = 0;
            int error = 0;
            while (separators.size() < needed) {
                if (search) {
                    const std::size_t found =
                        search->find(value.data, value.size, offset);
                    if (found == std::string::npos) break;
                    separators.push_back(Span{found, search->size()});
                    offset = found + search->size();
                } else {
                    const int rc = pcre2_match(
                        code, reinterpret_cast<PCRE2_SPTR8>(value.data),
                        static_cast<PCRE2_SIZE>(value.size),
                        static_cast<PCRE2_SIZE>(offset), 0, match_data, NULL
                    );
                    if (rc == PCRE2_ERROR_NOMATCH) break;
                    if (rc < 0) {
                        error = rc;
                        break;
                    }
                    const PCRE2_SIZE* ovector =
                        pcre2_get_ovector_pointer(match_data);
                    separators.push_back(
                        Span{ovector[0], ovector[1] - ovector[0]}
                    );
                    offset = ovector[1];
                }
                if (offset > value.size) break;
            }
            if (error != 0) {
                errors[i] = error;
                store_missing(i, results);
                continue;
            }
            const std::size_t pieces = separators.size() + 1;
            std::size_t index;  // 0-based field
            if (field > 0) {
                if (static_cast<unsigned long long>(field) > pieces) {
                    store_missing(i, results);
                    continue;
                }
                index = static_cast<std::size_t>(field - 1);
            } else {
                const unsigned long long back =
                    static_cast<unsigned long long>(-field);
                if (back > pieces) {
                    store_missing(i, results);
                    continue;
                }
                index = pieces - static_cast<std::size_t>(back);
            }
            const std::size_t start = index == 0
                ? 0
                : separators[index - 1].offset + separators[index - 1].length;
            const std::size_t stop = index < separators.size()
                ? separators[index].offset
                : value.size;
            store_slice(i, value, start, stop - start, results);
        }
        if (match_data) pcre2_match_data_free(match_data);
    }
};

// ---------------------------------------------------------------------------
// fsquish(): trim and collapse runs of whitespace to one space. Whitespace
// is Unicode's White_Space property; in byte mode only its ASCII members.
// ---------------------------------------------------------------------------

struct SquishWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    bool utf8;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    SquishWorker(const StringView* strings_,
                 const std::vector<TextWorkChunk>& chunks_, bool utf8_,
                 std::vector<TextResult>& results_, TextArenas& arenas_)
        : strings(strings_), chunks(chunks_), utf8(utf8_), results(results_),
          arenas(arenas_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string buffer;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                const unsigned char* data =
                    reinterpret_cast<const unsigned char*>(value.data);
                const std::size_t size = value.size;
                // Leading and trailing whitespace.
                std::size_t start = 0;
                while (start < size) {
                    const std::size_t w = whitespace_width(data, start, size, utf8);
                    if (w == 0) break;
                    start += w;
                }
                std::size_t stop = start;
                bool inner = false;  // whitespace other than single spaces
                std::size_t last_end = start;  // end of the last non-space
                std::size_t k = start;
                while (k < size) {
                    const std::size_t w = whitespace_width(data, k, size, utf8);
                    if (w == 0) {
                        ++k;
                        last_end = k;
                        continue;
                    }
                    std::size_t run = k;
                    while (run < size) {
                        const std::size_t rw =
                            whitespace_width(data, run, size, utf8);
                        if (rw == 0) break;
                        run += rw;
                    }
                    if (run < size && (run - k != 1 || data[k] != ' '))
                        inner = true;
                    k = run;
                }
                stop = last_end;
                if (!inner) {
                    store_slice(i, value, start, stop - start, results);
                    continue;
                }
                buffer.clear();
                k = start;
                while (k < stop) {
                    const std::size_t w = whitespace_width(data, k, stop, utf8);
                    if (w == 0) {
                        buffer.push_back(static_cast<char>(data[k]));
                        ++k;
                        continue;
                    }
                    while (k < stop) {
                        const std::size_t rw =
                            whitespace_width(data, k, stop, utf8);
                        if (rw == 0) break;
                        k += rw;
                    }
                    buffer.push_back(' ');
                }
                store_changed_text(i, chunk, buffer.data(), buffer.size(),
                                   results, arenas);
            }
        }
    }
};

}  // namespace

// [[Rcpp::export]]
CharacterVector fast_extract_impl(const std::string& pattern,
                                  const StringVector& x, bool ignore_case,
                                  int syntax, bool fixed, bool use_bytes,
                                  int group, int nthreads) {
    StringSnapshot snapshot(x, snapshot_text(use_bytes));
    const bool byte_mode = use_bytes || snapshot.any_bytes();
    const std::size_t n = snapshot.size();
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas;
    if (fixed) {
        if (pattern.empty()) stop("zero-length pattern");
        if (group != 0) stop("`group` must be 0 when `fixed = TRUE`.");
        const PreparedFixedSearch search(pattern, ignore_case);
        FixedExtractWorker worker(snapshot.data(), search, results);
        dispatch_for(0, n, worker, estimated_string_work(snapshot), 250000,
                     nthreads);
        return finalize_text_results(snapshot, results, arenas, byte_mode);
    }
    RegexPlan plan;
    prepare_regex(plan, pattern, ignore_case, syntax, use_bytes, snapshot);
    if (group < 0 || static_cast<std::uint32_t>(group) > capture_count(plan.code))
        stop("`group` %d does not exist in the pattern.", group);
    std::vector<int> errors(n, 0);
    RegexExtractWorker worker(snapshot.data(), plan.code,
                              static_cast<std::size_t>(group), results, errors);
    dispatch_for(0, n, worker, estimated_string_work(snapshot), 250000,
                 nthreads);
    report_pcre2_errors(errors, "matching");
    return finalize_text_results(snapshot, results, arenas, byte_mode);
}

// [[Rcpp::export]]
List fast_extract_all_impl(const std::string& pattern, const StringVector& x,
                           bool ignore_case, int syntax, bool fixed,
                           bool use_bytes, int group, int nthreads) {
    StringSnapshot snapshot(x, snapshot_text(use_bytes));
    const std::size_t n = snapshot.size();
    const std::size_t work = estimated_string_work(snapshot, 16);
    const std::size_t threshold = 250000;
    const TextWorkPlan plan_chunks =
        make_text_work_plan(snapshot, work, threshold, nthreads, 16);
    const std::vector<TextWorkChunk>& chunks = plan_chunks.chunks;
    std::vector<std::vector<Span>> spans(chunks.size());
    std::vector<std::size_t> first(n, 0), count(n, 0);
    if (fixed) {
        if (pattern.empty()) stop("zero-length pattern");
        if (group != 0) stop("`group` must be 0 when `fixed = TRUE`.");
        const PreparedFixedSearch search(pattern, ignore_case);
        FixedExtractAllWorker worker(snapshot.data(), chunks, search, spans,
                                     first, count);
        dispatch_for(0, chunks.size(), worker, work, threshold,
                     plan_chunks.dispatch_threads, plan_chunks.grain_size);
        return spans_to_list(snapshot, chunks, spans, first, count);
    }
    RegexPlan plan;
    prepare_regex(plan, pattern, ignore_case, syntax, use_bytes, snapshot);
    if (group < 0 || static_cast<std::uint32_t>(group) > capture_count(plan.code))
        stop("`group` %d does not exist in the pattern.", group);
    std::vector<int> errors(n, 0);
    RegexExtractAllWorker worker(snapshot.data(), chunks, plan.code,
                                 static_cast<std::size_t>(group), plan.utf,
                                 plan.extended, spans, first, count, errors);
    dispatch_for(0, chunks.size(), worker, work, threshold,
                 plan_chunks.dispatch_threads, plan_chunks.grain_size);
    report_pcre2_errors(errors, "matching");
    return spans_to_list(snapshot, chunks, spans, first, count);
}

// [[Rcpp::export]]
CharacterVector fast_split_field_impl(const std::string& split,
                                      const StringVector& x, double field,
                                      bool ignore_case, int syntax,
                                      bool fixed, bool use_bytes,
                                      int nthreads) {
    StringSnapshot snapshot(x, snapshot_text(use_bytes));
    const bool byte_mode = use_bytes || snapshot.any_bytes();
    const std::size_t n = snapshot.size();
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas;
    std::vector<int> errors(n, 0);
    const long long index = static_cast<long long>(field);
    if (fixed) {
        if (split.empty()) stop("`split` must not be empty.");
        const PreparedFixedSearch search(split, ignore_case);
        SplitFieldWorker worker(snapshot.data(), &search, nullptr, index,
                                results, errors);
        dispatch_for(0, n, worker, estimated_string_work(snapshot), 250000,
                     nthreads);
        return finalize_text_results(snapshot, results, arenas, byte_mode);
    }
    RegexPlan plan;
    prepare_regex(plan, split, ignore_case, syntax, use_bytes, snapshot);
    std::uint32_t match_empty = 0;
    pcre2_pattern_info(plan.code, PCRE2_INFO_MATCHEMPTY, &match_empty);
    if (match_empty) stop("`split` must not match the empty string.");
    SplitFieldWorker worker(snapshot.data(), nullptr, plan.code, index,
                            results, errors);
    dispatch_for(0, n, worker, estimated_string_work(snapshot), 250000,
                 nthreads);
    report_pcre2_errors(errors, "matching");
    return finalize_text_results(snapshot, results, arenas, byte_mode);
}

// [[Rcpp::export]]
CharacterVector fast_squish_impl(const StringVector& x, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool byte_mode = snapshot.any_bytes();
    const std::size_t work = estimated_string_work(snapshot, 16);
    const std::size_t threshold = 300000;
    const TextWorkPlan plan =
        make_text_work_plan(snapshot, work, threshold, nthreads, 16);
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(plan.chunks.size());
    SquishWorker worker(snapshot.data(), plan.chunks, !byte_mode, results,
                        arenas);
    dispatch_for(0, plan.chunks.size(), worker, work, threshold,
                 plan.dispatch_threads, plan.grain_size);
    return finalize_text_results(snapshot, results, arenas, byte_mode);
}

// [[Rcpp::export]]
SEXP fast_strsplit_impl(const std::string& split, const StringVector& x,
                        bool ignore_case, int syntax, bool fixed,
                        bool use_bytes, bool simplify, int nthreads) {
    StringSnapshot snapshot(x, snapshot_text(use_bytes));
    const std::size_t n = snapshot.size();
    const std::size_t work = estimated_string_work(snapshot, 16);
    const std::size_t threshold = 250000;
    const TextWorkPlan plan_chunks =
        make_text_work_plan(snapshot, work, threshold, nthreads, 16);
    const std::vector<TextWorkChunk>& chunks = plan_chunks.chunks;
    std::vector<std::vector<Span>> spans(chunks.size());
    std::vector<std::size_t> first(n, 0), count(n, 0);
    std::vector<int> errors(n, 0);
    const bool characters = split.empty();
    const bool utf8 = !use_bytes && !snapshot.any_bytes();

    RegexPlan plan;
    PreparedFixedSearch search(characters ? std::string(" ") : split,
                               ignore_case);
    const PreparedFixedSearch* search_ptr = nullptr;
    pcre2_code* code = nullptr;
    if (!characters) {
        if (fixed) {
            search_ptr = &search;
        } else {
            prepare_regex(plan, split, ignore_case, syntax, use_bytes,
                          snapshot);
            std::uint32_t match_empty = 0;
            pcre2_pattern_info(plan.code, PCRE2_INFO_MATCHEMPTY, &match_empty);
            if (match_empty) stop("`split` must not match the empty string.");
            code = plan.code;
        }
    }
    SplitAllWorker worker(snapshot.data(), chunks, search_ptr, code,
                          characters, utf8, spans, first, count, errors);
    dispatch_for(0, chunks.size(), worker, work, threshold,
                 plan_chunks.dispatch_threads, plan_chunks.grain_size);
    if (code) report_pcre2_errors(errors, "matching");
    return simplify
        ? spans_to_matrix(snapshot, chunks, spans, first, count)
        : Rcpp::wrap(spans_to_list(snapshot, chunks, spans, first, count));
}
