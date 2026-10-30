// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "dm_table.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"
#include "text_results.h"
#include "utf8_text.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <vector>

using namespace Rcpp;
using namespace RcppParallel;

// daitch_mokotoff(): the Daitch-Mokotoff Soundex, a port of Apache Commons
// Codec's DaitchMokotoffSoundex with its coding rules (see dm_table.h and
// tools/gen_dm_table.py). A name can code several ways, so each worker
// writes its fixed-width codes to a per-chunk buffer and records how many
// each row produced; the main thread then builds the R strings.

namespace {

// Where a letter's rules start in DM_RULES, or DM_LETTER_COUNT for a byte
// that is not one of the alphabet's letters.
inline std::size_t dm_letter_index(unsigned char letter) {
    if (letter >= 'a' && letter <= 'z')
        return static_cast<std::size_t>(letter - 'a');
    if (letter >= 1 && letter <= DM_LETTER_COUNT - 26)
        return 25u + letter;
    return DM_LETTER_COUNT;
}

const DmFold* dm_fold_lookup(std::uint32_t point) {
    std::size_t lo = 0, hi = DM_FOLD_COUNT;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        if (DM_FOLDS[mid].codepoint < point) lo = mid + 1;
        else hi = mid;
    }
    return lo < DM_FOLD_COUNT && DM_FOLDS[lo].codepoint == point
        ? &DM_FOLDS[lo] : nullptr;
}

// Lower-cases, folds the accented letters the rules list, and drops
// everything that is not one of the alphabet's letters -- digits,
// punctuation, spaces and other scripts -- as Commons Codec's cleanup()
// does before coding.
void dm_cleanup(const StringView& value, std::string& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    out.clear();
    out.reserve(n);
    std::size_t i = 0;
    while (i < n) {
        if (s[i] < 0x80) {
            const unsigned char c = s[i];
            if (c >= 'A' && c <= 'Z')
                out.push_back(static_cast<char>(c - 'A' + 'a'));
            else if (c >= 'a' && c <= 'z')
                out.push_back(static_cast<char>(c));
            ++i;
            continue;
        }
        std::uint32_t point;
        const std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) {
            ++i;
            continue;
        }
        const DmFold* fold = dm_fold_lookup(point);
        if (fold != nullptr) out.push_back(fold->letter);
        i += width;
    }
}

// One possible code of a name, as far as it has been built.
struct DmBranch {
    char code[DM_MAX_CODE_LENGTH];
    std::uint8_t length = 0;
    const char* last = nullptr;  // the replacement appended last, if any

    bool same_code(const DmBranch& other) const {
        return length == other.length &&
            std::memcmp(code, other.code, length) == 0;
    }

    // Appends one replacement, unless it repeats the last one: two letters
    // that code the same digit are coded once. `force` overrides that, for
    // the "mn"/"nm" rule.
    void append(const char* replacement, bool force) {
        const std::size_t size = std::strlen(replacement);
        const bool repeat = last != nullptr && ends_with(last, replacement);
        if ((!repeat || force) && length < DM_MAX_CODE_LENGTH) {
            for (std::size_t k = 0; k < size && length < DM_MAX_CODE_LENGTH;
                 ++k)
                code[length++] = replacement[k];
        }
        last = replacement;
    }

    static bool ends_with(const char* text, const char* suffix) {
        const std::size_t text_size = std::strlen(text);
        const std::size_t suffix_size = std::strlen(suffix);
        return suffix_size <= text_size &&
            std::memcmp(text + text_size - suffix_size, suffix,
                        suffix_size) == 0;
    }
};

inline bool dm_is_vowel(char letter) {
    return letter == 'a' || letter == 'e' || letter == 'i' ||
        letter == 'o' || letter == 'u';
}

// Picks the replacement for where the rule matched: at the start of the
// name, before a vowel, or anywhere else.
inline const DmReplacement& dm_replacement(const DmRule& rule,
                                           const std::string& input,
                                           std::size_t at, bool at_start) {
    if (at_start) return rule.at_start;
    const std::size_t next = at + rule.length;
    return next < input.size() && dm_is_vowel(input[next])
        ? rule.before_vowel
        : rule.other;
}

// Codes one cleaned-up name into `branches`, which ends up holding one
// entry per code, in the order the reference implementation gives them.
void dm_code(const std::string& input, std::vector<DmBranch>& branches,
             std::vector<DmBranch>& next) {
    branches.clear();
    branches.push_back(DmBranch{});
    char last_letter = 0;
    std::size_t i = 0;
    while (i < input.size()) {
        const char letter = input[i];
        const std::size_t group =
            dm_letter_index(static_cast<unsigned char>(letter));
        if (group == DM_LETTER_COUNT) {
            ++i;
            continue;
        }
        const DmRule* matched = nullptr;
        for (std::uint16_t k = DM_RULE_FIRST[group];
             k < DM_RULE_FIRST[group + 1]; ++k) {
            const DmRule& rule = DM_RULES[k];
            if (input.compare(i, rule.length, rule.pattern, rule.length) == 0) {
                matched = &rule;
                break;
            }
        }
        if (matched == nullptr) {
            last_letter = letter;
            ++i;
            continue;
        }
        const DmReplacement& replacement =
            dm_replacement(*matched, input, i, last_letter == 0);
        // Two letters of "mn" or "nm" are coded separately, not once.
        const bool force = (last_letter == 'm' && letter == 'n') ||
            (last_letter == 'n' && letter == 'm');
        next.clear();
        for (std::size_t b = 0; b < branches.size(); ++b) {
            const char* alternatives[2] = {replacement.first,
                                           replacement.second};
            for (std::size_t a = 0; a < 2; ++a) {
                if (alternatives[a] == nullptr) break;
                DmBranch branch = branches[b];
                branch.append(alternatives[a], force);
                next.push_back(branch);
            }
        }
        // Branches that have reached the same code are one code.
        branches.clear();
        for (std::size_t k = 0; k < next.size(); ++k) {
            bool seen = false;
            for (std::size_t j = 0; j < branches.size() && !seen; ++j)
                seen = branches[j].same_code(next[k]);
            if (!seen) branches.push_back(next[k]);
        }
        last_letter = letter;
        i += matched->length;
    }
    for (std::size_t b = 0; b < branches.size(); ++b) {
        while (branches[b].length < DM_MAX_CODE_LENGTH)
            branches[b].code[branches[b].length++] = '0';
    }
}

struct DmWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    std::vector<std::vector<char>>& codes;  // DM_MAX_CODE_LENGTH bytes each
    std::vector<std::size_t>& first;
    std::vector<std::size_t>& count;

    DmWorker(const StringView* strings_,
             const std::vector<TextWorkChunk>& chunks_,
             std::vector<std::vector<char>>& codes_,
             std::vector<std::size_t>& first_,
             std::vector<std::size_t>& count_)
        : strings(strings_), chunks(chunks_), codes(codes_), first(first_),
          count(count_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string input;
        std::vector<DmBranch> branches, next;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            std::vector<char>& out = codes[chunk];
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                first[i] = out.size() / DM_MAX_CODE_LENGTH;
                count[i] = 0;
                if (value.is_na()) continue;
                dm_cleanup(value, input);
                // A value with no codable letter has no code, as the other
                // phonetic encoders of this package also report.
                if (input.empty()) continue;
                dm_code(input, branches, next);
                for (std::size_t b = 0; b < branches.size(); ++b)
                    out.insert(out.end(), branches[b].code,
                               branches[b].code + DM_MAX_CODE_LENGTH);
                count[i] = branches.size();
            }
        }
    }
};

inline const char* dm_code_at(const std::vector<char>& chunk_codes,
                              std::size_t index) {
    return chunk_codes.data() + index * DM_MAX_CODE_LENGTH;
}

List dm_to_list(const StringSnapshot& snapshot,
                const std::vector<TextWorkChunk>& chunks,
                const std::vector<std::vector<char>>& codes,
                const std::vector<std::size_t>& first,
                const std::vector<std::size_t>& count) {
    const std::size_t n = snapshot.size();
    List output(static_cast<R_xlen_t>(n));
    for (std::size_t chunk = 0; chunk < chunks.size(); ++chunk) {
        const std::vector<char>& chunk_codes = codes[chunk];
        for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end; ++i) {
            if (count[i] == 0) {
                output[static_cast<R_xlen_t>(i)] =
                    CharacterVector::create(NA_STRING);
                continue;
            }
            CharacterVector row(static_cast<R_xlen_t>(count[i]));
            for (std::size_t k = 0; k < count[i]; ++k) {
                SET_STRING_ELT(
                    row, static_cast<R_xlen_t>(k),
                    Rf_mkCharLen(dm_code_at(chunk_codes, first[i] + k),
                                 static_cast<int>(DM_MAX_CODE_LENGTH))
                );
            }
            output[static_cast<R_xlen_t>(i)] = row;
        }
    }
    return output;
}

SEXP dm_to_matrix(const StringSnapshot& snapshot,
                  const std::vector<TextWorkChunk>& chunks,
                  const std::vector<std::vector<char>>& codes,
                  const std::vector<std::size_t>& first,
                  const std::vector<std::size_t>& count) {
    const std::size_t n = snapshot.size();
    std::size_t columns = 0;
    for (std::size_t i = 0; i < n; ++i) columns = (std::max)(columns, count[i]);
    if (n != 0 && columns == 0) columns = 1;
    const std::size_t limit =
        static_cast<std::size_t>((std::numeric_limits<R_xlen_t>::max)());
    if (columns != 0 && n > limit / columns)
        stop("The codes are too many for a matrix.");
    CharacterVector output(static_cast<R_xlen_t>(n * columns));
    for (R_xlen_t k = 0; k < output.size(); ++k)
        SET_STRING_ELT(output, k, NA_STRING);
    for (std::size_t chunk = 0; chunk < chunks.size(); ++chunk) {
        const std::vector<char>& chunk_codes = codes[chunk];
        for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end; ++i) {
            for (std::size_t k = 0; k < count[i]; ++k) {
                SET_STRING_ELT(
                    output, static_cast<R_xlen_t>(i + k * n),
                    Rf_mkCharLen(dm_code_at(chunk_codes, first[i] + k),
                                 static_cast<int>(DM_MAX_CODE_LENGTH))
                );
            }
        }
    }
    output.attr("dim") = Dimension(n, columns);
    return output;
}

std::size_t dm_work(const StringSnapshot& snapshot) {
    const std::uint64_t work = snapshot.total_bytes() +
        24u * static_cast<std::uint64_t>(snapshot.size());
    const std::uint64_t limit =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return static_cast<std::size_t>((std::min)(work, limit));
}

}  // namespace

// [[Rcpp::export]]
SEXP fast_daitch_mokotoff_impl(const StringVector& x, bool simplify,
                               int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    const std::size_t work = dm_work(snapshot);
    const std::size_t threshold = 100000;
    const TextWorkPlan plan =
        make_text_work_plan(snapshot, work, threshold, nthreads, 24);
    std::vector<std::vector<char>> codes(plan.chunks.size());
    std::vector<std::size_t> first(n, 0), count(n, 0);
    DmWorker worker(snapshot.data(), plan.chunks, codes, first, count);
    dispatch_for(0, plan.chunks.size(), worker, work, threshold,
                 plan.dispatch_threads, plan.grain_size);
    return simplify
        ? dm_to_matrix(snapshot, plan.chunks, codes, first, count)
        : Rcpp::wrap(dm_to_list(snapshot, plan.chunks, codes, first, count));
}
