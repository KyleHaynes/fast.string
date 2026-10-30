// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "case_table.h"
#include "html_entities.h"
#include "parallel_dispatch.h"
#include "shape_table.h"
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

// freplace_tokens(), fpad(), string_shape(), url_encode(), url_decode(),
// html_unescape(), html_escape(), fupper(), flower() and ftitle_case().
// As in fold_text.cpp, each string is rewritten into a per-worker buffer and
// strings that come out unchanged keep their CHARSXP.

namespace {

std::size_t text_work(const StringSnapshot& snapshot) {
    const std::uint64_t work =
        snapshot.total_bytes() + 16u * static_cast<std::uint64_t>(snapshot.size());
    const std::uint64_t limit =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return static_cast<std::size_t>((std::min)(work, limit));
}

const std::size_t TEXT_PARALLEL_THRESHOLD = 200000;

// A token, or any other byte range of a string.
struct Span {
    std::size_t start;
    std::size_t size;
};

// Runs `body(value, out, spans)` on every non-NA string; `body` returns
// false to leave the string unchanged without writing `out`. `spans` is
// per-worker scratch space, so bodies need not allocate per string.
template <typename Body>
struct TextWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    std::vector<TextResult>& results;
    TextArenas& arenas;
    const Body& body;

    TextWorker(const StringView* strings_,
               const std::vector<TextWorkChunk>& chunks_,
               std::vector<TextResult>& results_, TextArenas& arenas_,
               const Body& body_)
        : strings(strings_), chunks(chunks_), results(results_),
          arenas(arenas_), body(body_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string out;
        std::vector<Span> spans;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                out.clear();
                if (!body(value, out, spans)) continue;
                store_text_output(i, chunk, value, out.data(), out.size(),
                                  results, arenas);
            }
        }
    }
};

template <typename Body>
CharacterVector run_text(const StringSnapshot& snapshot, int nthreads,
                         const Body& body) {
    const std::size_t work = text_work(snapshot);
    const TextWorkPlan plan = make_text_work_plan(
        snapshot, work, TEXT_PARALLEL_THRESHOLD, nthreads, 16);
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(plan.chunks.size());
    TextWorker<Body> worker(snapshot.data(), plan.chunks, results, arenas,
                            body);
    dispatch_for(0, plan.chunks.size(), worker, work, TEXT_PARALLEL_THRESHOLD,
                 plan.dispatch_threads, plan.grain_size);
    return finalize_text_results(snapshot, results, arenas);
}

inline unsigned char ascii_lower(unsigned char c) {
    return c >= 'A' && c <= 'Z' ? static_cast<unsigned char>(c + 32) : c;
}

inline int hex_value(unsigned char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

// ---------------------------------------------------------------------------
// freplace_tokens(): whole-token dictionary replacement. Tokens are runs of
// non-whitespace; the output joins the kept tokens with single spaces.
// ---------------------------------------------------------------------------

class TokenTable {
public:
    TokenTable(const StringSnapshot& from, const StringSnapshot& to,
               bool ignore_case, bool utf8)
        : ignore_case_(ignore_case) {
        const std::size_t n = from.size();
        std::size_t capacity = 16;
        while (capacity < 2 * n + 2) capacity <<= 1;
        slots_.assign(capacity, -1);
        mask_ = capacity - 1;
        keys_.reserve(n);
        values_.reserve(n);
        for (std::size_t i = 0; i < n; ++i) {
            const StringView& key = from[i];
            const unsigned char* data =
                reinterpret_cast<const unsigned char*>(key.data);
            if (key.size == 0)
                stop("`from` must not contain empty strings.");
            for (std::size_t k = 0; k < key.size; ++k) {
                if (whitespace_width(data, k, key.size, utf8) != 0)
                    stop("`from` entries must not contain whitespace; "
                         "each one is a single token.");
            }
            std::string folded(key.data, key.size);
            if (ignore_case_) {
                for (char& c : folded)
                    c = static_cast<char>(
                        ascii_lower(static_cast<unsigned char>(c)));
            }
            std::size_t slot = hash(folded.data(), folded.size()) & mask_;
            bool duplicate = false;
            while (slots_[slot] >= 0) {
                const std::string& other = keys_[slots_[slot]];
                if (other == folded) {
                    duplicate = true;  // the first entry wins, as match()
                    break;
                }
                slot = (slot + 1) & mask_;
            }
            if (duplicate) continue;
            slots_[slot] = static_cast<std::int32_t>(keys_.size());
            keys_.push_back(folded);
            values_.emplace_back(to[i].data, to[i].size);
        }
    }

    // The replacement for token data[0, size), or nullptr.
    const std::string* find(const char* data, std::size_t size) const {
        std::size_t slot = hash(data, size) & mask_;
        while (slots_[slot] >= 0) {
            const std::string& key = keys_[slots_[slot]];
            if (key.size() == size && equal(key.data(), data, size))
                return &values_[slots_[slot]];
            slot = (slot + 1) & mask_;
        }
        return nullptr;
    }

private:
    std::size_t hash(const char* data, std::size_t size) const {
        std::uint64_t h = 1469598103934665603ULL;
        for (std::size_t i = 0; i < size; ++i) {
            unsigned char c = static_cast<unsigned char>(data[i]);
            if (ignore_case_) c = ascii_lower(c);
            h = (h ^ c) * 1099511628211ULL;
        }
        h ^= h >> 29;
        return static_cast<std::size_t>(h);
    }

    // `key` is already folded.
    bool equal(const char* key, const char* data, std::size_t size) const {
        if (!ignore_case_) return std::memcmp(key, data, size) == 0;
        for (std::size_t i = 0; i < size; ++i) {
            if (key[i] != static_cast<char>(
                              ascii_lower(static_cast<unsigned char>(data[i]))))
                return false;
        }
        return true;
    }

    bool ignore_case_;
    std::vector<std::string> keys_;
    std::vector<std::string> values_;
    std::vector<std::int32_t> slots_;
    std::size_t mask_ = 0;
};

// ---------------------------------------------------------------------------
// html_unescape(): Python's html.unescape(), which matches
//   &(#[0-9]+;?|#[xX][0-9a-fA-F]+;?|[^\t\n\f <&#;]{1,32};?)
// and replaces each match as html._replace_charref() does.
// ---------------------------------------------------------------------------

int compare_entity(const HtmlEntity& entity, const char* name,
                   std::size_t size) {
    const int c = std::memcmp(HTML_ENTITY_NAMES + entity.offset, name,
                              (std::min)(static_cast<std::size_t>(entity.length),
                                         size));
    if (c != 0) return c;
    if (entity.length == size) return 0;
    return entity.length < size ? -1 : 1;
}

const HtmlEntity* find_entity(const char* name, std::size_t size) {
    std::size_t lo = 0, hi = HTML_ENTITY_COUNT;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        const int c = compare_entity(HTML_ENTITIES[mid], name, size);
        if (c == 0) return &HTML_ENTITIES[mid];
        if (c < 0) lo = mid + 1;
        else hi = mid;
    }
    return nullptr;
}

void append_numeric_charref(std::string& out, std::uint32_t number) {
    for (std::size_t k = 0; k < HTML_INVALID_CHARREF_COUNT; ++k) {
        if (HTML_INVALID_CHARREFS[k].number == number) {
            utf8_append(out, HTML_INVALID_CHARREFS[k].point);
            return;
        }
    }
    if ((number >= 0xD800 && number <= 0xDFFF) || number > 0x10FFFF) {
        utf8_append(out, 0xFFFD);
        return;
    }
    if (std::binary_search(HTML_INVALID_CODEPOINTS,
                           HTML_INVALID_CODEPOINTS + HTML_INVALID_CODEPOINT_COUNT,
                           number))
        return;
    utf8_append(out, number);
}

inline bool html_name_char(unsigned char c) {
    return c != '\t' && c != '\n' && c != '\f' && c != ' ' && c != '<' &&
        c != '&' && c != '#' && c != ';';
}

void html_unescape_string(const StringView& value, std::string& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    const std::uint32_t too_big = 0x110000;
    out.reserve(n);
    std::size_t i = 0;
    while (i < n) {
        const char* amp = static_cast<const char*>(
            std::memchr(s + i, '&', n - i));
        if (!amp) {
            out.append(reinterpret_cast<const char*>(s + i), n - i);
            break;
        }
        const std::size_t at = static_cast<std::size_t>(
            reinterpret_cast<const unsigned char*>(amp) - s);
        out.append(reinterpret_cast<const char*>(s + i), at - i);
        std::size_t j = at + 1;
        if (j < n && s[j] == '#') {
            std::size_t k = j + 1;
            std::uint32_t number = 0;
            std::size_t end = k;
            if (k + 1 < n && (s[k] == 'x' || s[k] == 'X') &&
                hex_value(s[k + 1]) >= 0) {
                end = k + 1;
                while (end < n && hex_value(s[end]) >= 0) {
                    if (number < too_big)
                        number = number * 16 +
                            static_cast<std::uint32_t>(hex_value(s[end]));
                    ++end;
                }
            } else if (k < n && s[k] >= '0' && s[k] <= '9') {
                while (end < n && s[end] >= '0' && s[end] <= '9') {
                    if (number < too_big)
                        number = number * 10 +
                            static_cast<std::uint32_t>(s[end] - '0');
                    ++end;
                }
            } else {
                // "&#" not followed by a number: no match at this '&'.
                out.push_back('&');
                i = j;
                continue;
            }
            if (number > too_big) number = too_big;
            if (end < n && s[end] == ';') ++end;
            append_numeric_charref(out, number);
            i = end;
            continue;
        }
        // Named reference: up to 32 characters (code points) of the class.
        std::size_t end = j;
        std::size_t chars = 0;
        std::size_t ascii_prefix = 0;  // leading ASCII characters
        bool all_ascii = true;
        while (end < n && chars < 32 && html_name_char(s[end])) {
            std::size_t width = 1;
            if (s[end] >= 0x80) {
                std::uint32_t point;
                width = utf8_decode(s, n, end, point);
                if (width == 0) width = 1;
                all_ascii = false;
            } else if (all_ascii) {
                ++ascii_prefix;
            }
            end += width;
            ++chars;
        }
        if (chars == 0) {
            out.push_back('&');
            i = j;
            continue;
        }
        const bool semicolon = end < n && s[end] == ';';
        const std::size_t match_end = end + (semicolon ? 1 : 0);
        const char* name = reinterpret_cast<const char*>(s + j);
        const HtmlEntity* entity =
            all_ascii ? find_entity(name, match_end - j) : nullptr;
        if (entity) {
            utf8_append(out, entity->first);
            if (entity->second) utf8_append(out, entity->second);
            i = match_end;
            continue;
        }
        // The longest prefix s[:x], 2 <= x < len(s), that is a name; names
        // are ASCII, so only prefixes inside the leading ASCII run qualify.
        const std::size_t total = chars + (semicolon ? 1 : 0);
        std::size_t x = (std::min)(total - 1, ascii_prefix);
        const HtmlEntity* prefix = nullptr;
        for (; x >= 2; --x) {
            prefix = find_entity(name, x);
            if (prefix) break;
        }
        if (prefix) {
            utf8_append(out, prefix->first);
            if (prefix->second) utf8_append(out, prefix->second);
            out.append(name + x, match_end - j - x);
        } else {
            out.push_back('&');
            out.append(name, match_end - j);
        }
        i = match_end;
    }
}

// ---------------------------------------------------------------------------
// string_shape()
// ---------------------------------------------------------------------------

std::uint8_t shape_class(std::uint32_t point) {
    std::size_t lo = 0, hi = SHAPE_RUN_COUNT;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        if (SHAPE_RUNS[mid].last < point) lo = mid + 1;
        else hi = mid;
    }
    return lo < SHAPE_RUN_COUNT && SHAPE_RUNS[lo].first <= point
        ? SHAPE_RUNS[lo].kind : 0;
}

// ---------------------------------------------------------------------------
// Case conversion (case_table.h): fupper(), flower() and ftitle_case().
// ---------------------------------------------------------------------------

// Which case each mapping in a CaseEntry is.
enum class CaseForm { upper, lower, title };

const CaseEntry* case_lookup(std::uint32_t point) {
    std::size_t lo = 0, hi = CASE_ENTRY_COUNT;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        if (CASE_ENTRIES[mid].codepoint < point) lo = mid + 1;
        else hi = mid;
    }
    return lo < CASE_ENTRY_COUNT && CASE_ENTRIES[lo].codepoint == point
        ? &CASE_ENTRIES[lo] : nullptr;
}

inline unsigned char ascii_upper(unsigned char c) {
    return c >= 'a' && c <= 'z' ? static_cast<unsigned char>(c - 32) : c;
}

// shape_table.h covers U+0080 and up, so ASCII is classified here: 1 upper
// case, 2 lower case, 4 digit, as in string_shape().
inline std::uint8_t char_class(std::uint32_t point) {
    if (point < 0x80) {
        if (point >= 'A' && point <= 'Z') return 1;
        if (point >= 'a' && point <= 'z') return 2;
        if (point >= '0' && point <= '9') return 4;
        return 0;
    }
    return shape_class(point);
}

inline bool is_cased(std::uint32_t point) {
    const std::uint8_t cls = char_class(point);
    return cls == 1 || cls == 2;
}

// Part of a word, for ftitle_case(): letters, digits and the combining marks
// that belong to the letter before them.
inline bool is_word_char(std::uint32_t point) {
    const std::uint8_t cls = char_class(point);
    return cls >= 1 && cls <= 5;
}

inline bool is_apostrophe(std::uint32_t point) {
    return point == '\'' || point == 0x2019;
}

// Characters Unicode's case conversion looks through, enough of
// Case_Ignorable for the Final_Sigma rule: combining marks, apostrophes and
// the soft hyphen.
inline bool is_case_ignorable(std::uint32_t point) {
    return char_class(point) == 5 || is_apostrophe(point) || point == 0x00AD;
}

// Appends the `form` mapping of `point`, or its own bytes when it has none.
void append_cased(std::string& out, std::uint32_t point,
                  const unsigned char* bytes, std::size_t width,
                  CaseForm form) {
    if (point < 0x80) {
        // ASCII is not in the table; its title case is its upper case.
        const unsigned char c = static_cast<unsigned char>(point);
        out.push_back(static_cast<char>(
            form == CaseForm::lower ? ascii_lower(c) : ascii_upper(c)));
        return;
    }
    const CaseEntry* entry = case_lookup(point);
    if (entry == nullptr) {
        out.append(reinterpret_cast<const char*>(bytes), width);
        return;
    }
    std::uint16_t offset = entry->upper_offset;
    std::uint8_t length = entry->upper_length;
    if (form == CaseForm::lower) {
        offset = entry->lower_offset;
        length = entry->lower_length;
    } else if (form == CaseForm::title) {
        offset = entry->title_offset;
        length = entry->title_length;
    }
    out.append(reinterpret_cast<const char*>(CASE_POOL) + offset, length);
}

// Unicode's Final_Sigma: a capital sigma lower-cases to the final form when
// a cased character precedes it and none follows, so "ΟΔΟΣ" gives "οδος".
bool final_sigma(const unsigned char* s, std::size_t n, std::size_t next,
                 bool prev_cased) {
    if (!prev_cased) return false;
    std::size_t i = next;
    while (i < n) {
        std::uint32_t point;
        const std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) return true;
        if (is_case_ignorable(point)) {
            i += width;
            continue;
        }
        return !is_cased(point);
    }
    return true;
}

// Writes `value` entirely in one case. ASCII runs are mapped a byte at a
// time; everything else goes through the table, which can make a string
// longer ("ß" to "SS").
void case_string(const StringView& value, std::string& out, bool upper) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    const CaseForm form = upper ? CaseForm::upper : CaseForm::lower;
    out.clear();
    out.reserve(n);
    bool prev_cased = false;
    std::size_t i = 0;
    while (i < n) {
        if (s[i] < 0x80) {
            const unsigned char c = s[i];
            out.push_back(static_cast<char>(upper ? ascii_upper(c)
                                                  : ascii_lower(c)));
            prev_cased = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
            ++i;
            continue;
        }
        std::uint32_t point;
        const std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) {
            out.push_back(static_cast<char>(s[i]));
            prev_cased = false;
            ++i;
            continue;
        }
        if (!upper && point == 0x03A3 &&
            final_sigma(s, n, i + width, prev_cased)) {
            utf8_append(out, 0x03C2);
        } else {
            append_cased(out, point, s + i, width, form);
        }
        prev_cased = is_cased(point);
        i += width;
    }
}

// ---------------------------------------------------------------------------
// ftitle_case(): one word at a time, so the name rules can see whole words.
// ---------------------------------------------------------------------------

// Generational suffixes written in Roman numerals. Only forms no name or
// word is spelled like are listed: "II" and "III" are safe, while "DIX",
// "MIX" and "LID" are valid numerals and also surnames or words.
const char* const ROMAN_SUFFIXES[] = {
    "ii", "iii", "iv", "vi", "vii", "viii", "ix", "xi", "xii", "xiii"
};
const std::size_t ROMAN_SUFFIX_COUNT =
    sizeof(ROMAN_SUFFIXES) / sizeof(ROMAN_SUFFIXES[0]);

// Splits `value` into words. A word is a run of letters, digits and marks;
// an apostrophe inside one ("O'Brien", "don't") belongs to it, so the rules
// below can see the whole name, but a hyphen separates words so that
// "mary-jane" titles as "Mary-Jane".
void split_words(const unsigned char* s, std::size_t n,
                 std::vector<Span>& words) {
    words.clear();
    std::size_t i = 0;
    while (i < n) {
        std::uint32_t point;
        std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) {
            // A byte that is not valid UTF-8 separates words.
            width = 1;
            point = 0;
        }
        if (!is_word_char(point)) {
            i += width;
            continue;
        }
        const std::size_t start = i;
        i += width;
        while (i < n) {
            std::uint32_t next;
            std::size_t next_width = utf8_decode(s, n, i, next);
            if (next_width == 0) break;
            if (is_word_char(next)) {
                i += next_width;
                continue;
            }
            if (!is_apostrophe(next)) break;
            // Keep the apostrophe only when a letter follows it.
            std::uint32_t after;
            const std::size_t after_at = i + next_width;
            const std::size_t after_width =
                utf8_decode(s, n, after_at, after);
            if (after_width == 0 || !is_word_char(after)) break;
            i = after_at + after_width;
        }
        words.push_back(Span{start, i - start});
    }
}

// The ASCII letters of a word, lower-cased, for comparing against the
// particle and Roman-numeral lists. Empty when the word is not all ASCII.
bool ascii_word(const unsigned char* s, const Span& word, std::string& out) {
    out.clear();
    for (std::size_t k = 0; k < word.size; ++k) {
        const unsigned char c = s[word.start + k];
        if (c >= 0x80) return false;
        out.push_back(static_cast<char>(ascii_lower(c)));
    }
    return true;
}

bool in_sorted_list(const std::vector<std::string>& list,
                    const std::string& value) {
    return std::binary_search(list.begin(), list.end(), value);
}

bool is_roman_suffix(const std::string& word) {
    for (std::size_t k = 0; k < ROMAN_SUFFIX_COUNT; ++k) {
        if (word == ROMAN_SUFFIXES[k]) return true;
    }
    return false;
}

// Titles one word: its first cased character takes the title mapping and
// the rest lower-case. With `name_rules`, a letter straight after "Mc" or
// after a one-letter prefix and an apostrophe ("O'Brien", "d'Arcy") is
// capitalised too.
void title_word(const unsigned char* s, const Span& word, bool name_rules,
                std::string& out) {
    std::size_t i = word.start;
    const std::size_t end = word.start + word.size;
    // Position (in characters) within the word, and where a capital is
    // forced by the "Mc"/"O'" rules.
    std::size_t index = 0;
    std::size_t force_at = 0;
    bool forced = false;
    if (name_rules && word.size >= 3 && s[word.start] < 0x80) {
        // "Mc" before a letter, or a one-letter prefix and an apostrophe.
        const unsigned char a = ascii_lower(s[word.start]);
        std::uint32_t second;
        const std::size_t second_width =
            utf8_decode(s, end, word.start + 1, second);
        if (second_width != 0 &&
            (is_apostrophe(second) ||
             (a == 'm' && second < 0x80 &&
              ascii_lower(static_cast<unsigned char>(second)) == 'c'))) {
            force_at = 2;
            forced = true;
        }
    }
    bool prev_cased = false;
    while (i < end) {
        std::uint32_t point;
        std::size_t width = utf8_decode(s, end, i, point);
        if (width == 0) {
            out.push_back(static_cast<char>(s[i]));
            prev_cased = false;
            ++i;
            ++index;
            continue;
        }
        // The capital comes from the first character of the word, so a word
        // that starts with a digit keeps its letters lower case ("3rd").
        const bool capital = (index == 0 && is_cased(point)) ||
            (forced && index == force_at);
        if (capital) {
            append_cased(out, point, s + i, width, CaseForm::title);
        } else if (point == 0x03A3 &&
                   final_sigma(s, end, i + width, prev_cased)) {
            utf8_append(out, 0x03C2);
        } else {
            append_cased(out, point, s + i, width, CaseForm::lower);
        }
        prev_cased = is_cased(point);
        i += width;
        ++index;
    }
}

void upper_word(const unsigned char* s, const Span& word, std::string& out) {
    const std::size_t end = word.start + word.size;
    std::size_t i = word.start;
    while (i < end) {
        std::uint32_t point;
        std::size_t width = utf8_decode(s, end, i, point);
        if (width == 0) {
            out.push_back(static_cast<char>(s[i]));
            ++i;
            continue;
        }
        append_cased(out, point, s + i, width, CaseForm::upper);
        i += width;
    }
}

void lower_word(const unsigned char* s, const Span& word, std::string& out) {
    const std::size_t end = word.start + word.size;
    std::size_t i = word.start;
    bool prev_cased = false;
    while (i < end) {
        std::uint32_t point;
        std::size_t width = utf8_decode(s, end, i, point);
        if (width == 0) {
            out.push_back(static_cast<char>(s[i]));
            prev_cased = false;
            ++i;
            continue;
        }
        if (point == 0x03A3 && final_sigma(s, end, i + width, prev_cased)) {
            utf8_append(out, 0x03C2);
        } else {
            append_cased(out, point, s + i, width, CaseForm::lower);
        }
        prev_cased = is_cased(point);
        i += width;
    }
}

void title_case_string(const StringView& value,
                       const std::vector<std::string>& particles,
                       bool name_rules, std::vector<Span>& words,
                       std::string& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    split_words(s, n, words);
    out.clear();
    out.reserve(n);
    std::string folded;
    std::size_t at = 0;
    for (std::size_t w = 0; w < words.size(); ++w) {
        const Span& word = words[w];
        // Separators between words are copied unchanged.
        out.append(value.data + at, word.start - at);
        at = word.start + word.size;
        const bool interior = w != 0 && w + 1 != words.size();
        const bool ascii = ascii_word(s, word, folded);
        if (name_rules && ascii && w != 0 && words.size() > 1 &&
            is_roman_suffix(folded)) {
            upper_word(s, word, out);
        } else if (name_rules && ascii && interior &&
                   in_sorted_list(particles, folded)) {
            lower_word(s, word, out);
        } else {
            title_word(s, word, name_rules, out);
        }
    }
    out.append(value.data + at, n - at);
}

// ---------------------------------------------------------------------------
// Tokens: runs of non-whitespace, as in freplace_tokens().
// ---------------------------------------------------------------------------

void split_tokens(const StringView& value, bool utf8, std::vector<Span>& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    out.clear();
    std::size_t i = 0;
    while (i < n) {
        const std::size_t w = whitespace_width(s, i, n, utf8);
        if (w != 0) {
            i += w;
            continue;
        }
        const std::size_t start = i;
        while (i < n && whitespace_width(s, i, n, utf8) == 0) ++i;
        out.push_back(Span{start, i - start});
    }
}

// Width of the character at s[i] (1 for a byte that is not valid UTF-8).
inline std::size_t char_width(const unsigned char* s, std::size_t n,
                              std::size_t i, bool utf8) {
    if (!utf8 || s[i] < 0x80) return 1;
    std::uint32_t point;
    const std::size_t w = utf8_decode(s, n, i, point);
    return w == 0 ? 1 : w;
}

// Byte-wise FNV-1a, folding ASCII case on request; used incrementally by
// the affix table.
inline std::uint64_t fnv_step(std::uint64_t h, unsigned char c,
                              bool ignore_case) {
    if (ignore_case) c = ascii_lower(c);
    return (h ^ c) * 1099511628211ULL;
}
const std::uint64_t FNV_SEED = 1469598103934665603ULL;

inline bool bytes_equal(const char* a, const char* b, std::size_t n,
                        bool ignore_case) {
    if (!ignore_case) return std::memcmp(a, b, n) == 0;
    for (std::size_t i = 0; i < n; ++i) {
        if (ascii_lower(static_cast<unsigned char>(a[i])) !=
            ascii_lower(static_cast<unsigned char>(b[i])))
            return false;
    }
    return true;
}

// Token frequencies: an open-addressing table whose keys live in one arena.
class TokenCounter {
public:
    explicit TokenCounter(bool ignore_case) : ignore_case_(ignore_case) {
        slots_.assign(1024, -1);
    }

    void add(const char* data, std::size_t size, std::size_t doc) {
        std::uint64_t h = FNV_SEED;
        for (std::size_t i = 0; i < size; ++i)
            h = fnv_step(h, static_cast<unsigned char>(data[i]), ignore_case_);
        Entry& e = entries_[find_or_insert(data, size, h)];
        e.count += 1;
        if (e.last_doc != doc) {
            e.docs += 1;
            e.last_doc = doc;
        }
    }

    // Adds `other`'s totals; documents never span two counters.
    void merge(const TokenCounter& other) {
        for (const Entry& o : other.entries_) {
            Entry& e = entries_[find_or_insert(other.arena_.data() + o.offset,
                                               o.size, o.hash)];
            e.count += o.count;
            e.docs += o.docs;
        }
    }

    std::size_t size() const { return entries_.size(); }
    const char* token(std::size_t i) const {
        return arena_.data() + entries_[i].offset;
    }
    std::size_t token_size(std::size_t i) const { return entries_[i].size; }
    double count(std::size_t i) const { return entries_[i].count; }
    double docs(std::size_t i) const { return entries_[i].docs; }

private:
    struct Entry {
        std::uint64_t hash;
        std::size_t offset;
        std::size_t size;
        double count;
        double docs;
        std::size_t last_doc;
    };

    std::size_t find_or_insert(const char* data, std::size_t size,
                               std::uint64_t h) {
        std::size_t mask = slots_.size() - 1;
        std::size_t slot = static_cast<std::size_t>(h ^ (h >> 31)) & mask;
        while (slots_[slot] >= 0) {
            const Entry& e = entries_[static_cast<std::size_t>(slots_[slot])];
            // Stored keys are already folded; `data` may not be.
            if (e.hash == h && e.size == size &&
                bytes_equal(arena_.data() + e.offset, data, size, ignore_case_))
                return static_cast<std::size_t>(slots_[slot]);
            slot = (slot + 1) & mask;
        }
        const std::size_t index = entries_.size();
        Entry e;
        e.hash = h;
        e.offset = arena_.size();
        e.size = size;
        e.count = 0;
        e.docs = 0;
        e.last_doc = static_cast<std::size_t>(-1);
        if (ignore_case_) {
            for (std::size_t i = 0; i < size; ++i)
                arena_.push_back(static_cast<char>(
                    ascii_lower(static_cast<unsigned char>(data[i]))));
        } else {
            arena_.append(data, size);
        }
        entries_.push_back(e);
        slots_[slot] = static_cast<std::int64_t>(index);
        if (2 * entries_.size() > slots_.size()) grow();
        return index;
    }

    void grow() {
        std::vector<std::int64_t> next(slots_.size() * 2, -1);
        const std::size_t mask = next.size() - 1;
        for (std::size_t k = 0; k < entries_.size(); ++k) {
            const std::uint64_t h = entries_[k].hash;
            std::size_t slot = static_cast<std::size_t>(h ^ (h >> 31)) & mask;
            while (next[slot] >= 0) slot = (slot + 1) & mask;
            next[slot] = static_cast<std::int64_t>(k);
        }
        slots_.swap(next);
    }

    bool ignore_case_;
    std::vector<std::int64_t> slots_;
    std::vector<Entry> entries_;
    std::string arena_;
};

struct TokenCountWorker : public Worker {
    const StringView* strings;
    bool utf8;
    bool ignore_case;
    TokenCounter counter;

    TokenCountWorker(const StringView* strings_, bool utf8_, bool ignore_case_)
        : strings(strings_), utf8(utf8_), ignore_case(ignore_case_),
          counter(ignore_case_) {}
    TokenCountWorker(const TokenCountWorker& other, Split)
        : strings(other.strings), utf8(other.utf8),
          ignore_case(other.ignore_case), counter(other.ignore_case) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::vector<Span> tokens;
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) continue;
            split_tokens(value, utf8, tokens);
            for (const Span& t : tokens)
                counter.add(value.data + t.start, t.size, i);
        }
    }

    void join(const TokenCountWorker& other) { counter.merge(other.counter); }
};

// Prefixes or suffixes, keyed by (hash of the bytes read from the matching
// end, length), for longest_prefix() / longest_suffix().
class AffixTable {
public:
    AffixTable(const StringSnapshot& affixes, bool suffix, bool ignore_case)
        : suffix_(suffix), ignore_case_(ignore_case) {
        const std::size_t n = affixes.size();
        std::size_t capacity = 16;
        while (capacity < 2 * n + 2) capacity <<= 1;
        slots_.assign(capacity, -1);
        for (std::size_t k = 0; k < n; ++k) {
            const StringView& a = affixes[k];
            if (a.is_na() || a.size == 0)
                stop("Affixes must be non-empty and not NA.");
            const std::uint64_t h = hash_from_end(a.data, a.size, a.size);
            if (lookup(a.data, a.size, a.size, h) >= 0) continue;  // first wins
            std::size_t slot = slot_of(h, a.size);
            while (slots_[slot] >= 0) slot = (slot + 1) & (slots_.size() - 1);
            slots_[slot] = static_cast<std::int64_t>(entries_.size());
            entries_.push_back(Entry{h, a.size, std::string(a.data, a.size),
                                     static_cast<int>(k) + 1});
            if (a.size > max_size_) max_size_ = a.size;
        }
        has_size_.assign(max_size_ + 1, 0);
        for (const Entry& e : entries_) has_size_[e.size] = 1;
    }

    // The 1-based index of the longest affix of s, or NA.
    int longest(const StringView& s) const {
        const std::size_t limit = (std::min)(s.size, max_size_);
        std::uint64_t h = FNV_SEED;
        int best = NA_INTEGER;
        for (std::size_t len = 1; len <= limit; ++len) {
            const unsigned char c = static_cast<unsigned char>(
                suffix_ ? s.data[s.size - len] : s.data[len - 1]);
            h = fnv_step(h, c, ignore_case_);
            if (!has_size_[len]) continue;
            const int found = lookup(s.data, s.size, len, h);
            if (found >= 0) best = found;
        }
        return best;
    }

private:
    struct Entry {
        std::uint64_t hash;
        std::size_t size;
        std::string text;
        int index;
    };

    std::size_t slot_of(std::uint64_t h, std::size_t len) const {
        const std::uint64_t mixed = (h ^ (h >> 29)) + len * 0x9E3779B97F4A7C15ULL;
        return static_cast<std::size_t>(mixed ^ (mixed >> 32)) &
            (slots_.size() - 1);
    }

    // Hash of the first (or, for suffixes, last) `len` bytes of data,
    // read from the matching end.
    std::uint64_t hash_from_end(const char* data, std::size_t size,
                                std::size_t len) const {
        std::uint64_t h = FNV_SEED;
        for (std::size_t i = 1; i <= len; ++i) {
            const unsigned char c = static_cast<unsigned char>(
                suffix_ ? data[size - i] : data[i - 1]);
            h = fnv_step(h, c, ignore_case_);
        }
        return h;
    }

    int lookup(const char* data, std::size_t size, std::size_t len,
               std::uint64_t h) const {
        const char* part = suffix_ ? data + size - len : data;
        std::size_t slot = slot_of(h, len);
        while (slots_[slot] >= 0) {
            const Entry& e = entries_[static_cast<std::size_t>(slots_[slot])];
            if (e.hash == h && e.size == len &&
                bytes_equal(e.text.data(), part, len, ignore_case_))
                return e.index;
            slot = (slot + 1) & (slots_.size() - 1);
        }
        return -1;
    }

    bool suffix_;
    bool ignore_case_;
    std::vector<std::int64_t> slots_;
    std::vector<Entry> entries_;
    std::vector<unsigned char> has_size_;
    std::size_t max_size_ = 0;
};

struct AffixWorker : public Worker {
    const StringView* strings;
    const AffixTable& table;
    int* out;

    AffixWorker(const StringView* strings_, const AffixTable& table_, int* out_)
        : strings(strings_), table(table_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i)
            out[i] = strings[i].is_na() ? NA_INTEGER : table.longest(strings[i]);
    }
};

}  // namespace

// [[Rcpp::export]]
CharacterVector fast_replace_tokens_impl(const StringVector& x,
                                         const StringVector& from,
                                         const StringVector& to,
                                         bool ignore_case, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    StringSnapshot from_snapshot(from, SnapshotText::utf8);
    StringSnapshot to_snapshot(to, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    const TokenTable table(from_snapshot, to_snapshot, ignore_case, utf8);
    auto body = [&table, utf8](const StringView& value, std::string& out,
                               std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        std::size_t i = 0;
        while (i < n) {
            const std::size_t w = whitespace_width(s, i, n, utf8);
            if (w != 0) {
                i += w;
                continue;
            }
            const std::size_t start = i;
            while (i < n && whitespace_width(s, i, n, utf8) == 0) ++i;
            const char* token = value.data + start;
            const std::size_t size = i - start;
            const std::string* replacement = table.find(token, size);
            if (replacement) {
                if (replacement->empty()) continue;
                if (!out.empty()) out.push_back(' ');
                out.append(*replacement);
            } else {
                if (!out.empty()) out.push_back(' ');
                out.append(token, size);
            }
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_pad_impl(const StringVector& x, int width, int side,
                              const std::string& pad, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    const std::size_t target = static_cast<std::size_t>(width);
    auto body = [target, side, &pad, utf8](const StringView& value,
                                           std::string& out,
                                           std::vector<Span>&) {
        std::size_t chars = value.size;
        if (utf8) {
            chars = 0;
            for (std::size_t i = 0; i < value.size; ++i)
                chars += (static_cast<unsigned char>(value.data[i]) & 0xC0) != 0x80;
        }
        if (chars >= target) return false;
        const std::size_t extra = target - chars;
        const std::size_t left = side == 0 ? extra : side == 1 ? 0 : extra / 2;
        const std::size_t right = extra - left;
        out.reserve(value.size + extra * pad.size());
        for (std::size_t k = 0; k < left; ++k) out.append(pad);
        out.append(value.data, value.size);
        for (std::size_t k = 0; k < right; ++k) out.append(pad);
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_string_shape_impl(const StringVector& x, bool collapse,
                                       bool ignore_case, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    auto body = [collapse, ignore_case, utf8](const StringView& value,
                                              std::string& out,
                                              std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        out.reserve(n);
        const char upper = ignore_case ? 'a' : 'A';
        std::size_t i = 0;
        while (i < n) {
            char code = 0;
            std::size_t width = 1;
            const unsigned char c = s[i];
            if (c < 0x80 || !utf8) {
                if (c >= 'A' && c <= 'Z') code = upper;
                else if (c >= 'a' && c <= 'z') code = 'a';
                else if (c >= '0' && c <= '9') code = '9';
            } else {
                std::uint32_t point;
                width = utf8_decode(s, n, i, point);
                if (width == 0) {
                    width = 1;
                } else {
                    switch (shape_class(point)) {
                        case 1: code = upper; break;
                        case 2:
                        case 3: code = 'a'; break;
                        case 4: code = '9'; break;
                        case 5: i += width; continue;  // combining mark
                        default: break;
                    }
                }
            }
            if (code) {
                if (!(collapse && !out.empty() && out.back() == code))
                    out.push_back(code);
            } else {
                out.append(value.data + i, width);
            }
            i += width;
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_url_encode_impl(const StringVector& x, bool reserved,
                                     bool repeated, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    // utils::URLencode(): unreserved characters always stay; with
    // reserved = FALSE so do the reserved ones of RFC 3986 and "[]".
    bool keep[256] = {false};
    for (int c = 'A'; c <= 'Z'; ++c) keep[c] = true;
    for (int c = 'a'; c <= 'z'; ++c) keep[c] = true;
    for (int c = '0'; c <= '9'; ++c) keep[c] = true;
    for (const char* p = "._~-"; *p; ++p)
        keep[static_cast<unsigned char>(*p)] = true;
    if (!reserved) {
        for (const char* p = "[]!$&'()*+,;=:/?@#"; *p; ++p)
            keep[static_cast<unsigned char>(*p)] = true;
    }
    static const char digits[] = "0123456789ABCDEF";
    auto body = [&keep, repeated](const StringView& value, std::string& out,
                                  std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        if (!repeated) {
            // Already encoded (contains %XX): returned unchanged.
            for (std::size_t i = 0; i + 2 < n; ++i) {
                if (s[i] == '%' && hex_value(s[i + 1]) >= 0 &&
                    hex_value(s[i + 2]) >= 0)
                    return false;
            }
        }
        std::size_t i = 0;
        while (i < n && keep[s[i]]) ++i;
        if (i == n) return false;
        out.reserve(n + 16);
        out.append(value.data, i);
        for (; i < n; ++i) {
            if (keep[s[i]]) {
                out.push_back(static_cast<char>(s[i]));
            } else {
                out.push_back('%');
                out.push_back(digits[s[i] >> 4]);
                out.push_back(digits[s[i] & 15]);
            }
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_url_decode_impl(const StringVector& x, bool plus,
                                     int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    auto body = [plus](const StringView& value, std::string& out,
                       std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        std::size_t i = 0;
        while (i < n && s[i] != '%' && !(plus && s[i] == '+')) ++i;
        if (i == n) return false;
        out.reserve(n);
        out.append(value.data, i);
        for (; i < n; ++i) {
            if (s[i] == '%' && i + 2 < n && hex_value(s[i + 1]) >= 0 &&
                hex_value(s[i + 2]) >= 0) {
                out.push_back(static_cast<char>(
                    hex_value(s[i + 1]) * 16 + hex_value(s[i + 2])));
                i += 2;
            } else if (plus && s[i] == '+') {
                out.push_back(' ');
            } else {
                out.push_back(static_cast<char>(s[i]));
            }
        }
        return true;
    };
    CharacterVector result = run_text(snapshot, nthreads, body);
    // Decoded bytes that are not valid UTF-8 are returned in the native
    // encoding, as utils::URLdecode() returns everything.
    const R_xlen_t len = result.size();
    for (R_xlen_t i = 0; i < len; ++i) {
        SEXP elem = STRING_ELT(result, i);
        if (elem == NA_STRING || Rf_getCharCE(elem) != CE_UTF8) continue;
        const unsigned char* data =
            reinterpret_cast<const unsigned char*>(CHAR(elem));
        const std::size_t size = static_cast<std::size_t>(LENGTH(elem));
        std::size_t k = 0;
        while (k < size) {
            if (data[k] < 0x80) {
                ++k;
                continue;
            }
            std::uint32_t point;
            const std::size_t w = utf8_decode(data, size, k, point);
            if (w == 0) break;
            k += w;
        }
        if (k < size) {
            SET_STRING_ELT(result, i,
                           Rf_mkCharLenCE(CHAR(elem), static_cast<int>(size),
                                          CE_NATIVE));
        }
    }
    return result;
}

// [[Rcpp::export]]
CharacterVector fast_html_unescape_impl(const StringVector& x, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    auto body = [](const StringView& value, std::string& out,
                   std::vector<Span>&) {
        if (!std::memchr(value.data, '&', value.size)) return false;
        html_unescape_string(value, out);
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_token_sort_impl(const StringVector& x, bool unique,
                                     bool decreasing, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    auto body = [utf8, unique, decreasing](const StringView& value,
                                           std::string& out,
                                           std::vector<Span>& tokens) {
        split_tokens(value, utf8, tokens);
        const char* base = value.data;
        auto less = [base](const Span& a, const Span& b) {
            const int c = std::memcmp(base + a.start, base + b.start,
                                      (std::min)(a.size, b.size));
            return c != 0 ? c < 0 : a.size < b.size;
        };
        std::sort(tokens.begin(), tokens.end(), less);
        if (unique) {
            tokens.erase(std::unique(tokens.begin(), tokens.end(),
                                     [base](const Span& a, const Span& b) {
                                         return a.size == b.size &&
                                             std::memcmp(base + a.start,
                                                         base + b.start,
                                                         a.size) == 0;
                                     }),
                         tokens.end());
        }
        if (decreasing) std::reverse(tokens.begin(), tokens.end());
        out.reserve(value.size);
        for (std::size_t t = 0; t < tokens.size(); ++t) {
            if (t) out.push_back(' ');
            out.append(base + tokens[t].start, tokens[t].size);
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_token_initials_impl(const StringVector& x,
                                         const std::string& sep,
                                         int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    auto body = [utf8, &sep](const StringView& value, std::string& out,
                             std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        bool first = true;
        std::size_t i = 0;
        while (i < n) {
            const std::size_t w = whitespace_width(s, i, n, utf8);
            if (w != 0) {
                i += w;
                continue;
            }
            if (!first) out.append(sep);
            first = false;
            const std::size_t cw = char_width(s, n, i, utf8);
            out.append(value.data + i, cw);
            i += cw;
            while (i < n && whitespace_width(s, i, n, utf8) == 0) ++i;
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_reverse_impl(const StringVector& x, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    auto body = [utf8](const StringView& value, std::string& out,
                       std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        const std::size_t n = value.size;
        out.resize(n);
        // Walk forwards over characters, keeping each base character and
        // the combining marks after it together, and write each such
        // cluster at the mirrored position.
        std::size_t i = 0;
        while (i < n) {
            const std::size_t start = i;
            i += char_width(s, n, i, utf8);
            while (utf8 && i < n && s[i] >= 0x80) {
                std::uint32_t point;
                const std::size_t w = utf8_decode(s, n, i, point);
                if (w == 0 || shape_class(point) != 5) break;
                i += w;
            }
            std::memcpy(&out[n - i], value.data + start, i - start);
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
List fast_token_counts_impl(const StringVector& x, bool ignore_case,
                            int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const bool utf8 = !snapshot.any_bytes();
    TokenCountWorker worker(snapshot.data(), utf8, ignore_case);
    const std::size_t n = snapshot.size();
    if (nthreads == 1 || text_work(snapshot) < TEXT_PARALLEL_THRESHOLD) {
        worker(0, n);
    } else {
        parallelReduce(0, n, worker, 4096, nthreads);
    }
    const TokenCounter& counter = worker.counter;
    const std::size_t m = counter.size();
    // Most frequent first, ties by byte order of the token.
    std::vector<std::size_t> order(m);
    for (std::size_t k = 0; k < m; ++k) order[k] = k;
    std::sort(order.begin(), order.end(),
              [&counter](std::size_t a, std::size_t b) {
                  if (counter.count(a) != counter.count(b))
                      return counter.count(a) > counter.count(b);
                  const std::size_t la = counter.token_size(a);
                  const std::size_t lb = counter.token_size(b);
                  const int c = std::memcmp(counter.token(a), counter.token(b),
                                            (std::min)(la, lb));
                  return c != 0 ? c < 0 : la < lb;
              });
    CharacterVector token(m);
    NumericVector count(Rcpp::no_init(m)), docs(Rcpp::no_init(m));
    const cetype_t encoding = utf8 ? CE_UTF8 : CE_BYTES;
    for (std::size_t k = 0; k < m; ++k) {
        const std::size_t e = order[k];
        SET_STRING_ELT(token, k,
                       make_text_charsxp(counter.token(e), counter.token_size(e),
                                         encoding));
        count[k] = counter.count(e);
        docs[k] = counter.docs(e);
    }
    return List::create(_["token"] = token, _["n"] = count, _["docs"] = docs);
}

// [[Rcpp::export]]
IntegerVector fast_longest_affix_impl(const StringVector& x,
                                      const StringVector& affixes, bool suffix,
                                      bool ignore_case, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    StringSnapshot affix_snapshot(affixes, SnapshotText::utf8);
    const AffixTable table(affix_snapshot, suffix, ignore_case);
    IntegerVector result(Rcpp::no_init(x.size()));
    AffixWorker worker(snapshot.data(), table, INTEGER(result));
    dispatch_for(0, snapshot.size(), worker, text_work(snapshot),
                 TEXT_PARALLEL_THRESHOLD, nthreads, 2048);
    return result;
}

// [[Rcpp::export]]
CharacterVector fast_html_escape_impl(const StringVector& x, bool attribute,
                                      int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    // As htmltools::htmlEscape() does: "&", "<" and ">" always, and in an
    // attribute value the quotes and the line breaks as well.
    const char* table[256] = {nullptr};
    table[static_cast<unsigned char>('&')] = "&amp;";
    table[static_cast<unsigned char>('<')] = "&lt;";
    table[static_cast<unsigned char>('>')] = "&gt;";
    if (attribute) {
        table[static_cast<unsigned char>('"')] = "&quot;";
        table[static_cast<unsigned char>('\'')] = "&#39;";
        table[static_cast<unsigned char>('\r')] = "&#13;";
        table[static_cast<unsigned char>('\n')] = "&#10;";
    }
    auto body = [&table](const StringView& value, std::string& out,
                         std::vector<Span>&) {
        const unsigned char* s =
            reinterpret_cast<const unsigned char*>(value.data);
        std::size_t i = 0;
        while (i < value.size && table[s[i]] == nullptr) ++i;
        if (i == value.size) return false;
        out.assign(value.data, i);
        for (; i < value.size; ++i) {
            const char* replacement = table[s[i]];
            if (replacement != nullptr) out.append(replacement);
            else out.push_back(value.data[i]);
        }
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_case_impl(const StringVector& x, bool upper,
                               int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    auto body = [upper](const StringView& value, std::string& out,
                        std::vector<Span>&) {
        case_string(value, out, upper);
        return true;
    };
    return run_text(snapshot, nthreads, body);
}

// [[Rcpp::export]]
CharacterVector fast_title_case_impl(const StringVector& x,
                                     const std::vector<std::string>& particles,
                                     bool name_rules, int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    std::vector<std::string> sorted(particles);
    std::sort(sorted.begin(), sorted.end());
    auto body = [&sorted, name_rules](const StringView& value,
                                      std::string& out,
                                      std::vector<Span>& words) {
        title_case_string(value, sorted, name_rules, words, out);
        return true;
    };
    return run_text(snapshot, nthreads, body);
}
