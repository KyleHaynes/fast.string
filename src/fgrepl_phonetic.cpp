// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string>
#include <vector>
#include "caverphone_core.h"
#include "double_metaphone_core.h"
#include "latin_fold.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

using namespace Rcpp;
using namespace RcppParallel;

static const std::uint8_t PHONETIC_NA_LENGTH = 255;

// Every encoder sees its input with accented Latin letters folded to ASCII
// (see latin_fold.h), from a snapshot that has translated latin1 to UTF-8.
static inline StringView fold_for_phonetics(const StringView& value,
                                            std::string& scratch,
                                            const char* c_cedilla = "C") {
    if (!fold_latin_letters(value.data, value.size, scratch, c_cedilla))
        return value;
    return StringView{scratch.data(), scratch.size()};
}

static inline bool is_alpha_ascii(unsigned char value) {
    return (value >= 'A' && value <= 'Z') ||
        (value >= 'a' && value <= 'z');
}

static inline char to_upper_ascii(unsigned char value) {
    return value >= 'a' && value <= 'z'
        ? static_cast<char>(value - 'a' + 'A')
        : static_cast<char>(value);
}

static inline bool is_vowel(char value) {
    return value == 'A' || value == 'E' || value == 'I' ||
        value == 'O' || value == 'U';
}

static inline std::size_t phonetic_work(const StringSnapshot& snapshot) {
    const std::uint64_t units = static_cast<std::uint64_t>(snapshot.size()) +
        snapshot.total_bytes() / 32u;
    const std::uint64_t maximum =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return units > maximum
        ? (std::numeric_limits<std::size_t>::max)()
        : static_cast<std::size_t>(units);
}

static inline std::size_t checked_slots(std::size_t n, std::size_t width) {
    if (n != 0 && width > (std::numeric_limits<std::size_t>::max)() / n)
        stop("Phonetic output is too large.");
    return n * width;
}

static inline int soundex_digit(char value) {
    switch (value) {
        case 'B': case 'F': case 'P': case 'V': return 1;
        case 'C': case 'G': case 'J': case 'K': case 'Q':
        case 'S': case 'X': case 'Z': return 2;
        case 'D': case 'T': return 3;
        case 'L': return 4;
        case 'M': case 'N': return 5;
        case 'R': return 6;
        default: return 0;
    }
}

static bool soundex_code(const char* input, std::size_t length, char* output) {
    std::size_t i = 0;
    while (i < length &&
           !is_alpha_ascii(static_cast<unsigned char>(input[i])))
        ++i;
    if (i == length)
        return false;

    const char first = to_upper_ascii(static_cast<unsigned char>(input[i]));
    output[0] = first;
    output[1] = '0';
    output[2] = '0';
    output[3] = '0';
    int output_length = 1;
    int previous = soundex_digit(first);

    for (++i; i < length && output_length < 4; ++i) {
        const unsigned char byte = static_cast<unsigned char>(input[i]);
        if (!is_alpha_ascii(byte))
            continue;
        const char value = to_upper_ascii(byte);
        if (value == 'H' || value == 'W')
            continue;
        const int digit = soundex_digit(value);
        if (digit != 0 && digit != previous)
            output[output_length++] = static_cast<char>('0' + digit);
        previous = digit;
    }
    return true;
}

struct SoundexWorker : public Worker {
    const StringView* strings;
    char* bytes;
    std::uint8_t* lengths;

    SoundexWorker(const StringView* strings_,
                  char* bytes_,
                  std::uint8_t* lengths_)
        : strings(strings_), bytes(bytes_), lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string scratch;
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            const StringView value = fold_for_phonetics(strings[i], scratch);
            lengths[i] = soundex_code(value.data, value.size, bytes + i * 4u)
                ? 4
                : PHONETIC_NA_LENGTH;
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_soundex_impl(const StringVector& x, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 4));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    SoundexWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == PHONETIC_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLen(bytes.data() + i * 4u, 4)
            );
        }
    }
    return result;
}

// NYSIIS as Apache Commons Codec's Nysiis implements it (in strict mode, so
// keys are capped at six characters). Letters are transcoded in place, left
// to right, so the H and W rules see the previous letter as already
// transcoded -- a vowel there has become A, never E/I/O/U.
static inline int nysiis_transcode(char previous, char current, char next,
                                   char after_next, char* out) {
    if (current == 'E' && next == 'V') { out[0] = 'A'; out[1] = 'F'; return 2; }
    if (is_vowel(current)) { out[0] = 'A'; return 1; }
    if (current == 'Q') { out[0] = 'G'; return 1; }
    if (current == 'Z') { out[0] = 'S'; return 1; }
    if (current == 'M') { out[0] = 'N'; return 1; }
    if (current == 'K') {
        if (next == 'N') { out[0] = 'N'; out[1] = 'N'; return 2; }
        out[0] = 'C';
        return 1;
    }
    if (current == 'S' && next == 'C' && after_next == 'H') {
        out[0] = out[1] = out[2] = 'S';
        return 3;
    }
    if (current == 'P' && next == 'H') { out[0] = 'F'; out[1] = 'F'; return 2; }
    // A letter past either end counts as a non-vowel.
    if (current == 'H' && (!is_vowel(previous) || !is_vowel(next))) {
        out[0] = previous;
        return 1;
    }
    if (current == 'W' && is_vowel(previous)) { out[0] = previous; return 1; }
    out[0] = current;
    return 1;
}

// `max_length` 0 leaves the key uncapped (ONCA's first stage).
static bool nysiis_code(const char* input,
                        std::size_t length,
                        std::string& output,
                        std::size_t max_length = 6) {
    std::string word;
    word.reserve(length);
    for (std::size_t i = 0; i < length; ++i) {
        const unsigned char value = static_cast<unsigned char>(input[i]);
        if (is_alpha_ascii(value))
            word.push_back(to_upper_ascii(value));
    }
    if (word.empty())
        return false;

    if (word.size() >= 3 && word.compare(0, 3, "MAC") == 0)
        word.replace(0, 3, "MCC");
    else if (word.size() >= 2 && word.compare(0, 2, "KN") == 0)
        word.replace(0, 2, "NN");
    else if (word[0] == 'K')
        word[0] = 'C';
    if (word.size() >= 2 &&
        (word.compare(0, 2, "PH") == 0 ||
         word.compare(0, 2, "PF") == 0)) {
        word.replace(0, 2, "FF");
    } else if (word.size() >= 3 && word.compare(0, 3, "SCH") == 0) {
        word.replace(0, 3, "SSS");
    }

    std::size_t n = word.size();
    if (n >= 2) {
        const std::string tail = word.substr(n - 2);
        if (tail == "EE" || tail == "IE")
            word.replace(n - 2, 2, "Y");
    }
    n = word.size();
    if (n >= 2) {
        const std::string tail = word.substr(n - 2);
        if (tail == "DT" || tail == "RT" || tail == "RD" ||
            tail == "NT" || tail == "ND")
            word.replace(n - 2, 2, "D");
    }

    std::string key(1, word[0]);
    n = word.size();
    for (std::size_t i = 1; i < n; ++i) {
        const char next = i + 1 < n ? word[i + 1] : ' ';
        const char after_next = i + 2 < n ? word[i + 2] : ' ';
        char transcoded[3];
        const int count = nysiis_transcode(
            word[i - 1], word[i], next, after_next, transcoded
        );
        for (int k = 0; k < count; ++k) word[i + k] = transcoded[k];
        if (word[i] != word[i - 1]) key.push_back(word[i]);
    }

    if (key.size() > 1) {
        char last = key.back();
        if (last == 'S') {
            key.pop_back();
            last = key.back();
        }
        if (key.size() > 2 && key[key.size() - 2] == 'A' && last == 'Y')
            key.erase(key.size() - 2, 1);
        if (last == 'A') key.pop_back();
    }
    if (max_length != 0 && key.size() > max_length)
        key.resize(max_length);
    output.swap(key);
    return true;
}

struct NysiisWorker : public Worker {
    const StringView* strings;
    char* bytes;
    std::uint8_t* lengths;

    NysiisWorker(const StringView* strings_,
                 char* bytes_,
                 std::uint8_t* lengths_)
        : strings(strings_), bytes(bytes_), lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string code, scratch;
        for (std::size_t i = begin; i < end; ++i) {
            code.clear();
            if (strings[i].is_na()) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            const StringView value = fold_for_phonetics(strings[i], scratch);
            if (!nysiis_code(value.data, value.size, code)) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            lengths[i] = static_cast<std::uint8_t>(code.size());
            std::memcpy(bytes + i * 6u, code.data(), code.size());
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_nysiis_impl(const StringVector& x, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 6));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    NysiisWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == PHONETIC_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLen(
                    bytes.data() + i * 6u,
                    static_cast<int>(lengths[i])
                )
            );
        }
    }
    return result;
}

static bool clean_ascii_letters(const char* input,
                                std::size_t length,
                                std::string& output) {
    output.clear();
    output.reserve(length);
    for (std::size_t i = 0; i < length; ++i) {
        const unsigned char value = static_cast<unsigned char>(input[i]);
        if (is_alpha_ascii(value)) output.push_back(to_upper_ascii(value));
    }
    return !output.empty();
}

static bool refined_soundex_code(const char* input,
                                 std::size_t length,
                                 std::string& output) {
    static const char MAPPING[] = "01360240043788015936020505";
    std::string word;
    if (!clean_ascii_letters(input, length, word)) return false;

    output.clear();
    output.reserve(word.size() + 1);
    output.push_back(word.front());
    char previous = '*';
    for (char value : word) {
        const char code = MAPPING[value - 'A'];
        if (code != previous) output.push_back(code);
        previous = code;
    }
    return true;
}

static inline bool one_of(char value, const char* values) {
    return std::strchr(values, value) != nullptr;
}

static inline void cologne_put(char code,
                               std::string& output,
                               char& previous_code) {
    if (code != '-' && code != previous_code &&
        (code != '0' || output.empty()))
        output.push_back(code);
    // Vowels are omitted except at the start, but they still split duplicate
    // consonant codes. Ignored H resets the duplicate state without output.
    previous_code = code;
}

static bool cologne_code(const char* input,
                         std::size_t length,
                         std::string& output) {
    // Umlauts and sharp s arrive folded to A/O/U and SS (fold_for_phonetics).
    std::string word;
    if (!clean_ascii_letters(input, length, word)) return false;

    output.clear();
    output.reserve(word.size() * 2);
    char previous_code = '/';
    char previous_char = '-';
    for (std::size_t i = 0; i < word.size(); ++i) {
        const char value = word[i];
        const char next = i + 1 < word.size() ? word[i + 1] : '-';
        if (one_of(value, "AEIJOUY")) {
            cologne_put('0', output, previous_code);
        } else if (value == 'B' || (value == 'P' && next != 'H')) {
            cologne_put('1', output, previous_code);
        } else if (one_of(value, "DT") && !one_of(next, "CSZ")) {
            cologne_put('2', output, previous_code);
        } else if (one_of(value, "FPVW")) {
            cologne_put('3', output, previous_code);
        } else if (one_of(value, "GKQ")) {
            cologne_put('4', output, previous_code);
        } else if (value == 'C') {
            if (output.empty()) {
                cologne_put(
                    one_of(next, "AHKLOQRUX") ? '4' : '8',
                    output, previous_code
                );
            } else {
                cologne_put(
                    one_of(previous_char, "SZ") ||
                        !one_of(next, "AHKOQUX") ? '8' : '4',
                    output, previous_code
                );
            }
        } else if (value == 'X' && !one_of(previous_char, "CKQ")) {
            cologne_put('4', output, previous_code);
            cologne_put('8', output, previous_code);
        } else if (value == 'L') {
            cologne_put('5', output, previous_code);
        } else if (one_of(value, "MN")) {
            cologne_put('6', output, previous_code);
        } else if (value == 'R') {
            cologne_put('7', output, previous_code);
        } else if (value == 'H') {
            cologne_put('-', output, previous_code);
        } else if (one_of(value, "SZDTX")) {
            cologne_put('8', output, previous_code);
        }
        previous_char = value;
    }
    return true;
}

typedef bool (*PhoneticStringFunction)(
    const char*, std::size_t, std::string&
);

struct DynamicPhoneticWorker : public Worker {
    const StringView* strings;
    PhoneticStringFunction encode;
    std::vector<std::string>& output;
    std::vector<std::uint8_t>& missing;

    DynamicPhoneticWorker(const StringView* strings_,
                          PhoneticStringFunction encode_,
                          std::vector<std::string>& output_,
                          std::vector<std::uint8_t>& missing_)
        : strings(strings_), encode(encode_), output(output_), missing(missing_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string scratch;
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) {
                missing[i] = 1;
                continue;
            }
            const StringView value = fold_for_phonetics(strings[i], scratch);
            if (!encode(value.data, value.size, output[i]))
                missing[i] = 1;
        }
    }
};

static CharacterVector run_dynamic_phonetic(const StringVector& x,
                                            PhoneticStringFunction encode,
                                            int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<std::string> encoded(n);
    std::vector<std::uint8_t> missing(n, 0);
    DynamicPhoneticWorker worker(
        snapshot.data(), encode, encoded, missing
    );
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        SET_STRING_ELT(
            result, static_cast<R_xlen_t>(i),
            missing[i]
                ? NA_STRING
                : Rf_mkCharLen(
                    encoded[i].data(), static_cast<int>(encoded[i].size())
                )
        );
    }
    return result;
}

// [[Rcpp::export]]
CharacterVector fast_refined_soundex_impl(const StringVector& x,
                                          int nthreads) {
    return run_dynamic_phonetic(x, refined_soundex_code, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_cologne_impl(const StringVector& x, int nthreads) {
    return run_dynamic_phonetic(x, cologne_code, nthreads);
}

struct DoubleMetaphoneWorker : public Worker {
    const StringView* strings;
    char* primary;
    char* secondary;
    std::uint8_t* primary_lengths;
    std::uint8_t* secondary_lengths;

    DoubleMetaphoneWorker(const StringView* strings_,
                          char* primary_,
                          char* secondary_,
                          std::uint8_t* primary_lengths_,
                          std::uint8_t* secondary_lengths_)
        : strings(strings_),
          primary(primary_),
          secondary(secondary_),
          primary_lengths(primary_lengths_),
          secondary_lengths(secondary_lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string first;
        std::string second;
        std::string scratch;
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) {
                primary_lengths[i] = PHONETIC_NA_LENGTH;
                secondary_lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            // Commons Codec codes C with cedilla as S (and N with tilde as N).
            const StringView value =
                fold_for_phonetics(strings[i], scratch, "S");
            double_metaphone_code(
                std::string(value.data, value.size),
                first,
                second
            );
            if (first.empty()) {
                primary_lengths[i] = PHONETIC_NA_LENGTH;
                secondary_lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            primary_lengths[i] = static_cast<std::uint8_t>(first.size());
            secondary_lengths[i] = static_cast<std::uint8_t>(second.size());
            std::memcpy(primary + i * 4u, first.data(), first.size());
            if (!second.empty())
                std::memcpy(secondary + i * 4u, second.data(), second.size());
        }
    }
};

// [[Rcpp::export]]
List fast_double_metaphone_impl(const StringVector& x, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<char> primary(checked_slots(n, 4));
    std::vector<char> secondary(checked_slots(n, 4));
    std::vector<std::uint8_t> primary_lengths(n, PHONETIC_NA_LENGTH);
    std::vector<std::uint8_t> secondary_lengths(n, PHONETIC_NA_LENGTH);
    DoubleMetaphoneWorker worker(
        snapshot.data(),
        primary.data(),
        secondary.data(),
        primary_lengths.data(),
        secondary_lengths.data()
    );
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);

    CharacterVector primary_output(static_cast<R_xlen_t>(n));
    CharacterVector secondary_output(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (primary_lengths[i] == PHONETIC_NA_LENGTH) {
            SET_STRING_ELT(primary_output, static_cast<R_xlen_t>(i), NA_STRING);
            SET_STRING_ELT(secondary_output, static_cast<R_xlen_t>(i), NA_STRING);
            continue;
        }
        SET_STRING_ELT(
            primary_output,
            static_cast<R_xlen_t>(i),
            Rf_mkCharLen(
                primary.data() + i * 4u,
                static_cast<int>(primary_lengths[i])
            )
        );
        const char* alternate = secondary_lengths[i] == 0
            ? ""
            : secondary.data() + i * 4u;
        SET_STRING_ELT(
            secondary_output,
            static_cast<R_xlen_t>(i),
            Rf_mkCharLen(alternate, static_cast<int>(secondary_lengths[i]))
        );
    }
    return List::create(
        Named("primary") = primary_output,
        Named("secondary") = secondary_output
    );
}

struct CaverphoneWorker : public Worker {
    const StringView* strings;
    char* bytes;
    std::uint8_t* lengths;

    CaverphoneWorker(const StringView* strings_,
                     char* bytes_,
                     std::uint8_t* lengths_)
        : strings(strings_), bytes(bytes_), lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string scratch;
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            const StringView value = fold_for_phonetics(strings[i], scratch);
            const std::string code = caverphone2_code(
                std::string(value.data, value.size)
            );
            lengths[i] = static_cast<std::uint8_t>(code.size());
            std::memcpy(bytes + i * 10u, code.data(), code.size());
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_caverphone_impl(const StringVector& x, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 10));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    CaverphoneWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == PHONETIC_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLen(
                    bytes.data() + i * 10u,
                    static_cast<int>(lengths[i])
                )
            );
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Metaphone, Match Rating Approach, Statistics Canada, Lein, Phonex, Roger
// Root and ONCA. Metaphone follows Apache Commons Codec's Metaphone; MRA
// follows the 1977 specification as the jellyfish package implements it;
// Statistics Canada, Lein, Phonex and Roger Root follow the phonics package
// (James P. Howard II, BSD-2-Clause), rule for rule; ONCA is this package's
// NYSIIS followed by Soundex.
// ---------------------------------------------------------------------------

// Every encoder sees accented Latin letters folded to ASCII first. Encoders
// return false (NA) when the input has no ASCII letter.
template <typename Encoder>
struct PhoneticWorker : public Worker {
    const StringView* strings;
    const Encoder& encode;
    std::vector<std::string>& output;
    std::vector<std::uint8_t>& missing;

    PhoneticWorker(const StringView* strings_, const Encoder& encode_,
                   std::vector<std::string>& output_,
                   std::vector<std::uint8_t>& missing_)
        : strings(strings_), encode(encode_), output(output_),
          missing(missing_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string scratch;
        for (std::size_t i = begin; i < end; ++i) {
            if (strings[i].is_na()) {
                missing[i] = 1;
                continue;
            }
            const StringView value = fold_for_phonetics(strings[i], scratch);
            if (!encode(value.data, value.size, output[i])) missing[i] = 1;
        }
    }
};

template <typename Encoder>
static CharacterVector run_phonetic(const StringVector& x,
                                    const Encoder& encode, int nthreads) {
    const StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t n = snapshot.size();
    std::vector<std::string> encoded(n);
    std::vector<std::uint8_t> missing(n, 0);
    PhoneticWorker<Encoder> worker(snapshot.data(), encode, encoded, missing);
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000, nthreads);
    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        SET_STRING_ELT(
            result, static_cast<R_xlen_t>(i),
            missing[i]
                ? NA_STRING
                : Rf_mkCharLen(encoded[i].data(),
                               static_cast<int>(encoded[i].size()))
        );
    }
    return result;
}

static inline bool has_ascii_letter(const char* input, std::size_t length) {
    for (std::size_t i = 0; i < length; ++i)
        if (is_alpha_ascii(static_cast<unsigned char>(input[i]))) return true;
    return false;
}

// Collapses runs of one repeated character in s[from, end) to one.
static inline void collapse_runs(std::string& s, std::size_t from = 0) {
    std::size_t write = from;
    for (std::size_t read = from; read < s.size(); ++read) {
        if (write > from && s[write - 1] == s[read]) continue;
        s[write++] = s[read];
    }
    s.resize(write);
}

// Pads with '0' and truncates to `max_length`.
static inline void pad_zeros(std::string& code, std::size_t max_length) {
    if (code.size() < max_length) code.append(max_length - code.size(), '0');
    code.resize(max_length);
}

static inline bool all_zeros(const std::string& code) {
    return code.find_first_not_of('0') == std::string::npos;
}

// --- Metaphone (Apache Commons Codec) --------------------------------------

struct MetaphoneEncoder {
    std::size_t max_length;

    static bool vowel(char c) { return std::strchr("AEIOU", c) != nullptr && c; }
    static bool front_vowel(char c) { return c && std::strchr("EIY", c) != nullptr; }
    static bool varson(char c) { return c && std::strchr("CSPTG", c) != nullptr; }

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        code.clear();
        if (!has_ascii_letter(input, length)) return false;
        std::string word(input, length);
        for (char& c : word) c = to_upper_ascii(static_cast<unsigned char>(c));
        if (word.size() == 1) {
            code = word;
            return true;
        }
        // Initial-letter exceptions.
        std::string local;
        switch (word[0]) {
        case 'K': case 'G': case 'P':
            local = word[1] == 'N' ? word.substr(1) : word;
            break;
        case 'A':
            local = word[1] == 'E' ? word.substr(1) : word;
            break;
        case 'W':
            if (word[1] == 'R') {
                local = word.substr(1);
            } else if (word[1] == 'H') {
                local = word.substr(1);
                local[0] = 'W';
            } else {
                local = word;
            }
            break;
        case 'X':
            local = word;
            local[0] = 'S';
            break;
        default:
            local = word;
        }

        const std::size_t size = local.size();
        auto at = [&local, size](std::size_t i) -> char {
            return i < size ? local[i] : '\0';
        };
        auto last = [size](std::size_t i) { return i + 1 == size; };
        auto next_is = [&local, size](std::size_t i, char c) {
            return i + 1 < size && local[i + 1] == c;
        };
        auto previous_is = [&local, size](std::size_t i, char c) {
            return i > 0 && i < size && local[i - 1] == c;
        };
        auto region = [&local, size](std::size_t i, const char* test) {
            const std::size_t n = std::strlen(test);
            return i + n <= size && local.compare(i, n, test) == 0;
        };

        std::size_t n = 0;
        while (code.size() < max_length && n < size) {
            const char symbol = local[n];
            if (symbol != 'C' && previous_is(n, symbol)) {
                ++n;
                continue;
            }
            switch (symbol) {
            case 'A': case 'E': case 'I': case 'O': case 'U':
                if (n == 0) code.push_back(symbol);
                break;
            case 'B':
                if (!(previous_is(n, 'M') && last(n))) code.push_back('B');
                break;
            case 'C':
                if (previous_is(n, 'S') && !last(n) && front_vowel(at(n + 1)))
                    break;
                if (region(n, "CIA")) {
                    code.push_back('X');
                    break;
                }
                if (!last(n) && front_vowel(at(n + 1))) {
                    code.push_back('S');
                    break;
                }
                if (previous_is(n, 'S') && next_is(n, 'H')) {
                    code.push_back('K');
                    break;
                }
                if (!next_is(n, 'H') || (n == 0 && size >= 3 && vowel(at(2))))
                    code.push_back('K');
                else
                    code.push_back('X');
                break;
            case 'D':
                if (!last(n + 1) && next_is(n, 'G') && front_vowel(at(n + 2))) {
                    code.push_back('J');
                    n += 2;
                } else {
                    code.push_back('T');
                }
                break;
            case 'G': {
                if (last(n + 1) && next_is(n, 'H')) break;
                if (!last(n + 1) && next_is(n, 'H') && !vowel(at(n + 2))) break;
                if (n > 0 && (region(n, "GN") || region(n, "GNED"))) break;
                const bool hard = previous_is(n, 'G');
                if (!last(n) && front_vowel(at(n + 1)) && !hard)
                    code.push_back('J');
                else
                    code.push_back('K');
                break;
            }
            case 'H':
                if (last(n)) break;
                if (n > 0 && varson(local[n - 1])) break;
                if (vowel(at(n + 1))) code.push_back('H');
                break;
            case 'F': case 'J': case 'L': case 'M': case 'N': case 'R':
                code.push_back(symbol);
                break;
            case 'K':
                if (n == 0 || !previous_is(n, 'C')) code.push_back('K');
                break;
            case 'P':
                code.push_back(next_is(n, 'H') ? 'F' : 'P');
                break;
            case 'Q':
                code.push_back('K');
                break;
            case 'S':
                code.push_back(region(n, "SH") || region(n, "SIO") ||
                               region(n, "SIA") ? 'X' : 'S');
                break;
            case 'T':
                if (region(n, "TIA") || region(n, "TIO")) {
                    code.push_back('X');
                    break;
                }
                if (region(n, "TCH")) break;
                code.push_back(region(n, "TH") ? '0' : 'T');
                break;
            case 'V':
                code.push_back('F');
                break;
            case 'W': case 'Y':
                if (!last(n) && vowel(at(n + 1))) code.push_back(symbol);
                break;
            case 'X':
                code.push_back('K');
                code.push_back('S');
                break;
            case 'Z':
                code.push_back('S');
                break;
            default:
                break;
            }
            ++n;
            if (code.size() > max_length) code.resize(max_length);
        }
        return true;
    }
};

// --- Match Rating Approach (Western Airlines, 1977) ------------------------

static bool mra_codex(const char* input, std::size_t length, std::string& code) {
    std::string word;
    if (!clean_ascii_letters(input, length, word)) return false;
    code.clear();
    char previous = '\0';
    for (std::size_t i = 0; i < word.size(); ++i) {
        const char c = word[i];
        // The first letter, then consonants not repeating the letter before.
        if (i == 0 || (!is_vowel(c) && c != previous)) code.push_back(c);
        previous = c;
    }
    if (code.size() > 6) code = code.substr(0, 3) + code.substr(code.size() - 3);
    return true;
}

struct MraEncoder {
    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        return mra_codex(input, length, code);
    }
};

// 1 match, 0 no match (including codes whose lengths differ by 3 or more),
// NA_LOGICAL when either name has no letter.
static int mra_compare_codes(const std::string& a, const std::string& b) {
    const std::size_t la = a.size(), lb = b.size();
    if ((la > lb ? la - lb : lb - la) >= 3) return 0;
    const std::size_t sum = la + lb;
    const int minimum = sum <= 4 ? 5 : sum <= 7 ? 4 : sum <= 11 ? 3 : 2;
    // Drop the characters that agree position by position from the left,
    // then compare what is left from the right.
    std::string left_a, left_b;
    for (std::size_t i = 0; i < (std::max)(la, lb); ++i) {
        const char ca = i < la ? a[i] : '\0';
        const char cb = i < lb ? b[i] : '\0';
        if (ca == cb) continue;
        if (ca) left_a.push_back(ca);
        if (cb) left_b.push_back(cb);
    }
    int unmatched_a = 0, unmatched_b = 0;
    const std::size_t ra = left_a.size(), rb = left_b.size();
    for (std::size_t i = 0; i < (std::max)(ra, rb); ++i) {
        const char ca = i < ra ? left_a[ra - 1 - i] : '\0';
        const char cb = i < rb ? left_b[rb - 1 - i] : '\0';
        if (ca == cb) continue;
        if (ca) ++unmatched_a;
        if (cb) ++unmatched_b;
    }
    return 6 - (std::max)(unmatched_a, unmatched_b) >= minimum ? 1 : 0;
}

struct MraCompareWorker : public Worker {
    const StringView* a;
    const StringView* b;
    RVector<int> out;

    MraCompareWorker(const StringView* a_, const StringView* b_,
                     LogicalVector& out_)
        : a(a_), b(b_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string scratch_a, scratch_b, code_a, code_b;
        for (std::size_t i = begin; i < end; ++i) {
            if (a[i].is_na() || b[i].is_na()) {
                out[i] = NA_LOGICAL;
                continue;
            }
            const StringView va = fold_for_phonetics(a[i], scratch_a);
            const StringView vb = fold_for_phonetics(b[i], scratch_b);
            if (!mra_codex(va.data, va.size, code_a) ||
                !mra_codex(vb.data, vb.size, code_b)) {
                out[i] = NA_LOGICAL;
                continue;
            }
            out[i] = mra_compare_codes(code_a, code_b);
        }
    }
};

// --- Statistics Canada name coding (phonics::statcan) ----------------------

struct StatcanEncoder {
    std::size_t max_length;

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        std::string word;
        if (!clean_ascii_letters(input, length, word)) return false;
        code.assign(1, word[0]);
        for (std::size_t i = 1; i < word.size(); ++i)
            if (!is_vowel(word[i]) && word[i] != 'Y') code.push_back(word[i]);
        collapse_runs(code);
        if (code.size() > max_length) code.resize(max_length);
        return true;
    }
};

// --- Lein name coding (phonics::lein) --------------------------------------

struct LeinEncoder {
    std::size_t max_length;

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        std::string word;
        if (!clean_ascii_letters(input, length, word)) return false;
        std::string rest;
        for (std::size_t i = 1; i < word.size(); ++i)
            if (!one_of(word[i], "AEIOUYWH")) rest.push_back(word[i]);
        collapse_runs(rest);
        code.assign(1, word[0]);
        for (char c : rest) {
            if (one_of(c, "DT")) code.push_back('1');
            else if (one_of(c, "MN")) code.push_back('2');
            else if (one_of(c, "LR")) code.push_back('3');
            else if (one_of(c, "BFPV")) code.push_back('4');
            else code.push_back('5');  // C J K G Q S X Z
        }
        pad_zeros(code, max_length);
        return true;
    }
};

// --- Phonex (Lait & Randell 1996; phonics::phonex) -------------------------

struct PhonexEncoder {
    std::size_t max_length;

    // Replaces each R (or L) followed by a letter of AEHIOUWY, or ending the
    // string, with `digit` (the following letter goes too) and deletes the
    // others: "R[AEHIOUWY]|R$" -> 6, then R -> "".
    static void liquid(std::string& s, char letter, char digit) {
        std::string out;
        for (std::size_t i = 0; i < s.size(); ++i) {
            if (s[i] != letter) {
                out.push_back(s[i]);
                continue;
            }
            if (i + 1 < s.size() && one_of(s[i + 1], "AEHIOUWY")) {
                out.push_back(digit);
                ++i;
            } else if (i + 1 == s.size()) {
                out.push_back(digit);
            }
        }
        s.swap(out);
    }

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        std::string word;
        if (!clean_ascii_letters(input, length, word)) return false;
        while (!word.empty() && word.back() == 'S') word.pop_back();
        // Initial-letter rules, each applied once, in order.
        if (word.compare(0, 2, "KN") == 0) word.replace(0, 2, "N");
        if (word.compare(0, 2, "WR") == 0) word.replace(0, 2, "R");
        if (word.compare(0, 2, "PH") == 0) word.replace(0, 2, "F");
        if (!word.empty() && word[0] == 'H') word.erase(0, 1);
        if (!word.empty() && one_of(word[0], "EIOUY")) word[0] = 'A';
        if (!word.empty() && word[0] == 'P') word[0] = 'B';
        if (!word.empty() && word[0] == 'V') word[0] = 'F';
        if (!word.empty() && one_of(word[0], "KQ")) word[0] = 'C';
        if (!word.empty() && word[0] == 'J') word[0] = 'G';
        if (!word.empty() && word[0] == 'Z') word[0] = 'S';
        if (word.empty()) {
            code.clear();
            return true;
        }
        std::string rest = word.substr(1);
        liquid(rest, 'R', '6');
        liquid(rest, 'L', '4');
        std::string step;
        for (char c : rest)
            if (!one_of(c, "AEHIOUWY")) step.push_back(c);
        rest.clear();
        for (std::size_t i = 0; i < step.size(); ++i) {
            const char c = step[i];
            if (one_of(c, "MN")) {  // [MN][DG]* -> 5
                rest.push_back('5');
                while (i + 1 < step.size() && one_of(step[i + 1], "DG")) ++i;
            } else if (one_of(c, "BFPV")) {
                rest.push_back('1');
            } else {
                rest.push_back(c);
            }
        }
        step.clear();
        for (std::size_t i = 0; i < rest.size(); ++i) {  // [DT]C -> C
            if (one_of(rest[i], "DT") && i + 1 < rest.size() && rest[i + 1] == 'C') {
                step.push_back('C');
                ++i;
            } else {
                step.push_back(rest[i]);
            }
        }
        for (char& c : step) {
            if (one_of(c, "DT")) c = '3';
            else if (one_of(c, "CGJKQSXZ")) c = '2';
        }
        collapse_runs(step);
        code.assign(1, word[0]);
        code += step;
        pad_zeros(code, max_length);
        if (all_zeros(code)) code.clear();
        return true;
    }
};

// --- Roger Root name coding (Root 1975; phonics::rogerroot) ----------------

struct RogerRootEncoder {
    std::size_t max_length;

    static void replace_all(std::string& s, const char* from, const char* to) {
        const std::size_t n = std::strlen(from);
        std::string out;
        std::size_t i = 0;
        while (i < s.size()) {
            if (s.compare(i, n, from) == 0) {
                out += to;
                i += n;
            } else {
                out.push_back(s[i++]);
            }
        }
        s.swap(out);
    }

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        static const char* const FIRST[][2] = {
            {"A", "1"}, {"B", "09"}, {"CE", "00"}, {"CH", "06"}, {"CI", "00"},
            {"CY", "00"}, {"C", "07"}, {"DG", "07"}, {"D", "01"}, {"E", "1"},
            {"F", "08"}, {"GF", "08"}, {"GM", "03"}, {"GN", "02"}, {"G", "07"},
            {"H", "2"}, {"I", "1"}, {"J", "3"}, {"KN", "02"}, {"K", "07"},
            {"L", "05"}, {"M", "03"}, {"N", "02"}, {"O", "1"}, {"PF", "08"},
            {"PH", "08"}, {"PN", "02"}, {"P", "09"}, {"Q", "07"}, {"R", "04"},
            {"SCH", "06"}, {"SH", "06"}, {"S", "00"}, {"TSCH", "06"},
            {"TSH", "06"}, {"TS", "00"}, {"T", "01"}, {"U", "1"}, {"V", "08"},
            {"WR", "04"}, {"W", "4"}, {"X", "07"}, {"Y", "5"}, {"Z", "00"}};
        static const char* const REST[][2] = {
            {"B", "9"}, {"CE", "0"}, {"CH", "6"}, {"CI", "0"}, {"CY", "0"},
            {"C", "7"}, {"DG", "7"}, {"D", "1"}, {"F", "8"}, {"G", "7"},
            {"J", "6"}, {"K", "7"}, {"L", "5"}, {"M", "3"}, {"N", "2"},
            {"PH", "8"}, {"P", "8"}, {"Q", "7"}, {"R", "4"}, {"SCH", "6"},
            {"SH", "6"}, {"S", "0"}, {"TSCH", "6"}, {"TSH", "6"}, {"TS", "0"},
            {"T", "1"}, {"V", "8"}, {"X", "7"}, {"Z", "0"}};
        std::string word;
        if (!clean_ascii_letters(input, length, word)) return false;
        // The first rule (in table order) matching the start of the name.
        for (const auto& rule : FIRST) {
            const std::size_t n = std::strlen(rule[0]);
            if (word.compare(0, n, rule[0]) == 0) {
                word.replace(0, n, rule[1]);
                break;
            }
        }
        // Then every rule over the whole name, one after another.
        for (const auto& rule : REST) replace_all(word, rule[0], rule[1]);
        // Runs of one non-zero digit collapse; then any character followed
        // by two or more zeros becomes one zero (phonics' ".([0])\\1+").
        std::string step;
        for (char c : word) {
            if (c >= '1' && c <= '9' && !step.empty() && step.back() == c)
                continue;
            step.push_back(c);
        }
        word.clear();
        for (std::size_t i = 0; i < step.size();) {
            if (i + 2 < step.size() && step[i + 1] == '0' && step[i + 2] == '0') {
                std::size_t j = i + 1;
                while (j < step.size() && step[j] == '0') ++j;
                word.push_back('0');
                i = j;
            } else {
                word.push_back(step[i++]);
            }
        }
        code.clear();
        for (char c : word)
            if (c >= '0' && c <= '9') code.push_back(c);
        pad_zeros(code, max_length);
        if (all_zeros(code)) code.clear();
        return true;
    }
};

// --- ONCA (Oxford Name Compression Algorithm): NYSIIS, then Soundex ---------

struct OncaEncoder {
    std::size_t max_length;

    bool operator()(const char* input, std::size_t length,
                    std::string& code) const {
        std::string key;
        if (!nysiis_code(input, length, key, 0)) return false;
        code.clear();
        // NYSIIS can strip a name to nothing ("Aess"): no code either.
        if (key.empty()) return true;
        // American Soundex of the NYSIIS key, `max_length` characters.
        code.assign(1, key[0]);
        int previous = soundex_digit(key[0]);
        for (std::size_t i = 1; i < key.size() && code.size() < max_length; ++i) {
            const char value = key[i];
            if (value == 'H' || value == 'W') continue;
            const int digit = soundex_digit(value);
            if (digit != 0 && digit != previous)
                code.push_back(static_cast<char>('0' + digit));
            previous = digit;
        }
        pad_zeros(code, max_length);
        return true;
    }
};

// [[Rcpp::export]]
CharacterVector fast_metaphone_impl(const StringVector& x, int max_length,
                                    int nthreads) {
    const MetaphoneEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_mra_encode_impl(const StringVector& x, int nthreads) {
    const MraEncoder encode{};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
LogicalVector fast_mra_compare_impl(const StringVector& a,
                                    const StringVector& b, int nthreads) {
    if (a.size() != b.size()) stop("`a` and `b` must have the same length.");
    const StringSnapshot sa(a, SnapshotText::utf8), sb(b, SnapshotText::utf8);
    LogicalVector out = no_init(a.size());
    MraCompareWorker worker(sa.data(), sb.data(), out);
    dispatch_for(0, sa.size(), worker,
                 phonetic_work(sa) + phonetic_work(sb), 10000, nthreads);
    return out;
}

// [[Rcpp::export]]
CharacterVector fast_statcan_impl(const StringVector& x, int max_length,
                                  int nthreads) {
    const StatcanEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_lein_impl(const StringVector& x, int max_length,
                               int nthreads) {
    const LeinEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_phonex_impl(const StringVector& x, int max_length,
                                 int nthreads) {
    const PhonexEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_roger_root_impl(const StringVector& x, int max_length,
                                     int nthreads) {
    const RogerRootEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}

// [[Rcpp::export]]
CharacterVector fast_onca_impl(const StringVector& x, int max_length,
                               int nthreads) {
    const OncaEncoder encode{static_cast<std::size_t>(max_length)};
    return run_phonetic(x, encode, nthreads);
}
