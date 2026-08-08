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
#include "parallel_dispatch.h"
#include "string_snapshot.h"

using namespace Rcpp;
using namespace RcppParallel;

static const std::uint8_t PHONETIC_NA_LENGTH = 255;

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
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na() ||
                !soundex_code(value.data, value.size, bytes + i * 4u)) {
                lengths[i] = PHONETIC_NA_LENGTH;
            } else {
                lengths[i] = 4;
            }
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_soundex_impl(const StringVector& x) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 4));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    SoundexWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000);

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

static inline void nysiis_push(char value,
                               std::string& key,
                               char& previous) {
    if (value != previous) {
        key.push_back(value);
        previous = value;
    }
}

static bool nysiis_code(const char* input,
                        std::size_t length,
                        std::string& output) {
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
    char previous = word[0];
    n = word.size();
    for (std::size_t i = 1; i < n;) {
        const char value = word[i];
        const char before = word[i - 1];
        const char after = i + 1 < n ? word[i + 1] : '\0';
        if (value == 'E' && after == 'V') {
            nysiis_push('A', key, previous);
            nysiis_push('F', key, previous);
            i += 2;
            continue;
        }

        char translated;
        if (is_vowel(value))
            translated = 'A';
        else if (value == 'Q')
            translated = 'G';
        else if (value == 'Z')
            translated = 'S';
        else if (value == 'M')
            translated = 'N';
        else if (value == 'K')
            translated = after == 'N' ? 'N' : 'C';
        else if (value == 'H')
            translated = !is_vowel(before) ||
                (after != '\0' && !is_vowel(after)) ? before : value;
        else if (value == 'W' && is_vowel(before))
            translated = 'A';
        else
            translated = value;

        nysiis_push(translated, key, previous);
        ++i;
    }

    if (key.size() > 1 && key.back() == 'S')
        key.pop_back();
    if (key.size() >= 2 &&
        key[key.size() - 2] == 'A' && key.back() == 'Y')
        key.erase(key.size() - 2, 1);
    if (key.size() > 1 && key.back() == 'A')
        key.pop_back();
    if (key.size() > 6)
        key.resize(6);
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
        std::string code;
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            code.clear();
            if (value.is_na() || !nysiis_code(value.data, value.size, code)) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            lengths[i] = static_cast<std::uint8_t>(code.size());
            std::memcpy(bytes + i * 6u, code.data(), code.size());
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_nysiis_impl(const StringVector& x) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 6));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    NysiisWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000);

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

static bool clean_cologne_letters(const char* input,
                                  std::size_t length,
                                  std::string& output) {
    output.clear();
    output.reserve(length);
    for (std::size_t i = 0; i < length; ++i) {
        const unsigned char value = static_cast<unsigned char>(input[i]);
        if (is_alpha_ascii(value)) {
            output.push_back(to_upper_ascii(value));
            continue;
        }
        if (value == 0xC3 && i + 1 < length) {
            const unsigned char next = static_cast<unsigned char>(input[i + 1]);
            if (next == 0x84 || next == 0xA4) output.push_back('A');
            else if (next == 0x96 || next == 0xB6) output.push_back('O');
            else if (next == 0x9C || next == 0xBC) output.push_back('U');
            else if (next == 0x9F) output.append("SS");
            else continue;
            ++i;
        } else if (value == 0xE1 && i + 2 < length &&
                   static_cast<unsigned char>(input[i + 1]) == 0xBA &&
                   static_cast<unsigned char>(input[i + 2]) == 0x9E) {
            output.append("SS");
            i += 2;
        }
    }
    return !output.empty();
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
    std::string word;
    if (!clean_cologne_letters(input, length, word)) return false;

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
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na() ||
                !encode(value.data, value.size, output[i])) {
                missing[i] = 1;
            }
        }
    }
};

static CharacterVector run_dynamic_phonetic(const StringVector& x,
                                            PhoneticStringFunction encode) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<std::string> encoded(n);
    std::vector<std::uint8_t> missing(n, 0);
    DynamicPhoneticWorker worker(
        snapshot.data(), encode, encoded, missing
    );
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000);

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
CharacterVector fast_refined_soundex_impl(const StringVector& x) {
    return run_dynamic_phonetic(x, refined_soundex_code);
}

// [[Rcpp::export]]
CharacterVector fast_cologne_impl(const StringVector& x) {
    return run_dynamic_phonetic(x, cologne_code);
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
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                primary_lengths[i] = PHONETIC_NA_LENGTH;
                secondary_lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
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
List fast_double_metaphone_impl(const StringVector& x) {
    const StringSnapshot snapshot(x);
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
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000);

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
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                lengths[i] = PHONETIC_NA_LENGTH;
                continue;
            }
            const std::string code = caverphone2_code(
                std::string(value.data, value.size)
            );
            lengths[i] = static_cast<std::uint8_t>(code.size());
            std::memcpy(bytes + i * 10u, code.data(), code.size());
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_caverphone_impl(const StringVector& x) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<char> bytes(checked_slots(n, 10));
    std::vector<std::uint8_t> lengths(n, PHONETIC_NA_LENGTH);
    CaverphoneWorker worker(snapshot.data(), bytes.data(), lengths.data());
    dispatch_for(0, n, worker, phonetic_work(snapshot), 10000);

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
