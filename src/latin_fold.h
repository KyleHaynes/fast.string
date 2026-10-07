#ifndef FAST_STRING_LATIN_FOLD_H
#define FAST_STRING_LATIN_FOLD_H

#include <cstddef>
#include <cstdint>
#include <string>

// Uppercase ASCII spellings of the accented Latin letters that names carry:
// Latin-1 Supplement and Latin Extended-A (U+00C0-U+017F), Romanian S and T
// with comma below (U+0218-U+021B) and capital sharp s (U+1E9E). The
// phonetic encoders work on ASCII letters, so without this an accented
// letter was dropped and "Emile" and its accented spelling got different
// codes. "" marks the two non-letters in the block (multiplication and
// division signs), which are dropped.
static const char* const LATIN_FOLD_C0_17F[192] = {
    "A", "A", "A", "A", "A", "A", "AE", "C",  // U+00C0
    "E", "E", "E", "E", "I", "I", "I", "I",  // U+00C8
    "D", "N", "O", "O", "O", "O", "O", "",  // U+00D0
    "O", "U", "U", "U", "U", "Y", "TH", "SS",  // U+00D8
    "A", "A", "A", "A", "A", "A", "AE", "C",  // U+00E0
    "E", "E", "E", "E", "I", "I", "I", "I",  // U+00E8
    "D", "N", "O", "O", "O", "O", "O", "",  // U+00F0
    "O", "U", "U", "U", "U", "Y", "TH", "Y",  // U+00F8
    "A", "A", "A", "A", "A", "A", "C", "C",  // U+0100
    "C", "C", "C", "C", "C", "C", "D", "D",  // U+0108
    "D", "D", "E", "E", "E", "E", "E", "E",  // U+0110
    "E", "E", "E", "E", "G", "G", "G", "G",  // U+0118
    "G", "G", "G", "G", "H", "H", "H", "H",  // U+0120
    "I", "I", "I", "I", "I", "I", "I", "I",  // U+0128
    "I", "I", "IJ", "IJ", "J", "J", "K", "K",  // U+0130
    "K", "L", "L", "L", "L", "L", "L", "L",  // U+0138
    "L", "L", "L", "N", "N", "N", "N", "N",  // U+0140
    "N", "N", "N", "N", "O", "O", "O", "O",  // U+0148
    "O", "O", "OE", "OE", "R", "R", "R", "R",  // U+0150
    "R", "R", "S", "S", "S", "S", "S", "S",  // U+0158
    "S", "S", "T", "T", "T", "T", "T", "T",  // U+0160
    "U", "U", "U", "U", "U", "U", "U", "U",  // U+0168
    "U", "U", "U", "U", "W", "W", "Y", "Y",  // U+0170
    "Y", "Z", "Z", "Z", "Z", "Z", "Z", "S",  // U+0178
};

static inline const char* latin_fold_codepoint(std::uint32_t point) {
    if (point >= 0xC0 && point <= 0x17F) return LATIN_FOLD_C0_17F[point - 0xC0];
    if (point == 0x218 || point == 0x219) return "S";
    if (point == 0x21A || point == 0x21B) return "T";
    if (point == 0x1E9E) return "SS";
    return nullptr;
}

// Writes `input` to `out` with the letters above folded to uppercase ASCII;
// every other byte, including other non-ASCII characters, is copied as is.
// `c_cedilla` replaces C with cedilla (Double Metaphone codes it as "S").
// Returns false, leaving `out` untouched, when there was nothing to fold, so
// callers can go on using the input in place. Expects UTF-8.
static inline bool fold_latin_letters(const char* input, std::size_t length,
                                      std::string& out,
                                      const char* c_cedilla = "C") {
    std::size_t first = 0;
    while (first < length && static_cast<unsigned char>(input[first]) < 0x80)
        ++first;
    if (first == length) return false;

    out.assign(input, first);
    for (std::size_t i = first; i < length; ++i) {
        const unsigned char byte = static_cast<unsigned char>(input[i]);
        if (byte < 0x80) {
            out.push_back(static_cast<char>(byte));
            continue;
        }
        if ((byte & 0xE0) == 0xC0 && i + 1 < length &&
            (static_cast<unsigned char>(input[i + 1]) & 0xC0) == 0x80) {
            const std::uint32_t point =
                (static_cast<std::uint32_t>(byte & 0x1F) << 6) |
                (static_cast<unsigned char>(input[i + 1]) & 0x3F);
            const char* folded = latin_fold_codepoint(point);
            if (folded) {
                out.append(point == 0xC7 || point == 0xE7 ? c_cedilla : folded);
            } else {
                out.append(input + i, 2);
            }
            ++i;
            continue;
        }
        if (byte == 0xE1 && i + 2 < length &&
            static_cast<unsigned char>(input[i + 1]) == 0xBA &&
            static_cast<unsigned char>(input[i + 2]) == 0x9E) {
            out.append("SS");
            i += 2;
            continue;
        }
        out.push_back(static_cast<char>(byte));
    }
    return true;
}

#endif // FAST_STRING_LATIN_FOLD_H
