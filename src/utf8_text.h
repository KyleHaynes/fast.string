#ifndef FAST_STRING_UTF8_TEXT_H
#define FAST_STRING_UTF8_TEXT_H

// Small UTF-8 helpers shared by the text-transforming functions (fsquish(),
// ascii_fold(), the fingerprints, the token-based metrics). Nothing here
// touches the R API.

#include <cstddef>
#include <cstdint>
#include <string>

// Decodes the code point starting at s[i] into `point` and returns its
// width in bytes, or 0 when s[i] does not start a valid UTF-8 sequence
// (overlong forms, surrogates and values above U+10FFFF are invalid).
static inline std::size_t utf8_decode(const unsigned char* s, std::size_t n,
                                      std::size_t i, std::uint32_t& point) {
    const unsigned char c = s[i];
    if (c < 0x80) {
        point = c;
        return 1;
    }
    std::size_t width;
    std::uint32_t value;
    if (c >= 0xC2 && c <= 0xDF) {
        width = 2;
        value = c & 0x1F;
    } else if (c >= 0xE0 && c <= 0xEF) {
        width = 3;
        value = c & 0x0F;
    } else if (c >= 0xF0 && c <= 0xF4) {
        width = 4;
        value = c & 0x07;
    } else {
        return 0;
    }
    if (i + width > n) return 0;
    for (std::size_t k = 1; k < width; ++k) {
        const unsigned char next = s[i + k];
        if ((next & 0xC0) != 0x80) return 0;
        value = (value << 6) | (next & 0x3F);
    }
    if ((width == 3 && value < 0x800) || (width == 4 && value < 0x10000) ||
        (value >= 0xD800 && value <= 0xDFFF) || value > 0x10FFFF)
        return 0;
    point = value;
    return width;
}

static inline void utf8_append(std::string& out, std::uint32_t point) {
    if (point < 0x80) {
        out.push_back(static_cast<char>(point));
    } else if (point < 0x800) {
        out.push_back(static_cast<char>(0xC0 | (point >> 6)));
        out.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    } else if (point < 0x10000) {
        out.push_back(static_cast<char>(0xE0 | (point >> 12)));
        out.push_back(static_cast<char>(0x80 | ((point >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (point >> 18)));
        out.push_back(static_cast<char>(0x80 | ((point >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((point >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (point & 0x3F)));
    }
}

// Unicode's White_Space property.
static inline bool is_white_space(std::uint32_t point) {
    if (point <= 0x20) return point == 0x20 || (point >= 0x09 && point <= 0x0D);
    if (point < 0x85) return false;
    return point == 0x85 || point == 0xA0 || point == 0x1680 ||
        (point >= 0x2000 && point <= 0x200A) || point == 0x2028 ||
        point == 0x2029 || point == 0x202F || point == 0x205F ||
        point == 0x3000;
}

// Byte length of the White_Space character at data[i], or 0. With `utf8`
// false (strings marked "bytes") only ASCII whitespace counts.
static inline std::size_t whitespace_width(const unsigned char* data,
                                           std::size_t i, std::size_t size,
                                           bool utf8) {
    const unsigned char c = data[i];
    if (c == ' ' || (c >= 0x09 && c <= 0x0D)) return 1;
    if (!utf8 || c < 0xC2) return 0;
    if (c == 0xC2) {
        // U+0085 and U+00A0
        return i + 1 < size && (data[i + 1] == 0x85 || data[i + 1] == 0xA0)
            ? 2 : 0;
    }
    if (i + 2 >= size) return 0;
    const unsigned char c1 = data[i + 1];
    const unsigned char c2 = data[i + 2];
    if (c == 0xE1) return c1 == 0x9A && c2 == 0x80 ? 3 : 0;  // U+1680
    if (c == 0xE2) {
        if (c1 == 0x80)  // U+2000-200A, U+2028, U+2029, U+202F
            return (c2 >= 0x80 && c2 <= 0x8A) || c2 == 0xA8 || c2 == 0xA9 ||
                c2 == 0xAF ? 3 : 0;
        return c1 == 0x81 && c2 == 0x9F ? 3 : 0;  // U+205F
    }
    if (c == 0xE3) return c1 == 0x80 && c2 == 0x80 ? 3 : 0;  // U+3000
    return 0;
}

#endif // FAST_STRING_UTF8_TEXT_H
