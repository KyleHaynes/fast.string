#ifndef FAST_STRING_FIXED_SEARCH_H
#define FAST_STRING_FIXED_SEARCH_H

#include <array>
#include <cctype>
#include <cstddef>
#include <cstring>
#include <string>

// Prepared byte-oriented literal search. The skip table is built once per
// pattern so repetitive long strings avoid the quadratic behaviour of a
// naive std::search-based literal scan.
class PreparedFixedSearch {
public:
    explicit PreparedFixedSearch(const std::string& pattern,
                                 bool ignore_case = false)
        : needle_(pattern),
          ignore_case_(ignore_case),
          strategy_(select_strategy(pattern.size())) {
        for (std::size_t i = 0; i < fold_.size(); ++i) {
            fold_[i] = ignore_case_
                ? static_cast<unsigned char>(std::tolower(static_cast<unsigned char>(i)))
                : static_cast<unsigned char>(i);
        }
        if (ignore_case_) {
            for (char& c : needle_)
                c = static_cast<char>(fold_[static_cast<unsigned char>(c)]);
        }

        const std::size_t m = needle_.size();
        skip_.fill(m == 0 ? 1 : m);
        if (m >= 2) {
            for (std::size_t i = 0; i + 1 < m; ++i)
                skip_[static_cast<unsigned char>(needle_[i])] = m - 1 - i;
        }
    }

    bool empty() const noexcept { return needle_.empty(); }
    std::size_t size() const noexcept { return needle_.size(); }

    std::size_t find(const char* haystack, std::size_t haystack_size,
                     std::size_t start = 0) const noexcept {
        if (start > haystack_size) return std::string::npos;
        const std::size_t m = needle_.size();
        if (m == 0) return start;
        if (m > haystack_size - start) return std::string::npos;

        switch (strategy_) {
            case Strategy::one_byte:
                return find_one(haystack, haystack_size, start);
            case Strategy::short_needle:
                return find_short(haystack, haystack_size, start);
            case Strategy::bmh:
                return find_bmh(haystack, haystack_size, start);
            case Strategy::empty:
                return start;
        }
        return std::string::npos;
    }

private:
    enum class Strategy : unsigned char {
        empty,
        one_byte,
        short_needle,
        bmh
    };

    static Strategy select_strategy(std::size_t size) noexcept {
        if (size == 0) return Strategy::empty;
        if (size == 1) return Strategy::one_byte;
        if (size <= 3) return Strategy::short_needle;
        return Strategy::bmh;
    }

    unsigned char fold(unsigned char c) const noexcept {
        return fold_[c];
    }

    std::size_t find_one(const char* haystack, std::size_t haystack_size,
                         std::size_t start) const noexcept {
        const unsigned char target =
            static_cast<unsigned char>(needle_[0]);
        if (!ignore_case_) {
            const void* found = std::memchr(
                haystack + start, target, haystack_size - start
            );
            return found
                ? static_cast<const char*>(found) - haystack
                : std::string::npos;
        }

        for (std::size_t i = start; i < haystack_size; ++i) {
            if (fold(static_cast<unsigned char>(haystack[i])) == target)
                return i;
        }
        return std::string::npos;
    }

    std::size_t find_short(const char* haystack, std::size_t haystack_size,
                           std::size_t start) const noexcept {
        const std::size_t m = needle_.size();
        const std::size_t last_start = haystack_size - m;
        if (!ignore_case_) {
            const unsigned char first =
                static_cast<unsigned char>(needle_[0]);
            std::size_t pos = start;
            while (pos <= last_start) {
                const void* found = std::memchr(
                    haystack + pos, first, last_start - pos + 1
                );
                if (!found) return std::string::npos;
                pos = static_cast<const char*>(found) - haystack;
                if (std::memcmp(haystack + pos, needle_.data(), m) == 0)
                    return pos;
                ++pos;
            }
            return std::string::npos;
        }

        for (std::size_t pos = start; pos <= last_start; ++pos) {
            std::size_t j = 0;
            while (j < m &&
                   fold(static_cast<unsigned char>(haystack[pos + j])) ==
                       static_cast<unsigned char>(needle_[j])) {
                ++j;
            }
            if (j == m) return pos;
        }
        return std::string::npos;
    }

    std::size_t find_bmh(const char* haystack, std::size_t haystack_size,
                         std::size_t start) const noexcept {
        const std::size_t m = needle_.size();
        const std::size_t last_start = haystack_size - m;
        std::size_t pos = start;

        while (pos <= last_start) {
            const unsigned char tail =
                fold(static_cast<unsigned char>(haystack[pos + m - 1]));
            if (tail == static_cast<unsigned char>(needle_[m - 1])) {
                if (!ignore_case_) {
                    if (std::memcmp(haystack + pos, needle_.data(), m - 1) == 0)
                        return pos;
                } else {
                    std::size_t j = 0;
                    while (j + 1 < m &&
                           fold(static_cast<unsigned char>(haystack[pos + j])) ==
                               static_cast<unsigned char>(needle_[j])) {
                        ++j;
                    }
                    if (j + 1 == m) return pos;
                }
            }

            const std::size_t shift = skip_[tail];
            if (shift > last_start - pos) break;
            pos += shift;
        }
        return std::string::npos;
    }

    std::string needle_;
    bool ignore_case_;
    Strategy strategy_;
    std::array<unsigned char, 256> fold_;
    std::array<std::size_t, 256> skip_;
};

#endif // FAST_STRING_FIXED_SEARCH_H
