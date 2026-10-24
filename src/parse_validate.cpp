// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "iban_table.h"
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>
#include <vector>

// std::from_chars() for doubles: libstdc++ from GCC 11, libc++ once it
// defines the feature macro.
#if defined(__has_include)
#if __has_include(<charconv>)
#include <charconv>
#include <system_error>
#endif
#endif
#if defined(__cpp_lib_to_chars) && __cpp_lib_to_chars >= 201611L
#define FAST_STRING_FROM_CHARS_DOUBLE 1
#else
#define FAST_STRING_FROM_CHARS_DOUBLE 0
#endif

using namespace Rcpp;
using namespace RcppParallel;

// fparse_number() and valid_id(). Both write one number per string, so the
// workers fill the result directly.

namespace {

inline bool is_digit(unsigned char c) { return c >= '0' && c <= '9'; }

inline bool ascii_alnum(unsigned char c) {
    return is_digit(c) || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

inline bool mark_at(const unsigned char* s, std::size_t n, std::size_t i,
                    const std::string& mark) {
    return !mark.empty() && i + mark.size() <= n &&
        std::memcmp(s + i, mark.data(), mark.size()) == 0;
}

std::size_t row_work(const StringSnapshot& snapshot) {
    return static_cast<std::size_t>(
        (std::min)(snapshot.total_bytes() + 8u * snapshot.size(),
                   static_cast<std::uint64_t>(
                       (std::numeric_limits<std::size_t>::max)())));
}

// ---------------------------------------------------------------------------
// fparse_number()
// ---------------------------------------------------------------------------

struct NumberSyntax {
    std::string decimal;
    std::vector<std::string> grouping;
};

// Converts "[-]digits[.digits][e[-]digits]" to the nearest double. Most
// values take Clinger's fast path -- at most 19 significant digits forming
// an integer m <= 2^53, and a decimal exponent e with |e| <= 22, so m and
// 10^|e| are exact doubles and one IEEE multiplication or division rounds
// correctly. The rest go to std::from_chars() where the standard library
// has it for doubles, else strtod(). (strtod() alone is not an option: on
// Windows it takes a lock on the locale, so threads made it slower.)
double decimal_to_double(const std::string& text) {
    static const double powers[23] = {
        1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
        1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
    };
    const char* p = text.data();
    const char* const end = p + text.size();
    const bool negative = p < end && *p == '-';
    if (negative) ++p;
    std::uint64_t mantissa = 0;
    int digits = 0;
    long exponent = 0;
    bool exact = true;
    for (; p < end && is_digit(*p); ++p) {
        if (mantissa == 0 && *p == '0') continue;
        if (digits == 19) {
            exact = false;
            break;
        }
        mantissa = mantissa * 10 + static_cast<std::uint64_t>(*p - '0');
        ++digits;
    }
    if (exact && p < end && *p == '.') {
        for (++p; p < end && is_digit(*p); ++p) {
            if (mantissa == 0 && *p == '0') {
                --exponent;
                continue;
            }
            if (digits == 19) {
                exact = false;
                break;
            }
            mantissa = mantissa * 10 + static_cast<std::uint64_t>(*p - '0');
            ++digits;
            --exponent;
        }
    }
    if (exact && p < end && *p == 'e') {
        ++p;
        const bool exp_negative = p < end && *p == '-';
        if (exp_negative) ++p;
        long value = 0;
        for (; p < end && is_digit(*p); ++p)
            if (value < 100000) value = value * 10 + (*p - '0');
        exponent += exp_negative ? -value : value;
    }
    if (exact) {
        if (mantissa == 0) return negative ? -0.0 : 0.0;
        if (mantissa <= (static_cast<std::uint64_t>(1) << 53) &&
            exponent >= -22 && exponent <= 22) {
            double value = static_cast<double>(mantissa);
            value = exponent < 0 ? value / powers[-exponent]
                                 : value * powers[exponent];
            return negative ? -value : value;
        }
    }
#if FAST_STRING_FROM_CHARS_DOUBLE
    double value = 0;
    const std::from_chars_result r =
        std::from_chars(text.data(), text.data() + text.size(), value);
    if (r.ec == std::errc()) return value;
#endif
    // R keeps LC_NUMERIC at "C", so strtod() reads '.' as the decimal point;
    // it also gives the overflow (+-Inf) and underflow results.
    return std::strtod(text.c_str(), nullptr);
}

// The first number in s, or NA. See ?fparse_number for the grammar.
double parse_first_number(const StringView& value, const NumberSyntax& syntax,
                          std::string& buffer) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    const std::string& decimal = syntax.decimal;
    std::size_t start = n;
    for (std::size_t i = 0; i < n; ++i) {
        if (is_digit(s[i]) ||
            (mark_at(s, n, i, decimal) && i + decimal.size() < n &&
             is_digit(s[i + decimal.size()]))) {
            start = i;
            break;
        }
    }
    if (start == n) return NA_REAL;

    // A sign before the number, possibly separated from it by spaces,
    // currency symbols or other punctuation, but not one that joins two
    // words or codes ("SKU-123").
    bool negative = false;
    for (std::size_t k = start; k > 0; --k) {
        const unsigned char c = s[k - 1];
        if (k >= 3 && s[k - 3] == 0xE2 && s[k - 2] == 0x88 && c == 0x92) {
            negative = k == 3 || !ascii_alnum(s[k - 4]);  // U+2212 MINUS SIGN
            break;
        }
        if (c == '-' || c == '+') {
            negative = c == '-' && (k == 1 || !ascii_alnum(s[k - 2]));
            break;
        }
        if (ascii_alnum(c)) break;
    }

    buffer.clear();
    if (negative) buffer.push_back('-');
    std::size_t i = start;
    std::size_t int_digits = 0;
    while (i < n) {
        if (is_digit(s[i])) {
            buffer.push_back(static_cast<char>(s[i]));
            ++i;
            ++int_digits;
            continue;
        }
        bool grouped = false;
        if (int_digits > 0) {
            for (const std::string& mark : syntax.grouping) {
                if (mark_at(s, n, i, mark) && i + mark.size() < n &&
                    is_digit(s[i + mark.size()])) {
                    i += mark.size();
                    grouped = true;
                    break;
                }
            }
        }
        if (!grouped) break;
    }
    if (mark_at(s, n, i, decimal)) {
        const std::size_t j = i + decimal.size();
        if (j < n && is_digit(s[j])) {
            buffer.push_back('.');
            i = j;
            while (i < n && is_digit(s[i]))
                buffer.push_back(static_cast<char>(s[i++]));
        }
    }
    if (i < n && (s[i] == 'e' || s[i] == 'E')) {
        std::size_t j = i + 1;
        const bool exp_negative = j < n && s[j] == '-';
        if (j < n && (s[j] == '+' || s[j] == '-')) ++j;
        if (j < n && is_digit(s[j])) {
            buffer.push_back('e');
            if (exp_negative) buffer.push_back('-');
            while (j < n && is_digit(s[j]))
                buffer.push_back(static_cast<char>(s[j++]));
        }
    }
    return decimal_to_double(buffer);
}

struct ParseNumberWorker : public Worker {
    const StringView* strings;
    const NumberSyntax& syntax;
    double* out;

    ParseNumberWorker(const StringView* strings_, const NumberSyntax& syntax_,
                      double* out_)
        : strings(strings_), syntax(syntax_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string buffer;
        for (std::size_t i = begin; i < end; ++i) {
            out[i] = strings[i].is_na()
                ? NA_REAL
                : parse_first_number(strings[i], syntax, buffer);
        }
    }
};

// ---------------------------------------------------------------------------
// valid_id(). The rules follow python-stdnum's luhn, au.abn, au.acn, au.tfn,
// isbn, ean and iban modules; Medicare follows Services Australia's
// published check-digit rule.
// ---------------------------------------------------------------------------

enum IdType {
    ID_LUHN = 0, ID_ABN, ID_ACN, ID_TFN, ID_MEDICARE, ID_ISBN, ID_EAN, ID_IBAN
};

inline int floor_mod(long long a, long long m) {
    const long long r = a % m;
    return static_cast<int>(r < 0 ? r + m : r);
}

bool all_digits(const std::string& s, std::size_t from = 0,
                std::size_t to = std::string::npos) {
    if (to == std::string::npos) to = s.size();
    for (std::size_t i = from; i < to; ++i)
        if (!is_digit(static_cast<unsigned char>(s[i]))) return false;
    return true;
}

inline int digit(const std::string& s, std::size_t i) { return s[i] - '0'; }

bool luhn_ok(const std::string& s) {
    if (s.empty() || !all_digits(s)) return false;
    int sum = 0;
    bool twice = false;
    for (std::size_t k = s.size(); k-- > 0;) {
        int d = digit(s, k);
        if (twice) {
            d *= 2;
            if (d > 9) d -= 9;
        }
        sum += d;
        twice = !twice;
    }
    return sum % 10 == 0;
}

bool ean_ok(const std::string& s) {
    const std::size_t n = s.size();
    if (!all_digits(s) || !(n == 8 || n == 12 || n == 13 || n == 14))
        return false;
    int sum = 0;
    // Weights 3, 1, 3, ... from the digit before the check digit leftwards.
    for (std::size_t k = 0; k + 1 < n; ++k) {
        const std::size_t from_right = n - 2 - k;
        sum += digit(s, from_right) * (k % 2 == 0 ? 3 : 1);
    }
    return floor_mod(10 - sum, 10) == digit(s, n - 1);
}

int iban_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return -1;
}

bool iban_ok(const std::string& s) {
    const std::size_t n = s.size();
    if (n < 4) return false;
    // ISO 7064 mod 97-10 over number[4:] + number[:4], letters as 10..35.
    int mod = 0;
    for (std::size_t k = 0; k < n; ++k) {
        const int v = iban_value(s[(k + 4) % n]);
        if (v < 0) return false;
        mod = v >= 10 ? (mod * 100 + v) % 97 : (mod * 10 + v) % 97;
    }
    if (mod != 1) return false;
    const IbanCountry* country = nullptr;
    for (std::size_t c = 0; c < IBAN_COUNTRY_COUNT; ++c) {
        if (IBAN_COUNTRIES[c].code[0] == s[0] &&
            IBAN_COUNTRIES[c].code[1] == s[1]) {
            country = &IBAN_COUNTRIES[c];
            break;
        }
    }
    if (!country) return false;
    const char* bban = country->bban;
    if (std::strlen(bban) != n - 4) return false;
    for (std::size_t k = 4; k < n; ++k) {
        const char c = s[k];
        const char kind = bban[k - 4];
        const bool is_num = c >= '0' && c <= '9';
        const bool is_upper = c >= 'A' && c <= 'Z';
        if (kind == 'n' ? !is_num : kind == 'a' ? !is_upper
                                                : !(is_num || is_upper))
            return false;
    }
    return true;
}

// TRUE/FALSE, or NA for an 8-digit TFN (whose weights are not published).
int check_id(const StringView& value, int type, std::string& s) {
    s.clear();
    for (std::size_t i = 0; i < value.size; ++i) {
        const char c = value.data[i];
        if (c == ' ' || c == '-' || c == '\t' || c == '\n' || c == '\r' ||
            c == '\f' || c == '\v' || (type == ID_IBAN && c == '.'))
            continue;
        s.push_back(type == ID_ISBN || type == ID_IBAN
                        ? static_cast<char>(c >= 'a' && c <= 'z' ? c - 32 : c)
                        : c);
    }
    const std::size_t n = s.size();
    switch (type) {
        case ID_LUHN:
            return luhn_ok(s);
        case ID_ABN: {
            if (n != 11 || !all_digits(s)) return false;
            static const int weights[9] = {3, 5, 7, 9, 11, 13, 15, 17, 19};
            long long sum = 0;
            for (int k = 0; k < 9; ++k) sum -= weights[k] * digit(s, 2 + k);
            return 11 + floor_mod(sum - 1, 89) == digit(s, 0) * 10 + digit(s, 1);
        }
        case ID_ACN: {
            if (n != 9 || !all_digits(s)) return false;
            long long sum = 0;
            for (int k = 0; k < 8; ++k) sum += digit(s, k) * (k - 8);
            return floor_mod(sum, 10) == digit(s, 8);
        }
        case ID_TFN: {
            if (!all_digits(s)) return false;
            if (n == 8) return NA_LOGICAL;
            if (n != 9) return false;
            static const int weights[9] = {1, 4, 3, 7, 5, 8, 6, 9, 10};
            int sum = 0;
            for (int k = 0; k < 9; ++k) sum += weights[k] * digit(s, k);
            return sum % 11 == 0;
        }
        case ID_MEDICARE: {
            if ((n != 10 && n != 11) || !all_digits(s)) return false;
            if (s[0] < '2' || s[0] > '6') return false;
            static const int weights[8] = {1, 3, 7, 9, 1, 3, 7, 9};
            int sum = 0;
            for (int k = 0; k < 8; ++k) sum += weights[k] * digit(s, k);
            return sum % 10 == digit(s, 8);
        }
        case ID_ISBN: {
            if (n == 9) s.insert(s.begin(), '0');  // an SBN
            const std::size_t m = s.size();
            if (m == 0 || !all_digits(s, 0, m - 1)) return false;
            if (m == 10) {
                int sum = 0;
                for (int k = 0; k < 9; ++k) sum += (k + 1) * digit(s, k);
                const int check = sum % 11;
                return check == 10 ? s[9] == 'X' : s[9] == '0' + check;
            }
            if (m == 13) {
                return ean_ok(s) &&
                    (s.compare(0, 3, "978") == 0 || s.compare(0, 3, "979") == 0);
            }
            return false;
        }
        case ID_EAN:
            return ean_ok(s);
        case ID_IBAN:
            return iban_ok(s);
    }
    return false;
}

struct ValidIdWorker : public Worker {
    const StringView* strings;
    int type;
    int* out;

    ValidIdWorker(const StringView* strings_, int type_, int* out_)
        : strings(strings_), type(type_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string buffer;
        for (std::size_t i = begin; i < end; ++i) {
            out[i] = strings[i].is_na() ? NA_LOGICAL
                                        : check_id(strings[i], type, buffer);
        }
    }
};

}  // namespace

// [[Rcpp::export]]
NumericVector fast_parse_number_impl(const StringVector& x,
                                     const std::string& decimal_mark,
                                     const StringVector& grouping_marks,
                                     int nthreads) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    NumberSyntax syntax;
    syntax.decimal = decimal_mark;
    for (R_xlen_t k = 0; k < grouping_marks.size(); ++k) {
        const std::string mark = Rcpp::as<std::string>(grouping_marks[k]);
        if (!mark.empty()) syntax.grouping.push_back(mark);
    }
    NumericVector result(Rcpp::no_init(x.size()));
    ParseNumberWorker worker(snapshot.data(), syntax, REAL(result));
    dispatch_for(0, snapshot.size(), worker, row_work(snapshot), 100000,
                 nthreads, 2048);
    return result;
}

// [[Rcpp::export]]
LogicalVector fast_valid_id_impl(const StringVector& x, int type,
                                 int nthreads) {
    StringSnapshot snapshot(x);
    LogicalVector result(Rcpp::no_init(x.size()));
    ValidIdWorker worker(snapshot.data(), type, LOGICAL(result));
    dispatch_for(0, snapshot.size(), worker, row_work(snapshot), 100000,
                 nthreads, 2048);
    return result;
}
