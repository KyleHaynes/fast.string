#ifndef FAST_STRING_CODEPOINT_SNAPSHOT_H
#define FAST_STRING_CODEPOINT_SNAPSHOT_H

#include <Rcpp.h>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

struct CodepointView {
    const std::uint32_t* data;
    std::size_t size;
    bool missing;
    bool ascii;

    bool is_na() const noexcept { return missing; }
};

// Decode on the calling R thread. Workers only receive immutable integer
// views and therefore never call R's encoding or allocation APIs.
class CodepointSnapshot {
public:
    CodepointSnapshot(const Rcpp::StringVector& input,
                      const char* argument_name)
        : offsets_(static_cast<std::size_t>(input.size())),
          lengths_(static_cast<std::size_t>(input.size())),
          missing_(static_cast<std::size_t>(input.size()), false),
          ascii_(static_cast<std::size_t>(input.size()), true) {
        const std::size_t n = static_cast<std::size_t>(input.size());
        for (std::size_t i = 0; i < n; ++i) {
            SEXP value = STRING_ELT(input, static_cast<R_xlen_t>(i));
            offsets_[i] = codepoints_.size();
            if (value == NA_STRING) {
                missing_[i] = true;
                continue;
            }
            if (Rf_getCharCE(value) == CE_BYTES) {
                Rcpp::stop("`%s[%llu]` is bytes-encoded and cannot be compared as Unicode; use `use_bytes = TRUE`.",
                           argument_name,
                           static_cast<unsigned long long>(i + 1));
            }

            const char* utf8 = Rf_translateCharUTF8(value);
            const std::size_t bytes = std::strlen(utf8);
            std::size_t position = 0;
            bool is_ascii = true;
            while (position < bytes) {
                const unsigned char first =
                    static_cast<unsigned char>(utf8[position]);
                std::uint32_t point = 0;
                std::size_t width = 0;
                if (first <= 0x7f) {
                    point = first;
                    width = 1;
                } else if (first >= 0xc2 && first <= 0xdf) {
                    width = 2;
                    point = first & 0x1f;
                } else if (first >= 0xe0 && first <= 0xef) {
                    width = 3;
                    point = first & 0x0f;
                } else if (first >= 0xf0 && first <= 0xf4) {
                    width = 4;
                    point = first & 0x07;
                } else {
                    invalid_utf8(argument_name, i);
                }
                if (position + width > bytes)
                    invalid_utf8(argument_name, i);
                for (std::size_t j = 1; j < width; ++j) {
                    const unsigned char continuation =
                        static_cast<unsigned char>(utf8[position + j]);
                    if ((continuation & 0xc0) != 0x80)
                        invalid_utf8(argument_name, i);
                    point = (point << 6) | (continuation & 0x3f);
                }
                if ((width == 3 && point < 0x800) ||
                    (width == 4 && point < 0x10000) ||
                    (point >= 0xd800 && point <= 0xdfff) ||
                    point > 0x10ffff)
                    invalid_utf8(argument_name, i);
                codepoints_.push_back(point);
                if (point > 0x7f) is_ascii = false;
                position += width;
            }
            lengths_[i] = codepoints_.size() - offsets_[i];
            ascii_[i] = is_ascii;
        }

        views_.resize(n);
        const std::uint32_t* base = codepoints_.empty()
            ? nullptr
            : codepoints_.data();
        for (std::size_t i = 0; i < n; ++i) {
            views_[i] = CodepointView{
                base == nullptr ? nullptr : base + offsets_[i],
                lengths_[i], missing_[i], ascii_[i]
            };
        }
    }

    const CodepointView* data() const noexcept { return views_.data(); }
    std::size_t size() const noexcept { return views_.size(); }

private:
    [[noreturn]] static void invalid_utf8(const char* argument_name,
                                          std::size_t index) {
        Rcpp::stop("`%s[%llu]` is not valid UTF-8.", argument_name,
                   static_cast<unsigned long long>(index + 1));
    }

    std::vector<std::uint32_t> codepoints_;
    std::vector<std::size_t> offsets_;
    std::vector<std::size_t> lengths_;
    std::vector<bool> missing_;
    std::vector<bool> ascii_;
    std::vector<CodepointView> views_;
};

#endif // FAST_STRING_CODEPOINT_SNAPSHOT_H
