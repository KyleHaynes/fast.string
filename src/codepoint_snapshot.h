#ifndef FAST_STRING_CODEPOINT_SNAPSHOT_H
#define FAST_STRING_CODEPOINT_SNAPSHOT_H

#include <Rcpp.h>
#include <RcppParallel.h>
#include <Rversion.h>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <string>
#include <vector>
#include "parallel_dispatch.h"
#include "string_snapshot.h"

struct CodepointView {
    const std::uint32_t* data;
    std::size_t size;
    bool missing;
    bool ascii;

    bool is_na() const noexcept { return missing; }
};

// UTF-8 code points of every string, for the metrics' use_bytes = FALSE
// mode. The R API is only touched in a serial pass on the calling thread,
// which resolves each string's UTF-8 bytes (translating latin1 and native
// text); decoding those bytes -- the bulk of the work -- then runs in
// parallel into one array sized by the byte count, an upper bound on the
// number of code points. Workers only ever receive the immutable views.
class CodepointSnapshot {
public:
    CodepointSnapshot(const Rcpp::StringVector& input,
                      const char* argument_name)
        : owner_(input), size_(static_cast<std::size_t>(input.size())),
          sources_(size_), views_(size_) {
        if (ALTREP(static_cast<SEXP>(input))) {
            Rcpp::CharacterVector materialized(input.size());
            for (R_xlen_t i = 0; i < input.size(); ++i)
                SET_STRING_ELT(materialized, i, STRING_ELT(input, i));
            owner_ = materialized;
        }
        const SEXP* atoms = static_cast<const SEXP*>(DATAPTR_RO(owner_));
        const SEXP na = NA_STRING;
        std::size_t total = 0;
        for (std::size_t i = 0; i < size_; ++i) {
            const R_xlen_t index = static_cast<R_xlen_t>(i);
            prefetch_charsxp(atoms, index, static_cast<R_xlen_t>(size_));
            const SEXP value = atoms[i];
            Source& source = sources_[i];
            source.offset = total;
            if (value == na) {
                source.data = nullptr;
                source.bytes = 0;
                source.ascii = true;
                continue;
            }
            if (Rf_getCharCE(value) == CE_BYTES) {
                Rcpp::stop("`%s[%llu]` is bytes-encoded and cannot be compared as Unicode; use `use_bytes = TRUE`.",
                           argument_name,
                           static_cast<unsigned long long>(i + 1));
            }
            source.bytes = static_cast<std::size_t>(LENGTH(value));
            source.data = CHAR(value);
            source.ascii = is_ascii(value, source.data, source.bytes);
            if (!source.ascii && !is_utf8(value)) {
                source.data = Rf_translateCharUTF8(value);
                source.bytes = std::strlen(source.data);
            }
            total += source.bytes;
        }

        codepoints_.reset(new std::uint32_t[total == 0 ? 1 : total]);
        std::vector<std::size_t> lengths(size_, 0);
        std::vector<std::uint8_t> invalid(size_, 0);
        DecodeWorker worker(sources_.data(), codepoints_.get(),
                            lengths.data(), invalid.data());
        // Decoding is one cheap pass over the bytes, so threads only pay off
        // for large inputs.
        dispatch_for(0, size_, worker, total / 4 + size_, 200000, -1, 256);

        for (std::size_t i = 0; i < size_; ++i) {
            if (invalid[i]) {
                Rcpp::stop("`%s[%llu]` is not valid UTF-8.", argument_name,
                           static_cast<unsigned long long>(i + 1));
            }
            const Source& source = sources_[i];
            views_[i] = CodepointView{
                codepoints_.get() + source.offset, lengths[i],
                source.data == nullptr, source.ascii
            };
        }
    }

    const CodepointView* data() const noexcept { return views_.data(); }
    std::size_t size() const noexcept { return views_.size(); }

private:
    struct Source {
        const char* data;
        std::size_t bytes;
        std::size_t offset;
        bool ascii;
    };

    // Decodes and validates (no overlongs, surrogates or values above
    // U+10FFFF) each string's UTF-8 into the shared array.
    struct DecodeWorker : public RcppParallel::Worker {
        const Source* sources;
        std::uint32_t* out;
        std::size_t* lengths;
        std::uint8_t* invalid;

        DecodeWorker(const Source* sources_, std::uint32_t* out_,
                     std::size_t* lengths_, std::uint8_t* invalid_)
            : sources(sources_), out(out_), lengths(lengths_),
              invalid(invalid_) {}

        void operator()(std::size_t begin, std::size_t end) {
            for (std::size_t i = begin; i < end; ++i) {
                const Source& source = sources[i];
                if (source.data == nullptr) continue;
                std::uint32_t* target = out + source.offset;
                const unsigned char* utf8 =
                    reinterpret_cast<const unsigned char*>(source.data);
                const std::size_t bytes = source.bytes;
                if (source.ascii) {
                    for (std::size_t k = 0; k < bytes; ++k) target[k] = utf8[k];
                    lengths[i] = bytes;
                    continue;
                }
                std::size_t count = 0;
                std::size_t position = 0;
                bool ok = true;
                while (position < bytes) {
                    const unsigned char first = utf8[position];
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
                        ok = false;
                        break;
                    }
                    if (position + width > bytes) {
                        ok = false;
                        break;
                    }
                    for (std::size_t j = 1; j < width; ++j) {
                        const unsigned char continuation = utf8[position + j];
                        if ((continuation & 0xc0) != 0x80) {
                            ok = false;
                            break;
                        }
                        point = (point << 6) | (continuation & 0x3f);
                    }
                    if (!ok ||
                        (width == 3 && point < 0x800) ||
                        (width == 4 && point < 0x10000) ||
                        (point >= 0xd800 && point <= 0xdfff) ||
                        point > 0x10ffff) {
                        ok = false;
                        break;
                    }
                    target[count++] = point;
                    position += width;
                }
                lengths[i] = count;
                invalid[i] = !ok;
            }
        }
    };

    static bool is_ascii(SEXP value, const char* data, std::size_t size) {
#if R_VERSION >= R_Version(4, 5, 0)
        (void)data; (void)size;
        return Rf_charIsASCII(value);
#else
        (void)value;
        return bytes_are_ascii(data, size);
#endif
    }

    // Whether the CHAR() bytes are already UTF-8 (UTF-8 marked, or native
    // text in a UTF-8 locale); otherwise they are translated.
    static bool is_utf8(SEXP value) {
#if R_VERSION >= R_Version(4, 5, 0)
        return Rf_charIsUTF8(value);
#else
        return Rf_getCharCE(value) == CE_UTF8;
#endif
    }

    Rcpp::CharacterVector owner_;
    std::size_t size_;
    std::vector<Source> sources_;
    std::unique_ptr<std::uint32_t[]> codepoints_;
    std::vector<CodepointView> views_;
};

#endif // FAST_STRING_CODEPOINT_SNAPSHOT_H
