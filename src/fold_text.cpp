// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include "ascii_fold_table.h"
#include "fingerprint_table.h"
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

// ascii_fold(), fingerprint() and ngram_fingerprint(). Each string is
// rewritten into a per-worker buffer; strings that come out unchanged keep
// their CHARSXP.

namespace {

std::size_t fold_work(const StringSnapshot& snapshot) {
    const std::uint64_t work =
        snapshot.total_bytes() + 16u * static_cast<std::uint64_t>(snapshot.size());
    const std::uint64_t limit =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return static_cast<std::size_t>((std::min)(work, limit));
}

inline bool ascii_alnum(unsigned char c) {
    return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') ||
        (c >= 'a' && c <= 'z');
}

// ---------------------------------------------------------------------------
// ascii_fold(): ICU's Latin-ASCII transliteration (ascii_fold_table.h).
// ---------------------------------------------------------------------------

const AsciiFoldEntry* ascii_fold_lookup(std::uint32_t point) {
    const AsciiFoldEntry* first = ASCII_FOLD_ENTRIES;
    const AsciiFoldEntry* last = ASCII_FOLD_ENTRIES + ASCII_FOLD_ENTRY_COUNT;
    const AsciiFoldEntry* it = std::lower_bound(
        first, last, point,
        [](const AsciiFoldEntry& entry, std::uint32_t value) {
            return entry.codepoint < value;
        });
    return it != last && it->codepoint == point ? it : nullptr;
}

bool in_ranges(const std::uint32_t (*ranges)[2], std::size_t count,
               std::uint32_t point) {
    std::size_t lo = 0, hi = count;
    while (lo < hi) {
        const std::size_t mid = lo + (hi - lo) / 2;
        if (ranges[mid][1] < point) lo = mid + 1;
        else hi = mid;
    }
    return lo < count && ranges[lo][0] <= point;
}

bool ascii_fold_mark(std::uint32_t point) {
    return in_ranges(ASCII_FOLD_MARK_RANGES, ASCII_FOLD_MARK_RANGE_COUNT, point);
}

bool ascii_fold_latin(std::uint32_t point) {
    return in_ranges(ASCII_FOLD_LATIN_RANGES, ASCII_FOLD_LATIN_RANGE_COUNT,
                     point);
}

// Appends `bytes` (UTF-8), replacing every non-ASCII character with `sub`
// when one is given.
void append_folded(std::string& out, const unsigned char* bytes,
                   std::size_t length, const std::string* sub) {
    if (!sub) {
        out.append(reinterpret_cast<const char*>(bytes), length);
        return;
    }
    std::size_t i = 0;
    while (i < length) {
        if (bytes[i] < 0x80) {
            out.push_back(static_cast<char>(bytes[i]));
            ++i;
            continue;
        }
        std::uint32_t point;
        const std::size_t width = utf8_decode(bytes, length, i, point);
        out.append(*sub);
        i += width == 0 ? 1 : width;
    }
}

void ascii_fold_string(const StringView& value, const std::string* sub,
                       std::string& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    out.clear();
    out.reserve(n);
    // Whether the last character read (other than a deleted mark) is a
    // Latin-script character or an ASCII digit: ICU deletes the combining
    // marks that follow one.
    bool after_latin = false;
    std::size_t i = 0;
    while (i < n) {
        if (s[i] < 0x80) {
            out.push_back(static_cast<char>(s[i]));
            after_latin = ascii_alnum(s[i]);
            ++i;
            continue;
        }
        std::uint32_t point;
        const std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) {
            append_folded(out, s + i, 1, sub);
            after_latin = false;
            ++i;
            continue;
        }
        if (after_latin && ascii_fold_mark(point)) {
            i += width;
            continue;
        }
        const AsciiFoldEntry* entry = ascii_fold_lookup(point);
        if (entry) {
            append_folded(out, ASCII_FOLD_POOL + entry->offset, entry->length,
                          sub);
        } else {
            append_folded(out, s + i, width, sub);
        }
        after_latin = ascii_fold_latin(point);
        i += width;
    }
}

struct AsciiFoldWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    const std::string* sub;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    AsciiFoldWorker(const StringView* strings_,
                    const std::vector<TextWorkChunk>& chunks_,
                    const std::string* sub_, std::vector<TextResult>& results_,
                    TextArenas& arenas_)
        : strings(strings_), chunks(chunks_), sub(sub_), results(results_),
          arenas(arenas_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string out;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                if (value.is_na() || bytes_are_ascii(value.data, value.size))
                    continue;
                ascii_fold_string(value, sub, out);
                store_text_output(i, chunk, value, out.data(), out.size(),
                                  results, arenas);
            }
        }
    }
};

// ---------------------------------------------------------------------------
// OpenRefine's fingerprint normalisation (fingerprint_table.h): lowercase,
// NFKD, delete diacritics, modifier letters and symbols, punctuation and
// non-whitespace controls, and map a few letters such as o-slash to "oe".
// ---------------------------------------------------------------------------

const FingerprintEntry* fingerprint_lookup(std::uint32_t point) {
    const FingerprintEntry* first = FINGERPRINT_ENTRIES;
    const FingerprintEntry* last = FINGERPRINT_ENTRIES + FINGERPRINT_ENTRY_COUNT;
    const FingerprintEntry* it = std::lower_bound(
        first, last, point,
        [](const FingerprintEntry& entry, std::uint32_t value) {
            return entry.codepoint < value;
        });
    return it != last && it->codepoint == point ? it : nullptr;
}

void fingerprint_normalize(const StringView& value, std::string& out) {
    const unsigned char* s = reinterpret_cast<const unsigned char*>(value.data);
    const std::size_t n = value.size;
    out.clear();
    out.reserve(n);
    std::size_t i = 0;
    while (i < n) {
        if (s[i] < 0x80) {
            const int mapped = FINGERPRINT_ASCII[s[i]];
            if (mapped >= 0) out.push_back(static_cast<char>(mapped));
            ++i;
            continue;
        }
        std::uint32_t point;
        const std::size_t width = utf8_decode(s, n, i, point);
        if (width == 0) {
            out.push_back(static_cast<char>(s[i]));
            ++i;
            continue;
        }
        if (point >= 0xAC00 && point <= 0xD7A3) {
            // NFKD of a Hangul syllable: its leading, vowel and trailing jamo.
            const std::uint32_t index = point - 0xAC00;
            utf8_append(out, 0x1100 + index / 588);
            utf8_append(out, 0x1161 + (index % 588) / 28);
            if (index % 28 != 0) utf8_append(out, 0x11A7 + index % 28);
        } else if (const FingerprintEntry* entry = fingerprint_lookup(point)) {
            out.append(reinterpret_cast<const char*>(FINGERPRINT_POOL) +
                           entry->offset,
                       entry->length);
        } else {
            out.append(reinterpret_cast<const char*>(s + i), width);
        }
        i += width;
    }
}

struct Piece {
    std::size_t offset;
    std::size_t length;
};

struct FingerprintWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    FingerprintWorker(const StringView* strings_,
                      const std::vector<TextWorkChunk>& chunks_,
                      std::vector<TextResult>& results_, TextArenas& arenas_)
        : strings(strings_), chunks(chunks_), results(results_),
          arenas(arenas_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string normal, out;
        std::vector<Piece> tokens;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                fingerprint_normalize(value, normal);
                const unsigned char* data =
                    reinterpret_cast<const unsigned char*>(normal.data());
                const std::size_t size = normal.size();
                tokens.clear();
                std::size_t k = 0;
                while (k < size) {
                    std::size_t w = whitespace_width(data, k, size, true);
                    if (w != 0) {
                        k += w;
                        continue;
                    }
                    const std::size_t start = k;
                    while (k < size &&
                           whitespace_width(data, k, size, true) == 0)
                        ++k;
                    tokens.push_back(Piece{start, k - start});
                }
                const char* base = normal.data();
                auto less = [base](const Piece& a, const Piece& b) {
                    const int c = std::memcmp(base + a.offset, base + b.offset,
                                              (std::min)(a.length, b.length));
                    return c != 0 ? c < 0 : a.length < b.length;
                };
                auto same = [base](const Piece& a, const Piece& b) {
                    return a.length == b.length &&
                        std::memcmp(base + a.offset, base + b.offset,
                                    a.length) == 0;
                };
                std::sort(tokens.begin(), tokens.end(), less);
                tokens.erase(std::unique(tokens.begin(), tokens.end(), same),
                             tokens.end());
                out.clear();
                for (std::size_t t = 0; t < tokens.size(); ++t) {
                    if (t) out.push_back(' ');
                    out.append(base + tokens[t].offset, tokens[t].length);
                }
                store_text_output(i, chunk, value, out.data(), out.size(),
                                  results, arenas);
            }
        }
    }
};

struct NgramFingerprintWorker : public Worker {
    const StringView* strings;
    const std::vector<TextWorkChunk>& chunks;
    std::size_t size;
    std::vector<TextResult>& results;
    TextArenas& arenas;

    NgramFingerprintWorker(const StringView* strings_,
                           const std::vector<TextWorkChunk>& chunks_,
                           std::size_t size_, std::vector<TextResult>& results_,
                           TextArenas& arenas_)
        : strings(strings_), chunks(chunks_), size(size_), results(results_),
          arenas(arenas_) {}

    void operator()(std::size_t begin, std::size_t end) {
        std::string normal, out;
        std::vector<std::uint32_t> points;
        std::vector<std::size_t> grams;
        for (std::size_t chunk = begin; chunk < end; ++chunk) {
            for (std::size_t i = chunks[chunk].begin; i < chunks[chunk].end;
                 ++i) {
                const StringView& value = strings[i];
                if (value.is_na()) continue;
                fingerprint_normalize(value, normal);
                // Delete control characters and whitespace. A byte that is
                // not valid UTF-8 is kept as a code point of its own, above
                // the Unicode range.
                const unsigned char* data =
                    reinterpret_cast<const unsigned char*>(normal.data());
                points.clear();
                std::size_t k = 0;
                while (k < normal.size()) {
                    std::uint32_t point;
                    std::size_t w = utf8_decode(data, normal.size(), k, point);
                    if (w == 0) {
                        point = 0x110000u + data[k];
                        w = 1;
                    }
                    k += w;
                    if (point < 0x20 || (point >= 0x7F && point <= 0x9F) ||
                        is_white_space(point))
                        continue;
                    points.push_back(point);
                }
                out.clear();
                const auto put = [&out](std::uint32_t point) {
                    if (point >= 0x110000u)
                        out.push_back(static_cast<char>(point - 0x110000u));
                    else
                        utf8_append(out, point);
                };
                if (points.size() < size) {
                    // Shorter than one n-gram: the key is the text itself.
                    for (std::uint32_t point : points) put(point);
                } else {
                    grams.resize(points.size() - size + 1);
                    for (std::size_t g = 0; g < grams.size(); ++g) grams[g] = g;
                    const std::uint32_t* p = points.data();
                    const std::size_t width = size;
                    std::sort(grams.begin(), grams.end(),
                              [p, width](std::size_t a, std::size_t b) {
                                  return std::lexicographical_compare(
                                      p + a, p + a + width, p + b, p + b + width);
                              });
                    grams.erase(
                        std::unique(grams.begin(), grams.end(),
                                    [p, width](std::size_t a, std::size_t b) {
                                        return std::equal(p + a, p + a + width,
                                                          p + b);
                                    }),
                        grams.end());
                    for (std::size_t g : grams)
                        for (std::size_t c = 0; c < size; ++c) put(p[g + c]);
                }
                store_text_output(i, chunk, value, out.data(), out.size(),
                                  results, arenas);
            }
        }
    }
};

template <typename MakeWorker>
CharacterVector run_text_transform(const StringVector& x, int nthreads,
                                   MakeWorker make_worker) {
    StringSnapshot snapshot(x, SnapshotText::utf8);
    const std::size_t work = fold_work(snapshot);
    const std::size_t threshold = 200000;
    const TextWorkPlan plan =
        make_text_work_plan(snapshot, work, threshold, nthreads, 16);
    std::vector<TextResult> results = make_text_results(snapshot);
    TextArenas arenas(plan.chunks.size());
    auto worker = make_worker(snapshot.data(), plan.chunks, results, arenas);
    dispatch_for(0, plan.chunks.size(), worker, work, threshold,
                 plan.dispatch_threads, plan.grain_size);
    return finalize_text_results(snapshot, results, arenas);
}

}  // namespace

// [[Rcpp::export]]
CharacterVector fast_ascii_fold_impl(const StringVector& x,
                                     const std::string& sub, bool has_sub,
                                     int nthreads) {
    const std::string* sub_ptr = has_sub ? &sub : nullptr;
    return run_text_transform(
        x, nthreads,
        [sub_ptr](const StringView* strings,
                  const std::vector<TextWorkChunk>& chunks,
                  std::vector<TextResult>& results, TextArenas& arenas) {
            return AsciiFoldWorker(strings, chunks, sub_ptr, results, arenas);
        });
}

// [[Rcpp::export]]
CharacterVector fast_fingerprint_impl(const StringVector& x, int nthreads) {
    return run_text_transform(
        x, nthreads,
        [](const StringView* strings, const std::vector<TextWorkChunk>& chunks,
           std::vector<TextResult>& results, TextArenas& arenas) {
            return FingerprintWorker(strings, chunks, results, arenas);
        });
}

// [[Rcpp::export]]
CharacterVector fast_ngram_fingerprint_impl(const StringVector& x, int n,
                                            int nthreads) {
    if (n < 1) stop("`n` must be a positive integer.");
    const std::size_t size = static_cast<std::size_t>(n);
    return run_text_transform(
        x, nthreads,
        [size](const StringView* strings,
               const std::vector<TextWorkChunk>& chunks,
               std::vector<TextResult>& results, TextArenas& arenas) {
            return NgramFingerprintWorker(strings, chunks, size, results,
                                          arenas);
        });
}
