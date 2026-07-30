// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>
#include "parallel_dispatch.h"
#include "string_snapshot.h"

using namespace Rcpp;
using namespace RcppParallel;

static inline std::size_t string_scan_work(const StringSnapshot& snapshot) {
    const std::uint64_t byte_units = snapshot.total_bytes() / 32u +
        (snapshot.total_bytes() % 32u != 0u);
    const std::uint64_t rows = static_cast<std::uint64_t>(snapshot.size());
    const std::uint64_t max_value =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    if (byte_units > max_value - rows)
        return (std::numeric_limits<std::size_t>::max)();
    return static_cast<std::size_t>(byte_units + rows);
}

static inline cetype_t output_encoding(SEXP charsxp) {
    cetype_t encoding = Rf_getCharCE(charsxp);
    return encoding == CE_ANY ? CE_NATIVE : encoding;
}

// ---------------------------------------------------------------------------
// trimws
// ---------------------------------------------------------------------------

static inline bool is_trim_ws(unsigned char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

enum SliceKind : std::uint8_t {
    SLICE_NA = 0,
    SLICE_ORIGINAL = 1,
    SLICE_RANGE = 2
};

struct SliceResult {
    std::size_t start;
    std::size_t length;
    std::uint8_t kind;
};

struct TrimWorker : public Worker {
    const StringView* strings;
    int which;
    SliceResult* results;

    TrimWorker(const StringView* strings_, int which_, SliceResult* results_)
        : strings(strings_), which(which_), results(results_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                results[i] = SliceResult{0, 0, SLICE_NA};
                continue;
            }

            std::size_t start = 0;
            std::size_t stop = value.size;
            if (which != 2) {
                while (start < stop &&
                       is_trim_ws(static_cast<unsigned char>(value.data[start])))
                    ++start;
            }
            if (which != 1) {
                while (stop > start &&
                       is_trim_ws(static_cast<unsigned char>(value.data[stop - 1])))
                    --stop;
            }

            const std::uint8_t kind =
                start == 0 && stop == value.size ? SLICE_ORIGINAL : SLICE_RANGE;
            results[i] = SliceResult{start, stop - start, kind};
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_trimws_impl(const StringVector& x, int which) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<SliceResult> slices(n);

    TrimWorker worker(snapshot.data(), which, slices.data());
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        const SliceResult& slice = slices[i];
        if (slice.kind == SLICE_NA) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else if (slice.kind == SLICE_ORIGINAL) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), snapshot.charsxp(i));
        } else {
            const StringView& value = snapshot[i];
            const char* data = slice.length == 0 ? "" : value.data + slice.start;
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLenCE(
                    data,
                    static_cast<int>(slice.length),
                    output_encoding(snapshot.charsxp(i))
                )
            );
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// substr
// ---------------------------------------------------------------------------

static inline bool is_utf8_lead(unsigned char c) {
    return (c & 0xC0u) != 0x80u;
}

static inline SliceResult byte_substr(const StringView& value, int start, int stop) {
    if (start < 1) start = 1;
    if (stop < start || static_cast<std::size_t>(start) > value.size)
        return SliceResult{0, 0, SLICE_RANGE};

    const std::size_t first = static_cast<std::size_t>(start - 1);
    const std::size_t requested_stop =
        stop < 1 ? 0 : static_cast<std::size_t>(stop);
    const std::size_t last = (std::min)(value.size, requested_stop);
    if (first >= last)
        return SliceResult{0, 0, SLICE_RANGE};
    const std::uint8_t kind =
        first == 0 && last == value.size ? SLICE_ORIGINAL : SLICE_RANGE;
    return SliceResult{first, last - first, kind};
}

static inline SliceResult utf8_substr(const StringView& value, int start, int stop) {
    if (start < 1) start = 1;
    if (stop < start)
        return SliceResult{0, 0, SLICE_RANGE};

    std::size_t first = value.size;
    std::size_t last = value.size;
    int character = 0;
    for (std::size_t byte = 0; byte < value.size; ++byte) {
        if (!is_utf8_lead(static_cast<unsigned char>(value.data[byte])))
            continue;
        ++character;
        if (character == start)
            first = byte;
        if (character > stop) {
            last = byte;
            break;
        }
    }

    if (first == value.size)
        return SliceResult{0, 0, SLICE_RANGE};
    if (stop >= character)
        last = value.size;
    if (last < first)
        last = first;
    const std::uint8_t kind =
        first == 0 && last == value.size ? SLICE_ORIGINAL : SLICE_RANGE;
    return SliceResult{first, last - first, kind};
}

struct SubstrWorker : public Worker {
    const StringView* strings;
    const int* start;
    const int* stop;
    const std::uint8_t* use_utf8;
    bool scalar_start;
    bool scalar_stop;
    SliceResult* results;

    SubstrWorker(const StringView* strings_,
                 const int* start_,
                 bool scalar_start_,
                 const int* stop_,
                 bool scalar_stop_,
                 const std::uint8_t* use_utf8_,
                 SliceResult* results_)
        : strings(strings_),
          start(start_),
          stop(stop_),
          use_utf8(use_utf8_),
          scalar_start(scalar_start_),
          scalar_stop(scalar_stop_),
          results(results_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            const int first = scalar_start ? start[0] : start[i];
            const int last = scalar_stop ? stop[0] : stop[i];
            if (value.is_na() || first == NA_INTEGER || last == NA_INTEGER) {
                results[i] = SliceResult{0, 0, SLICE_NA};
                continue;
            }
            results[i] = use_utf8[i]
                ? utf8_substr(value, first, last)
                : byte_substr(value, first, last);
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_substr_impl(const StringVector& x,
                                 const IntegerVector& start,
                                 const IntegerVector& stop,
                                 bool native_utf8) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<SliceResult> slices(n);
    std::vector<std::uint8_t> use_utf8(n, 0);
    for (std::size_t i = 0; i < n; ++i) {
        if (snapshot[i].is_na())
            continue;
        const cetype_t encoding = output_encoding(snapshot.charsxp(i));
        use_utf8[i] = encoding == CE_UTF8 ||
            (encoding == CE_NATIVE && native_utf8);
    }

    const bool scalar_start = start.size() == 1;
    const bool scalar_stop = stop.size() == 1;
    SubstrWorker worker(
        snapshot.data(),
        start.begin(),
        scalar_start,
        stop.begin(),
        scalar_stop,
        use_utf8.data(),
        slices.data()
    );
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        const SliceResult& slice = slices[i];
        if (slice.kind == SLICE_NA) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else if (slice.kind == SLICE_ORIGINAL) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), snapshot.charsxp(i));
        } else {
            const StringView& value = snapshot[i];
            const char* data = slice.length == 0 ? "" : value.data + slice.start;
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLenCE(
                    data,
                    static_cast<int>(slice.length),
                    output_encoding(snapshot.charsxp(i))
                )
            );
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// nchar
// ---------------------------------------------------------------------------

struct NcharWorker : public Worker {
    const StringView* strings;
    const std::uint8_t* use_utf8;
    int type;
    bool allow_na;
    RVector<int> out;

    NcharWorker(const StringView* strings_,
                const std::uint8_t* use_utf8_,
                int type_,
                bool allow_na_,
                IntegerVector& out_)
        : strings(strings_),
          use_utf8(use_utf8_),
          type(type_),
          allow_na(allow_na_),
          out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na()) {
                out[i] = allow_na ? NA_INTEGER : 2;
                continue;
            }
            if (type == 0 || !use_utf8[i]) {
                out[i] = static_cast<int>(value.size);
                continue;
            }

            int characters = 0;
            for (std::size_t j = 0; j < value.size; ++j) {
                if (is_utf8_lead(static_cast<unsigned char>(value.data[j])))
                    ++characters;
            }
            out[i] = characters;
        }
    }
};

// [[Rcpp::export]]
IntegerVector fast_nchar_impl(const StringVector& x,
                              int type,
                              bool allow_na,
                              bool native_utf8) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    std::vector<std::uint8_t> use_utf8(n, 0);
    for (std::size_t i = 0; i < n; ++i) {
        if (snapshot[i].is_na())
            continue;
        const cetype_t encoding = output_encoding(snapshot.charsxp(i));
        use_utf8[i] = encoding == CE_UTF8 ||
            (encoding == CE_NATIVE && native_utf8);
    }

    IntegerVector result(static_cast<R_xlen_t>(n));
    NcharWorker worker(snapshot.data(), use_utf8.data(), type, allow_na, result);
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000);
    return result;
}

// ---------------------------------------------------------------------------
// chartr
// ---------------------------------------------------------------------------

struct ChartrWorker : public Worker {
    const StringView* strings;
    const std::size_t* offsets;
    const unsigned char* table;
    char* bytes;
    std::uint8_t* changed;

    ChartrWorker(const StringView* strings_,
                 const std::size_t* offsets_,
                 const unsigned char* table_,
                 char* bytes_,
                 std::uint8_t* changed_)
        : strings(strings_),
          offsets(offsets_),
          table(table_),
          bytes(bytes_),
          changed(changed_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            if (value.is_na())
                continue;
            if (value.size == 0) {
                changed[i] = 0;
                continue;
            }
            char* output = bytes + offsets[i];
            bool any_changed = false;
            for (std::size_t j = 0; j < value.size; ++j) {
                const unsigned char input =
                    static_cast<unsigned char>(value.data[j]);
                const unsigned char translated = table[input];
                output[j] = static_cast<char>(translated);
                any_changed = any_changed || translated != input;
            }
            changed[i] = any_changed;
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_chartr_impl(const std::string& old_chars,
                                 const std::string& new_chars,
                                 const StringVector& x) {
    unsigned char table[256];
    for (int i = 0; i < 256; ++i)
        table[i] = static_cast<unsigned char>(i);
    const std::size_t map_length =
        (std::min)(old_chars.size(), new_chars.size());
    for (std::size_t i = 0; i < map_length; ++i) {
        table[static_cast<unsigned char>(old_chars[i])] =
            static_cast<unsigned char>(new_chars[i]);
    }

    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    if (snapshot.total_bytes() >
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)()))
        stop("Character data are too large to translate.");

    std::vector<std::size_t> offsets(n + 1, 0);
    for (std::size_t i = 0; i < n; ++i)
        offsets[i + 1] = offsets[i] + snapshot[i].size;
    std::vector<char> bytes(offsets[n]);
    std::vector<std::uint8_t> changed(n, 0);

    ChartrWorker worker(
        snapshot.data(),
        offsets.data(),
        table,
        bytes.data(),
        changed.data()
    );
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        const StringView& value = snapshot[i];
        if (value.is_na()) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else if (!changed[i]) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), snapshot.charsxp(i));
        } else {
            const char* data = value.size == 0 ? "" : bytes.data() + offsets[i];
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLenCE(
                    data,
                    static_cast<int>(value.size),
                    output_encoding(snapshot.charsxp(i))
                )
            );
        }
    }
    return result;
}
