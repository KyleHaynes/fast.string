// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <Rversion.h>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
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

static inline bool is_ascii_bytes(const char* data, std::size_t size) {
    std::uint64_t bits = 0;
    std::size_t i = 0;
    for (; i + 8 <= size; i += 8) {
        std::uint64_t word;
        std::memcpy(&word, data + i, 8);
        bits |= word;
    }
    for (; i < size; ++i)
        bits |= static_cast<unsigned char>(data[i]);
    return (bits & 0x8080808080808080ULL) == 0;
}

// R 4.5.0 exposed the ASCII flag R keeps on every CHARSXP; before that, scan.
static inline bool charsxp_is_ascii(SEXP value, std::size_t size) {
#if R_VERSION >= R_Version(4, 5, 0)
    (void)size;
    return Rf_charIsASCII(value);
#else
    return is_ascii_bytes(CHAR(value), size);
#endif
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

// ALTREP string vectors may create CHARSXPs on access; copy them into an
// ordinary STRSXP so every element stays protected while mkChar allocates
// and can be read through a plain pointer array.
static CharacterVector materialized_strings(const StringVector& x) {
    if (!ALTREP(static_cast<SEXP>(x))) return x;
    CharacterVector materialized(x.size());
    for (R_xlen_t i = 0; i < x.size(); ++i)
        SET_STRING_ELT(materialized, i, STRING_ELT(x, i));
    return materialized;
}

// One serial pass that slices and interns each string in turn.
// Rf_mkCharLenCE() (hash + global CHARSXP cache lookup) dominates these
// functions and must run on the main thread, so a separate parallel slicing
// pass only adds memory traffic. When a slice is byte-identical to the
// previous one (common in sorted or grouped data) the previous CHARSXP is
// reused instead of being looked up again.
//
// Rf_mkCharLenCE() stores ASCII bytes unmarked whatever encoding it is
// given, so the per-element R API calls are kept to the ones that matter:
// an ASCII slice needs no encoding lookup, and a slice byte-identical to an
// ASCII previous one is that same CHARSXP.
//
// slice_of(i, view, value) returns the slice of non-NA element i (value is
// its CHARSXP, for slicers that need its encoding), or SLICE_NA to produce
// NA (e.g. for an NA start/stop).
template <typename SliceOf>
static CharacterVector serial_slice_strings(const StringVector& x,
                                            SliceOf slice_of) {
    const CharacterVector source = materialized_strings(x);
    const SEXP* strings = static_cast<const SEXP*>(DATAPTR_RO(source));
    const R_xlen_t n = source.size();
    CharacterVector result(n);

    const SEXP na = NA_STRING;
    SEXP previous = na;
    // Points into the source string the previous result was sliced from,
    // which `source` keeps alive, rather than into the new CHARSXP.
    const char* previous_data = nullptr;
    std::size_t previous_length = 0;
    cetype_t previous_encoding = CE_NATIVE;
    bool previous_ascii = true;
    for (R_xlen_t i = 0; i < n; ++i) {
        prefetch_charsxp(strings, i, n);
        const SEXP value = strings[i];
        if (value == na) {
            SET_STRING_ELT(result, i, na);
            continue;
        }
        const StringView view{CHAR(value), static_cast<std::size_t>(LENGTH(value))};
        const SliceResult slice = slice_of(i, view, value);
        if (slice.kind == SLICE_NA) {
            SET_STRING_ELT(result, i, na);
            continue;
        }
        if (slice.kind == SLICE_ORIGINAL) {
            SET_STRING_ELT(result, i, value);
            continue;
        }
        if (slice.length == 0) {
            SET_STRING_ELT(result, i, R_BlankString);
            continue;
        }
        const char* data = view.data + slice.start;
        if (previous != na && slice.length == previous_length &&
            data[slice.length - 1] == previous_data[slice.length - 1] &&
            std::memcmp(data, previous_data, slice.length) == 0 &&
            (previous_ascii || output_encoding(value) == previous_encoding)) {
            SET_STRING_ELT(result, i, previous);
            continue;
        }
        const bool ascii = is_ascii_bytes(data, slice.length);
        const cetype_t encoding = ascii ? CE_NATIVE : output_encoding(value);
        previous = Rf_mkCharLenCE(data, static_cast<int>(slice.length), encoding);
        SET_STRING_ELT(result, i, previous);
        previous_data = data;
        previous_length = slice.length;
        previous_encoding = encoding;
        previous_ascii = ascii;
    }
    return result;
}

// ---------------------------------------------------------------------------
// trimws
// ---------------------------------------------------------------------------

static inline bool is_trim_ws(unsigned char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

// [[Rcpp::export]]
CharacterVector fast_trimws_impl(const StringVector& x, int which) {
    return serial_slice_strings(x, [which](R_xlen_t, const StringView& value,
                                           SEXP) {
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
        return SliceResult{start, stop - start, kind};
    });
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

// Characters 1..stop are single bytes when the first `stop` bytes are ASCII,
// so every encoding slices them as bytes and no character scan is needed.
static inline bool ascii_prefix(const StringView& value, int stop) {
    const std::size_t prefix = stop < 1
        ? 0
        : (std::min)(value.size, static_cast<std::size_t>(stop));
    return is_ascii_bytes(value.data, prefix);
}

static inline SliceResult utf8_substr_scan(const StringView& value,
                                           int start, int stop) {
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

static inline SliceResult utf8_substr(const StringView& value, int start, int stop) {
    return ascii_prefix(value, stop)
        ? byte_substr(value, start, stop)
        : utf8_substr_scan(value, start, stop);
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

// Beyond this many leading characters the UTF-8 prefix scan dominates the
// per-string cost, so it is worth scanning in parallel before the serial
// Rf_mkCharLenCE() pass. Below it, that extra pass only adds overhead.
static const int SUBSTR_PARALLEL_SCAN_START = 256;

static CharacterVector substr_serial(const StringVector& x,
                                     const int* start, bool scalar_start,
                                     const int* stop, bool scalar_stop,
                                     bool native_utf8) {
    return serial_slice_strings(x, [=](R_xlen_t i, const StringView& view,
                                       SEXP value) {
        const int first = scalar_start ? start[0] : start[i];
        const int last = scalar_stop ? stop[0] : stop[i];
        if (first == NA_INTEGER || last == NA_INTEGER)
            return SliceResult{0, 0, SLICE_NA};
        // The encoding is only looked up when a character scan might be needed.
        if (ascii_prefix(view, last))
            return byte_substr(view, first, last);
        const cetype_t encoding = output_encoding(value);
        const bool utf8 = encoding == CE_UTF8 ||
            (encoding == CE_NATIVE && native_utf8);
        return utf8
            ? utf8_substr_scan(view, first, last)
            : byte_substr(view, first, last);
    });
}

// [[Rcpp::export]]
CharacterVector fast_substr_impl(const StringVector& x,
                                 const IntegerVector& start,
                                 const IntegerVector& stop,
                                 bool native_utf8,
                                 int nthreads) {
    const int* start_begin = start.begin();
    const int max_start = start.size() == 0
        ? NA_INTEGER
        : *std::max_element(start_begin, start_begin + start.size());
    if (max_start <= SUBSTR_PARALLEL_SCAN_START) {
        return substr_serial(x, start_begin, start.size() == 1,
                             stop.begin(), stop.size() == 1, native_utf8);
    }

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
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000, nthreads);

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

struct PendingCount {
    R_xlen_t row;
    StringView value;
};

// Counts UTF-8 characters for the non-ASCII strings fast_nchar_impl()
// deferred; reads only the byte views resolved on the main thread.
struct NcharWorker : public Worker {
    const PendingCount* pending;
    int* out;

    NcharWorker(const PendingCount* pending_, int* out_)
        : pending(pending_), out(out_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t k = begin; k < end; ++k) {
            const StringView& value = pending[k].value;
            int characters = 0;
            for (std::size_t j = 0; j < value.size; ++j) {
                if (is_utf8_lead(static_cast<unsigned char>(value.data[j])))
                    ++characters;
            }
            out[pending[k].row] = characters;
        }
    }
};

// One serial pass answers NA, byte counts, non-UTF-8 and ASCII strings
// directly from the CHARSXP (touching each element once, with prefetch, is
// the whole cost for those); only non-ASCII UTF-8 strings are deferred to a
// character-counting pass, parallel when there is enough of it.
// [[Rcpp::export]]
IntegerVector fast_nchar_impl(const StringVector& x,
                              int type,
                              bool allow_na,
                              bool native_utf8,
                              int nthreads) {
    const CharacterVector source = materialized_strings(x);
    const SEXP* strings = static_cast<const SEXP*>(DATAPTR_RO(source));
    const R_xlen_t n = source.size();
    // Every element is written by the loop below.
    IntegerVector result = no_init(n);
    int* out = INTEGER(result);

    const SEXP na = NA_STRING;
    const int na_count = allow_na ? NA_INTEGER : 2;
    std::vector<PendingCount> pending;
    std::size_t pending_bytes = 0;
    for (R_xlen_t i = 0; i < n; ++i) {
        prefetch_charsxp(strings, i, n);
        const SEXP value = strings[i];
        if (value == na) {
            out[i] = na_count;
            continue;
        }
        const std::size_t size = static_cast<std::size_t>(LENGTH(value));
        out[i] = static_cast<int>(size);
        if (type == 0 || charsxp_is_ascii(value, size)) continue;
        const cetype_t encoding = output_encoding(value);
        if (encoding != CE_UTF8 && !(encoding == CE_NATIVE && native_utf8))
            continue;
        pending.push_back(PendingCount{i, StringView{CHAR(value), size}});
        pending_bytes += size;
    }

    if (!pending.empty()) {
        NcharWorker worker(pending.data(), out);
        const std::size_t work = pending_bytes / 32u + pending.size();
        dispatch_for(0, pending.size(), worker, work, 10000, nthreads);
    }
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
                                 const StringVector& x,
                                 int nthreads) {
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
    dispatch_for(0, n, worker, string_scan_work(snapshot), 10000, nthreads);

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
