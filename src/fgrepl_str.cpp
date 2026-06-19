// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include <cctype>
#include <cstring>
using namespace Rcpp;
using namespace RcppParallel;

// ---------------------------------------------------------------------------
// trimws
// which: 0=both, 1=left, 2=right
// Strips exactly the bytes matched by the R wrapper's default whitespace
// regex "[ \t\r\n]". Deliberately narrower than std::isspace(), which also
// treats \v and \f as whitespace and would diverge from base::trimws().
// ---------------------------------------------------------------------------

static inline bool is_trim_ws(unsigned char c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

struct TrimWorker : public Worker {
    SEXP x_sexp;
    int which;
    std::vector<std::string>& results;

    TrimWorker(SEXP x, int which, std::vector<std::string>& r)
        : x_sexp(x), which(which), results(r) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const char* s = CHAR(elem);
            std::size_t start = 0, stop = (std::size_t)LENGTH(elem);
            if (which != 2)  // left or both
                while (start < stop && is_trim_ws((unsigned char)s[start])) ++start;
            if (which != 1)  // right or both
                while (stop > start && is_trim_ws((unsigned char)s[stop-1])) --stop;
            results[i].assign(s + start, stop - start);
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_trimws_impl(const StringVector& x, int which) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    TrimWorker worker(x, which, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);
    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

// ---------------------------------------------------------------------------
// substr — 1-indexed, clamps to string bounds, R-compatible NA handling.
// NA in x, start, or stop all produce NA output.
// start/stop are pre-recycled in the R wrapper to length n or 1.
// ---------------------------------------------------------------------------

struct SubstrWorker : public Worker {
    SEXP x_sexp;
    const int* start_ptr;
    const int* stop_ptr;
    bool scalar_start;
    bool scalar_stop;
    std::vector<std::string>& results;
    std::vector<uint8_t>& is_na;

    SubstrWorker(SEXP x, const int* sp, bool ss,
                  const int* ep, bool es,
                  std::vector<std::string>& r, std::vector<uint8_t>& na)
        : x_sexp(x), start_ptr(sp), stop_ptr(ep),
          scalar_start(ss), scalar_stop(es), results(r), is_na(na) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            int st = scalar_start ? start_ptr[0] : start_ptr[i];
            int sp = scalar_stop  ? stop_ptr[0]  : stop_ptr[i];
            if (elem == NA_STRING || st == NA_INTEGER || sp == NA_INTEGER) {
                is_na[i] = 1; results[i] = ""; continue;
            }
            is_na[i] = 0;
            int len = LENGTH(elem);
            if (st < 1) st = 1;
            if (sp > len) sp = len;
            if (st > sp) { results[i] = ""; continue; }
            results[i].assign(CHAR(elem) + st - 1, (std::size_t)(sp - st + 1));
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_substr_impl(const StringVector& x,
                                  const IntegerVector& start,
                                  const IntegerVector& stop) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    std::vector<uint8_t>     is_na((std::size_t)n, 0);
    bool scalar_start = (start.size() == 1);
    bool scalar_stop  = (stop.size() == 1);
    SubstrWorker worker(x, start.begin(), scalar_start,
                         stop.begin(), scalar_stop, results, is_na);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);
    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (is_na[(std::size_t)i])
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

// ---------------------------------------------------------------------------
// nchar
// type: 0=bytes (LENGTH of CHARSXP), 1=chars (UTF-8 codepoints)
// allow_na: if false, NA → 2 (nchar("NA")) matching base R nchar(NA, allowNA=FALSE)
// ---------------------------------------------------------------------------

struct NcharWorker : public Worker {
    SEXP x_sexp;
    int type;
    bool allow_na;
    RVector<int> out;

    NcharWorker(SEXP x, int type, bool allow_na, IntegerVector& out)
        : x_sexp(x), type(type), allow_na(allow_na), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) {
                out[i] = allow_na ? NA_INTEGER : 2;
                continue;
            }
            if (type == 0) {
                out[i] = LENGTH(elem);
            } else {
                const unsigned char* s = (const unsigned char*)CHAR(elem);
                int len = LENGTH(elem), n = 0;
                for (int j = 0; j < len; ++j)
                    if ((s[j] & 0xC0u) != 0x80u) ++n;  // count non-continuation bytes
                out[i] = n;
            }
        }
    }
};

// [[Rcpp::export]]
IntegerVector fast_nchar_impl(const StringVector& x, int type, bool allow_na) {
    R_xlen_t n = x.size();
    IntegerVector result(n);
    NcharWorker worker(x, type, allow_na, result);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);
    return result;
}

// ---------------------------------------------------------------------------
// chartr — ASCII 256-byte lookup table. Falls back to base R for multi-byte
// characters (handled in the R wrapper).
// ---------------------------------------------------------------------------

struct ChartrWorker : public Worker {
    SEXP x_sexp;
    const unsigned char* table;
    std::vector<std::string>& results;

    ChartrWorker(SEXP x, const unsigned char* t, std::vector<std::string>& r)
        : x_sexp(x), table(t), results(r) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { results[i] = ""; continue; }
            const unsigned char* s = (const unsigned char*)CHAR(elem);
            int len = LENGTH(elem);
            results[i].resize((std::size_t)len);
            for (int j = 0; j < len; ++j)
                results[i][(std::size_t)j] = (char)table[s[j]];
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_chartr_impl(const std::string& old_chars,
                                   const std::string& new_chars,
                                   const StringVector& x) {
    // Build identity lookup table, then override old→new mappings.
    // table lives on the stack; parallelFor is synchronous so the pointer is valid.
    unsigned char table[256];
    for (int i = 0; i < 256; ++i) table[i] = (unsigned char)i;
    std::size_t map_len = std::min(old_chars.size(), new_chars.size());
    for (std::size_t k = 0; k < map_len; ++k)
        table[(unsigned char)old_chars[k]] = (unsigned char)new_chars[k];

    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);
    ChartrWorker worker(x, table, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (STRING_ELT(x, i) == NA_STRING)
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}
