// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <string>
#include <vector>
#include <cmath>
using namespace Rcpp;
using namespace RcppParallel;

// Richards' algorithm: Julian Day Number → (year, month, day).
// R's Date epoch: 1970-01-01 = JDN 2440588.
static inline void jdn_to_ymd(int jdn, int& year, int& month, int& day) {
    int l = jdn + 68569;
    int n = (4 * l) / 146097;
    l = l - (146097 * n + 3) / 4;
    int i = (4000 * (l + 1)) / 1461001;
    l = l - (1461 * i) / 4 + 31;
    int j = (80 * l) / 2447;
    day   = l - (2447 * j) / 80;
    l     = j / 11;
    month = j + 2 - 12 * l;
    year  = 100 * (n - 49) + i + l;
}

static inline void write2(char* p, int v) {
    p[0] = (char)('0' + v / 10);
    p[1] = (char)('0' + v % 10);
}
static inline void write4(char* p, int v) {
    p[0] = (char)('0' + v / 1000);
    p[1] = (char)('0' + (v / 100) % 10);
    p[2] = (char)('0' + (v / 10) % 10);
    p[3] = (char)('0' + v % 10);
}

// format_code: 0=YYYY-MM-DD  1=YYYYMMDD  2=DD/MM/YYYY  3=YYYY/MM/DD
struct DateFormatWorker : public Worker {
    const double* dates;
    int format_code;
    std::vector<std::string>& results;

    DateFormatWorker(const double* d, int fc, std::vector<std::string>& r)
        : dates(d), format_code(fc), results(r) {}

    void operator()(std::size_t begin, std::size_t end) {
        char buf[11];
        for (std::size_t i = begin; i < end; ++i) {
            double d = dates[i];
            if (std::isnan(d)) { results[i] = ""; continue; }
            int y, m, day;
            jdn_to_ymd((int)d + 2440588, y, m, day);
            switch (format_code) {
                case 0:  // YYYY-MM-DD
                    write4(buf, y);    buf[4] = '-';
                    write2(buf+5, m);  buf[7] = '-';
                    write2(buf+8, day); buf[10] = '\0';
                    results[i].assign(buf, 10);
                    break;
                case 1:  // YYYYMMDD
                    write4(buf, y);
                    write2(buf+4, m);
                    write2(buf+6, day); buf[8] = '\0';
                    results[i].assign(buf, 8);
                    break;
                case 2:  // DD/MM/YYYY
                    write2(buf, day);   buf[2] = '/';
                    write2(buf+3, m);   buf[5] = '/';
                    write4(buf+6, y);   buf[10] = '\0';
                    results[i].assign(buf, 10);
                    break;
                case 3:  // YYYY/MM/DD
                    write4(buf, y);    buf[4] = '/';
                    write2(buf+5, m);  buf[7] = '/';
                    write2(buf+8, day); buf[10] = '\0';
                    results[i].assign(buf, 10);
                    break;
            }
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_format_date_impl(const NumericVector& x, int format_code) {
    R_xlen_t n = x.size();
    std::vector<std::string> results((std::size_t)n);

    DateFormatWorker worker(x.begin(), format_code, results);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    CharacterVector result(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        if (std::isnan(x[i]))
            SET_STRING_ELT(result, i, NA_STRING);
        else
            SET_STRING_ELT(result, i,
                Rf_mkCharCE(results[(std::size_t)i].c_str(), CE_UTF8));
    }
    return result;
}

// ---------------------------------------------------------------------------
// fas.Date — fixed-format date parsing (inverse of format_date()'s 4
// formats). NOT a drop-in for base::as.Date(): no locale/strptime/format
// auto-detection, exactly one fixed format per call, minimal validation
// (length, digit/separator positions, month 1-12, day 1-31 — no
// days-in-month/leap-year check). Malformed elements become NA.
// ---------------------------------------------------------------------------

static inline bool is_digit_ascii(char c) { return c >= '0' && c <= '9'; }

// Fliegel & Van Flandern algorithm — Gregorian (y,m,d) -> Julian Day Number,
// using ordinary truncating integer division. Verified against the epoch
// constant used by jdn_to_ymd() above: ymd_to_jdn(1970,1,1) == 2440588.
static inline long ymd_to_jdn(int y, int m, int d) {
    long a = (m - 14) / 12;
    return (1461L * (y + 4800 + a)) / 4
         + (367L  * (m - 2 - 12 * a)) / 12
         - (3L    * ((y + 4900 + a) / 100)) / 4
         + d - 32075;
}

static inline bool parse2(const char* s, int pos, int& out) {
    if (!is_digit_ascii(s[pos]) || !is_digit_ascii(s[pos + 1])) return false;
    out = (s[pos] - '0') * 10 + (s[pos + 1] - '0');
    return true;
}
static inline bool parse4(const char* s, int pos, int& out) {
    for (int k = 0; k < 4; ++k) if (!is_digit_ascii(s[pos + k])) return false;
    out = (s[pos]-'0')*1000 + (s[pos+1]-'0')*100 + (s[pos+2]-'0')*10 + (s[pos+3]-'0');
    return true;
}

// format_code: 0=YYYY-MM-DD  1=YYYYMMDD  2=DD/MM/YYYY  3=YYYY/MM/DD
static bool parse_date(const char* s, int len, int format_code, double& out_days) {
    int y = 0, m = 0, d = 0;
    bool ok = false;
    switch (format_code) {
        case 0: // YYYY-MM-DD (10 chars)
            ok = len == 10 && s[4] == '-' && s[7] == '-' &&
                 parse4(s, 0, y) && parse2(s, 5, m) && parse2(s, 8, d);
            break;
        case 1: // YYYYMMDD (8 chars)
            ok = len == 8 &&
                 parse4(s, 0, y) && parse2(s, 4, m) && parse2(s, 6, d);
            break;
        case 2: // DD/MM/YYYY (10 chars)
            ok = len == 10 && s[2] == '/' && s[5] == '/' &&
                 parse2(s, 0, d) && parse2(s, 3, m) && parse4(s, 6, y);
            break;
        case 3: // YYYY/MM/DD (10 chars)
            ok = len == 10 && s[4] == '/' && s[7] == '/' &&
                 parse4(s, 0, y) && parse2(s, 5, m) && parse2(s, 8, d);
            break;
    }
    if (!ok || m < 1 || m > 12 || d < 1 || d > 31) return false;
    out_days = (double)(ymd_to_jdn(y, m, d) - 2440588L);
    return true;
}

struct DateParseWorker : public Worker {
    SEXP x_sexp;
    int format_code;
    RVector<double> out;

    DateParseWorker(SEXP x, int fc, NumericVector& out)
        : x_sexp(x), format_code(fc), out(out) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            SEXP elem = STRING_ELT(x_sexp, i);
            if (elem == NA_STRING) { out[i] = NA_REAL; continue; }
            double days;
            if (parse_date(CHAR(elem), LENGTH(elem), format_code, days))
                out[i] = days;
            else
                out[i] = NA_REAL;
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_parse_date_impl(const StringVector& x, int format_code) {
    R_xlen_t n = x.size();
    NumericVector result(n);
    DateParseWorker worker(x, format_code, result);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);
    return result;
}

struct DatePartsWorker : public Worker {
    const double* dates;
    RVector<int> years;
    RVector<int> months;
    RVector<int> days;

    DatePartsWorker(const double* d, IntegerVector& y, IntegerVector& m, IntegerVector& dy)
        : dates(d), years(y), months(m), days(dy) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            double d = dates[i];
            if (std::isnan(d)) {
                years[i] = NA_INTEGER; months[i] = NA_INTEGER; days[i] = NA_INTEGER;
            } else {
                int y, m, dy;
                jdn_to_ymd((int)d + 2440588, y, m, dy);
                years[i] = y; months[i] = m; days[i] = dy;
            }
        }
    }
};

// [[Rcpp::export]]
List fast_date_parts_impl(const NumericVector& x) {
    R_xlen_t n = x.size();
    IntegerVector years(n), months(n), days(n);

    DatePartsWorker worker(x.begin(), years, months, days);
    if (n >= 10000) parallelFor(0, (std::size_t)n, worker);
    else            worker(0, (std::size_t)n);

    List out = List::create(Named("year") = years,
                             Named("month") = months,
                             Named("day") = days);
    out.attr("class")     = "data.frame";
    out.attr("row.names") = IntegerVector::create(NA_INTEGER, -(int)n);
    return out;
}
