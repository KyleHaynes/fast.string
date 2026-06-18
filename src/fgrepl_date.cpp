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
