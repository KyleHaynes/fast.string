// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <limits>
#include <vector>
#include "parallel_dispatch.h"
#include "string_snapshot.h"

#if defined(__x86_64__) && (defined(__GNUC__) || defined(__clang__))
#include <immintrin.h>
#define FAST_STRING_EPOCH_AVX 1
#endif

using namespace Rcpp;
using namespace RcppParallel;

static const std::uint8_t DATE_NA_LENGTH = 255;
static const std::size_t DATE_SLOT_WIDTH = 10;

static inline void jdn_to_ymd(int jdn, int& year, int& month, int& day) {
    int l = jdn + 68569;
    int n = (4 * l) / 146097;
    l = l - (146097 * n + 3) / 4;
    int i = (4000 * (l + 1)) / 1461001;
    l = l - (1461 * i) / 4 + 31;
    int j = (80 * l) / 2447;
    day = l - (2447 * j) / 80;
    l = j / 11;
    month = j + 2 - 12 * l;
    year = 100 * (n - 49) + i + l;
}

static inline void write2(char* output, int value) {
    output[0] = static_cast<char>('0' + value / 10);
    output[1] = static_cast<char>('0' + value % 10);
}

static inline void write4(char* output, int value) {
    output[0] = static_cast<char>('0' + value / 1000);
    output[1] = static_cast<char>('0' + (value / 100) % 10);
    output[2] = static_cast<char>('0' + (value / 10) % 10);
    output[3] = static_cast<char>('0' + value % 10);
}

static inline int format_ymd(char* output,
                             int format_code,
                             int year,
                             int month,
                             int day) {
    switch (format_code) {
        case 0:
            write4(output, year);
            output[4] = '-';
            write2(output + 5, month);
            output[7] = '-';
            write2(output + 8, day);
            return 10;
        case 1:
            write4(output, year);
            write2(output + 4, month);
            write2(output + 6, day);
            return 8;
        case 2:
            write2(output, day);
            output[2] = '/';
            write2(output + 3, month);
            output[5] = '/';
            write4(output + 6, year);
            return 10;
        case 3:
            write4(output, year);
            output[4] = '/';
            write2(output + 5, month);
            output[7] = '/';
            write2(output + 8, day);
            return 10;
    }
    return 0;
}

static inline bool ymd_in_width(int year, int month, int day) {
    return year >= 0 && year <= 9999 &&
        month >= 0 && month <= 99 &&
        day >= 0 && day <= 99;
}

static inline long ymd_to_jdn(int year, int month, int day);

static inline bool numeric_date_to_ymd(double value,
                                       int& year,
                                       int& month,
                                       int& day) {
    if (!std::isfinite(value))
        return false;
    const double whole_days = std::floor(value);
    const double minimum_days =
        static_cast<double>(ymd_to_jdn(0, 1, 1) - 2440588L);
    const double maximum_days =
        static_cast<double>(ymd_to_jdn(9999, 12, 31) - 2440588L);
    if (whole_days < minimum_days || whole_days > maximum_days)
        return false;
    const double jdn = whole_days + 2440588.0;
    jdn_to_ymd(static_cast<int>(jdn), year, month, day);
    return ymd_in_width(year, month, day);
}

static inline std::size_t checked_output_bytes(std::size_t n,
                                               std::size_t width) {
    if (n != 0 && width > (std::numeric_limits<std::size_t>::max)() / n)
        stop("Date output is too large.");
    return n * width;
}

struct DateFormatWorker : public Worker {
    const double* dates;
    int format_code;
    char* bytes;
    std::uint8_t* lengths;

    DateFormatWorker(const double* dates_,
                     int format_code_,
                     char* bytes_,
                     std::uint8_t* lengths_)
        : dates(dates_),
          format_code(format_code_),
          bytes(bytes_),
          lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            int year;
            int month;
            int day;
            if (!numeric_date_to_ymd(dates[i], year, month, day)) {
                lengths[i] = DATE_NA_LENGTH;
                continue;
            }
            char* output = bytes + i * DATE_SLOT_WIDTH;
            lengths[i] = static_cast<std::uint8_t>(
                format_ymd(output, format_code, year, month, day)
            );
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_format_date_impl(const NumericVector& x, int format_code) {
    const std::size_t n = static_cast<std::size_t>(x.size());
    std::vector<char> bytes(checked_output_bytes(n, DATE_SLOT_WIDTH));
    std::vector<std::uint8_t> lengths(n, DATE_NA_LENGTH);
    DateFormatWorker worker(x.begin(), format_code, bytes.data(), lengths.data());
    dispatch_for(0, n, worker, n, 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == DATE_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLen(
                    bytes.data() + i * DATE_SLOT_WIDTH,
                    static_cast<int>(lengths[i])
                )
            );
        }
    }
    return result;
}

struct PartsFormatWorker : public Worker {
    const int* years;
    const int* months;
    const int* days;
    int format_code;
    char* bytes;
    std::uint8_t* lengths;

    PartsFormatWorker(const int* years_,
                      const int* months_,
                      const int* days_,
                      int format_code_,
                      char* bytes_,
                      std::uint8_t* lengths_)
        : years(years_),
          months(months_),
          days(days_),
          format_code(format_code_),
          bytes(bytes_),
          lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const int year = years[i];
            const int month = months[i];
            const int day = days[i];
            if (year == NA_INTEGER || month == NA_INTEGER ||
                day == NA_INTEGER || !ymd_in_width(year, month, day)) {
                lengths[i] = DATE_NA_LENGTH;
                continue;
            }
            char* output = bytes + i * DATE_SLOT_WIDTH;
            lengths[i] = static_cast<std::uint8_t>(
                format_ymd(output, format_code, year, month, day)
            );
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_format_date_parts_impl(const IntegerVector& year,
                                             const IntegerVector& month,
                                             const IntegerVector& day,
                                             int format_code) {
    const std::size_t n = static_cast<std::size_t>(year.size());
    std::vector<char> bytes(checked_output_bytes(n, DATE_SLOT_WIDTH));
    std::vector<std::uint8_t> lengths(n, DATE_NA_LENGTH);
    PartsFormatWorker worker(
        year.begin(),
        month.begin(),
        day.begin(),
        format_code,
        bytes.data(),
        lengths.data()
    );
    dispatch_for(0, n, worker, n, 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == DATE_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result,
                static_cast<R_xlen_t>(i),
                Rf_mkCharLen(
                    bytes.data() + i * DATE_SLOT_WIDTH,
                    static_cast<int>(lengths[i])
                )
            );
        }
    }
    return result;
}

static inline bool is_digit_ascii(char value) {
    return value >= '0' && value <= '9';
}

static inline long ymd_to_jdn(int year, int month, int day) {
    const long a = (month - 14) / 12;
    return (1461L * (year + 4800 + a)) / 4
        + (367L * (month - 2 - 12 * a)) / 12
        - (3L * ((year + 4900 + a) / 100)) / 4
        + day - 32075;
}

static inline bool parse2(const char* input, std::size_t offset, int& output) {
    if (!is_digit_ascii(input[offset]) ||
        !is_digit_ascii(input[offset + 1]))
        return false;
    output = (input[offset] - '0') * 10 + (input[offset + 1] - '0');
    return true;
}

static inline bool parse4(const char* input, std::size_t offset, int& output) {
    for (std::size_t i = 0; i < 4; ++i) {
        if (!is_digit_ascii(input[offset + i]))
            return false;
    }
    output = (input[offset] - '0') * 1000
        + (input[offset + 1] - '0') * 100
        + (input[offset + 2] - '0') * 10
        + (input[offset + 3] - '0');
    return true;
}

static bool parse_date(const char* input,
                       std::size_t length,
                       int format_code,
                       double& output) {
    int year = 0;
    int month = 0;
    int day = 0;
    bool valid = false;
    switch (format_code) {
        case 0:
            valid = length == 10 && input[4] == '-' && input[7] == '-' &&
                parse4(input, 0, year) && parse2(input, 5, month) &&
                parse2(input, 8, day);
            break;
        case 1:
            valid = length == 8 &&
                parse4(input, 0, year) && parse2(input, 4, month) &&
                parse2(input, 6, day);
            break;
        case 2:
            valid = length == 10 && input[2] == '/' && input[5] == '/' &&
                parse2(input, 0, day) && parse2(input, 3, month) &&
                parse4(input, 6, year);
            break;
        case 3:
            valid = length == 10 && input[4] == '/' && input[7] == '/' &&
                parse4(input, 0, year) && parse2(input, 5, month) &&
                parse2(input, 8, day);
            break;
    }
    if (!valid || month < 1 || month > 12 || day < 1 || day > 31)
        return false;
    output = static_cast<double>(ymd_to_jdn(year, month, day) - 2440588L);
    return true;
}

struct DateParseWorker : public Worker {
    const StringView* strings;
    int format_code;
    RVector<double> output;

    DateParseWorker(const StringView* strings_,
                    int format_code_,
                    NumericVector& output_)
        : strings(strings_), format_code(format_code_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            double days;
            output[i] = !value.is_na() &&
                parse_date(value.data, value.size, format_code, days)
                ? days
                : NA_REAL;
        }
    }
};

static inline std::size_t date_parse_work(const StringSnapshot& snapshot) {
    const std::uint64_t units = static_cast<std::uint64_t>(snapshot.size()) +
        snapshot.total_bytes() / 10u;
    const std::uint64_t max_value =
        static_cast<std::uint64_t>((std::numeric_limits<std::size_t>::max)());
    return units > max_value
        ? (std::numeric_limits<std::size_t>::max)()
        : static_cast<std::size_t>(units);
}

// [[Rcpp::export]]
NumericVector fast_parse_date_impl(const StringVector& x, int format_code) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    NumericVector result(static_cast<R_xlen_t>(n));
    DateParseWorker worker(snapshot.data(), format_code, result);
    dispatch_for(0, n, worker, date_parse_work(snapshot), 10000);
    return result;
}

// ---- Epoch counts -> Date (numeric fas.Date) -------------------------------
//
// A count from an epoch becomes days since 1970-01-01 as
// floor(x / divisor + frac) + shift. Only spss scales (it counts seconds) and
// only julian_day has frac = 0.5 (Julian days begin at noon). Excel's 1900
// date system also counts 1900-02-29, which never existed: serial 60 has no
// date, and serials below it are one day later than the shift alone gives.
//
// One pass that is bound by writing freshly allocated pages, so the kernel has
// to run at copy speed to keep up with base R's as.Date(x), which only copies.
// The scalar std::floor is a library call on x86-64 without SSE4.1, so on
// CPUs with AVX a VEX-encoded 128-bit kernel is chosen at run time. It is as
// fast as 256-bit vectors here and never spills a 32-byte value, which MinGW
// cannot align on the stack (GCC bug 54412).

struct EpochSpec {
    double divisor;
    double frac;
    double shift;
    bool excel_1900;
    bool scaled;  // divisor != 1 or frac != 0
    double na;    // NA_REAL, read on the main thread
};

// Measured on a Ryzen 3700X: up to ~1M elements the input and output stay in
// L3 and one thread matches a memcpy; beyond it threads halve the time. The
// integer relabel is a plain copy of half the bytes, and threads only beat a
// serial memcpy from ~4M elements.
static const std::size_t EPOCH_PARALLEL_THRESHOLD = std::size_t(1) << 20;
static const std::size_t EPOCH_COPY_PARALLEL_THRESHOLD = std::size_t(1) << 22;
static const std::size_t EPOCH_GRAIN = std::size_t(1) << 18;
// Non-temporal stores skip the read-for-ownership of each output line, but
// leave the result out of cache, so only use them once the output (512 KB)
// would not fit in L2 anyway.
static const std::size_t EPOCH_STREAM_THRESHOLD = std::size_t(1) << 16;

static inline double epoch_day(double whole, const EpochSpec& epoch) {
    if (!epoch.excel_1900) return whole + epoch.shift;
    if (whole == 60.0) return epoch.na;
    return whole + epoch.shift + (whole < 61.0 ? 1.0 : 0.0);
}

static inline double epoch_day_from_double(double value,
                                           const EpochSpec& epoch) {
    if (epoch.scaled) value = value / epoch.divisor + epoch.frac;
    return std::isfinite(value)
        ? epoch_day(std::floor(value), epoch)
        : epoch.na;
}

static inline double epoch_day_from_int(int value, const EpochSpec& epoch) {
    if (value == NA_INTEGER) return epoch.na;
    // floor(v + frac) == v for an integer v, as 0 <= frac < 1.
    return epoch.divisor == 1.0
        ? epoch_day(static_cast<double>(value), epoch)
        : epoch_day_from_double(static_cast<double>(value), epoch);
}

typedef void (*DoubleEpochKernel)(const double*, double*, std::size_t,
                                  std::size_t, const EpochSpec&);
typedef void (*IntEpochKernel)(const int*, double*, std::size_t,
                               std::size_t, const EpochSpec&);

static void epoch_days_double_scalar(const double* x, double* out,
                                     std::size_t begin, std::size_t end,
                                     const EpochSpec& epoch) {
    for (std::size_t i = begin; i < end; ++i)
        out[i] = epoch_day_from_double(x[i], epoch);
}

static void epoch_days_int_scalar(const int* x, double* out,
                                  std::size_t begin, std::size_t end,
                                  const EpochSpec& epoch) {
    for (std::size_t i = begin; i < end; ++i)
        out[i] = epoch_day_from_int(x[i], epoch);
}

#ifdef FAST_STRING_EPOCH_AVX
// Two whole-day values -> two Dates; lanes where `valid` is clear become NA.
template <bool Excel>
__attribute__((target("avx")))
static inline __m128d epoch_day_avx(__m128d whole, __m128d valid,
                                    const EpochSpec& epoch) {
    __m128d day = _mm_add_pd(whole, _mm_set1_pd(epoch.shift));
    if (Excel) {
        const __m128d before_bug = _mm_cmplt_pd(whole, _mm_set1_pd(61.0));
        day = _mm_add_pd(day, _mm_and_pd(before_bug, _mm_set1_pd(1.0)));
        valid = _mm_andnot_pd(_mm_cmpeq_pd(whole, _mm_set1_pd(60.0)), valid);
    }
    return _mm_blendv_pd(_mm_set1_pd(epoch.na), day, valid);
}

template <bool Stream>
__attribute__((target("avx")))
static inline void epoch_store_avx(double* out, __m128d day) {
    if (Stream) _mm_stream_pd(out, day);
    else _mm_storeu_pd(out, day);
}

// Streaming stores need 16-byte alignment; R only promises 8.
static inline std::size_t epoch_align_16(const double* out, std::size_t i,
                                         std::size_t end) {
    return i < end && (reinterpret_cast<std::uintptr_t>(out + i) & 15u)
        ? i + 1
        : i;
}

template <bool Excel, bool Scaled, bool Stream>
__attribute__((target("avx")))
static void epoch_days_double_avx(const double* x, double* out,
                                  std::size_t i, std::size_t end,
                                  const EpochSpec& epoch) {
    if (Stream) {
        const std::size_t aligned = epoch_align_16(out, i, end);
        for (; i < aligned; ++i) out[i] = epoch_day_from_double(x[i], epoch);
    }
    const __m128d sign = _mm_set1_pd(-0.0);
    const __m128d infinity = _mm_set1_pd(std::numeric_limits<double>::infinity());
    const __m128d divisor = _mm_set1_pd(epoch.divisor);
    const __m128d frac = _mm_set1_pd(epoch.frac);
    for (; i + 2 <= end; i += 2) {
        __m128d value = _mm_loadu_pd(x + i);
        if (Scaled) value = _mm_add_pd(_mm_div_pd(value, divisor), frac);
        // |value| < Inf is false for NA, NaN and both infinities.
        const __m128d finite =
            _mm_cmplt_pd(_mm_andnot_pd(sign, value), infinity);
        epoch_store_avx<Stream>(out + i, epoch_day_avx<Excel>(
            _mm_floor_pd(value), finite, epoch
        ));
    }
    for (; i < end; ++i) out[i] = epoch_day_from_double(x[i], epoch);
    if (Stream) _mm_sfence();
}

template <bool Excel, bool Stream>
__attribute__((target("avx")))
static void epoch_days_int_avx(const int* x, double* out,
                               std::size_t i, std::size_t end,
                               const EpochSpec& epoch) {
    if (Stream) {
        const std::size_t aligned = epoch_align_16(out, i, end);
        for (; i < aligned; ++i) out[i] = epoch_day_from_int(x[i], epoch);
    }
    // NA_INTEGER converts exactly, so NA lanes can be found after conversion.
    const __m128d na_integer = _mm_set1_pd(static_cast<double>(NA_INTEGER));
    for (; i + 2 <= end; i += 2) {
        const __m128d whole = _mm_cvtepi32_pd(
            _mm_loadl_epi64(reinterpret_cast<const __m128i*>(x + i))
        );
        epoch_store_avx<Stream>(out + i, epoch_day_avx<Excel>(
            whole, _mm_cmpneq_pd(whole, na_integer), epoch
        ));
    }
    for (; i < end; ++i) out[i] = epoch_day_from_int(x[i], epoch);
    if (Stream) _mm_sfence();
}

static bool epoch_cpu_has_avx() {
    static const bool has_avx = [] {
        __builtin_cpu_init();
        return __builtin_cpu_supports("avx") != 0;
    }();
    return has_avx;
}
#endif

static DoubleEpochKernel pick_double_epoch_kernel(const EpochSpec& epoch,
                                                  bool stream) {
#ifdef FAST_STRING_EPOCH_AVX
    if (epoch_cpu_has_avx()) {
        // Excel serials are plain day counts, so Excel never scales.
        if (epoch.excel_1900)
            return stream ? epoch_days_double_avx<true, false, true>
                          : epoch_days_double_avx<true, false, false>;
        if (epoch.scaled)
            return stream ? epoch_days_double_avx<false, true, true>
                          : epoch_days_double_avx<false, true, false>;
        return stream ? epoch_days_double_avx<false, false, true>
                      : epoch_days_double_avx<false, false, false>;
    }
#endif
    (void)stream;
    return epoch_days_double_scalar;
}

static IntEpochKernel pick_int_epoch_kernel(const EpochSpec& epoch,
                                            bool stream) {
#ifdef FAST_STRING_EPOCH_AVX
    if (epoch.divisor == 1.0 && epoch_cpu_has_avx()) {
        if (epoch.excel_1900)
            return stream ? epoch_days_int_avx<true, true>
                          : epoch_days_int_avx<true, false>;
        return stream ? epoch_days_int_avx<false, true>
                      : epoch_days_int_avx<false, false>;
    }
#endif
    (void)stream;
    return epoch_days_int_scalar;
}

struct EpochDateWorker : public Worker {
    const int* ints;        // integer input, or null
    const double* doubles;  // double input, or null
    double* days;           // double output, or null when relabelling
    int* int_days;          // integer output for a zero-offset day epoch
    IntEpochKernel int_kernel;
    DoubleEpochKernel double_kernel;
    EpochSpec epoch;

    EpochDateWorker(const int* ints_, const double* doubles_, double* days_,
                    int* int_days_, IntEpochKernel int_kernel_,
                    DoubleEpochKernel double_kernel_, const EpochSpec& epoch_)
        : ints(ints_), doubles(doubles_), days(days_), int_days(int_days_),
          int_kernel(int_kernel_), double_kernel(double_kernel_),
          epoch(epoch_) {}

    void operator()(std::size_t begin, std::size_t end) {
        if (int_days)
            std::memcpy(int_days + begin, ints + begin,
                        (end - begin) * sizeof(int));
        else if (ints)
            int_kernel(ints, days, begin, end, epoch);
        else
            double_kernel(doubles, days, begin, end, epoch);
    }
};

// `spec` is c(divisor, frac, shift, excel_1900) from .date_epochs in R.
// Integer input on a zero-offset day epoch keeps integer storage, exactly as
// base::as.Date(x) relabels it; everything else returns double days.
// [[Rcpp::export(rng = false)]]
SEXP fast_epoch_date_impl(SEXP x, const NumericVector& spec) {
    const int type = TYPEOF(x);
    if (type != INTSXP && type != REALSXP)
        stop("`x` must be an integer or double vector.");
    const EpochSpec epoch = {
        spec[0], spec[1], spec[2], spec[3] != 0.0,
        spec[0] != 1.0 || spec[1] != 0.0, NA_REAL
    };
    const std::size_t n = static_cast<std::size_t>(Rf_xlength(x));
    const bool relabel = type == INTSXP && !epoch.excel_1900 &&
        epoch.divisor == 1.0 && epoch.shift == 0.0;
    const bool stream = n >= EPOCH_STREAM_THRESHOLD;

    SEXP result = PROTECT(Rf_allocVector(
        relabel ? INTSXP : REALSXP, static_cast<R_xlen_t>(n)
    ));
    EpochDateWorker worker(
        type == INTSXP ? INTEGER(x) : NULL,
        type == REALSXP ? REAL(x) : NULL,
        relabel ? NULL : REAL(result),
        relabel ? INTEGER(result) : NULL,
        pick_int_epoch_kernel(epoch, stream),
        pick_double_epoch_kernel(epoch, stream),
        epoch
    );
    dispatch_for(0, n, worker, n,
                 relabel ? EPOCH_COPY_PARALLEL_THRESHOLD
                         : EPOCH_PARALLEL_THRESHOLD,
                 -1, EPOCH_GRAIN);

    SEXP names = Rf_getAttrib(x, R_NamesSymbol);
    if (names != R_NilValue) Rf_setAttrib(result, R_NamesSymbol, names);
    SEXP date_class = PROTECT(Rf_mkString("Date"));
    Rf_setAttrib(result, R_ClassSymbol, date_class);
    UNPROTECT(2);
    return result;
}

static const std::size_t DATETIME_SLOT_WIDTH = 25;

static inline bool is_leap_year(int year) {
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

static inline int days_in_month(int year, int month) {
    static const int DAYS[] = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };
    if (month < 1 || month > 12) return 0;
    return month == 2 && is_leap_year(year)
        ? 29
        : DAYS[month - 1];
}

static inline bool valid_datetime_parts(int year, int month, int day,
                                        int hour, int minute, int second) {
    return year >= 0 && year <= 9999 &&
        day >= 1 && day <= days_in_month(year, month) &&
        hour >= 0 && hour <= 23 &&
        minute >= 0 && minute <= 59 &&
        second >= 0 && second <= 59;
}

static bool parse_datetime(const char* input,
                           std::size_t length,
                           int format_code,
                           double& output) {
    int year = 0, month = 0, day = 0;
    int hour = 0, minute = 0, second = 0;
    int offset_minutes = 0;
    bool valid = false;

    switch (format_code) {
        case 0:
            valid = length == 19 && input[4] == '-' && input[7] == '-' &&
                input[10] == ' ' && input[13] == ':' && input[16] == ':' &&
                parse4(input, 0, year) && parse2(input, 5, month) &&
                parse2(input, 8, day) && parse2(input, 11, hour) &&
                parse2(input, 14, minute) && parse2(input, 17, second);
            break;
        case 1:
            valid = length == 20 && input[4] == '-' && input[7] == '-' &&
                input[10] == 'T' && input[13] == ':' && input[16] == ':' &&
                input[19] == 'Z' &&
                parse4(input, 0, year) && parse2(input, 5, month) &&
                parse2(input, 8, day) && parse2(input, 11, hour) &&
                parse2(input, 14, minute) && parse2(input, 17, second);
            break;
        case 2:
            valid = length == 14 &&
                parse4(input, 0, year) && parse2(input, 4, month) &&
                parse2(input, 6, day) && parse2(input, 8, hour) &&
                parse2(input, 10, minute) && parse2(input, 12, second);
            break;
        case 3: {
            int offset_hour = 0;
            int offset_minute = 0;
            valid = length == 25 && input[4] == '-' && input[7] == '-' &&
                input[10] == 'T' && input[13] == ':' && input[16] == ':' &&
                (input[19] == '+' || input[19] == '-') && input[22] == ':' &&
                parse4(input, 0, year) && parse2(input, 5, month) &&
                parse2(input, 8, day) && parse2(input, 11, hour) &&
                parse2(input, 14, minute) && parse2(input, 17, second) &&
                parse2(input, 20, offset_hour) &&
                parse2(input, 23, offset_minute) &&
                offset_hour <= 23 && offset_minute <= 59;
            offset_minutes = offset_hour * 60 + offset_minute;
            if (length == 25 && input[19] == '-')
                offset_minutes = -offset_minutes;
            break;
        }
    }

    if (!valid || !valid_datetime_parts(
            year, month, day, hour, minute, second))
        return false;
    const double days = static_cast<double>(
        ymd_to_jdn(year, month, day) - 2440588L
    );
    output = days * 86400.0 + hour * 3600.0 + minute * 60.0 + second
        - offset_minutes * 60.0;
    return true;
}

struct DateTimeParseWorker : public Worker {
    const StringView* strings;
    int format_code;
    RVector<double> output;

    DateTimeParseWorker(const StringView* strings_,
                        int format_code_,
                        NumericVector& output_)
        : strings(strings_), format_code(format_code_), output(output_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            const StringView& value = strings[i];
            double seconds;
            output[i] = !value.is_na() && parse_datetime(
                value.data, value.size, format_code, seconds
            ) ? seconds : NA_REAL;
        }
    }
};

// [[Rcpp::export]]
NumericVector fast_parse_datetime_impl(const StringVector& x,
                                       int format_code) {
    const StringSnapshot snapshot(x);
    const std::size_t n = snapshot.size();
    NumericVector result(static_cast<R_xlen_t>(n));
    DateTimeParseWorker worker(snapshot.data(), format_code, result);
    dispatch_for(0, n, worker, date_parse_work(snapshot), 10000);
    return result;
}

static bool datetime_to_parts(double value,
                              int offset_minutes,
                              int& year,
                              int& month,
                              int& day,
                              int& hour,
                              int& minute,
                              int& second) {
    if (!std::isfinite(value)) return false;
    const double adjusted = value + offset_minutes * 60.0;
    const double whole_seconds = std::floor(adjusted);
    const double whole_days = std::floor(whole_seconds / 86400.0);
    const double minimum_days =
        static_cast<double>(ymd_to_jdn(0, 1, 1) - 2440588L);
    const double maximum_days =
        static_cast<double>(ymd_to_jdn(9999, 12, 31) - 2440588L);
    if (whole_days < minimum_days || whole_days > maximum_days)
        return false;

    const int seconds_of_day = static_cast<int>(
        whole_seconds - whole_days * 86400.0
    );
    hour = seconds_of_day / 3600;
    minute = (seconds_of_day / 60) % 60;
    second = seconds_of_day % 60;
    jdn_to_ymd(
        static_cast<int>(whole_days + 2440588.0), year, month, day
    );
    return valid_datetime_parts(year, month, day, hour, minute, second);
}

static int format_datetime_value(char* output,
                                 int format_code,
                                 int offset_minutes,
                                 int year,
                                 int month,
                                 int day,
                                 int hour,
                                 int minute,
                                 int second) {
    if (format_code == 2) {
        format_ymd(output, 1, year, month, day);
        write2(output + 8, hour);
        write2(output + 10, minute);
        write2(output + 12, second);
        return 14;
    }

    format_ymd(output, 0, year, month, day);
    output[10] = format_code == 0 ? ' ' : 'T';
    write2(output + 11, hour);
    output[13] = ':';
    write2(output + 14, minute);
    output[16] = ':';
    write2(output + 17, second);
    if (format_code == 0) return 19;
    if (format_code == 1) {
        output[19] = 'Z';
        return 20;
    }

    int absolute_offset = offset_minutes;
    output[19] = '+';
    if (absolute_offset < 0) {
        output[19] = '-';
        absolute_offset = -absolute_offset;
    }
    write2(output + 20, absolute_offset / 60);
    output[22] = ':';
    write2(output + 23, absolute_offset % 60);
    return 25;
}

struct DateTimeFormatWorker : public Worker {
    const double* values;
    int format_code;
    int offset_minutes;
    char* bytes;
    std::uint8_t* lengths;

    DateTimeFormatWorker(const double* values_,
                         int format_code_,
                         int offset_minutes_,
                         char* bytes_,
                         std::uint8_t* lengths_)
        : values(values_), format_code(format_code_),
          offset_minutes(offset_minutes_), bytes(bytes_), lengths(lengths_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            int year, month, day, hour, minute, second;
            if (!datetime_to_parts(
                    values[i], offset_minutes,
                    year, month, day, hour, minute, second)) {
                lengths[i] = DATE_NA_LENGTH;
                continue;
            }
            lengths[i] = static_cast<std::uint8_t>(format_datetime_value(
                bytes + i * DATETIME_SLOT_WIDTH,
                format_code, offset_minutes,
                year, month, day, hour, minute, second
            ));
        }
    }
};

// [[Rcpp::export]]
CharacterVector fast_format_datetime_impl(const NumericVector& x,
                                          int format_code,
                                          int offset_minutes) {
    const std::size_t n = static_cast<std::size_t>(x.size());
    std::vector<char> bytes(checked_output_bytes(n, DATETIME_SLOT_WIDTH));
    std::vector<std::uint8_t> lengths(n, DATE_NA_LENGTH);
    DateTimeFormatWorker worker(
        x.begin(), format_code, offset_minutes, bytes.data(), lengths.data()
    );
    dispatch_for(0, n, worker, n, 10000);

    CharacterVector result(static_cast<R_xlen_t>(n));
    for (std::size_t i = 0; i < n; ++i) {
        if (lengths[i] == DATE_NA_LENGTH) {
            SET_STRING_ELT(result, static_cast<R_xlen_t>(i), NA_STRING);
        } else {
            SET_STRING_ELT(
                result, static_cast<R_xlen_t>(i),
                Rf_mkCharLen(
                    bytes.data() + i * DATETIME_SLOT_WIDTH,
                    static_cast<int>(lengths[i])
                )
            );
        }
    }
    return result;
}

struct DatePartsWorker : public Worker {
    const double* dates;
    RVector<int> years;
    RVector<int> months;
    RVector<int> days;

    DatePartsWorker(const double* dates_,
                    IntegerVector& years_,
                    IntegerVector& months_,
                    IntegerVector& days_)
        : dates(dates_), years(years_), months(months_), days(days_) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            int year;
            int month;
            int day;
            if (!numeric_date_to_ymd(dates[i], year, month, day)) {
                years[i] = NA_INTEGER;
                months[i] = NA_INTEGER;
                days[i] = NA_INTEGER;
            } else {
                years[i] = year;
                months[i] = month;
                days[i] = day;
            }
        }
    }
};

// [[Rcpp::export]]
List fast_date_parts_impl(const NumericVector& x) {
    const std::size_t n = static_cast<std::size_t>(x.size());
    IntegerVector years(static_cast<R_xlen_t>(n));
    IntegerVector months(static_cast<R_xlen_t>(n));
    IntegerVector days(static_cast<R_xlen_t>(n));
    DatePartsWorker worker(x.begin(), years, months, days);
    dispatch_for(0, n, worker, n, 10000);

    List output = List::create(
        Named("year") = years,
        Named("month") = months,
        Named("day") = days
    );
    output.attr("class") = "data.frame";
    output.attr("row.names") = IntegerVector::create(
        NA_INTEGER,
        -static_cast<int>(n)
    );
    return output;
}
