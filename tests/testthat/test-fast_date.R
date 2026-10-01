test_that("format_date iso matches strftime", {
    d <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31", NA))
    expect_identical(fast.string::format_date(d, "iso"), format(d, "%Y-%m-%d"))
})

test_that("format_date compact matches strftime", {
    d <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31"))
    expect_identical(fast.string::format_date(d, "compact"), format(d, "%Y%m%d"))
})

test_that("format_date dmy matches strftime", {
    d <- as.Date(c("2024-06-18", "1999-12-31"))
    expect_identical(fast.string::format_date(d, "dmy"), format(d, "%d/%m/%Y"))
})

test_that("format_date ymd_slash matches strftime", {
    d <- as.Date(c("2024-06-18", "1999-12-31"))
    expect_identical(fast.string::format_date(d, "ymd_slash"), format(d, "%Y/%m/%d"))
})

test_that("format_date accepts numeric days-since-epoch", {
    d <- as.Date("2024-06-18")
    expect_identical(fast.string::format_date(as.numeric(d), "iso"), fast.string::format_date(d, "iso"))
})

test_that("format_date propagates NA", {
    expect_true(is.na(fast.string::format_date(NA_real_, "iso")))
})

test_that("date helpers floor fractional days and reject non-finite values safely", {
    x <- c(0, 0.9, -0.1, Inf, -Inf, NaN, NA_real_)
    expect_identical(
        fast.string::format_date(x),
        c("1970-01-01", "1970-01-01", "1969-12-31",
          NA, NA, NA, NA)
    )

    parts <- fast.string::date_parts(x)
    expect_identical(parts$year[1:3], c(1970L, 1970L, 1969L))
    expect_true(all(is.na(parts$year[4:7])))
    expect_true(all(is.na(parts$month[4:7])))
    expect_true(all(is.na(parts$day[4:7])))
})

test_that("format_date errors on non-numeric, non-Date input", {
    expect_error(fast.string::format_date("2024-06-18"), "Date or numeric")
})

test_that("date_parts extracts year/month/day correctly", {
    d <- as.Date(c("1985-03-15", "1990-07-22", "1975-12-01"))
    parts <- fast.string::date_parts(d)
    expect_identical(parts$year,  c(1985L, 1990L, 1975L))
    expect_identical(parts$month, c(3L, 7L, 12L))
    expect_identical(parts$day,   c(15L, 22L, 1L))
})

test_that("date_parts propagates NA per-component", {
    parts <- fast.string::date_parts(as.Date(NA))
    expect_true(is.na(parts$year) && is.na(parts$month) && is.na(parts$day))
})

test_that("date_parts returns a data.frame with the right shape", {
    d <- as.Date(c("2020-01-01", "2021-02-02"))
    parts <- fast.string::date_parts(d)
    expect_s3_class(parts, "data.frame")
    expect_identical(nrow(parts), 2L)
    expect_identical(names(parts), c("year", "month", "day"))
})

test_that("format_date_parts matches format_date via date_parts round-trip", {
    d <- as.Date(c("1983-01-20", "2024-06-18", "1999-12-31", "2000-02-29"))
    parts <- fast.string::date_parts(d)
    for (fmt in c("iso", "compact", "dmy", "ymd_slash")) {
        expect_identical(
            fast.string::format_date_parts(parts$year, parts$month, parts$day, fmt),
            fast.string::format_date(d, fmt)
        )
    }
})

test_that("format_date_parts matches the motivating example", {
    expect_identical(fast.string::format_date_parts(1983, 1, 20, "iso"), "1983-01-20")
})

test_that("format_date_parts pads single digits and supports all 4 formats", {
    expect_identical(fast.string::format_date_parts(5, 1, 2, "iso"), "0005-01-02")
    expect_identical(fast.string::format_date_parts(1983, 1, 20, "compact"), "19830120")
    expect_identical(fast.string::format_date_parts(1983, 1, 20, "dmy"), "20/01/1983")
    expect_identical(fast.string::format_date_parts(1983, 1, 20, "ymd_slash"), "1983/01/20")
})

test_that("format_date_parts recycles shorter inputs", {
    res <- fast.string::format_date_parts(1983, 1, c(1, 2, 3), "iso")
    expect_identical(res, c("1983-01-01", "1983-01-02", "1983-01-03"))
})

test_that("format_date_parts returns NA for NA or out-of-width input, without erroring", {
    res <- fast.string::format_date_parts(
        year  = c(1983, NA,   10000, 1983),
        month = c(1,    1,    1,     1),
        day   = c(20,   20,   20,    100),
        format = "iso"
    )
    expect_false(is.na(res[1]))
    expect_true(is.na(res[2])) # NA year
    expect_true(is.na(res[3])) # year out of 0-9999 width
    expect_true(is.na(res[4])) # day out of 0-99 width
})

test_that("format_date_parts does not validate calendar correctness (documented trade-off)", {
    # month=13, day=99 fit the field width and are formatted as-is.
    expect_identical(fast.string::format_date_parts(2024, 13, 99, "iso"), "2024-13-99")
})

test_that("format_date_parts errors on non-numeric input", {
    expect_error(fast.string::format_date_parts("1983", 1, 20), "numeric vectors")
})

test_that("fas.Date round-trips with format_date for all 4 formats", {
    d <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31", "2000-02-29"))
    for (fmt in c("iso", "compact", "dmy", "ymd_slash")) {
        strs <- fast.string::format_date(d, fmt)
        expect_identical(fast.string::fas.Date(strs, fmt), d)
    }
})

test_that("fas.Date matches base::as.Date for well-formed input", {
    strs <- c("1970-01-01", "2024-06-18", "1999-12-31")
    expect_identical(fast.string::fas.Date(strs, "iso"), as.Date(strs))
})

test_that("fas.Date returns a Date object", {
    expect_s3_class(fast.string::fas.Date("2024-06-18", "iso"), "Date")
})

test_that("fas.Date propagates NA and rejects malformed input without erroring", {
    x <- c("2024-06-18", NA, "not-a-date", "2024-13-01", "2024-06-99", "2024-06-1")
    res <- fast.string::fas.Date(x, "iso")
    expect_false(is.na(res[1]))
    expect_true(is.na(res[2]))  # NA input
    expect_true(is.na(res[3]))  # non-digits
    expect_true(is.na(res[4]))  # month 13 out of range
    expect_true(is.na(res[5]))  # day 99 out of range
    expect_true(is.na(res[6]))  # wrong length (9 chars)
})

test_that("fas.Date does not validate days-in-month (documented trade-off)", {
    # "2024-02-30" has no calendar validation, unlike base::as.Date(), by
    # design -- base actually errors outright on this input.
    expect_false(is.na(fast.string::fas.Date("2024-02-30", "iso")))
    expect_error(as.Date("2024-02-30"), "unambiguous format")
})

test_that("fas.Date parses all 4 formats correctly", {
    expect_identical(fast.string::fas.Date("2024-06-18", "iso"), as.Date("2024-06-18"))
    expect_identical(fast.string::fas.Date("20240618", "compact"), as.Date("2024-06-18"))
    expect_identical(fast.string::fas.Date("18/06/2024", "dmy"), as.Date("2024-06-18"))
    expect_identical(fast.string::fas.Date("2024/06/18", "ymd_slash"), as.Date("2024-06-18"))
})

test_that("fas.Date preserves names and errors on unsupported input", {
    expect_identical(names(fast.string::fas.Date(c(a = "2024-06-18"))), "a")
    expect_error(fast.string::fas.Date(factor("2024-06-18")), "character vector")
    expect_error(fast.string::fas.Date(list("2024-06-18")), "character vector")
})

# Numeric fas.Date() worked out with plain R arithmetic from the same epoch
# table, so the vectorised C++ kernels can be checked element by element.
epoch_reference <- function(x, origin) {
    spec <- fast.string:::.date_epochs[[origin]]
    whole <- floor(as.double(x) / spec[["divisor"]] + spec[["frac"]])
    days <- whole + spec[["shift"]]
    if (spec[["excel"]] == 1) {
        days <- days + (whole < 61)
        days[which(whole == 60)] <- NA
    }
    days[!is.finite(days)] <- NA_real_
    structure(days, class = "Date")
}

test_that("fas.Date maps each standard epoch to its documented dates", {
    d <- function(...) as.Date(c(...))
    fd <- function(x, origin) fast.string::fas.Date(x, origin = origin)
    expect_identical(fd(c(0, 19892), "unix"), d("1970-01-01", "2024-06-18"))
    expect_identical(fd(c(1, 59, 61, 45000), "excel"),
                     d("1900-01-01", "1900-02-28", "1900-03-01", "2023-03-15"))
    expect_identical(fd(c(0, 43538), "excel1904"), d("1904-01-01", "2023-03-15"))
    expect_identical(fd(c(0, 14610), "sas"), d("1960-01-01", "2000-01-01"))
    expect_identical(fd(c(0, 14610), "stata"), d("1960-01-01", "2000-01-01"))
    expect_identical(fd(c(0, 13166064000, 13166064000 + 86399, 13166064000 + 86400), "spss"),
                     d("1582-10-14", "2000-01-01", "2000-01-01", "2000-01-02"))
    expect_identical(fd(c(1, 730486), "matlab"), d("0000-01-01", "2000-01-01"))
    # Julian days begin at noon: JD 2451544.5 is midnight starting 2000-01-01.
    expect_identical(fd(c(2451544.49, 2451544.5, 2451545, 2451545.49, 2451545.5), "julian_day"),
                     d("1999-12-31", "2000-01-01", "2000-01-01", "2000-01-01", "2000-01-02"))
    expect_identical(fd(2451545L, "julian_day"), d("2000-01-01"))
    expect_identical(fd(c(0, 51544), "mjd"), d("1858-11-17", "2000-01-01"))
    expect_identical(fd(c(1, 730120), "rata_die"), d("0001-01-01", "2000-01-01"))
})

test_that("fas.Date's excel epoch skips Excel's phantom 1900-02-29", {
    expect_identical(fast.string::fas.Date(c(59, 60, 60.5, 61), origin = "excel"),
                     as.Date(c("1900-02-28", NA, NA, "1900-03-01")))
    # From serial 61 on it agrees with the usual base idiom.
    x <- c(61, 25569, 45000, 60000)
    expect_identical(fast.string::fas.Date(x, origin = "excel"), as.Date(x, origin = "1899-12-30"))
    expect_identical(fast.string::fas.Date(as.integer(x), origin = "excel"),
                     as.Date(as.integer(x), origin = "1899-12-30"))
})

test_that("fas.Date accepts custom origins and other spellings of named ones", {
    x <- c(-400, 0, 1, 36524)
    expect_identical(fast.string::fas.Date(x, origin = "2000-01-01"), as.Date(x, origin = "2000-01-01"))
    expect_identical(fast.string::fas.Date(x, origin = as.Date("1899-12-30")),
                     as.Date(x, origin = "1899-12-30"))
    expect_identical(fast.string::fas.Date(x, origin = "EXCEL1904"),
                     fast.string::fas.Date(x, origin = "excel1904"))
})

test_that("fas.Date keeps integer storage only where base::as.Date() does", {
    x <- c(a = 0L, b = 19892L, c = NA)
    expect_identical(fast.string::fas.Date(x), as.Date(x))
    expect_type(unclass(fast.string::fas.Date(x)), "integer")
    expect_identical(fast.string::fas.Date(x, origin = "sas"), as.Date(x, origin = "1960-01-01"))
    expect_type(unclass(fast.string::fas.Date(x, origin = "excel")), "double")
    expect_identical(names(fast.string::fas.Date(c(a = 1.5, b = 2), origin = "sas")), c("a", "b"))
    expect_identical(fast.string::fas.Date(integer()), as.Date(integer()))
    expect_identical(fast.string::fas.Date(numeric(), origin = "excel"), as.Date(character()))
})

test_that("fas.Date floors fractional counts and gives NA for non-finite ones", {
    x <- c(0.25, -0.25, -1, 1.999, NA, NaN, Inf, -Inf, 3e9 + 0.5, -3e9 - 0.5)
    expect_identical(unclass(fast.string::fas.Date(x)),
                     c(0, -1, -1, 1, NA, NA, NA, NA, 3e9, -3e9 - 1))
})

test_that("numeric fas.Date matches the R reference for every epoch, type and code path", {
    set.seed(42)
    x <- sample(c(
        runif(2e5, -1e6, 1e6), sample(55:66, 1000, TRUE) + c(0, 0.5),
        2451545 + runif(1000, -2, 2), runif(1000, 1e10, 1.5e10),
        NA, NaN, Inf, -Inf, 1e300, -1e300, -0
    ))
    xi <- sample(c(sample(-1e6:1e6, 2e5, TRUE), 55:66, NA,
                   .Machine$integer.max, -.Machine$integer.max))
    for (origin in names(fast.string:::.date_epochs)) {
        # Each start shifts the output's 16-byte alignment and the tail length.
        for (start in 1:3) {
            xs <- x[start:length(x)]
            xis <- xi[start:length(xi)]
            expect_identical(fast.string::fas.Date(xs, origin = origin),
                             epoch_reference(xs, origin), info = origin)
            expect_identical(as.double(unclass(fast.string::fas.Date(xis, origin = origin))),
                             unclass(epoch_reference(xis, origin)), info = origin)
        }
    }
    # Long enough for the threaded path.
    big <- rep_len(x, 1.2e6)
    expect_identical(fast.string::fas.Date(big, origin = "excel"), epoch_reference(big, "excel"))
    big_int <- rep_len(xi, 1.2e6)
    expect_identical(fast.string::fas.Date(big_int, origin = "sas"), epoch_reference(big_int, "sas"))
    # The integer relabel (a plain copy) only threads from 2^22 elements.
    big_copy <- rep_len(xi, 4.3e6)
    expect_identical(fast.string::fas.Date(big_copy), as.Date(big_copy))
})

test_that("fas.Date rejects bad origins and arguments meant for the other input type", {
    msg <- "`origin` must be one of"
    expect_error(fast.string::fas.Date(1, origin = "lotus"), msg)
    expect_error(fast.string::fas.Date(1, origin = "2024-02-30"), msg)
    expect_error(fast.string::fas.Date(1, origin = c("unix", "excel")), msg)
    expect_error(fast.string::fas.Date(1, origin = NA_character_), msg)
    expect_error(fast.string::fas.Date(1, origin = 0), msg)
    expect_error(fast.string::fas.Date(1, origin = as.Date(NA)), msg)
    expect_error(fast.string::fas.Date(45000, "iso"), "`format` applies to character")
    expect_error(fast.string::fas.Date("45000", origin = "excel"), "`origin` applies to numeric")
    # An all-NA column of unknown type still gives NA dates either way.
    expect_identical(fast.string::fas.Date(NA, origin = "excel"), as.Date(NA))
})
