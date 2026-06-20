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

test_that("fas.Date preserves names and errors on non-character input", {
    expect_identical(names(fast.string::fas.Date(c(a = "2024-06-18"))), "a")
    expect_error(fast.string::fas.Date(20240618), "character vector")
})
