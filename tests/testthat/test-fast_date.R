test_that("format_date iso matches strftime", {
    d <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31", NA))
    expect_identical(fgrepl::format_date(d, "iso"), format(d, "%Y-%m-%d"))
})

test_that("format_date compact matches strftime", {
    d <- as.Date(c("1970-01-01", "2024-06-18", "1999-12-31"))
    expect_identical(fgrepl::format_date(d, "compact"), format(d, "%Y%m%d"))
})

test_that("format_date dmy matches strftime", {
    d <- as.Date(c("2024-06-18", "1999-12-31"))
    expect_identical(fgrepl::format_date(d, "dmy"), format(d, "%d/%m/%Y"))
})

test_that("format_date ymd_slash matches strftime", {
    d <- as.Date(c("2024-06-18", "1999-12-31"))
    expect_identical(fgrepl::format_date(d, "ymd_slash"), format(d, "%Y/%m/%d"))
})

test_that("format_date accepts numeric days-since-epoch", {
    d <- as.Date("2024-06-18")
    expect_identical(fgrepl::format_date(as.numeric(d), "iso"), fgrepl::format_date(d, "iso"))
})

test_that("format_date propagates NA", {
    expect_true(is.na(fgrepl::format_date(NA_real_, "iso")))
})

test_that("format_date errors on non-numeric, non-Date input", {
    expect_error(fgrepl::format_date("2024-06-18"), "Date or numeric")
})

test_that("date_parts extracts year/month/day correctly", {
    d <- as.Date(c("1985-03-15", "1990-07-22", "1975-12-01"))
    parts <- fgrepl::date_parts(d)
    expect_identical(parts$year,  c(1985L, 1990L, 1975L))
    expect_identical(parts$month, c(3L, 7L, 12L))
    expect_identical(parts$day,   c(15L, 22L, 1L))
})

test_that("date_parts propagates NA per-component", {
    parts <- fgrepl::date_parts(as.Date(NA))
    expect_true(is.na(parts$year) && is.na(parts$month) && is.na(parts$day))
})

test_that("date_parts returns a data.frame with the right shape", {
    d <- as.Date(c("2020-01-01", "2021-02-02"))
    parts <- fgrepl::date_parts(d)
    expect_s3_class(parts, "data.frame")
    expect_identical(nrow(parts), 2L)
    expect_identical(names(parts), c("year", "month", "day"))
})
