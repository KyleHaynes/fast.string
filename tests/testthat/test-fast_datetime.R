test_that("fas.POSIXct parses all supported fixed formats", {
    expected <- as.POSIXct("2024-06-18 12:34:56", tz = "UTC")
    expect_identical(
        fast.string::fas.POSIXct("2024-06-18 12:34:56", "iso"), expected
    )
    expect_identical(
        fast.string::fas.POSIXct("2024-06-18T12:34:56Z", "rfc3339"), expected
    )
    expect_identical(
        fast.string::fas.POSIXct("20240618123456", "compact"), expected
    )
    expect_identical(
        fast.string::fas.POSIXct("2024-06-18T22:34:56+10:00", "iso_offset"),
        expected
    )
})

test_that("datetime offset parsing handles both signs", {
    epoch <- structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_identical(
        fast.string::fas.POSIXct("1970-01-01T10:00:00+10:00", "iso_offset"),
        epoch
    )
    expect_identical(
        fast.string::fas.POSIXct("1969-12-31T19:00:00-05:00", "iso_offset"),
        epoch
    )
})

test_that("fas.POSIXct validates calendar and clock fields", {
    x <- c(
        "2024-02-29 23:59:59", "2023-02-29 12:00:00",
        "2024-04-31 12:00:00", "2024-01-01 24:00:00",
        "2024-01-01 12:60:00", "2024-01-01 12:00:60",
        "not-a-time", NA_character_
    )
    result <- fast.string::fas.POSIXct(x, "iso")
    expect_false(is.na(result[[1L]]))
    expect_true(all(is.na(result[-1L])))
})

test_that("format_datetime formats UTC and fixed offsets", {
    x <- as.POSIXct(c("1970-01-01 00:00:00", "2024-06-18 12:34:56"), tz = "UTC")
    expect_identical(
        fast.string::format_datetime(x, "iso"),
        c("1970-01-01 00:00:00", "2024-06-18 12:34:56")
    )
    expect_identical(
        fast.string::format_datetime(x, "rfc3339"),
        c("1970-01-01T00:00:00Z", "2024-06-18T12:34:56Z")
    )
    expect_identical(
        fast.string::format_datetime(x, "compact"),
        c("19700101000000", "20240618123456")
    )
    expect_identical(
        fast.string::format_datetime(x[[1L]], "iso_offset", "+10:00"),
        "1970-01-01T10:00:00+10:00"
    )
})

test_that("datetime formatting floors fractional seconds and handles pre-epoch values", {
    expect_identical(
        fast.string::format_datetime(c(-0.1, 0.9), "iso"),
        c("1969-12-31 23:59:59", "1970-01-01 00:00:00")
    )
})

test_that("datetime functions preserve names and stable empty types", {
    parsed <- fast.string::fas.POSIXct(c(a = "2024-01-02 03:04:05"))
    expect_identical(names(parsed), "a")
    expect_s3_class(parsed, "POSIXct")
    expect_identical(attr(parsed, "tzone"), "UTC")
    expect_identical(length(fast.string::fas.POSIXct(character())), 0L)
    expect_identical(
        fast.string::format_datetime(setNames(numeric(), character())),
        setNames(character(), character())
    )
})

test_that("datetime functions validate API inputs", {
    expect_error(fast.string::fas.POSIXct(20240101), "character vector")
    expect_error(fast.string::format_datetime("2024"), "POSIXct or numeric")
    expect_error(
        fast.string::format_datetime(0, "iso", "+10:00"),
        "unless.*iso_offset"
    )
    expect_error(
        fast.string::format_datetime(0, "iso_offset", "+24:00"),
        "hour"
    )
})
