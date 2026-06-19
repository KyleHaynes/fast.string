test_that("trimws matches base for default whitespace, including non-stripped chars", {
    x <- c("  hi  ", "\tworld\r\n", "abc\f", NA, "")
    expect_identical(fast.string::trimws(x), base::trimws(x))
    expect_identical(fast.string::trimws(x, "left"), base::trimws(x, "left"))
    expect_identical(fast.string::trimws(x, "right"), base::trimws(x, "right"))
})

test_that("trimws does not strip form feed (only ' \\t\\r\\n')", {
    expect_identical(fast.string::trimws("abc\f"), "abc\f")
})

test_that("trimws delegates to base for a custom whitespace regex", {
    x <- "xxhixx"
    expect_identical(fast.string::trimws(x, whitespace = "x"), base::trimws(x, whitespace = "x"))
})

test_that("trimws preserves names", {
    expect_identical(names(fast.string::trimws(c(a = " hi "))), "a")
})

test_that("trimws errors on non-character, non-NA input", {
    expect_error(fast.string::trimws(1:3), "character vector")
})

test_that("substr matches base including NA/out-of-range/recycling", {
    x <- c("hello", NA, "hi", "")
    expect_identical(fast.string::substr(x, 1, 3), base::substr(x, 1, 3))
    expect_identical(fast.string::substr(x, 2, 100), base::substr(x, 2, 100))
    expect_identical(fast.string::substr(x, c(1, 2, 1, 1), c(3, 4, 2, 1)),
                      base::substr(x, c(1, 2, 1, 1), c(3, 4, 2, 1)))
})

test_that("substr propagates NA from start/stop", {
    expect_true(is.na(fast.string::substr("hello", NA, 3)))
    expect_true(is.na(fast.string::substr("hello", 1, NA)))
})

test_that("substr preserves names", {
    expect_identical(names(fast.string::substr(c(a = "hello"), 1, 3)), "a")
})

test_that("nchar matches base for bytes/chars and NA handling", {
    x <- c("hello", NA, "", "café")
    expect_identical(fast.string::nchar(x), base::nchar(x))
    expect_identical(fast.string::nchar(x, "bytes"), base::nchar(x, "bytes"))
    expect_identical(fast.string::nchar(x, "chars"), base::nchar(x, "chars"))
})

test_that("nchar keepNA=FALSE matches base legacy behaviour", {
    x <- c("hello", NA)
    expect_identical(fast.string::nchar(x, keepNA = FALSE), base::nchar(x, keepNA = FALSE))
})

test_that("nchar type=width delegates to base", {
    x <- c("hello", NA)
    expect_identical(fast.string::nchar(x, type = "width"), base::nchar(x, type = "width"))
})

test_that("nchar coerces non-character input like base", {
    expect_identical(fast.string::nchar(12345), base::nchar(12345))
})

test_that("nchar is more lenient than base for factor input", {
    # base::nchar() errors on factors ("requires a character vector");
    # fast.string::nchar() coerces via as.character() first, so it succeeds.
    expect_error(base::nchar(factor("hello")), "character vector")
    expect_identical(fast.string::nchar(factor("hello")), base::nchar(as.character(factor("hello"))))
})

test_that("nchar preserves names", {
    expect_identical(names(fast.string::nchar(c(a = "hello"))), "a")
})

test_that("chartr matches base for simple ASCII translation", {
    x <- c("aabbcc", "zzz", NA)
    expect_identical(fast.string::chartr("abc", "xyz", x), base::chartr("abc", "xyz", x))
})

test_that("chartr allows equal character count with unequal byte count", {
    old <- "ab"
    new <- paste0(intToUtf8(233), "b") # 'eb' with an accented e (2 bytes, 1 char) + 'b'
    expect_identical(fast.string::chartr(old, new, "ab"), base::chartr(old, new, "ab"))
})

test_that("chartr errors when old/new have different character counts", {
    expect_error(fast.string::chartr("ab", "a", "ab"), "same number of characters")
})

test_that("chartr preserves names", {
    expect_identical(names(fast.string::chartr("a", "b", c(z = "a"))), "z")
})

test_that("chartr errors on invalid old/new", {
    expect_error(fast.string::chartr(c("a", "b"), "x", "abc"), "single string")
    expect_error(fast.string::chartr("a", c("x", "y"), "abc"), "single string")
})
