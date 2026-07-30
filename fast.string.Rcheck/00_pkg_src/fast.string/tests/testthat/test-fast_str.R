test_that("ftrimws matches base for default whitespace, including non-stripped chars", {
    x <- c("  hi  ", "\tworld\r\n", "abc\f", NA, "")
    expect_identical(fast.string::ftrimws(x), base::trimws(x))
    expect_identical(fast.string::ftrimws(x, "left"), base::trimws(x, "left"))
    expect_identical(fast.string::ftrimws(x, "right"), base::trimws(x, "right"))
})

test_that("ftrimws does not strip form feed (only ' \\t\\r\\n')", {
    expect_identical(fast.string::ftrimws("abc\f"), "abc\f")
})

test_that("ftrimws delegates to base for a custom whitespace regex", {
    x <- "xxhixx"
    expect_identical(fast.string::ftrimws(x, whitespace = "x"), base::trimws(x, whitespace = "x"))
})

test_that("ftrimws preserves names", {
    expect_identical(names(fast.string::ftrimws(c(a = " hi "))), "a")
})

test_that("ftrimws errors on non-character, non-NA input", {
    expect_error(fast.string::ftrimws(1:3), "character vector")
})

test_that("fsubstr matches base including NA/out-of-range/recycling", {
    x <- c("hello", NA, "hi", "")
    expect_identical(fast.string::fsubstr(x, 1, 3), base::substr(x, 1, 3))
    expect_identical(fast.string::fsubstr(x, 2, 100), base::substr(x, 2, 100))
    expect_identical(fast.string::fsubstr(x, c(1, 2, 1, 1), c(3, 4, 2, 1)),
                      base::substr(x, c(1, 2, 1, 1), c(3, 4, 2, 1)))
})

test_that("fsubstr propagates NA from start/stop", {
    expect_true(is.na(fast.string::fsubstr("hello", NA, 3)))
    expect_true(is.na(fast.string::fsubstr("hello", 1, NA)))
})

test_that("fsubstr preserves names", {
    expect_identical(names(fast.string::fsubstr(c(a = "hello"), 1, 3)), "a")
})

test_that("fsubstr indexes UTF-8 characters without splitting bytes", {
    x <- enc2utf8(c("\u00e9clair", "\u4f60\u597d\u4e16\u754c", NA_character_))
    expect_identical(
        fast.string::fsubstr(x, c(1, 2, 1), c(2, 3, 1)),
        base::substr(x, c(1, 2, 1), c(2, 3, 1))
    )
})

test_that("fsubstr handles empty index vectors like base", {
    expect_error(
        fast.string::fsubstr(c("a", "b"), integer(), 1L),
        "invalid substring arguments"
    )
})

test_that("ftrimws preserves the encoding of unchanged strings", {
    x <- enc2utf8("\u00e9clair")
    expect_identical(fast.string::ftrimws(x), x)
    expect_identical(Encoding(fast.string::ftrimws(x)), Encoding(x))
})

test_that("trimmed and substring slices preserve Latin-1 and bytes encodings", {
    latin1 <- iconv(" \u00e9clair ", from = "UTF-8", to = "latin1")
    Encoding(latin1) <- "latin1"
    bytes <- rawToChar(as.raw(c(0x20, 0xe9, 0x61, 0x20)))
    Encoding(bytes) <- "bytes"

    trimmed <- fast.string::ftrimws(c(latin1, bytes))
    substrings <- fast.string::fsubstr(c(latin1, bytes), 2L, 2L)

    expect_identical(Encoding(trimmed), c("latin1", "bytes"))
    expect_identical(Encoding(substrings), c("latin1", "bytes"))
    expect_identical(charToRaw(substrings[[1L]]), as.raw(0xe9))
    expect_identical(charToRaw(substrings[[2L]]), as.raw(0xe9))
})

test_that("fnchar matches base for bytes/chars and NA handling", {
    x <- c("hello", NA, "", "café")
    expect_identical(fast.string::fnchar(x), base::nchar(x))
    expect_identical(fast.string::fnchar(x, "bytes"), base::nchar(x, "bytes"))
    expect_identical(fast.string::fnchar(x, "chars"), base::nchar(x, "chars"))
})

test_that("fnchar keepNA=FALSE matches base legacy behaviour", {
    x <- c("hello", NA)
    expect_identical(fast.string::fnchar(x, keepNA = FALSE), base::nchar(x, keepNA = FALSE))
})

test_that("fnchar type=width delegates to base", {
    x <- c("hello", NA)
    expect_identical(fast.string::fnchar(x, type = "width"), base::nchar(x, type = "width"))
})

test_that("fnchar coerces non-character input like base", {
    expect_identical(fast.string::fnchar(12345), base::nchar(12345))
})

test_that("fnchar is more lenient than base for factor input", {
    # base::nchar() errors on factors ("requires a character vector");
    # fast.string::fnchar() coerces via as.character() first, so it succeeds.
    expect_error(base::nchar(factor("hello")), "character vector")
    expect_identical(fast.string::fnchar(factor("hello")), base::nchar(as.character(factor("hello"))))
})

test_that("fnchar preserves names", {
    expect_identical(names(fast.string::fnchar(c(a = "hello"))), "a")
})

test_that("fchartr matches base for simple ASCII translation", {
    x <- c("aabbcc", "zzz", NA)
    expect_identical(fast.string::fchartr("abc", "xyz", x), base::chartr("abc", "xyz", x))
})

test_that("fchartr allows equal character count with unequal byte count", {
    old <- "ab"
    new <- paste0(intToUtf8(233), "b") # 'eb' with an accented e (2 bytes, 1 char) + 'b'
    expect_identical(fast.string::fchartr(old, new, "ab"), base::chartr(old, new, "ab"))
})

test_that("fchartr errors when old/new have different character counts", {
    expect_error(fast.string::fchartr("ab", "a", "ab"), "same number of characters")
})

test_that("fchartr preserves names", {
    expect_identical(names(fast.string::fchartr("a", "b", c(z = "a"))), "z")
})

test_that("fchartr errors on invalid old/new", {
    expect_error(fast.string::fchartr(c("a", "b"), "x", "abc"), "single string")
    expect_error(fast.string::fchartr("a", c("x", "y"), "abc"), "single string")
})
