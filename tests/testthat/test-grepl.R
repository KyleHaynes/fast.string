test_that("grepl matches base for simple regex", {
    # No NA here deliberately: base::grepl() always returns FALSE for NA
    # input, while fast.string::fgrepl() returns NA (see next test) -- a
    # documented intentional divergence, not something to assert "matches
    # base" on.
    x <- c("hello world", "foo bar", "test123", "HELLO WORLD")
    expect_identical(fast.string::fgrepl("o", x), base::grepl("o", x))
    expect_identical(fast.string::fgrepl("^foo", x), base::grepl("^foo", x))
    expect_identical(fast.string::fgrepl("[0-9]+", x), base::grepl("[0-9]+", x))
})

test_that("fgrepl intentionally returns NA for NA input, unlike base", {
    expect_true(is.na(fast.string::fgrepl("o", NA_character_)))
    expect_false(is.na(base::grepl("o", NA_character_))) # base returns FALSE
})

test_that("fgrepl ignore.case matches base", {
    x <- c("Hello", "WORLD", "test")
    expect_identical(fast.string::fgrepl("hello", x, ignore.case = TRUE),
                      base::grepl("hello", x, ignore.case = TRUE))
})

test_that("fgrepl fixed matches base", {
    x <- c("a.b.c", "axbxc", "a.b")
    expect_identical(fast.string::fgrepl(".", x, fixed = TRUE),
                      base::grepl(".", x, fixed = TRUE))
})

test_that("fgrepl delegates PCRE-only syntax to base and warns once", {
    x <- c("foobar", "foo", "bar")
    pattern <- "foo(?=bar)"
    expect_message(res <- fast.string::fgrepl(pattern, x), "PCRE-only syntax")
    expect_identical(res, base::grepl(pattern, x, perl = TRUE))
})

test_that("fgrepl errors on invalid pattern/x", {
    expect_error(fast.string::fgrepl(c("a", "b"), "x"), "single character string")
    expect_error(fast.string::fgrepl("a", 1:3), "character vector")
})

test_that("fgrepl tolerates all-NA non-character x", {
    expect_true(all(is.na(fast.string::fgrepl("a", c(NA, NA)))))
})

test_that("fgrep returns indices matching base", {
    x <- c("hello world", "foo bar", NA, "test123", "HELLO WORLD")
    expect_identical(fast.string::fgrep("o", x), base::grep("o", x))
})

test_that("fgrep value=TRUE matches base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fast.string::fgrep("o", x, value = TRUE), base::grep("o", x, value = TRUE))
})

test_that("fgrep invert matches base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fast.string::fgrep("o", x, invert = TRUE), base::grep("o", x, invert = TRUE))
})

test_that("fsub replaces first match like base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fast.string::fsub("o", "0", x), base::sub("o", "0", x))
})

test_that("fsub supports capture groups", {
    x <- c("hello world", "foo bar")
    expect_identical(fast.string::fsub("(\\w+)", "[\\1]", x), base::sub("(\\w+)", "[\\1]", x))
})

test_that("fsub fixed matches base", {
    x <- c("a.b.c", "a.b")
    expect_identical(fast.string::fsub(".", "-", x, fixed = TRUE), base::sub(".", "-", x, fixed = TRUE))
})

test_that("fsub NA propagates", {
    expect_true(is.na(fast.string::fsub("x", "y", NA_character_)))
})

test_that("fsub delegates PCRE-only syntax to base", {
    x <- c("foobar", "foo")
    pattern <- "foo(?=bar)"
    expect_message(res <- fast.string::fsub(pattern, "X", x), "PCRE-only syntax")
    expect_identical(res, base::sub(pattern, "X", x, perl = TRUE))
})

test_that("fgsub replaces all matches like base", {
    x <- c("hello world", "foo bar", NA, "test123", "HELLO WORLD")
    expect_identical(fast.string::fgsub("o", "0", x), base::gsub("o", "0", x))
})

test_that("fgsub supports capture groups and case conversion", {
    x <- c("hello world")
    expect_identical(fast.string::fgsub("(\\w+)", "\\U\\1", x, perl = TRUE),
                      base::gsub("(\\w+)", "\\U\\1", x, perl = TRUE))
})

test_that("fgsub ignore.case regex matches base", {
    x <- c("hello world", "foo bar", "HELLO WORLD")
    expect_identical(fast.string::fgsub("hello", "HI", x, ignore.case = TRUE),
                      base::gsub("hello", "HI", x, ignore.case = TRUE))
})

test_that("fgsub fixed matches base", {
    x <- c("hello world", "foo bar")
    expect_identical(fast.string::fgsub("o", "0", x, fixed = TRUE), base::gsub("o", "0", x, fixed = TRUE))
})

test_that("fgsub NA propagates", {
    expect_true(is.na(fast.string::fgsub("x", "y", NA_character_)))
})
