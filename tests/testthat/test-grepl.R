test_that("grepl matches base for simple regex", {
    # No NA here deliberately: base::grepl() always returns FALSE for NA
    # input, while fgrepl::grepl() returns NA (see next test) -- a documented
    # intentional divergence, not something to assert "matches base" on.
    x <- c("hello world", "foo bar", "test123", "HELLO WORLD")
    expect_identical(fgrepl::grepl("o", x), base::grepl("o", x))
    expect_identical(fgrepl::grepl("^foo", x), base::grepl("^foo", x))
    expect_identical(fgrepl::grepl("[0-9]+", x), base::grepl("[0-9]+", x))
})

test_that("grepl intentionally returns NA for NA input, unlike base", {
    expect_true(is.na(fgrepl::grepl("o", NA_character_)))
    expect_false(is.na(base::grepl("o", NA_character_))) # base returns FALSE
})

test_that("grepl ignore.case matches base", {
    x <- c("Hello", "WORLD", "test")
    expect_identical(fgrepl::grepl("hello", x, ignore.case = TRUE),
                      base::grepl("hello", x, ignore.case = TRUE))
})

test_that("grepl fixed matches base", {
    x <- c("a.b.c", "axbxc", "a.b")
    expect_identical(fgrepl::grepl(".", x, fixed = TRUE),
                      base::grepl(".", x, fixed = TRUE))
})

test_that("grepl delegates PCRE-only syntax to base and warns once", {
    x <- c("foobar", "foo", "bar")
    pattern <- "foo(?=bar)"
    expect_message(res <- fgrepl::grepl(pattern, x), "PCRE-only syntax")
    expect_identical(res, base::grepl(pattern, x, perl = TRUE))
})

test_that("grepl errors on invalid pattern/x", {
    expect_error(fgrepl::grepl(c("a", "b"), "x"), "single character string")
    expect_error(fgrepl::grepl("a", 1:3), "character vector")
})

test_that("grepl tolerates all-NA non-character x", {
    expect_true(all(is.na(fgrepl::grepl("a", c(NA, NA)))))
})

test_that("grep returns indices matching base", {
    x <- c("hello world", "foo bar", NA, "test123", "HELLO WORLD")
    expect_identical(fgrepl::grep("o", x), base::grep("o", x))
})

test_that("grep value=TRUE matches base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fgrepl::grep("o", x, value = TRUE), base::grep("o", x, value = TRUE))
})

test_that("grep invert matches base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fgrepl::grep("o", x, invert = TRUE), base::grep("o", x, invert = TRUE))
})

test_that("sub replaces first match like base", {
    x <- c("hello world", "foo bar", NA, "test123")
    expect_identical(fgrepl::sub("o", "0", x), base::sub("o", "0", x))
})

test_that("sub supports capture groups", {
    x <- c("hello world", "foo bar")
    expect_identical(fgrepl::sub("(\\w+)", "[\\1]", x), base::sub("(\\w+)", "[\\1]", x))
})

test_that("sub fixed matches base", {
    x <- c("a.b.c", "a.b")
    expect_identical(fgrepl::sub(".", "-", x, fixed = TRUE), base::sub(".", "-", x, fixed = TRUE))
})

test_that("sub NA propagates", {
    expect_true(is.na(fgrepl::sub("x", "y", NA_character_)))
})

test_that("sub delegates PCRE-only syntax to base", {
    x <- c("foobar", "foo")
    pattern <- "foo(?=bar)"
    expect_message(res <- fgrepl::sub(pattern, "X", x), "PCRE-only syntax")
    expect_identical(res, base::sub(pattern, "X", x, perl = TRUE))
})

test_that("gsub replaces all matches like base", {
    x <- c("hello world", "foo bar", NA, "test123", "HELLO WORLD")
    expect_identical(fgrepl::gsub("o", "0", x), base::gsub("o", "0", x))
})

test_that("gsub supports capture groups and case conversion", {
    x <- c("hello world")
    expect_identical(fgrepl::gsub("(\\w+)", "\\U\\1", x, perl = TRUE),
                      base::gsub("(\\w+)", "\\U\\1", x, perl = TRUE))
})

test_that("gsub ignore.case regex matches base", {
    x <- c("hello world", "foo bar", "HELLO WORLD")
    expect_identical(fgrepl::gsub("hello", "HI", x, ignore.case = TRUE),
                      base::gsub("hello", "HI", x, ignore.case = TRUE))
})

test_that("gsub fixed matches base", {
    x <- c("hello world", "foo bar")
    expect_identical(fgrepl::gsub("o", "0", x, fixed = TRUE), base::gsub("o", "0", x, fixed = TRUE))
})

test_that("gsub NA propagates", {
    expect_true(is.na(fgrepl::gsub("x", "y", NA_character_)))
})

test_that("verbose=FALSE suppresses the masking message", {
    expect_no_message(fgrepl::grepl("a", "abc", verbose = FALSE))
})
