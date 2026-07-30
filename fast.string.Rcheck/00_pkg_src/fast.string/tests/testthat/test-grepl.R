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

test_that("PCRE2 substitution grows its buffer for expanding backreferences", {
    x <- rep(strrep("ab", 128L), 1500L)
    replacement <- "\\1\\1\\1\\1"

    for (nthreads in c(1L, 2L, 4L)) {
        expect_identical(
            fast.string::fgsub(
                "(ab)", replacement, x, perl = TRUE, nthreads = nthreads
            ),
            base::gsub("(ab)", replacement, x, perl = TRUE)
        )
    }
})

test_that("PCRE2 substitution handles zero-length matches like base", {
    x <- c("ab", "xxx", "", NA_character_)

    expect_identical(
        fast.string::fsub("x*", "_", x, perl = TRUE, nthreads = 2L),
        base::sub("x*", "_", x, perl = TRUE)
    )
    expect_identical(
        fast.string::fgsub("x*", "_", x, perl = TRUE, nthreads = 2L),
        base::gsub("x*", "_", x, perl = TRUE)
    )
})

test_that("substitution preserves the encoding of unchanged strings", {
    latin1 <- iconv("\u00e9clair", from = "UTF-8", to = "latin1")
    Encoding(latin1) <- "latin1"
    bytes <- rawToChar(as.raw(c(0xe9, 0x63, 0x6c, 0x61, 0x69, 0x72)))
    Encoding(bytes) <- "bytes"
    x <- c(latin1, bytes)

    fixed_unchanged <- fast.string::fgsub(
        "absent", "x", x, fixed = TRUE, nthreads = 2L
    )
    regex_unchanged <- fast.string::fgsub(
        "absent", "x", x, nthreads = 2L
    )
    expect_identical(Encoding(fixed_unchanged), Encoding(x))
    expect_identical(Encoding(regex_unchanged), Encoding(x))
})

test_that("substitution preserves the encoding of pure source slices", {
    latin1 <- iconv("\u00e9clair", from = "UTF-8", to = "latin1")
    Encoding(latin1) <- "latin1"
    latin1_prefixed <- iconv("x\u00e9clair", from = "UTF-8", to = "latin1")
    Encoding(latin1_prefixed) <- "latin1"
    fixed_slice <- fast.string::fsub(
        "x", "", latin1_prefixed, fixed = TRUE, nthreads = 2L
    )
    regex_slice <- fast.string::fsub(
        "^x", "", latin1_prefixed, nthreads = 2L
    )
    expect_identical(Encoding(fixed_slice), "latin1")
    expect_identical(Encoding(regex_slice), "latin1")
    expect_identical(charToRaw(fixed_slice), charToRaw(latin1))
    expect_identical(charToRaw(regex_slice), charToRaw(latin1))
})

test_that("substitution tags newly constructed output as UTF-8", {
    fixed_changed <- fast.string::fgsub(
        "a", "\u96ea", "a", fixed = TRUE, nthreads = 2L
    )
    regex_changed <- fast.string::fgsub(
        "a", "\u96ea", "a", nthreads = 2L
    )
    expect_identical(Encoding(fixed_changed), "UTF-8")
    expect_identical(Encoding(regex_changed), "UTF-8")
})

test_that("matching and substitution preserve input names", {
    x <- c(first = "alpha", second = "beta")
    expect_identical(names(fast.string::fgrepl("a", x, fixed = TRUE)),
                     names(x))
    expect_identical(names(fast.string::fgrepl("a", x)), names(x))
    expect_identical(names(fast.string::fsub("a", "x", x, fixed = TRUE)),
                     names(x))
    expect_identical(names(fast.string::fgsub("a", "x", x)), names(x))
})

test_that("nthreads is a positive integer cap", {
    invalid <- list(0, -1, 1.5, Inf, NA_real_, TRUE, "2", c(1, 2))
    for (value in invalid) {
        expect_error(
            fast.string::fgrepl("a", "a", nthreads = value),
            "positive integer"
        )
    }
    expect_identical(
        fast.string::fgrepl("a", "a", nthreads = 1L),
        TRUE
    )
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
