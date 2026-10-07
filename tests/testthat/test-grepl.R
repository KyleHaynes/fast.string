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

test_that("fgrepl runs PCRE-only syntax on PCRE2 like base perl = TRUE", {
    x <- c("foobar", "foo", "bar")
    pattern <- "foo(?=bar)"
    expect_silent(res <- fast.string::fgrepl(pattern, x))
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

test_that("fsub runs PCRE-only syntax on PCRE2 like base perl = TRUE", {
    x <- c("foobar", "foo")
    pattern <- "foo(?=bar)"
    expect_silent(res <- fast.string::fsub(pattern, "X", x))
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

test_that("literal replacements match base, including escapes and empty matches", {
    # Replacements without \1-\9 or \U/\L/\E are spliced in directly rather
    # than expanded by PCRE2; each must still read exactly as R reads it.
    x <- c("a1b22c333", "no digits", "", NA, "9", "x$y\\z",
           paste0("caf", intToUtf8(0xe9), " 42"))
    big <- c(x, rep(x, 60000L))  # also cross the parallel threshold
    replacements <- c("#", "", "$", "$1", "\\\\", "\\.", "<\\n>", "ab")
    for (r in replacements) {
        expect_identical(fast.string::fgsub("[0-9]+", r, x),
                         base::gsub("[0-9]+", r, x, perl = TRUE), info = r)
        expect_identical(fast.string::fsub("[0-9]+", r, x),
                         base::sub("[0-9]+", r, x, perl = TRUE), info = r)
        expect_identical(fast.string::fgsub("[0-9]*", r, x),
                         base::gsub("[0-9]*", r, x, perl = TRUE), info = r)
        # identical() rather than expect_identical(): a failure then reports
        # at once instead of diffing 420,000 strings.
        expect_true(identical(fast.string::fgsub("[0-9]+", r, big),
                              base::gsub("[0-9]+", r, big, perl = TRUE)),
                    info = r)
        expect_true(identical(fast.string::fgsub("[0-9]*", r, big),
                              base::gsub("[0-9]*", r, big, perl = TRUE)),
                    info = r)
    }
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
    x <- c("ab", "xx", "xxx", "xa", "xax", "", NA_character_)

    expect_identical(
        fast.string::fsub("x*", "_", x, perl = TRUE, nthreads = 2L),
        base::sub("x*", "_", x, perl = TRUE)
    )
    expect_identical(
        fast.string::fgsub("x*", "_", x, perl = TRUE, nthreads = 2L),
        base::gsub("x*", "_", x, perl = TRUE)
    )

    for (pattern in c("a*", ".*?", "(|x)", "x?", "(x|)")) {
        expect_identical(
            fast.string::fgsub(
                pattern, "_", x, perl = TRUE, nthreads = 2L
            ),
            base::gsub(pattern, "_", x, perl = TRUE)
        )
    }
})

test_that("zero-length regex substitution preserves backreferences", {
    x <- c("", "x", "xa", "xax", "ab", NA_character_)

    expect_identical(
        fast.string::fgsub(
            "((a)*)", "\\1-\\2", x, perl = TRUE, nthreads = 2L
        ),
        base::gsub("((a)*)", "\\1-\\2", x, perl = TRUE)
    )
})

test_that("disjoint regex deletions are not tagged as source slices", {
    # Under useBytes a pure slice keeps the source encoding while newly built
    # text is marked native, so latin1 input shows which path was taken.
    x <- iconv("\u00e9aaba", from = "UTF-8", to = "latin1")
    Encoding(x) <- "latin1"

    result <- fast.string::fsub(
        "ab", "", x, perl = TRUE, useBytes = TRUE, nthreads = 2L
    )
    expect_identical(charToRaw(result), as.raw(c(0xe9, 0x61, 0x61)))
    expect_identical(Encoding(result), "unknown")

    for (substitute in list(fast.string::fsub, fast.string::fgsub)) {
        fixed_result <- substitute(
            "ab", "", x, fixed = TRUE, useBytes = TRUE, nthreads = 2L
        )
        expect_identical(
            charToRaw(fixed_result),
            as.raw(c(0xe9, 0x61, 0x61))
        )
        expect_identical(Encoding(fixed_result), "unknown")
    }
})

test_that("latin1 input is matched and returned as UTF-8 like base", {
    x <- iconv(c("\u00e9aaba", "caf\u00e9", "plain"), "UTF-8", "latin1")
    expect_identical(fast.string::fgrepl("\u00e9", x), c(TRUE, TRUE, FALSE))
    expect_identical(fast.string::fgrepl("\u00e9", x, fixed = TRUE),
                     c(TRUE, TRUE, FALSE))
    for (fixed in c(FALSE, TRUE)) {
        result <- fast.string::fgsub("a", "", x, fixed = fixed)
        expect_identical(result, unname(base::gsub("a", "", x, fixed = fixed)))
        expect_true(all(validUTF8(result)))
        expect_identical(Encoding(result), c("UTF-8", "UTF-8", "unknown"))
    }
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
        "x", "", latin1_prefixed, fixed = TRUE, useBytes = TRUE,
        nthreads = 2L
    )
    regex_slice <- fast.string::fsub(
        "^x", "", latin1_prefixed, useBytes = TRUE, nthreads = 2L
    )
    expect_identical(Encoding(fixed_slice), "latin1")
    expect_identical(Encoding(regex_slice), "latin1")
    expect_identical(charToRaw(fixed_slice), charToRaw(latin1))
    expect_identical(charToRaw(regex_slice), charToRaw(latin1))

    # Without useBytes the latin1 input is translated, so the slice is UTF-8,
    # as base R returns it.
    utf8_slice <- fast.string::fsub("^x", "", latin1_prefixed)
    expect_identical(utf8_slice, base::sub("^x", "", latin1_prefixed))
    expect_identical(Encoding(utf8_slice), "UTF-8")
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

test_that("matching and substitution retain legacy name behavior", {
    x <- c(first = "alpha", second = "beta")
    expect_null(names(fast.string::fgrepl("a", x, fixed = TRUE)))
    expect_null(names(fast.string::fgrepl("a", x)))
    expect_null(names(fast.string::fsub("a", "x", x, fixed = TRUE)))
    expect_null(names(fast.string::fgsub("a", "x", x)))
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

test_that("fgrep preserves value names but returns unnamed indices like base", {
    x <- c(first = "alpha", second = "beta", missing = NA_character_)

    expect_identical(
        fast.string::fgrep("a", x, fixed = TRUE),
        base::grep("a", x, fixed = TRUE)
    )
    expect_identical(
        fast.string::fgrep("z", x, fixed = TRUE, invert = TRUE),
        base::grep("z", x, fixed = TRUE, invert = TRUE)
    )
    expect_identical(
        fast.string::fgrep("a", x, fixed = TRUE, value = TRUE),
        base::grep("a", x, fixed = TRUE, value = TRUE)
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

test_that("an NA pattern or replacement behaves as in base", {
    x <- c("NA", "abc", NA, "banana")
    expect_identical(fast.string::fgrepl(NA_character_, x), rep(NA, 4L))
    expect_identical(fast.string::fgrepl(NA, x, fixed = TRUE), rep(NA, 4L))
    expect_identical(fast.string::fgrep(NA, x), base::grep(NA, x))
    for (substitute in c("sub", "gsub")) {
        fast_fn <- getExportedValue("fast.string", paste0("f", substitute))
        base_fn <- getExportedValue("base", substitute)
        expect_identical(fast_fn(NA, "x", x), rep(NA_character_, 4L))
        for (fixed in c(FALSE, TRUE)) {
            expect_identical(fast_fn("a", NA, x, fixed = fixed),
                             base_fn("a", NA, x, fixed = fixed))
        }
    }
})

test_that("non-ASCII text is matched by character, as in base", {
    e <- intToUtf8(0xe9)
    E <- intToUtf8(0xc9)
    x <- c(paste0("caf", e, " 42"), paste0(E, "mile"), "plain", NA)
    for (perl in c(FALSE, TRUE)) {
        for (pattern in c("[0-9]*", "^.{4}$", ".", "x*", e, E)) {
            expect_identical(
                fast.string::fgsub(pattern, "#", x, perl = perl),
                base::gsub(pattern, "#", x, perl = perl),
                info = paste(pattern, perl)
            )
        }
        expect_identical(
            fast.string::fgrepl(E, x, ignore.case = TRUE, perl = perl),
            c(TRUE, TRUE, FALSE, NA)
        )
    }
    # Unicode-aware classes with perl = FALSE, ASCII-only ones with perl = TRUE.
    expect_identical(fast.string::fgsub("[[:alpha:]]+", "W", x),
                     base::gsub("[[:alpha:]]+", "W", x))
    expect_identical(fast.string::fgsub("\\w", "W", x, perl = TRUE),
                     base::gsub("\\w", "W", x, perl = TRUE))
    expect_true(all(validUTF8(fast.string::fgsub("[0-9]*", "#", x))))
})

test_that("useBytes = TRUE matches byte by byte like base", {
    x <- paste0("caf", intToUtf8(0xe9))
    expect_identical(fast.string::fgsub("x*", "-", x, useBytes = TRUE),
                     base::gsub("x*", "-", x, useBytes = TRUE))
    expect_identical(fast.string::fgrepl("^.{5}$", x, useBytes = TRUE),
                     base::grepl("^.{5}$", x, useBytes = TRUE))
})

test_that("perl = FALSE follows base R's default regular expressions", {
    x <- c("a\nb", "a\n", "the cat", "other", "d", "1", "\\", "a]b")
    for (pattern in c("a.b", "a$", "\\<the\\>", "\\<oth", "[\\d]", "[\\w]",
                      "[a\\]b]", "[][]", "[^]a]")) {
        expect_identical(fast.string::fgrepl(pattern, x),
                         base::grepl(pattern, x), info = pattern)
    }
    expect_identical(fast.string::fgsub("a.b", "X", x), base::gsub("a.b", "X", x))
    # perl = TRUE keeps PCRE's own rules.
    expect_identical(fast.string::fgrepl("a.b", x, perl = TRUE),
                     base::grepl("a.b", x, perl = TRUE))
    expect_identical(fast.string::fgrepl("a$", x, perl = TRUE),
                     base::grepl("a$", x, perl = TRUE))
    # Wide-character POSIX classes.
    euro <- intToUtf8(0x20ac)
    expect_identical(fast.string::fgrepl("[[:punct:]]", c(euro, "a")),
                     base::grepl("[[:punct:]]", c(euro, "a")))
    expect_identical(
        fast.string::fgrepl("[[:upper:]]", c("abc", intToUtf8(0xe9)),
                            ignore.case = TRUE),
        base::grepl("[[:upper:]]", c("abc", intToUtf8(0xe9)), ignore.case = TRUE)
    )
})

test_that("a match-limit error on one element warns instead of failing", {
    x <- c(paste0(strrep("a", 30), "!"), "aa", NA)
    pattern <- "^(a+)+$"
    expect_warning(res <- fast.string::fgrepl(pattern, x, perl = TRUE),
                   "match limit")
    expect_identical(res, c(FALSE, TRUE, NA))
    expect_warning(res <- fast.string::fgsub(pattern, "X", x, perl = TRUE),
                   "match limit")
    expect_identical(res, suppressWarnings(
        base::gsub(pattern, "X", x, perl = TRUE)
    ))
    expect_warning(res <- fast.string::fcount(pattern, x, perl = TRUE),
                   "match limit")
    expect_identical(res, c(0L, 1L, NA))
})
