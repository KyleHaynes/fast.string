test_that("gsub_all fixed sequential matches chained base gsub", {
    x <- c("THE CAT AND THE DOG", "FOR YOU ARE HERE", NA)
    patterns <- c("THE", "AND", "FOR", "YOU", "ARE")
    repls    <- c("the", "and", "for", "you", "are")

    expected <- x
    for (i in seq_along(patterns))
        expected <- base::gsub(patterns[i], repls[i], expected, fixed = TRUE)

    result <- fast.string::gsub_all(patterns, repls, x, fixed = TRUE, sequential = TRUE)
    expect_identical(result, expected)
})

test_that("gsub_all fixed single-scan differs from sequential when patterns overlap", {
    x <- "AAA"
    # sequential: "AAA" --A->B--> "BBB"; single-scan: each A matched once at its own position
    result_seq <- fast.string::gsub_all("A", "B", x, fixed = TRUE, sequential = TRUE)
    result_par <- fast.string::gsub_all("A", "B", x, fixed = TRUE, sequential = FALSE)
    expect_identical(result_seq, "BBB")
    expect_identical(result_par, "BBB")
})

test_that("gsub_all regex sequential matches chained base gsub with perl", {
    x <- c("the cat and the dog", "for you are here")
    patterns <- c("\\bthe\\b", "\\band\\b", "\\bfor\\b")
    repls    <- c("THE", "AND", "FOR")

    expected <- x
    for (i in seq_along(patterns))
        expected <- base::gsub(patterns[i], repls[i], expected, perl = TRUE)

    result <- fast.string::gsub_all(patterns, repls, x, sequential = TRUE)
    expect_identical(result, expected)
})

test_that("gsub_all regex mixes literal and backreference replacements like base", {
    x <- c("a1b22c333", "x$y", "", NA, "aaa 7")
    x <- c(x, rep(x, 100000L))
    patterns <- c("[0-9]+", "(a)", "y*", "\\$")
    repls <- c("#", "<\\1>", "-", "\\\\")
    expected <- x
    for (i in seq_along(patterns))
        expected <- base::gsub(patterns[i], repls[i], expected, perl = TRUE)

    expect_identical(fast.string::gsub_all(patterns, repls, x), expected)
})

test_that("gsub_all grows PCRE2 buffers for expanding backreferences", {
    x <- rep(strrep("ab", 128L), 1800L)
    patterns <- c("(ab)", "z")
    replacements <- c("\\1\\1\\1\\1", "q")
    expected <- base::gsub(
        patterns[[1L]], replacements[[1L]], x, perl = TRUE
    )
    expected <- base::gsub(
        patterns[[2L]], replacements[[2L]], expected, perl = TRUE
    )

    expect_identical(
        fast.string::gsub_all(
            patterns, replacements, x, sequential = TRUE, nthreads = 4L
        ),
        expected
    )
})

test_that("gsub_all handles zero-length regex matches like base", {
    x <- c("ab", "xxx", "xa", "xax", "", NA_character_)

    expect_identical(
        fast.string::gsub_all("x*", "_", x, nthreads = 2L),
        base::gsub("x*", "_", x, perl = TRUE)
    )

    patterns <- c("x*", "a")
    replacements <- c("_", "A")
    expected <- x
    for (i in seq_along(patterns)) {
        expected <- base::gsub(
            patterns[[i]], replacements[[i]], expected, perl = TRUE
        )
    }
    expect_identical(
        fast.string::gsub_all(
            patterns, replacements, x,
            sequential = TRUE, nthreads = 2L
        ),
        expected
    )
})

test_that("non-sequential regex substitution is rejected explicitly", {
    expect_error(
        fast.string::gsub_all(
            c("a", "b"), c("x", "y"), "ab",
            fixed = FALSE, sequential = FALSE
        ),
        "not supported for regular-expression patterns"
    )
})

test_that("gsub_all preserves pure source-slice encodings", {
    latin1 <- iconv("\u00e9clair", from = "UTF-8", to = "latin1")
    Encoding(latin1) <- "latin1"
    prefixed <- iconv("x\u00e9clair", from = "UTF-8", to = "latin1")
    Encoding(prefixed) <- "latin1"

    fixed_slice <- fast.string::gsub_all(
        "x", "", prefixed, fixed = TRUE, nthreads = 2L, useBytes = TRUE
    )
    regex_slice <- fast.string::gsub_all(
        "^x", "", prefixed, nthreads = 2L, useBytes = TRUE
    )
    expect_identical(Encoding(fixed_slice), "latin1")
    expect_identical(Encoding(regex_slice), "latin1")
    expect_identical(charToRaw(fixed_slice), charToRaw(latin1))
    expect_identical(charToRaw(regex_slice), charToRaw(latin1))
})

test_that("gsub_all does not tag disjoint deletions as source slices", {
    x <- iconv("\u00e9aaba", from = "UTF-8", to = "latin1")
    Encoding(x) <- "latin1"

    result <- fast.string::gsub_all(
        "ab", "", x, sequential = TRUE, nthreads = 2L, useBytes = TRUE
    )
    expect_identical(charToRaw(result), as.raw(c(0xe9, 0x61, 0x61)))
    expect_identical(Encoding(result), "unknown")

    for (sequential in c(TRUE, FALSE)) {
        fixed_result <- fast.string::gsub_all(
            "ab", "", x, fixed = TRUE,
            sequential = sequential, nthreads = 2L, useBytes = TRUE
        )
        expect_identical(
            charToRaw(fixed_result),
            as.raw(c(0xe9, 0x61, 0x61))
        )
        expect_identical(Encoding(fixed_result), "unknown")
    }
})

test_that("later deletions can restore a pure source slice", {
    x <- iconv("\u00e9ab", from = "UTF-8", to = "latin1")
    Encoding(x) <- "latin1"

    for (fixed in c(FALSE, TRUE)) {
        result <- fast.string::gsub_all(
            c("a", "b"), "", x, fixed = fixed,
            sequential = TRUE, nthreads = 2L, useBytes = TRUE
        )
        expect_identical(charToRaw(result), as.raw(0xe9))
        expect_identical(Encoding(result), "latin1")
    }
})

test_that("gsub_all retains legacy unnamed output", {
    x <- c(first = "alpha", second = "beta")
    expect_null(names(
        fast.string::gsub_all("a", "x", x, fixed = TRUE)
    ))
    expect_null(names(fast.string::gsub_all("a", "x", x)))
})

test_that("gsub_all recycles a single replacement to all patterns", {
    x <- "a-b-c"
    result <- fast.string::gsub_all(c("a", "b", "c"), "X", x, fixed = TRUE)
    expect_identical(result, "X-X-X")
})

test_that("gsub_all NA propagates", {
    expect_true(is.na(fast.string::gsub_all(c("a", "b"), c("x", "y"), NA_character_, fixed = TRUE)))
})

test_that("gsub_all validates arguments", {
    expect_error(fast.string::gsub_all(character(0), "x", "abc"), "non-empty")
    expect_error(fast.string::gsub_all(c("a", "b"), c("x", "y", "z"), "abc"), "length 1 or the same length")
})

test_that("gsub_all runs PCRE-only patterns on PCRE2", {
    x <- c("foobar", "foo")
    expect_silent(
        res <- fast.string::gsub_all("foo(?=bar)", "X", x)
    )
    expected <- base::gsub("foo(?=bar)", "X", x, perl = TRUE)
    expect_identical(res, expected)
})

test_that("gsub_all treats NA patterns and replacements like chained gsub", {
    x <- c("cat", "dog", NA)
    for (fixed in c(FALSE, TRUE)) {
        expect_identical(
            fast.string::gsub_all(c("a", NA), "x", x, fixed = fixed),
            rep(NA_character_, 3L)
        )
        expect_identical(
            fast.string::gsub_all(c("c", "o"), c("C", NA), x, fixed = fixed),
            unname(base::gsub("o", NA, base::gsub("c", "C", x, fixed = fixed),
                              fixed = fixed))
        )
    }
    expect_identical(
        fast.string::gsub_all(c("c", "o"), c("C", NA), x, fixed = TRUE,
                              sequential = FALSE),
        c("Cat", NA, NA)
    )
})

test_that("gsub_all single scan picks the leftmost match, first pattern on ties", {
    set.seed(42)
    patterns <- unique(replicate(40, paste(sample(letters[1:4], 2, TRUE), collapse = "")))
    x <- replicate(300, paste(sample(letters[1:4], 30, TRUE), collapse = ""))
    reference <- vapply(x, function(s) {
        out <- ""
        pos <- 1L
        n <- nchar(s)
        while (pos <= n) {
            hits <- vapply(patterns, function(p) {
                at <- regexpr(p, substr(s, pos, n), fixed = TRUE)
                if (at < 0) NA_integer_ else at + pos - 1L
            }, 1L)
            if (all(is.na(hits))) break
            best <- which.min(hits)
            out <- paste0(out, substr(s, pos, hits[best] - 1L), toupper(patterns[best]))
            pos <- hits[best] + nchar(patterns[best])
        }
        paste0(out, if (pos <= n) substr(s, pos, n) else "")
    }, "", USE.NAMES = FALSE)
    expect_identical(
        fast.string::gsub_all(patterns, toupper(patterns), x, fixed = TRUE,
                              sequential = FALSE),
        reference
    )
})
