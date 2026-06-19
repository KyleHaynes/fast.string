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

test_that("gsub_all delegates PCRE-only patterns to base", {
    x <- c("foobar", "foo")
    expect_message(
        res <- fast.string::gsub_all("foo(?=bar)", "X", x),
        "PCRE-only syntax"
    )
    expected <- base::gsub("foo(?=bar)", "X", x, perl = TRUE)
    expect_identical(res, expected)
})
