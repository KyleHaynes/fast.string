# `source[index]` as a character vector that is never built, which is what
# rLink() hands this package's own comparators. To R it has to be a
# character vector like any other; to the comparators it has to give the
# scores the built vector would.

indexed <- function(x, i) fast_indexed_strings_impl(x, as.integer(i), -1L)

test_that("an indexed vector is the character vector it stands for", {
    x <- c("SMITH", "JONES", NA, "", "BROWN")
    i <- c(1L, 1L, 3L, 5L, 2L, NA, 4L, 5L)
    v <- indexed(x, i)
    expect_type(v, "character")
    expect_length(v, length(i))
    expect_identical(v[seq_along(v)], x[i])
    expect_identical(v[c(8, 2)], x[i][c(8, 2)])
    expect_identical(is.na(v), is.na(x[i]))
    expect_identical(nchar(v), nchar(x[i]))
    expect_identical(v == "SMITH", x[i] == "SMITH")
    expect_identical(paste0(v, "!"), paste0(x[i], "!"))
    expect_identical(toupper(v), toupper(x[i]))
    expect_identical(sort(v), sort(x[i]))
    expect_identical(unique(v), unique(x[i]))
    expect_identical(match("BROWN", v), match("BROWN", x[i]))
    # Saved, it is an ordinary vector that needs nothing from this package.
    expect_identical(unserialize(serialize(v, NULL)), x[i])
    # An index that points nowhere reads as missing.
    expect_identical(indexed(x, c(0L, 6L, -1L, 2L))[1:4],
                     c(NA, NA, NA, "JONES"))
    expect_length(indexed(x, integer()), 0L)
    expect_error(fast_indexed_strings_impl(1:3, 1L, -1L), "character")
    expect_error(fast_indexed_strings_impl(x, 1, -1L), "integer")
})

test_that("changing an indexed vector leaves its source alone", {
    x <- c("A", "B", "C")
    v <- indexed(x, c(3L, 1L, 1L))
    w <- v
    w[2L] <- "Z"
    expect_identical(w, c("C", "Z", "A"))
    expect_identical(v[1:3], c("C", "A", "A"))
    expect_identical(x, c("A", "B", "C"))
    # Once it has been written to it is read as what it now holds.
    expect_identical(jaro_winkler(w, c("C", "Z", "A")), c(1, 1, 1))
})

test_that("comparators score an indexed vector as they score the strings", {
    set.seed(7)
    pool <- c("SMITH", "SMYTH", "JONES", "JONSE", "O'BRIEN", "VAN DER BERG",
              "", NA, "A", "ZOË", "MÜLLER")
    ascii <- pool[1:9]
    for (x in list(ascii, pool)) {
        y <- rev(x)
        # Small enough to run on the calling thread, and large enough to be
        # split across workers.
        for (n in c(40L, 250000L)) {
            i <- sample(c(seq_along(x), NA), n, TRUE)
            j <- sample(c(seq_along(y), NA), n, TRUE)
            a <- x[i]
            b <- y[j]
            va <- indexed(x, i)
            vb <- indexed(y, j)
            expect_identical(jaro_winkler(va, vb), jaro_winkler(a, b))
            expect_identical(jaro_winkler(va, vb, use_bytes = FALSE),
                             jaro_winkler(a, b, use_bytes = FALSE))
            expect_identical(levenshtein(va, vb), levenshtein(a, b))
            expect_identical(levenshtein(va, vb, use_bytes = FALSE),
                             levenshtein(a, b, use_bytes = FALSE))
            expect_identical(jaccard_index(va, vb, q = 2),
                             jaccard_index(a, b, q = 2))
            expect_identical(fuzz_ratio(va, vb), fuzz_ratio(a, b))
            expect_identical(fuzz_token_set_ratio(va, vb),
                             fuzz_token_set_ratio(a, b))
            expect_identical(jaro_winkler_tokens(va, vb),
                             jaro_winkler_tokens(a, b))
            # One side indexed and the other not.
            expect_identical(jaro_winkler(va, b), jaro_winkler(a, b))
            expect_identical(levenshtein(a, vb), levenshtein(a, b))
            # Text functions read it too, and give back ordinary strings.
            expect_identical(fupper(va), fupper(a))
            expect_identical(soundex(va), soundex(a))
            expect_identical(fgrepl("S", va, fixed = TRUE),
                             fgrepl("S", a, fixed = TRUE))
        }
    }
})

test_that("a string that cannot be read as Unicode is an error only if used", {
    bad <- "caf\xe9"
    Encoding(bad) <- "bytes"
    x <- c("SMITH", bad, "JONES")
    y <- c("SMITH", "JONES", "SMYTH")
    # No pair reaches the string, so nothing is wrong...
    expect_identical(levenshtein(indexed(x, c(1L, 3L)), indexed(y, c(1L, 2L)),
                                 use_bytes = FALSE), c(0, 0))
    # ...and when one does, the message gives its place among the pairs.
    expect_error(levenshtein(indexed(x, c(1L, 3L, 2L)),
                             indexed(y, c(1L, 2L, 3L)), use_bytes = FALSE),
                 "a\\[3\\]")
})

test_that("the thread cap an indexed vector carries is honoured", {
    x <- c("SMITH", "SMYTH", "JONES")
    i <- rep_len(1:3, 300000L)
    one <- fast_indexed_strings_impl(x, i, 1L)
    many <- fast_indexed_strings_impl(x, i, -1L)
    want <- jaro_winkler(x[i], x[rev(i)])
    expect_identical(jaro_winkler(one, x[rev(i)], nthreads = 1), want)
    expect_identical(jaro_winkler(many, x[rev(i)]), want)
})
