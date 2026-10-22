u <- function(...) intToUtf8(c(...))

# Plain-R references on code points.
chars <- function(s) strsplit(s, "")[[1]]
lcs_len <- function(a, b) {
    x <- chars(a); y <- chars(b)
    m <- matrix(0L, length(x) + 1L, length(y) + 1L)
    for (i in seq_along(x)) for (j in seq_along(y))
        m[i + 1L, j + 1L] <- if (x[i] == y[j]) m[i, j] + 1L else max(m[i, j + 1L], m[i + 1L, j])
    m[length(x) + 1L, length(y) + 1L]
}
weighted_lev <- function(a, b, w) {
    x <- chars(a); y <- chars(b)
    d <- matrix(0, length(x) + 1L, length(y) + 1L)
    d[, 1] <- (seq_len(length(x) + 1L) - 1) * w[2]
    d[1, ] <- (seq_len(length(y) + 1L) - 1) * w[1]
    for (i in seq_along(x)) for (j in seq_along(y))
        d[i + 1, j + 1] <- min(d[i, j + 1] + w[2], d[i + 1, j] + w[1],
                               d[i, j] + if (x[i] == y[j]) 0 else w[3])
    d[length(x) + 1L, length(y) + 1L]
}

set.seed(303)
alpha <- c("a", "b", "c", "d", " ", u(0xe9))
rs <- function(n, max) vapply(seq_len(n), function(i)
    paste(sample(alpha, sample(0:max, 1), TRUE), collapse = ""), "")
a <- c(rs(300, 10), "", "", "kitten", NA)
b <- c(rs(300, 10), "", "x", "sitting", "y")

test_that("LCSseq, Indel, Prefix and Postfix agree with plain references", {
    ok <- !is.na(a) & !is.na(b)
    lcs <- mapply(lcs_len, a[ok], b[ok], USE.NAMES = FALSE)
    la <- nchar(a[ok]); lb <- nchar(b[ok]); longest <- pmax(la, lb)
    expect_equal(lcs_seq_distance(a, b)[ok], longest - lcs)
    expect_equal(lcs_seq_similarity(a, b)[ok], ifelse(longest == 0, 1, lcs / longest))
    expect_equal(indel_distance(a, b)[ok], la + lb - 2 * lcs)
    expect_equal(indel_similarity(a, b)[ok],
                 ifelse(la + lb == 0, 1, 1 - (la + lb - 2 * lcs) / (la + lb)))
    pre <- mapply(function(s, t) {
        x <- chars(s); y <- chars(t); k <- 0L
        while (k < min(length(x), length(y)) && x[k + 1L] == y[k + 1L]) k <- k + 1L
        k
    }, a[ok], b[ok], USE.NAMES = FALSE)
    expect_equal(prefix_distance(a, b)[ok], longest - pre)
    expect_equal(prefix_similarity(a, b)[ok], ifelse(longest == 0, 1, pre / longest))
    post <- mapply(function(s, t) {
        x <- rev(chars(s)); y <- rev(chars(t)); k <- 0L
        while (k < min(length(x), length(y)) && x[k + 1L] == y[k + 1L]) k <- k + 1L
        k
    }, a[ok], b[ok], USE.NAMES = FALSE)
    expect_equal(postfix_distance(a, b)[ok], longest - post)
    expect_true(all(is.na(lcs_seq_distance(a, b)[!ok])))
    expect_equal(indel_similarity(a, b), fuzz_ratio(a, b, full_process = FALSE) / 100)
    expect_identical(lcs_seq_distance("kitten", "sitting"), 3)
    expect_identical(indel_distance("kitten", "sitting"), 5)
})

test_that("weighted Levenshtein matches a Wagner-Fischer reference", {
    ok <- !is.na(a) & !is.na(b)
    for (w in list(c(1, 1, 2), c(2, 3, 1), c(0, 1, 1), c(1, 2, 5))) {
        want <- mapply(weighted_lev, a[ok], b[ok], MoreArgs = list(w = w), USE.NAMES = FALSE)
        expect_equal(levenshtein(a, b, use_bytes = FALSE, weights = w)[ok], want,
                     info = paste(w, collapse = ","))
        maximum <- pmin(nchar(a[ok]) * w[2] + nchar(b[ok]) * w[1],
                        ifelse(nchar(a[ok]) >= nchar(b[ok]),
                               nchar(b[ok]) * w[3] + (nchar(a[ok]) - nchar(b[ok])) * w[2],
                               nchar(a[ok]) * w[3] + (nchar(b[ok]) - nchar(a[ok])) * w[1]))
        expect_equal(levenshtein_similarity(a, b, use_bytes = FALSE, weights = w)[ok],
                     ifelse(maximum > 0, 1 - want / maximum, 1))
    }
    m <- levenshtein_matrix(c(x = "kitten", y = "flaw"), c("sitting", "lawn"),
                            weights = c(1, 1, 2))
    expect_equal(unname(m), outer(c("kitten", "flaw"), c("sitting", "lawn"),
                                  Vectorize(function(s, t) weighted_lev(s, t, c(1, 1, 2)))))
    expect_identical(rownames(m), c("x", "y"))
    expect_identical(levenshtein("kitten", "sitting", weights = c(substitute = 2, insert = 1, delete = 1)),
                     levenshtein("kitten", "sitting", weights = c(1, 1, 2)))
    expect_identical(levenshtein("kitten", "sitting", weights = c(1, 1, 1)), 3)
    expect_error(levenshtein("a", "b", weights = c(1, -1, 1)), "non-negative")
})

test_that("edit_ops() gives RapidFuzz's editops and opcodes", {
    # rapidfuzz.distance.Levenshtein.editops / opcodes (0-based there).
    e <- edit_ops("kitten", "sitting")
    expect_identical(e$type, c("replace", "replace", "insert"))
    expect_identical(e$a_pos, c(1L, 5L, 7L))
    expect_identical(e$b_pos, c(1L, 5L, 7L))
    o <- edit_ops("kitten", "sitting", type = "opcodes")
    expect_identical(o$type, c("replace", "equal", "replace", "equal", "insert"))
    expect_identical(o$a_start, c(1L, 2L, 5L, 6L, 7L))
    expect_identical(o$a_end, c(1L, 4L, 5L, 6L, 6L))
    expect_identical(o$b_start, c(1L, 2L, 5L, 6L, 7L))
    expect_identical(o$b_end, c(1L, 4L, 5L, 6L, 7L))
    e2 <- edit_ops("Dept of Health", "Queensland Health")
    expect_identical(e2$type, c("insert", "replace", "insert", "insert", rep("replace", 5)))
    expect_identical(e2$a_pos, c(1L, 1L, 3L, 3L, 3L, 4L, 5L, 6L, 7L))
    expect_identical(e2$b_pos, c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L))
    expect_identical(edit_ops("abc", "")$type, rep("delete", 3))
})

test_that("applying the edit ops turns a into b", {
    apply_ops <- function(s, t, ops) {
        x <- chars(s); y <- chars(t)
        # editops are in order; apply from the end so positions stay valid
        for (k in rev(seq_len(nrow(ops)))) {
            p <- ops$a_pos[k]
            x <- switch(ops$type[k],
                replace = { x[p] <- y[ops$b_pos[k]]; x },
                delete = x[-p],
                insert = append(x, y[ops$b_pos[k]], after = p - 1L))
        }
        paste(x, collapse = "")
    }
    ok <- which(!is.na(a) & !is.na(b))
    ops <- edit_ops(a[ok], b[ok])
    counts <- tabulate(ops$pair, length(ok))
    expect_equal(counts, levenshtein(a[ok], b[ok], use_bytes = FALSE))
    for (i in seq_along(ok)) {
        expect_identical(apply_ops(a[ok][i], b[ok][i], ops[ops$pair == i, ]),
                         b[ok][i])
    }
})
