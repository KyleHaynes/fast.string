u <- function(...) intToUtf8(c(...))
chars <- function(s) strsplit(s, "")[[1]]

test_that("editex() matches textdistance's Editex", {
    # textdistance.Editex() and its normalized_similarity()
    a <- c("niall", "niall", "ATCG", "Phillips", "", "cat", "Smith", NA)
    b <- c("neal", "nihl", "TAGC", "Filips", "abc", "hat", "smith", "x")
    expect_identical(editex(a, b), c(1, 2, 6, 3, 6, 2, 0, NA))
    expect_equal(editex_similarity(a, b), c(0.9, 0.8, 0.25, 0.8125, 0, 2 / 3, 1, NA))
    expect_identical(editex(paste0(u(0xc9), "mile"), "emile"), 0)
})

test_that("lcs_str() finds the earliest longest common substring", {
    ref <- function(s, t) {
        x <- chars(s); y <- chars(t)
        best <- ""
        for (i in seq_along(x)) for (j in seq_along(y)) {
            k <- 0L
            while (i + k <= length(x) && j + k <= length(y) && x[i + k] == y[j + k]) k <- k + 1L
            if (k > nchar(best)) best <- paste(x[i:(i + k - 1L)], collapse = "")
        }
        best
    }
    set.seed(404)
    alpha <- c("a", "b", "c", " ", u(0xe9))
    a <- vapply(1:200, function(i) paste(sample(alpha, sample(0:10, 1), TRUE), collapse = ""), "")
    b <- vapply(1:200, function(i) paste(sample(alpha, sample(0:10, 1), TRUE), collapse = ""), "")
    expect_identical(lcs_str(a, b), mapply(ref, a, b, USE.NAMES = FALSE))
    expect_equal(lcs_str_similarity(a, b),
                 ifelse(pmax(nchar(a), nchar(b)) == 0, 1,
                        nchar(lcs_str(a, b)) / pmax(nchar(a), nchar(b))))
    expect_identical(lcs_str(c("Acme Holdings", NA), c("ACME HOLDINGS", "x")), c(" H", NA))
    expect_identical(lcs_str("abcxabd", "abd"), "abd")
})

test_that("smith_waterman() matches a three-matrix Gotoh reference", {
    gotoh <- function(s, t, match, mismatch, open, extend) {
        x <- chars(s); y <- chars(t)
        n <- length(x); m <- length(y)
        if (!n || !m) return(0)
        H <- matrix(0, n + 1, m + 1); E <- F <- matrix(-Inf, n + 1, m + 1)
        for (i in 1:n) for (j in 1:m) {
            E[i + 1, j + 1] <- max(H[i + 1, j] + open, E[i + 1, j] + extend)
            F[i + 1, j + 1] <- max(H[i, j + 1] + open, F[i, j + 1] + extend)
            H[i + 1, j + 1] <- max(0, H[i, j] + if (x[i] == y[j]) match else mismatch,
                                   E[i + 1, j + 1], F[i + 1, j + 1])
        }
        max(H)
    }
    set.seed(505)
    alpha <- c("a", "b", "c", "d")
    a <- vapply(1:150, function(i) paste(sample(alpha, sample(0:12, 1), TRUE), collapse = ""), "")
    b <- vapply(1:150, function(i) paste(sample(alpha, sample(0:12, 1), TRUE), collapse = ""), "")
    for (p in list(c(1, -2, -0.5, -0.5), c(2, -1, -3, -1))) {
        want <- mapply(gotoh, a, b, MoreArgs = list(match = p[1], mismatch = p[2],
                                                    open = p[3], extend = p[4]),
                       USE.NAMES = FALSE)
        expect_equal(smith_waterman(a, b, p[1], p[2], p[3], p[4], normalize = FALSE), want)
    }
    # SimMetrics SmithWatermanGotoh normalisation
    expect_equal(smith_waterman("Dept of Health", "Queensland Health"), 0.5)
    expect_identical(smith_waterman(c("", "", "a"), c("", "a", "")), c(1, 0, 0))
    expect_error(smith_waterman("a", "b", match = 0), "positive")
    expect_error(smith_waterman("a", "b", gap_open = 1), "zero or negative")
})

test_that("monge_elkan() averages the best word similarities", {
    tokens <- function(s) { t <- strsplit(s, "[[:space:]]+")[[1]]; t[nzchar(t)] }
    ref <- function(s, t, sim, symmetric = FALSE) {
        ts <- tokens(s); tt <- tokens(t)
        if (!length(ts) || !length(tt)) return(as.numeric(!length(ts) && !length(tt)))
        dir <- function(p, q) mean(vapply(p, function(w) max(sim(rep(w, length(q)), q)), 0))
        if (symmetric) (dir(ts, tt) + dir(tt, ts)) / 2 else dir(ts, tt)
    }
    set.seed(606)
    w <- c("smith", "smyth", "john", "jon", "12", "st", "street", "kyle")
    a <- vapply(1:120, function(i) paste(sample(w, sample(0:4, 1), TRUE), collapse = " "), "")
    b <- vapply(1:120, function(i) paste(sample(w, sample(0:4, 1), TRUE), collapse = "  "), "")
    jw <- function(p, q) jaro_winkler(p, q, use_bytes = FALSE)
    lv <- function(p, q) levenshtein_similarity(p, q, use_bytes = FALSE)
    expect_equal(monge_elkan(a, b), mapply(ref, a, b, MoreArgs = list(sim = jw), USE.NAMES = FALSE))
    expect_equal(monge_elkan(a, b, symmetric = TRUE),
                 mapply(ref, a, b, MoreArgs = list(sim = jw, symmetric = TRUE), USE.NAMES = FALSE))
    expect_equal(monge_elkan(a, b, "levenshtein"),
                 mapply(ref, a, b, MoreArgs = list(sim = lv), USE.NAMES = FALSE))
    expect_identical(monge_elkan("Smith", "John Smith"), 1)
    expect_identical(monge_elkan(c("", "", NA), c("", "a", "a")), c(1, 0, NA))
})
