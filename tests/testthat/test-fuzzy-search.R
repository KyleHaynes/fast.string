u <- function(...) intToUtf8(c(...))

test_that("fagrepl() agrees with base::agrepl()", {
    set.seed(707)
    alpha <- c("a", "b", "c", "d", "A", " ", u(0xe9), u(0xc9))
    rs <- function(n, max) vapply(seq_len(n), function(i)
        paste(sample(alpha, sample(0:max, 1), TRUE), collapse = ""), "")
    x <- c(rs(500, 20), "")
    for (trial in 1:15) {
        pat <- ""
        while (!nzchar(pat)) pat <- rs(1, 8)
        for (md in list(0, 1, 2, 0.1, 0.5, list(all = 1), list(cost = 0.2)))
            for (ic in c(FALSE, TRUE))
                expect_identical(fagrepl(pat, x, max.distance = md, ignore.case = ic),
                                 agrepl(pat, x, max.distance = md, ignore.case = ic),
                                 info = paste(pat, deparse(md), ic))
    }
    long <- rs(50, 150)
    pat <- substr(paste(long, collapse = ""), 1, 70)
    for (md in c(0, 5, 0.2))
        expect_identical(fagrepl(pat, long, max.distance = md),
                         agrepl(pat, long, max.distance = md))
})

test_that("fagrepl() and fagrep() handle NA, names, delegation and errors", {
    x <- c(a = "1 lazy 2", b = "1 lasy 2", c = NA, d = "1 LAZY 2")
    expect_identical(fagrepl("lasy", x), c(a = TRUE, b = TRUE, c = NA, d = FALSE))
    expect_identical(fagrep("lasy", x), 1:2)
    expect_identical(fagrep("lasy", x, value = TRUE), x[1:2])
    expect_identical(fagrepl("lasy", x, costs = c(ins = 2, del = 1, sub = 1)),
                     c(a = TRUE, b = TRUE, c = NA, d = FALSE))
    expect_identical(fagrepl("la.y", "lazy", fixed = FALSE),
                     agrepl("la.y", "lazy", fixed = FALSE))
    expect_identical(fagrepl(NA, c("a", "b")), c(NA, NA))
    expect_error(fagrepl("", "a"), "non-empty")
})

test_that("fuzzy_within() returns exactly the pairs within max_distance", {
    set.seed(808)
    alpha <- c("a", "b", "c", "d", u(0xe9))
    rs <- function(n, max) vapply(seq_len(n), function(i)
        paste(sample(alpha, sample(0:max, 1), TRUE), collapse = ""), "")
    table <- c(rs(150, 8), NA, "")
    table <- c(table, sample(table, 15))
    x <- c(rs(60, 8), sample(table, 10), NA)
    brute <- function(method, k) {
        m <- switch(method,
            osa = osa_distance_matrix(x, table, use_bytes = FALSE),
            levenshtein = levenshtein_matrix(x, table, use_bytes = FALSE),
            damerau_levenshtein = damerau_levenshtein_matrix(x, table, use_bytes = FALSE))
        idx <- which(!is.na(m) & m <= k, arr.ind = TRUE)
        d <- data.frame(query_index = unname(idx[, 1]), table_index = unname(idx[, 2]),
                        distance = as.integer(m[idx]))
        d <- d[order(d$query_index, d$distance, d$table_index), ]
        rownames(d) <- NULL
        d
    }
    for (method in c("osa", "levenshtein", "damerau_levenshtein"))
        for (k in 0:2)
            for (p in c(2, 7))
                expect_equal(fuzzy_within(x, table, k, method, prefix_length = p),
                             brute(method, k), info = paste(method, k, p))
    expect_error(fuzzy_within("a", "b", max_distance = 9), "0 to 8")
})

test_that("tfidf_top_n() and tfidf_match() rank by TF-IDF n-gram cosine", {
    grams <- function(s, q) {
        n <- nchar(s)
        if (n == 0) return(character())
        if (n < q) return(s)
        substring(s, 1:(n - q + 1), q:n)
    }
    ref_scores <- function(x, table, q) {
        tg <- lapply(table, grams, q = q)
        N <- length(table)
        df <- table(unlist(lapply(tg, unique)))
        idf <- function(g) log((1 + N) / (1 + ifelse(g %in% names(df), df[g], 0))) + 1
        vec <- function(g) {
            if (!length(g)) return(numeric())
            tf <- table(g)
            w <- as.numeric(tf) * idf(names(tf))
            setNames(w / sqrt(sum(w^2)), names(tf))
        }
        tv <- lapply(tg, vec)
        t(vapply(x, function(s) {
            qv <- vec(grams(s, q))
            vapply(tv, function(v) {
                common <- intersect(names(qv), names(v))
                sum(qv[common] * v[common])
            }, 0)
        }, numeric(length(table))))
    }
    set.seed(909)
    syl <- c("ACME", " PTY", " LTD", "SMI", "TH", "JON", "ES", " ST", "X", " ")
    table <- unique(vapply(1:150, function(i) paste(sample(syl, sample(1:4, 1), TRUE), collapse = ""), ""))
    x <- c(vapply(1:40, function(i) paste(sample(syl, sample(0:4, 1), TRUE), collapse = ""), ""), "A")
    for (q in c(2, 3)) {
        s <- ref_scores(x, table, q)
        got <- tfidf_top_n(x, table, top_n = length(table), q = q)
        for (i in seq_along(x)) {
            g <- got[got$query_index == i, ]
            pos <- which(s[i, ] > 1e-15)
            expect_setequal(g$table_index, pos)
            expect_equal(g$score, pmin(1, s[i, g$table_index]), tolerance = 1e-12)
            expect_false(is.unsorted(-g$score))
        }
    }
    best <- tfidf_top_n(x, table, top_n = 1)
    m <- tfidf_match(x, table)
    expect_identical(unname(m[best$query_index]), best$table_index)
    expect_true(all(tfidf_top_n(x, table, top_n = 10, min_score = 0.5)$score >= 0.5))
    expect_identical(tfidf_match(c("ACME LTD", "ZZZZ"), c("ACME PTY LTD", "BETA"),
                                 nomatch = 0L), c(1L, 0L))
})

test_that("minhash_pairs() finds verified pairs reproducibly", {
    # datasketch's _optimal_param(threshold, 128, 0.5, 0.5)
    expect_identical(fast.string:::.minhash_banding(0.5, 128), c(bands = 25L, rows = 5L))
    expect_identical(fast.string:::.minhash_banding(0.8, 128), c(bands = 9L, rows = 13L))
    set.seed(1001)
    syl <- c("SMITH", "SMYTH", " ST", " STREET", " RD", "JONES", "12 ", "45 ", "KING", " ")
    base_s <- vapply(1:300, function(i) paste(sample(syl, sample(2:5, 1), TRUE), collapse = ""), "")
    x <- c(base_s, vapply(base_s[1:100], function(s) { substr(s, 1, 1) <- "X"; s }, "",
                          USE.NAMES = FALSE), "", NA)
    j <- jaccard_matrix(x, x, q = 3, use_bytes = FALSE)
    got <- minhash_pairs(x, threshold = 0.6)
    expect_true(all(got$x_index < got$y_index))
    expect_true(all(got$jaccard >= 0.6))
    expect_equal(got$jaccard, j[cbind(got$x_index, got$y_index)])
    truth <- which(upper.tri(j) & !is.na(j) & j >= 0.6, arr.ind = TRUE)
    expect_gt(mean(paste(truth[, 1], truth[, 2]) %in% paste(got$x_index, got$y_index)), 0.5)
    expect_identical(minhash_pairs(x, threshold = 0.6, nthreads = 1), got)
    raw <- minhash_pairs(x, threshold = 0.6, verify = FALSE)
    expect_true(all(paste(got$x_index, got$y_index) %in% paste(raw$x_index, raw$y_index)))
    cross <- minhash_pairs(x[1:200], x[201:402], threshold = 0.6)
    jc <- jaccard_matrix(x[1:200], x[201:402], q = 3, use_bytes = FALSE)
    expect_equal(cross$jaccard, jc[cbind(cross$x_index, cross$y_index)])
    expect_identical(attr(got, "bands"), 18L)
    expect_error(minhash_pairs(x, threshold = 1), "between 0 and 1")
})
