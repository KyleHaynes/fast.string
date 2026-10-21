choices <- c("Atlanta Falcons", "New York Jets", "New York Giants",
             "Dallas Cowboys", NA, "new york jets", "")

# Every score of x[i] against choices, as a brute-force reference.
all_scores <- function(query, choices, scorer, full_process = TRUE) {
    fast.string::fuzz_cpdist(rep(query, length(choices)), choices,
                             scorer = scorer, full_process = full_process)
}

# What process.extract() returns, written out directly: drop NA choices
# and those below the cutoff, order by score (ties by position), cut to limit.
reference_extract <- function(query, choices, scorer, limit, cutoff) {
    s <- all_scores(query, choices, scorer)
    keep <- which(!is.na(s) & s >= cutoff)
    keep <- keep[order(-s[keep], keep)]
    if (!is.null(limit)) keep <- head(keep, limit)
    list(index = keep, score = s[keep])
}

test_that("fuzz_extract matches a brute-force ranking for every scorer", {
    queries <- c("new york jets", "dallas", "atlanta falcon", "", "giants ny")
    for (scorer in fast.string:::.fuzz_scorers) {
        for (limit in list(1L, 3L, NULL)) {
            for (cutoff in c(0, 50)) {
                got <- fast.string::fuzz_extract(queries, choices, scorer = scorer,
                                                 limit = limit,
                                                 score_cutoff = cutoff)
                for (q in seq_along(queries)) {
                    want <- reference_extract(queries[q], choices, scorer,
                                              limit, cutoff)
                    rows <- got[got$query_index == q, ]
                    expect_identical(rows$choice_index, want$index)
                    expect_identical(rows$score, want$score)
                    expect_identical(rows$rank, seq_along(want$index))
                    expect_identical(rows$choice, choices[want$index])
                }
            }
        }
    }
})

test_that("fuzz_extract follows rapidfuzz.process.extract", {
    # process.extract("abcd", ["abce", "abcd", "x", "abcd"], scorer=ratio,
    #                 limit=None)
    got <- fast.string::fuzz_extract("abcd", c("abce", "abcd", "x", "abcd"),
                                     scorer = "ratio", limit = NULL)
    expect_identical(got$choice_index, c(2L, 4L, 1L, 3L))
    expect_identical(got$score, c(100, 100, 75, 0))
    expect_identical(names(got),
                     c("query_index", "choice_index", "choice", "score", "rank"))
})

test_that("named choices do not leak into row names", {
    named <- c(a = "abcd", b = "abce", a = "abcd")
    got <- fast.string::fuzz_extract("abcd", named, scorer = "ratio")
    expect_identical(got$choice, c("abcd", "abcd", "abce"))
    expect_identical(rownames(got), c("1", "2", "3"))
    one <- fast.string::fuzz_extract_one(c("abcd", "zz"), named,
                                         scorer = "ratio", score_cutoff = 90)
    expect_identical(one$choice, c("abcd", NA))
})

test_that("fuzz_extract_one returns the first best match per query", {
    queries <- c("cowboys", "falcon", "chargers", NA, "new york jets")
    got <- fast.string::fuzz_extract_one(queries, choices, score_cutoff = 50)
    expect_identical(nrow(got), length(queries))
    for (q in c(1L, 2L, 5L)) {
        want <- reference_extract(queries[q], choices, "wratio", 1L, 50)
        expect_identical(got$choice_index[q], want$index)
        expect_identical(got$score[q], want$score)
    }
    # "new york jets" scores 100 against two choices; the first one wins.
    expect_identical(got$choice_index[5], 2L)
    expect_true(all(is.na(got[3:4, ])))
    # process.extractOne("abcd", ["abce", "abcf"], scorer=ratio)
    one <- fast.string::fuzz_extract_one("abcd", c("abce", "abcf"),
                                         scorer = "ratio")
    expect_identical(one, data.frame(choice = "abce", score = 75,
                                     choice_index = 1L))
})

test_that("a few queries against many choices split the work and agree", {
    set.seed(7)
    words <- c("acme", "pty", "ltd", "holdings", "group", "smith", "jones",
               "and", "co", "trading", "services", "australia")
    pool <- vapply(seq_len(5000L), function(i) {
        paste(sample(words, sample(1:4, 1L)), collapse = " ")
    }, "")
    queries <- c("acme holdings", "smith and jones")
    for (scorer in c("wratio", "token_set_ratio", "partial_ratio")) {
        parallel <- fast.string::fuzz_extract(queries, pool, scorer = scorer,
                                              limit = 25L, nthreads = 4)
        serial <- fast.string::fuzz_extract(queries, pool, scorer = scorer,
                                            limit = 25L, nthreads = 1)
        expect_identical(parallel, serial)
        for (q in seq_along(queries)) {
            want <- reference_extract(queries[q], pool, scorer, 25L, 0)
            rows <- parallel[parallel$query_index == q, ]
            expect_identical(rows$choice_index, want$index)
        }
    }
})

test_that("fuzz_cdist matches the pairwise scores, with or without symmetry", {
    x <- c(a = "new york jets", b = "", c = "dallas  cowboys", d = NA,
           e = "NY Jets")
    copy <- x[seq_along(x)]
    for (scorer in fast.string:::.fuzz_scorers) {
        self <- fast.string::fuzz_cdist(x, scorer = scorer)
        other <- fast.string::fuzz_cdist(x, copy, scorer = scorer)
        expect_identical(self, other)
        expect_identical(dimnames(self), list(names(x), names(x)))
        want <- outer(seq_along(x), seq_along(x), function(i, j) {
            fast.string::fuzz_cpdist(x[i], x[j], scorer = scorer)
        })
        expect_identical(unname(self), want)
    }
    m <- fast.string::fuzz_cdist(c("new york", "dallas"), choices,
                                 scorer = "token_set_ratio", score_cutoff = 50)
    expect_identical(dim(m), c(2L, length(choices)))
    expect_true(all(is.na(m[, 5])))
    expect_true(all(m[!is.na(m)] == 0 | m[!is.na(m)] >= 50))
})

test_that("large matrices use threads and agree with serial runs", {
    set.seed(3)
    x <- replicate(150, paste(sample(c(letters[1:6], " "), 12, TRUE),
                              collapse = ""))
    y <- rev(x)
    expect_identical(fast.string::fuzz_cdist(x, y, scorer = "wratio", nthreads = 4),
                     fast.string::fuzz_cdist(x, y, scorer = "wratio", nthreads = 1))
    expect_identical(fast.string::fuzz_cdist(x, scorer = "partial_ratio",
                                             nthreads = 4),
                     fast.string::fuzz_cdist(x, scorer = "partial_ratio",
                                             nthreads = 1))
})

test_that("scorers can be named in several ways", {
    a <- "fuzzy was a bear"
    b <- "fuzzy fuzzy was a bear"
    want <- fast.string::fuzz_wratio(a, b)
    expect_identical(fast.string::fuzz_cpdist(a, b, scorer = "WRatio"), want)
    expect_identical(fast.string::fuzz_cpdist(a, b, scorer = "fuzz_wratio"), want)
    expect_identical(
        fast.string::fuzz_cpdist(a, b, scorer = fast.string::fuzz_wratio), want
    )
    expect_identical(fast.string::fuzz_cpdist(a, b),
                     fast.string::fuzz_ratio(a, b))
    expect_error(fast.string::fuzz_cpdist(a, b, scorer = "levenshtein"),
                 "must be one of")
    expect_error(fast.string::fuzz_cpdist(a, b, scorer = toupper), "scorers")
})

test_that("process functions validate their arguments", {
    expect_error(fast.string::fuzz_extract("a", 1), "character vectors")
    expect_error(fast.string::fuzz_extract("a", "b", limit = 0), "limit")
    expect_error(fast.string::fuzz_extract("a", "b", limit = 1.5), "limit")
    expect_error(fast.string::fuzz_extract_one("a", "b", score_cutoff = -1),
                 "between 0 and 100")
    expect_error(fast.string::fuzz_cdist(1), "character vectors")
    expect_identical(nrow(fast.string::fuzz_extract(character(), "a")), 0L)
    expect_identical(nrow(fast.string::fuzz_extract("a", character())), 0L)
    expect_identical(dim(fast.string::fuzz_cdist(character(), "a")), c(0L, 1L))
})
