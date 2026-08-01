lookup_score <- function(query, table, method) {
    query <- rep(query, length(table))
    switch(
        method,
        jaro_winkler = fast.string::jaro_winkler(
            query, table, use_bytes = FALSE
        ),
        levenshtein = fast.string::levenshtein_similarity(
            query, table, use_bytes = FALSE
        ),
        osa = fast.string::osa_similarity(
            query, table, use_bytes = FALSE
        ),
        damerau_levenshtein = fast.string::damerau_levenshtein_similarity(
            query, table, use_bytes = FALSE
        )
    )
}

test_that("fuzzy_match agrees with brute-force scoring for every method", {
    queries <- c("SMITH", "kitten", "ca")
    table <- c("SMYTH", "sitting", "abc", "mitten", "SMITH")
    methods <- c(
        "jaro_winkler", "levenshtein", "osa", "damerau_levenshtein"
    )

    for (method in methods) {
        expected <- vapply(queries, function(query) {
            scores <- lookup_score(query, table, method)
            which.max(scores)
        }, integer(1L))
        expect_identical(
            fast.string::fuzzy_match(queries, table, method = method),
            unname(expected),
            info = method
        )
    }
})

test_that("fuzzy_top_n is ordered by query, score, and stable table index", {
    result <- fast.string::fuzzy_top_n(
        c("ab", "zz"), c("ab", "ab", "az", "zz"),
        method = "levenshtein", top_n = 3L
    )
    expect_identical(result$query_index, c(1L, 1L, 1L, 2L, 2L, 2L))
    expect_identical(result$rank, c(1L, 2L, 3L, 1L, 2L, 3L))
    expect_identical(result$table_index[1:3], c(1L, 2L, 3L))
    expect_true(all(diff(result$score[result$query_index == 1L]) <= 0))
})

test_that("lookup cutoffs, missing values, and empty inputs are stable", {
    expect_identical(
        fast.string::fuzzy_match(
            c("kitten", "cat"), c("sitting", "dog"),
            method = "levenshtein", max_distance = 2L, nomatch = 0L
        ),
        c(0L, 0L)
    )
    expect_identical(
        fast.string::fuzzy_match(
            c(NA, "a"), c("b", NA, "a"), match_na = TRUE
        ),
        c(2L, 3L)
    )
    expect_identical(
        fast.string::fuzzy_match(c(a = "x"), character(), nomatch = 0L),
        c(a = 0L)
    )
    empty <- fast.string::fuzzy_top_n(character(), c("a", "b"))
    expect_identical(
        names(empty), c("query_index", "table_index", "score", "rank")
    )
    expect_identical(nrow(empty), 0L)
    expect_type(empty$query_index, "integer")
    expect_type(empty$table_index, "integer")
})

test_that("lookup supports Unicode and agrees across thread counts", {
    e_acute <- intToUtf8(0x00e9)
    queries <- rep(c(e_acute, "SMITH", "kitten"), 400L)
    table <- c("e", e_acute, "SMYTH", "sitting")
    serial <- fast.string::fuzzy_match(
        queries, table, method = "levenshtein", nthreads = 1L
    )
    parallel <- fast.string::fuzzy_match(
        queries, table, method = "levenshtein", nthreads = 4L
    )
    expect_identical(serial, parallel)
    expect_identical(serial[1L], 2L)
})

test_that("lookup validates method-specific and scalar arguments", {
    expect_error(
        fast.string::fuzzy_match("a", "b", max_distance = 1L),
        "only available for edit-distance"
    )
    expect_error(fast.string::fuzzy_top_n("a", "b", top_n = 0L), "positive")
    expect_error(fast.string::fuzzy_match(1, "b"), "character vectors")
    expect_error(fast.string::fuzzy_match("a", "b", min_score = 2), "between")
})
