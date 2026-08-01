matrix_functions <- list(
    jaro_winkler = fast.string::jaro_winkler_matrix,
    levenshtein = fast.string::levenshtein_matrix,
    damerau = fast.string::damerau_levenshtein_matrix,
    osa = fast.string::osa_distance_matrix,
    jaccard = fast.string::jaccard_matrix,
    dice = fast.string::dice_matrix,
    tversky = fast.string::tversky_matrix
)

test_that("all-pairs kernels preserve zero matrix dimensions", {
    for (matrix_function in matrix_functions) {
        expect_identical(
            dim(matrix_function(character(), c("a", "b"), nthreads = 4L)),
            c(0L, 2L)
        )
        expect_identical(
            dim(matrix_function(c("a", "b"), character(), nthreads = 4L)),
            c(2L, 0L)
        )
        expect_identical(
            dim(matrix_function(character(), character(), nthreads = 4L)),
            c(0L, 0L)
        )
    }
})

test_that("linear matrix dispatch preserves 1 by N and N by 1 orientation", {
    a <- "night"
    b <- c("night", "nacht", "other", NA_character_)

    expect_identical(
        fast.string::levenshtein_matrix(a, b, nthreads = 4L),
        matrix(fast.string::levenshtein(rep(a, length(b)), b), nrow = 1L)
    )
    expect_identical(
        fast.string::jaccard_matrix(a, b, nthreads = 4L),
        matrix(fast.string::jaccard_index(rep(a, length(b)), b), nrow = 1L)
    )
    expect_identical(
        fast.string::levenshtein_matrix(b, a, nthreads = 4L),
        matrix(fast.string::levenshtein(b, rep(a, length(b))), ncol = 1L)
    )
    expect_identical(
        fast.string::jaccard_matrix(b, a, nthreads = 4L),
        matrix(fast.string::jaccard_index(b, rep(a, length(b))), ncol = 1L)
    )
})

test_that("Tversky matrices retain asymmetric input orientation", {
    forward <- fast.string::tversky_matrix(
        "ab", "abc", q = 1L, alpha = 0.2, beta = 0.8, nthreads = 4L
    )
    reverse <- fast.string::tversky_matrix(
        "abc", "ab", q = 1L, alpha = 0.2, beta = 0.8, nthreads = 4L
    )

    expect_equal(forward[[1L]], 2 / (2 + 0.8))
    expect_equal(reverse[[1L]], 2 / (2 + 0.2))
    expect_false(isTRUE(all.equal(forward[[1L]], reverse[[1L]])))
})
