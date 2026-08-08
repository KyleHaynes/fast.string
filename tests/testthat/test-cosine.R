test_that("cosine_similarity matches hand-computed q-gram profiles", {
    expect_equal(fast.string::cosine_similarity("night", "nacht"), 0.25)
    expect_equal(
        fast.string::cosine_similarity("aaaa", "aaab"),
        6 / sqrt(45), tolerance = 1e-12
    )
    expect_equal(fast.string::cosine_similarity("same", "same"), 1)
})

test_that("cosine_similarity has explicit empty-profile behavior", {
    expect_identical(fast.string::cosine_similarity("", "", q = 2L), 1)
    expect_identical(fast.string::cosine_similarity("a", "b", q = 2L), 1)
    expect_identical(fast.string::cosine_similarity("a", "ab", q = 2L), 0)
    expect_true(is.na(fast.string::cosine_similarity(NA_character_, "ab")))
})

test_that("cosine_similarity agrees with stringdist for non-empty profiles", {
    skip_if_not_installed("stringdist")
    a <- c("night", "aaaa", "abcdef", "xyxyxy")
    b <- c("nacht", "aaab", "abcxef", "xyxy")
    expect_equal(
        fast.string::cosine_similarity(a, b, q = 2L),
        stringdist::stringsim(a, b, method = "cosine", q = 2L),
        tolerance = 1e-12
    )
})

test_that("cosine matrices match pairwise values and prepared reuse", {
    a <- rep(c("night", "aaaa", "", NA_character_), length.out = 128L)
    b <- rep(c("nacht", "aaab", "abc", NA_character_), length.out = 128L)
    matrix_result <- fast.string::cosine_matrix(a, b, q = 2L, nthreads = 2L)
    pair_result <- fast.string::cosine_similarity(
        rep(a, times = length(b)), rep(b, each = length(a)),
        q = 2L, nthreads = 1L
    )
    expect_identical(
        matrix_result,
        matrix(pair_result, nrow = length(a))
    )
})

test_that("cosine functions validate inputs", {
    expect_error(fast.string::cosine_similarity(c("a", "b"), "a"), "same length")
    expect_error(fast.string::cosine_similarity(1, "a"), "character vectors")
    expect_error(fast.string::cosine_similarity("a", "b", q = 0), "q.*>= 1")
})
