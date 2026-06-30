test_that("levenshtein matches classic reference values", {
    expect_identical(fast.string::levenshtein("kitten", "sitting"), 3)
    expect_identical(fast.string::levenshtein("", ""), 0)
    expect_identical(fast.string::levenshtein("abc", ""), 3)
    expect_identical(fast.string::levenshtein("same", "same"), 0)
})

test_that("levenshtein bit-parallel path agrees with the long-string DP fallback", {
    a <- paste(rep("x", 100), collapse = "")
    b <- a
    substr(b, 51, 51) <- "y"
    expect_identical(fast.string::levenshtein(a, b), 1)
})

test_that("damerau_levenshtein counts an adjacent transposition as one edit", {
    expect_identical(fast.string::damerau_levenshtein("ab", "ba"), 1)
    expect_identical(fast.string::damerau_levenshtein("ca", "abc"), 3)
    expect_lte(fast.string::damerau_levenshtein("ab", "ba"),
               fast.string::levenshtein("ab", "ba")) # transposition never costs more than plain Levenshtein
})

test_that("hamming matches classic reference values and Inf for unequal length", {
    expect_identical(fast.string::hamming("karolin", "kathrin"), 3)
    expect_identical(fast.string::hamming("abc", "abc"), 0)
    expect_identical(fast.string::hamming("abc", "ab"), Inf)
})

test_that("edit-distance functions are vectorised and NA-aware", {
    a <- c("kitten", NA, "abc")
    b <- c("sitting", "x", "abc")
    expect_identical(fast.string::levenshtein(a, b), c(3, NA, 0))
    expect_identical(fast.string::damerau_levenshtein(a, b), c(3, NA, 0))
    expect_identical(fast.string::hamming(c("abc", NA), c("abd", "x")), c(1, NA))
})

test_that("edit-distance matrices match pairwise element-by-element", {
    a <- c("kitten", "abc")
    b <- c("sitting", "abc", "xyz")
    lm <- fast.string::levenshtein_matrix(a, b)
    dm <- fast.string::damerau_levenshtein_matrix(a, b)
    expect_identical(dim(lm), c(2L, 3L))
    for (i in seq_along(a)) for (j in seq_along(b)) {
        expect_identical(lm[i, j], fast.string::levenshtein(a[i], b[j]))
        expect_identical(dm[i, j], fast.string::damerau_levenshtein(a[i], b[j]))
    }
})

test_that("edit-distance functions survive the RcppParallel threshold (n >= 1000)", {
    # Regression test: thread_local non-POD scratch buffers (std::vector)
    # crashed the first time they were touched inside an RcppParallel/TBB
    # worker thread on this toolchain. Small vectors below the threshold
    # never exercised the parallel path and so never caught it.
    set.seed(1)
    n <- 1500
    a <- replicate(n, paste(sample(letters, sample(3:15, 1), TRUE), collapse = ""))
    b <- rev(a)
    expect_length(fast.string::levenshtein(a, b), n)
    expect_length(fast.string::damerau_levenshtein(a, b), n)
    expect_length(fast.string::hamming(a, b), n)
    expect_false(anyNA(fast.string::levenshtein(a, b)))
    expect_false(anyNA(fast.string::damerau_levenshtein(a, b)))
})

test_that("edit-distance functions error on mismatched lengths or non-character input", {
    expect_error(fast.string::levenshtein(c("a", "b"), "x"), "same length")
    expect_error(fast.string::levenshtein(1, "x"), "character vectors")
})
