test_that("jaccard_index matches hand-computed bigram set overlap", {
    # "night" -> ni,ig,gh,ht ; "nacht" -> na,ac,ch,ht ; intersection={ht}, union size 7
    expect_equal(fast.string::jaccard_index("night", "nacht"), 1 / 7, tolerance = 1e-10)
    expect_equal(fast.string::jaccard_index("same", "same"), 1)
    expect_equal(fast.string::jaccard_index("", ""), 1)
    expect_equal(fast.string::jaccard_index("ab", "cd"), 0)
})

test_that("dice_coefficient matches hand-computed bigram set overlap", {
    # 2*|inter| / (|A|+|B|) = 2*1 / (4+4) = 0.25
    expect_equal(fast.string::dice_coefficient("night", "nacht"), 0.25, tolerance = 1e-10)
    expect_gte(fast.string::dice_coefficient("night", "nacht"),
               fast.string::jaccard_index("night", "nacht")) # Dice >= Jaccard always
})

test_that("tversky_index reduces to Jaccard and Dice at the right weights", {
    a <- c("night", "Kyle Haynes", "abc")
    b <- c("nacht", "Kyle Haynes", "xyz")
    expect_equal(fast.string::tversky_index(a, b, alpha = 1, beta = 1),
                 fast.string::jaccard_index(a, b))
    expect_equal(fast.string::tversky_index(a, b, alpha = 0.5, beta = 0.5),
                 fast.string::dice_coefficient(a, b))
})

test_that("q-gram metrics are vectorised and NA-aware", {
    a <- c("Kyle Haynes", NA, "abc")
    b <- c("Kyle Haynes", "xyz", "abc")
    expect_identical(fast.string::jaccard_index(a, b), c(1, NA, 1))
    expect_identical(fast.string::dice_coefficient(a, b), c(1, NA, 1))
    expect_identical(fast.string::tversky_index(a, b), c(1, NA, 1))
})

test_that("q-gram metrics matrices match pairwise element-by-element", {
    a <- c("night", "abc")
    b <- c("nacht", "abc", "xyz")
    jm <- fast.string::jaccard_matrix(a, b)
    dm <- fast.string::dice_matrix(a, b)
    tm <- fast.string::tversky_matrix(a, b)
    expect_identical(dim(jm), c(2L, 3L))
    for (i in seq_along(a)) for (j in seq_along(b)) {
        expect_equal(jm[i, j], fast.string::jaccard_index(a[i], b[j]))
        expect_equal(dm[i, j], fast.string::dice_coefficient(a[i], b[j]))
        expect_equal(tm[i, j], fast.string::tversky_index(a[i], b[j]))
    }
})

test_that("q-gram metrics respect the q parameter", {
    expect_equal(fast.string::jaccard_index("abcdef", "abcdef", q = 3), 1)
    expect_true(fast.string::jaccard_index("abc", "abd", q = 1) > 0) # shared unigrams a,b
})

test_that("q-gram sets deduplicate repeated grams and handle short strings", {
    expect_equal(
        fast.string::jaccard_index("aaaa", "aaab", q = 2L),
        0.5
    )
    expect_equal(
        fast.string::dice_coefficient("aaaa", "aaab", q = 2L),
        2 / 3
    )
    expect_equal(
        fast.string::jaccard_index("short", "tiny", q = 8L),
        1
    )
    expect_equal(
        fast.string::jaccard_index("short", "long enough", q = 8L),
        0
    )
})

test_that("q-gram metrics survive the RcppParallel threshold (n >= 1000)", {
    # Regression test: thread_local non-POD scratch buffers (std::vector)
    # crashed the first time they were touched inside an RcppParallel/TBB
    # worker thread on this toolchain. Small vectors below the threshold
    # never exercised the parallel path and so never caught it.
    set.seed(1)
    n <- 1500
    a <- replicate(n, paste(sample(letters, sample(3:15, 1), TRUE), collapse = ""))
    b <- rev(a)
    expect_length(fast.string::jaccard_index(a, b), n)
    expect_length(fast.string::dice_coefficient(a, b), n)
    expect_length(fast.string::tversky_index(a, b), n)
    expect_false(anyNA(fast.string::jaccard_index(a, b)))
    expect_false(anyNA(fast.string::dice_coefficient(a, b)))
    expect_false(anyNA(fast.string::tversky_index(a, b)))
})

test_that("q-gram metrics error on bad input", {
    expect_error(fast.string::jaccard_index(c("a", "b"), "x"), "same length")
    expect_error(fast.string::jaccard_index(1, "x"), "character vectors")
    expect_error(fast.string::jaccard_index("a", "b", q = 0), "q.*>= 1")
    expect_error(fast.string::tversky_index("a", "b", alpha = -1), "alpha")
})

test_that("prepared q-gram matrices match pairwise scores including fallback", {
    a <- rep(
        c("night", "nacht", "", "aaaa", NA_character_, "abcdef", "xyxyxy"),
        length.out = 64
    )
    b <- rep(
        c("nacht", "night", "", "bbbb", "abcdef", NA_character_, "xyxy"),
        length.out = 64
    )
    pair_a <- rep(a, times = length(b))
    pair_b <- rep(b, each = length(a))

    for (q in c(1L, 2L, 8L, 9L)) {
        expect_identical(
            fast.string::jaccard_matrix(a, b, q = q, nthreads = 2),
            matrix(
                fast.string::jaccard_index(
                    pair_a, pair_b, q = q, nthreads = 1
                ),
                nrow = length(a)
            )
        )
        expect_identical(
            fast.string::dice_matrix(a, b, q = q, nthreads = 2),
            matrix(
                fast.string::dice_coefficient(
                    pair_a, pair_b, q = q, nthreads = 1
                ),
                nrow = length(a)
            )
        )
        expect_identical(
            fast.string::tversky_matrix(
                a, b, q = q, alpha = 0.3, beta = 0.7, nthreads = 2
            ),
            matrix(
                fast.string::tversky_index(
                    pair_a, pair_b, q = q, alpha = 0.3, beta = 0.7,
                    nthreads = 1
                ),
                nrow = length(a)
            )
        )
    }
})

test_that("q-gram matrix threshold rejection preserves exact results", {
    a <- sprintf("a%03d-abcdefgh", seq_len(63L))
    b <- sprintf("b%03d-abcdefgi", seq_len(65L))
    pair_a <- rep(a, times = length(b))
    pair_b <- rep(b, each = length(a))

    expect_identical(
        fast.string::jaccard_matrix(a, b, q = 8L, nthreads = 4L),
        matrix(
            fast.string::jaccard_index(
                pair_a, pair_b, q = 8L, nthreads = 1L
            ),
            nrow = length(a)
        )
    )
})
