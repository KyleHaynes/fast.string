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
    # Neither string has an 8-gram, but they differ, so nothing is shared.
    expect_equal(
        fast.string::jaccard_index("short", "tiny", q = 8L),
        0
    )
    expect_equal(fast.string::jaccard_index("short", "short", q = 8L), 1)
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

test_that("jaccard and cosine match stringdist across q and the stack-buffer limit", {
    skip_if_not_installed("stringdist")
    # Up to 128 q-grams per string use a stack buffer; longer strings spill
    # to the heap. Small alphabets force repeated grams (multiset counts).
    set.seed(3)
    make <- function(n, alphabet) {
        paste(sample(alphabet, n, replace = TRUE), collapse = "")
    }
    lengths <- c(0:9, 60L, 127:131, 136L, 300L)
    a <- unlist(lapply(lengths, function(n) c(make(n, c("a", "b")), make(n, letters))))
    b <- c(rev(a)[-1], "")
    for (q in 1:8) {
        # Empty-profile conventions are pinned by their own tests.
        keep <- nchar(a) >= q & nchar(b) >= q
        expect_equal(
            fast.string::jaccard_index(a[keep], b[keep], q = q),
            stringdist::stringsim(a[keep], b[keep], method = "jaccard", q = q),
            tolerance = 1e-12, info = paste("q =", q)
        )
        expect_equal(
            fast.string::cosine_similarity(a[keep], b[keep], q = q),
            stringdist::stringsim(a[keep], b[keep], method = "cosine", q = q),
            tolerance = 1e-12, info = paste("q =", q)
        )
    }
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
        length.out = 128
    )
    b <- rep(
        c("nacht", "night", "", "bbbb", "abcdef", NA_character_, "xyxy"),
        length.out = 128
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

test_that("strings without q-grams only match themselves", {
    a <- c("a", "ab", "", "ab")
    b <- c("b", "ab", "", "cd")
    for (fn in list(fast.string::jaccard_index, fast.string::dice_coefficient,
                    fast.string::tversky_index, fast.string::cosine_similarity)) {
        expect_identical(fn(a, b, q = 3), c(0, 1, 1, 0))
    }
    # The prepared matrix path (>= 4096 cells) agrees with the pairwise one.
    x <- rep(c("a", "b", "ab", "abc", "abd"), 20)
    for (pair in list(
        list(fast.string::jaccard_matrix, fast.string::jaccard_index),
        list(fast.string::cosine_matrix, fast.string::cosine_similarity))) {
        m <- pair[[1]](x, x, q = 3)
        expect_identical(as.vector(m),
                         pair[[2]](rep(x, times = 100), rep(x, each = 100), q = 3))
    }
    # alpha = beta = 0 leaves 0/0 for disjoint sets: no overlap, so 0.
    expect_identical(fast.string::tversky_index("abc", "xyz", alpha = 0, beta = 0), 0)
})

test_that("q-gram arguments are validated", {
    expect_error(fast.string::jaccard_index("a", "b", q = NA), "integer >= 1")
    expect_error(fast.string::jaccard_index("a", "b", q = 2.5), "integer >= 1")
    expect_error(fast.string::tversky_index("a", "b", alpha = NA), "non-negative")
})
