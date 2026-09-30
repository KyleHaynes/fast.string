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

test_that("levenshtein agrees with base at word-size dispatch boundaries", {
    lengths <- c(63L, 64L, 65L, 127L, 128L)
    make_string <- function(n, offset) {
        paste0(
            letters[(seq_len(n) + offset - 1L) %% length(letters) + 1L],
            collapse = ""
        )
    }
    a <- vapply(lengths, make_string, character(1L), offset = 0L)
    b <- vapply(lengths, make_string, character(1L), offset = 7L)
    expected <- diag(utils::adist(a, b))

    expect_identical(
        fast.string::levenshtein(a, b, nthreads = 1L),
        as.double(expected)
    )
    expect_identical(
        fast.string::levenshtein(a, b, nthreads = 4L),
        as.double(expected)
    )
})

test_that("OSA and unrestricted Damerau-Levenshtein remain distinct", {
    expect_identical(fast.string::osa_distance("ab", "ba"), 1)
    expect_identical(fast.string::damerau_levenshtein("ab", "ba"), 1)
    expect_identical(fast.string::osa_distance("ca", "abc"), 3)
    expect_identical(fast.string::damerau_levenshtein("ca", "abc"), 2)
    expect_lte(fast.string::osa_distance("ab", "ba"),
               fast.string::levenshtein("ab", "ba"))

    skip_if_not_installed("stringdist")
    expect_identical(
        fast.string::osa_distance("ca", "abc"),
        stringdist::stringdist("ca", "abc", method = "osa", useBytes = TRUE)
    )
    expect_identical(
        fast.string::damerau_levenshtein("ca", "abc"),
        stringdist::stringdist("ca", "abc", method = "dl", useBytes = TRUE)
    )
})

test_that("OSA and Damerau-Levenshtein agree with stringdist across dispatch paths", {
    skip_if_not_installed("stringdist")
    # Near-duplicates exercise the transposition terms and shared affixes;
    # lengths straddle the 64-byte bit-vector and 256-byte stack limits.
    set.seed(42)
    make <- function(n, alphabet) {
        paste(sample(alphabet, n, replace = TRUE), collapse = "")
    }
    mutate <- function(s) {
        chars <- strsplit(s, "")[[1]]
        if (length(chars) < 2L) return(paste0(s, "x"))
        pos <- sample.int(length(chars) - 1L, 1L)
        chars[c(pos, pos + 1L)] <- chars[c(pos + 1L, pos)]
        chars[sample.int(length(chars), 1L)] <- "z"
        paste(chars, collapse = "")
    }
    lengths <- c(0:5, 20L, 63:66, 127L, 255:258, 300L)
    a <- unlist(lapply(lengths, function(n) c(
        make(n, c("a", "b")), make(n, letters[1:4]), make(n, letters)
    )))
    b <- c(vapply(a, mutate, character(1L), USE.NAMES = FALSE), rev(a))
    a <- c(a, a)

    expect_identical(
        fast.string::osa_distance(a, b),
        stringdist::stringdist(a, b, method = "osa", useBytes = TRUE)
    )
    expect_identical(
        fast.string::damerau_levenshtein(a, b),
        stringdist::stringdist(a, b, method = "dl", useBytes = TRUE)
    )
    expect_identical(
        fast.string::osa_distance(b, a), fast.string::osa_distance(a, b)
    )
    expect_identical(
        fast.string::damerau_levenshtein(b, a),
        fast.string::damerau_levenshtein(a, b)
    )
})

test_that("code-point Damerau-Levenshtein matches stringdist on non-ASCII text", {
    skip_if_not_installed("stringdist")
    pool <- c("a", "b", intToUtf8(0x00e9), intToUtf8(0x4f60), intToUtf8(0x1f600))
    set.seed(7)
    a <- vapply(sample(0:12, 200L, TRUE), function(n) {
        paste(sample(pool, n, TRUE), collapse = "")
    }, character(1L))
    b <- vapply(sample(0:12, 200L, TRUE), function(n) {
        paste(sample(pool, n, TRUE), collapse = "")
    }, character(1L))
    expect_identical(
        fast.string::damerau_levenshtein(a, b, use_bytes = FALSE),
        stringdist::stringdist(a, b, method = "dl")
    )
    expect_identical(
        fast.string::osa_distance(a, b, use_bytes = FALSE),
        stringdist::stringdist(a, b, method = "osa")
    )
})

test_that("damerau_levenshtein is symmetric with very unequal lengths", {
    short <- "ab"
    long <- paste0("ba", strrep("c", 300L))

    expect_identical(
        fast.string::damerau_levenshtein(short, long),
        fast.string::damerau_levenshtein(long, short)
    )
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
    expect_identical(fast.string::osa_distance(a, b), c(3, NA, 0))
    expect_identical(fast.string::damerau_levenshtein(a, b), c(3, NA, 0))
    expect_identical(fast.string::hamming(c("abc", NA), c("abd", "x")), c(1, NA))
})

test_that("edit-distance matrices match pairwise element-by-element", {
    a <- c("kitten", "abc")
    b <- c("sitting", "abc", "xyz")
    lm <- fast.string::levenshtein_matrix(a, b)
    dm <- fast.string::damerau_levenshtein_matrix(a, b)
    om <- fast.string::osa_distance_matrix(a, b)
    expect_identical(dim(lm), c(2L, 3L))
    for (i in seq_along(a)) for (j in seq_along(b)) {
        expect_identical(lm[i, j], fast.string::levenshtein(a[i], b[j]))
        expect_identical(om[i, j], fast.string::osa_distance(a[i], b[j]))
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
    expect_length(fast.string::osa_distance(a, b), n)
    expect_length(fast.string::hamming(a, b), n)
    expect_false(anyNA(fast.string::levenshtein(a, b)))
    expect_false(anyNA(fast.string::damerau_levenshtein(a, b)))
    expect_false(anyNA(fast.string::osa_distance(a, b)))
})

test_that("edit distances support UTF-8 code-point comparison", {
    e_acute <- intToUtf8(0x00e9)
    grin <- intToUtf8(0x1f600)
    beam <- intToUtf8(0x1f601)

    expect_identical(fast.string::levenshtein(e_acute, "e"), 2)
    expect_identical(
        fast.string::levenshtein(e_acute, "e", use_bytes = FALSE), 1
    )
    expect_identical(
        fast.string::hamming(grin, beam, use_bytes = FALSE), 1
    )
    expect_identical(
        fast.string::levenshtein("ASCII", "ASCI", use_bytes = TRUE),
        fast.string::levenshtein("ASCII", "ASCI", use_bytes = FALSE)
    )
})

test_that("normalized edit similarities have stable empty and missing behavior", {
    expect_identical(
        fast.string::levenshtein_similarity(
            c("", "kitten", NA), c("", "sitting", "x")
        ),
        c(1, 1 - 3 / 7, NA)
    )
    expect_identical(fast.string::osa_similarity("ca", "abc"), 0)
    expect_equal(
        fast.string::damerau_levenshtein_similarity("ca", "abc"),
        1 / 3
    )
})

test_that("levenshtein_within agrees with exact distance at each cutoff", {
    a <- c("kitten", "cat", "", NA)
    b <- c("sitting", "dog", "abc", "x")
    for (cutoff in 0:4) {
        expect_identical(
            fast.string::levenshtein_within(a, b, cutoff),
            fast.string::levenshtein(a, b) <= cutoff
        )
    }
    expect_error(
        fast.string::levenshtein_within("a", "b", -1),
        "non-negative integer"
    )
})

test_that("bytes-encoded strings require explicit byte comparison", {
    bytes <- rawToChar(as.raw(0xff))
    Encoding(bytes) <- "bytes"
    expect_error(
        fast.string::levenshtein(bytes, "x", use_bytes = FALSE),
        "a\\[1\\].*bytes-encoded"
    )
    expect_identical(
        fast.string::levenshtein(bytes, "x", use_bytes = TRUE), 1
    )
})

test_that("edit-distance functions error on mismatched lengths or non-character input", {
    expect_error(fast.string::levenshtein(c("a", "b"), "x"), "same length")
    expect_error(fast.string::levenshtein(1, "x"), "character vectors")
})
