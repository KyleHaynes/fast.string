test_that("jaro_winkler matches classic published reference values", {
    expect_equal(fast.string::jaro_winkler("MARTHA", "MARHTA"), 0.961111, tolerance = 1e-5)
    expect_equal(fast.string::jaro_winkler("DWAYNE", "DUANE"),  0.84,     tolerance = 1e-5)
    expect_equal(fast.string::jaro_winkler("DIXON", "DICKSONX"), 0.813333, tolerance = 1e-5)
    expect_equal(fast.string::jaro_winkler("JELLYFISH", "SMELLYFISH"), 0.896296, tolerance = 1e-5)
})

test_that("jaro_winkler edge cases", {
    expect_equal(fast.string::jaro_winkler("SAME", "SAME"), 1.0)
    expect_equal(fast.string::jaro_winkler("", ""), 1.0)
    expect_equal(fast.string::jaro_winkler("", "A"), 0.0)
})

# Direct O(l1 * window) Jaro-Winkler, to check the bit-parallel kernel
# (strings of up to 64 bytes) and the scalar one (longer strings) against.
.reference_jaro_winkler <- function(s1, s2, p = 0.1) {
    a <- strsplit(s1, "", fixed = TRUE)[[1L]]
    b <- strsplit(s2, "", fixed = TRUE)[[1L]]
    l1 <- length(a)
    l2 <- length(b)
    if (l1 == 0L && l2 == 0L) return(1)
    if (l1 == 0L || l2 == 0L) return(0)
    window <- max(0L, max(l1, l2) %/% 2L - 1L)
    matched_a <- logical(l1)
    matched_b <- logical(l2)
    for (i in seq_len(l1)) {
        lo <- max(1L, i - window)
        hi <- min(l2, i + window)
        if (lo > hi) next
        for (j in lo:hi) {
            if (!matched_b[j] && a[i] == b[j]) {
                matched_a[i] <- TRUE
                matched_b[j] <- TRUE
                break
            }
        }
    }
    m <- sum(matched_a)
    if (m == 0L) return(0)
    half_transpositions <- sum(a[matched_a] != b[matched_b]) / 2
    jaro <- (m / l1 + m / l2 + (m - half_transpositions) / m) / 3
    prefix <- 0L
    while (prefix < min(4L, l1, l2) && a[prefix + 1L] == b[prefix + 1L])
        prefix <- prefix + 1L
    jaro + prefix * p * (1 - jaro)
}

test_that("jaro_winkler matches a direct implementation on match-heavy random strings", {
    # A three-letter alphabet makes matches, window edges and transpositions
    # common; lengths either side of 64 bytes cover both kernels.
    set.seed(20260930)
    random_strings <- function(n, max_len) {
        vapply(sample(0:max_len, n, replace = TRUE), function(len) {
            paste(sample(c("a", "b", "c"), len, replace = TRUE), collapse = "")
        }, character(1))
    }
    a <- c(random_strings(1500, 12), random_strings(150, 70))
    b <- c(random_strings(1500, 12), random_strings(150, 70))
    b[1:50] <- a[1:50]
    expected <- mapply(.reference_jaro_winkler, a, b, USE.NAMES = FALSE)

    expect_equal(fast.string::jaro_winkler(a, b), expected, tolerance = 1e-12)
    expect_equal(fast.string::jaro_winkler(a, b, nthreads = 1L), expected,
                 tolerance = 1e-12)
    expect_equal(fast.string::jaro_winkler(a, b, p = 0), mapply(
        .reference_jaro_winkler, a, b, MoreArgs = list(p = 0), USE.NAMES = FALSE
    ), tolerance = 1e-12)
})

test_that("jaro_winkler supports code-point comparison without changing ASCII", {
    e_acute <- intToUtf8(0x00e9)
    expect_equal(
        fast.string::jaro_winkler("MARTHA", "MARHTA", use_bytes = FALSE),
        fast.string::jaro_winkler("MARTHA", "MARHTA", use_bytes = TRUE)
    )
    expect_false(isTRUE(all.equal(
        fast.string::jaro_winkler(paste0(e_acute, "a"), "ea"),
        fast.string::jaro_winkler(
            paste0(e_acute, "a"), "ea", use_bytes = FALSE
        )
    )))
})

test_that("jaro_winkler is vectorised and NA-aware", {
    a <- c("JOHN", NA, "MARY")
    b <- c("JON", "MARIE", "MARIE")
    res <- fast.string::jaro_winkler(a, b)
    expect_length(res, 3)
    expect_true(is.na(res[2]))
    expect_false(is.na(res[1]))
    expect_false(is.na(res[3]))
})

test_that("jaro_winkler p=0 (pure Jaro) differs from p=0.1 when there's a common prefix", {
    jw_default <- fast.string::jaro_winkler("SMITH", "SMYTH", p = 0.1)
    jw_noprefix <- fast.string::jaro_winkler("SMITH", "SMYTH", p = 0)
    expect_gt(jw_default, jw_noprefix)
})

test_that("jaro_winkler errors on mismatched lengths or non-character input", {
    expect_error(fast.string::jaro_winkler(c("a", "b"), "x"), "same length")
    expect_error(fast.string::jaro_winkler(1, "x"), "character vectors")
})

test_that("jaro_winkler_matrix matches pairwise jaro_winkler element-by-element", {
    a <- c("JOHN SMITH", "MARY JONES")
    b <- c("JON SMYTH", "MARIE JONES", "JOHN SMITH")
    m <- fast.string::jaro_winkler_matrix(a, b)

    expect_identical(dim(m), c(2L, 3L))
    for (i in seq_along(a)) {
        for (j in seq_along(b)) {
            expect_equal(m[i, j], fast.string::jaro_winkler(a[i], b[j]), tolerance = 1e-10)
        }
    }
})

test_that("jaro_winkler_matrix NA propagates per cell", {
    m <- fast.string::jaro_winkler_matrix(c("A", NA), c("A", "B"))
    expect_false(is.na(m[1, 1]))
    expect_true(is.na(m[2, 1]))
    expect_true(is.na(m[2, 2]))
})

test_that("jaro_winkler_matrix errors on non-character input", {
    expect_error(fast.string::jaro_winkler_matrix(1, "x"), "character vectors")
})
