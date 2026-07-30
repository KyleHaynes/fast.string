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
