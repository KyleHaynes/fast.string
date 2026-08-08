base_count <- function(pattern, x, ...) {
    hits <- gregexpr(pattern, x, ...)
    vapply(hits, function(hit) {
        if (length(hit) == 1L && is.na(hit)) return(NA_integer_)
        if (length(hit) == 1L && hit[[1L]] < 0L) return(0L)
        length(hit)
    }, integer(1L))
}

test_that("fcount matches base for fixed and regular-expression searches", {
    x <- c("aaaa", "abc 123 456", "none", "", NA_character_)
    expect_identical(
        fast.string::fcount("aa", x, fixed = TRUE),
        base_count("aa", x, fixed = TRUE)
    )
    expect_identical(
        fast.string::fcount("[0-9]+", x),
        base_count("[0-9]+", x)
    )
})

test_that("fcount handles case folding, names, and empty patterns", {
    x <- c(first = "AaA", second = "bbb", missing = NA_character_)
    expect_identical(
        fast.string::fcount("a", x, fixed = TRUE, ignore.case = TRUE),
        c(first = 3L, second = 0L, missing = NA_integer_)
    )
    utf8 <- c("éé", "abc", "", NA_character_)
    expect_identical(
        unname(fast.string::fcount("", utf8, fixed = TRUE)),
        unname(base_count("", utf8, fixed = TRUE))
    )
})

test_that("fcount matches base empty-match progression", {
    x <- c("aaa", "baab", "bbb", "", NA_character_)
    for (pattern in c("a*", "a?", "^|$", "|a", ".*?", "(?=a)")) {
        expect_identical(
            fast.string::fcount(pattern, x, perl = TRUE),
            base_count(pattern, x, perl = TRUE)
        )
    }
})

test_that("fcount exercises parallel fixed and regex paths", {
    x <- rep(c("alpha beta alpha", "none", NA_character_), 600L)
    expect_identical(
        fast.string::fcount("alpha", x, fixed = TRUE, nthreads = 2L),
        base_count("alpha", x, fixed = TRUE)
    )
    expect_identical(
        fast.string::fcount("a[a-z]+", x, perl = TRUE, nthreads = 2L),
        base_count("a[a-z]+", x, perl = TRUE)
    )
})

test_that("fcount validates inputs", {
    expect_error(fast.string::fcount(c("a", "b"), "a"), "single")
    expect_error(fast.string::fcount(NA_character_, "a"), "non-missing")
    expect_error(fast.string::fcount("a", 1), "character vector")
})
