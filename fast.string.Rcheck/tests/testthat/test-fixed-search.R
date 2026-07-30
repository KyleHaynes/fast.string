test_that("prepared fixed search matches base across needle sizes", {
    x <- c(
        "", "a", "ba", "abc", "xxabcxx",
        paste0(strrep("a", 1024L), "b"),
        paste0(strrep("a", 1024L), "c"),
        NA_character_
    )

    for (pattern in c("", "a", "ab", "abc", "aaaab")) {
        expect_identical(
            fast.string::fgrepl(pattern, x, fixed = TRUE, nthreads = 1L),
            c(base::grepl(pattern, x[-length(x)], fixed = TRUE), NA)
        )
    }
})

test_that("fixed ignore.case search and substitution preserve package behavior", {
    x <- c("Alpha BETA alpha", "nothing", "", NA_character_)

    expect_identical(
        fast.string::fgrepl("ALPHA", x, fixed = TRUE,
                            ignore.case = TRUE, nthreads = 2L),
        c(TRUE, FALSE, FALSE, NA)
    )
    expect_identical(
        fast.string::fgsub("ALPHA", "x", x, fixed = TRUE,
                           ignore.case = TRUE, nthreads = 2L),
        c("x BETA x", "nothing", "", NA)
    )
})

test_that("fixed substitution rejects zero-length patterns", {
    expect_error(
        fast.string::fsub("", "x", "abc", fixed = TRUE),
        "zero-length pattern"
    )
    expect_error(
        fast.string::fgsub("", "x", "abc", fixed = TRUE),
        "zero-length pattern"
    )
    expect_error(
        fast.string::gsub_all(c("a", ""), "x", "abc", fixed = TRUE),
        "zero-length pattern"
    )
})

test_that("character ALTREP inputs are safe in parallel workers", {
    x <- as.character(seq_len(12000L))

    expect_identical(
        fast.string::fgrepl("999", x, fixed = TRUE, nthreads = 2L),
        base::grepl("999", x, fixed = TRUE)
    )
    expect_identical(
        fast.string::fgsub("999", "x", x, fixed = TRUE, nthreads = 2L),
        base::gsub("999", "x", x, fixed = TRUE)
    )
    expect_identical(
        fast.string::fgrepl("9+$", x, nthreads = 2L),
        base::grepl("9+$", x, perl = TRUE)
    )
})

test_that("deferred character ALTREP inputs survive repeated calls and gc", {
    cases <- expand.grid(
        operation = c("fixed grep", "regex grep", "fixed substitution"),
        nthreads = c(1L, 2L, 4L),
        stringsAsFactors = FALSE
    )

    for (iteration in seq_len(100L)) {
        case <- cases[(iteration - 1L) %% nrow(cases) + 1L, ]
        values <- seq_len(20000L) + iteration
        x <- as.character(values)
        reference <- as.character(values)

        if (case$operation == "fixed grep") {
            actual <- fast.string::fgrepl(
                "999", x, fixed = TRUE, nthreads = case$nthreads
            )
            expected <- base::grepl("999", reference, fixed = TRUE)
        } else if (case$operation == "regex grep") {
            actual <- fast.string::fgrepl(
                "9+$", x, nthreads = case$nthreads
            )
            expected <- base::grepl("9+$", reference, perl = TRUE)
        } else {
            actual <- fast.string::fgsub(
                "999", "x", x, fixed = TRUE, nthreads = case$nthreads
            )
            expected <- base::gsub("999", "x", reference, fixed = TRUE)
        }

        expect_identical(actual, expected)
        if (iteration %% 5L == 0L)
            gc()
    }
})

test_that("gsub_all fixed modes use prepared searches", {
    x <- c("a", "A-a", "none", NA_character_)

    expect_identical(
        fast.string::gsub_all(
            c("a", "x"), c("x", "y"), x,
            fixed = TRUE, ignore.case = TRUE, sequential = TRUE,
            nthreads = 2L
        ),
        c("y", "y-y", "none", NA_character_)
    )
    expect_identical(
        fast.string::gsub_all(
            c("a", "x"), c("x", "y"), x,
            fixed = TRUE, ignore.case = TRUE, sequential = FALSE,
            nthreads = 2L
        ),
        c("x", "x-x", "none", NA_character_)
    )
})

test_that("nthreads is validated without changing global thread options", {
    expect_error(
        fast.string::fgrepl("a", "a", nthreads = 0L),
        "positive integer"
    )
    expect_error(
        fast.string::fgsub("a", "b", "a", nthreads = c(1L, 2L)),
        "positive integer"
    )
})
