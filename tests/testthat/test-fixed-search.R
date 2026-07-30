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

test_that("prepared fixed search and substitution agree with base on mixed corpora", {
    set.seed(20260729)
    alphabet <- c(letters[1:6], LETTERS[1:6], "-", "_", " ")
    x <- replicate(
        6000L,
        paste0(sample(alphabet, 48L, replace = TRUE), collapse = "")
    )
    x[c(17L, 4999L)] <- NA_character_
    x[33L] <- ""
    names(x) <- sprintf("row-%04d", seq_along(x))

    for (ignore_case in c(FALSE, TRUE)) {
        for (pattern in c("a", "ab", "aba", "aaaaab", "ABCDEF")) {
            if (ignore_case) {
                literal_pattern <- paste0("\\Q", pattern, "\\E")
                expected_grepl <- base::grepl(
                    literal_pattern, unname(x), ignore.case = TRUE,
                    perl = TRUE, useBytes = TRUE
                )
                expected_sub <- base::sub(
                    literal_pattern, "<x>", x, ignore.case = TRUE,
                    perl = TRUE, useBytes = TRUE
                )
                expected_gsub <- base::gsub(
                    literal_pattern, "<x>", x, ignore.case = TRUE,
                    perl = TRUE, useBytes = TRUE
                )
            } else {
                expected_grepl <- base::grepl(
                    pattern, unname(x), fixed = TRUE
                )
                expected_sub <- base::sub(
                    pattern, "<x>", x, fixed = TRUE
                )
                expected_gsub <- base::gsub(
                    pattern, "<x>", x, fixed = TRUE
                )
            }
            expected_grepl[is.na(x)] <- NA
            expected_sub <- unname(expected_sub)
            expected_gsub <- unname(expected_gsub)

            expect_identical(
                fast.string::fgrepl(
                    pattern, x, fixed = TRUE,
                    ignore.case = ignore_case, nthreads = 4L
                ),
                expected_grepl
            )
            expect_identical(
                fast.string::fsub(
                    pattern, "<x>", x, fixed = TRUE,
                    ignore.case = ignore_case, nthreads = 4L
                ),
                expected_sub
            )
            expect_identical(
                fast.string::fgsub(
                    pattern, "<x>", x, fixed = TRUE,
                    ignore.case = ignore_case, nthreads = 4L
                ),
                expected_gsub
            )
        }
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

test_that("deferred ALTREP snapshots protect similarity and utility workers", {
    x <- as.character(seq_len(12000L))

    for (nthreads in c(1L, 2L, 4L)) {
        expect_identical(
            fast.string::jaro_winkler(x, x, nthreads = nthreads),
            rep(1, length(x))
        )
        expect_identical(
            fast.string::jaro_winkler_tokens(
                x, x, strip = NULL, nthreads = nthreads
            ),
            rep(1, length(x))
        )
        expect_identical(
            fast.string::levenshtein(x, x, nthreads = nthreads),
            rep(0, length(x))
        )
        expect_identical(
            fast.string::jaccard_index(x, x, q = 2L, nthreads = nthreads),
            rep(1, length(x))
        )
        expect_identical(
            fast.string::fuzz_ratio(x, x, nthreads = nthreads),
            rep(100, length(x))
        )
        gc()
    }

    expect_identical(
        fast.string::fsubstr(x, 1L, 2L),
        base::substr(as.character(seq_len(12000L)), 1L, 2L)
    )
    expect_identical(
        fast.string::soundex(x),
        rep(NA_character_, length(x))
    )
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
