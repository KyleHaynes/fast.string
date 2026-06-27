test_that("jaro_winkler_tokens rescues reordered multi-token names", {
    reordered <- fast.string::jaro_winkler_tokens("Kyle John Haynes", "John Kylie Haynes")
    plain <- fast.string::jaro_winkler("Kyle John Haynes", "John Kylie Haynes")
    expect_gt(reordered, plain)
    expect_gt(reordered, 0.95)
})

test_that("jaro_winkler_tokens rescues punctuation/spacing token splits", {
    res <- fast.string::jaro_winkler_tokens(
        c("OBrien", "OBrien", "O Brien"),
        c("O Brien", "O'Brien", "O'Brien")
    )
    expect_equal(res, c(1, 1, 1))
})

test_that("jaro_winkler_tokens degenerates to jaro_winkler for single tokens", {
    expect_equal(
        fast.string::jaro_winkler_tokens("SMITH", "SMYTH"),
        fast.string::jaro_winkler("SMITH", "SMYTH")
    )
})

test_that("jaro_winkler_tokens penalises missing/extra tokens", {
    score <- fast.string::jaro_winkler_tokens("John Smith", "John Smith Jones")
    expect_lt(score, 1.0)
    expect_gt(score, 0.5)
})

test_that("jaro_winkler_tokens is case-sensitive by default and can be relaxed", {
    res <- fast.string::jaro_winkler_tokens("john smith", "JOHN SMITH")
    expect_lt(res, 1.0)
    expect_equal(fast.string::jaro_winkler_tokens("john smith", "JOHN SMITH", ignore_case = TRUE), 1.0)
})

test_that("jaro_winkler_tokens is NA-aware and vectorised", {
    res <- fast.string::jaro_winkler_tokens(c("JOHN SMITH", NA), c("JON SMYTH", "MARY"))
    expect_length(res, 2)
    expect_true(is.na(res[2]))
    expect_false(is.na(res[1]))
})

test_that("jaro_winkler_tokens handles empty strings", {
    expect_equal(fast.string::jaro_winkler_tokens("", ""), 1.0)
    expect_equal(fast.string::jaro_winkler_tokens("", "A"), 0.0)
})

test_that("jaro_winkler_tokens errors on mismatched lengths or non-character input", {
    expect_error(fast.string::jaro_winkler_tokens(c("a", "b"), "x"), "same length")
    expect_error(fast.string::jaro_winkler_tokens(1, "x"), "character vectors")
})

test_that("extra_penalty = 0 ignores a stray extra token entirely", {
    default_score <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle")
    ignored_score <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                                                        extra_penalty = 0)
    expect_gt(ignored_score, default_score)
    expect_gt(ignored_score, 0.95)
})

test_that("extra_penalty applies a per-token subtractive discount", {
    light <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                                                extra_penalty = 0.1)
    none  <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                                                extra_penalty = 0)
    heavy <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                                                extra_penalty = 1)
    expect_lt(light, none)
    expect_lt(heavy, light)
    expect_gte(heavy, 0)
})

test_that("extra_penalty has no effect when token counts already match", {
    a <- "Kyle John Haynes"; b <- "John Kylie Haynes"
    expect_equal(fast.string::jaro_winkler_tokens(a, b),
                 fast.string::jaro_winkler_tokens(a, b, extra_penalty = 0))
    expect_equal(fast.string::jaro_winkler_tokens(a, b),
                 fast.string::jaro_winkler_tokens(a, b, extra_penalty = 1))
})

test_that("jaro_winkler_tokens validates extra_penalty", {
    expect_error(fast.string::jaro_winkler_tokens("a", "b", extra_penalty = -1), "non-negative")
    expect_error(fast.string::jaro_winkler_tokens("a", "b", extra_penalty = c(0, 1)), "non-negative")
    expect_error(fast.string::jaro_winkler_tokens("a", "b", extra_penalty = "x"), "non-negative")
})

test_that("contractions rescues a non-adjacent split-vs-joined token pair", {
    # "JOHN" sits between "KYLE" and "HAYNES" in the original, so this case
    # is NOT covered by the collapsed (whole-string) framing alone.
    without <- fast.string::jaro_winkler_tokens("KYLEHAYNES JOHN", "KYLE JOHN HAYNES")
    with <- fast.string::jaro_winkler_tokens("KYLEHAYNES JOHN", "KYLE JOHN HAYNES",
                                               contractions = TRUE)
    expect_lt(without, 1.0)
    expect_equal(with, 1.0)
})

test_that("contractions rescues an adjacent split-vs-joined token pair", {
    expect_equal(
        fast.string::jaro_winkler_tokens("KYLEJOHN HAYNES", "KYLE JOHN HAYNES",
                                          contractions = TRUE),
        1.0
    )
})

test_that("contractions only matches tokens in their original left-to-right order", {
    # "HAYNESKYLE" is KYLE+HAYNES reversed, which must NOT be treated as a
    # valid contraction of "KYLE ... HAYNES".
    res <- fast.string::jaro_winkler_tokens("HAYNESKYLE JOHN", "KYLE JOHN HAYNES",
                                              contractions = TRUE)
    expect_lt(res, 1.0)
})

test_that("contractions does not change scores when no merge is beneficial", {
    pairs <- list(
        c("Kyle John Haynes", "John Kylie Haynes"),
        c("SMITH", "SMYTH"),
        c("John Smith", "John Smith Jones"),
        c("", ""), c("", "A")
    )
    for (pr in pairs) {
        expect_equal(
            fast.string::jaro_winkler_tokens(pr[1], pr[2], contractions = FALSE),
            fast.string::jaro_winkler_tokens(pr[1], pr[2], contractions = TRUE)
        )
    }
})

test_that("contractions composes with extra_penalty and is NA-aware/vectorised", {
    res <- fast.string::jaro_winkler_tokens(
        c("KYLEHAYNES JOHN", NA, "John Smith"),
        c("KYLE JOHN HAYNES", "x", "John Smith"),
        contractions = TRUE
    )
    expect_equal(res, c(1, NA, 1))

    score <- fast.string::jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
                                                extra_penalty = 0, contractions = TRUE)
    expect_gt(score, 0.95)
})
