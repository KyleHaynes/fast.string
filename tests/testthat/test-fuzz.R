# Reference values come from the Python rapidfuzz package (3.14.6), written
# by tools/gen_rapidfuzz_fixtures.py. Strings are stored as dot-separated
# hexadecimal code points to keep the file ASCII.

decode_points <- function(x) {
    vapply(x, function(s) {
        if (!nzchar(s)) return("")
        intToUtf8(strtoi(strsplit(s, ".", fixed = TRUE)[[1L]], 16L))
    }, "", USE.NAMES = FALSE)
}

rapidfuzz_fixture <- function() {
    d <- utils::read.csv(
        test_path("fixtures", "rapidfuzz_scores.csv"),
        colClasses = "character", na.strings = character()
    )
    for (column in c("a", "b", "a_processed", "b_processed"))
        d[[column]] <- decode_points(d[[column]])
    d
}

scorer_functions <- list(
    ratio = fast.string::fuzz_ratio,
    partial_ratio = fast.string::fuzz_partial_ratio,
    token_sort_ratio = fast.string::fuzz_token_sort_ratio,
    token_set_ratio = fast.string::fuzz_token_set_ratio,
    token_ratio = fast.string::fuzz_token_ratio,
    partial_token_sort_ratio = fast.string::fuzz_partial_token_sort_ratio,
    partial_token_set_ratio = fast.string::fuzz_partial_token_set_ratio,
    partial_token_ratio = fast.string::fuzz_partial_token_ratio,
    wratio = fast.string::fuzz_wratio,
    qratio = fast.string::fuzz_qratio
)

test_that("every scorer matches rapidfuzz on the reference pairs", {
    d <- rapidfuzz_fixture()
    for (name in names(scorer_functions)) {
        fn <- scorer_functions[[name]]
        expect_equal(
            fn(d$a, d$b, full_process = FALSE, nthreads = 1),
            as.numeric(d[[paste0(name, "_raw")]]),
            tolerance = 1e-12, label = paste(name, "(raw)")
        )
        expect_equal(
            fn(d$a, d$b, full_process = TRUE, nthreads = 1),
            as.numeric(d[[paste0(name, "_proc")]]),
            tolerance = 1e-12, label = paste(name, "(default_process)")
        )
        expect_equal(
            fn(d$a, d$b, full_process = FALSE, score_cutoff = 60, nthreads = 1),
            as.numeric(d[[paste0(name, "_cut60")]]),
            tolerance = 1e-12, label = paste(name, "(score_cutoff = 60)")
        )
    }
})

test_that("multi-threaded scoring gives the serial results", {
    d <- rapidfuzz_fixture()
    a <- rep(d$a, 8L)
    b <- rep(d$b, 8L)
    for (name in names(scorer_functions)) {
        fn <- scorer_functions[[name]]
        expect_identical(fn(a, b, nthreads = 4), fn(a, b, nthreads = 1),
                         label = name)
    }
})

test_that("fuzz_partial_ratio_alignment matches rapidfuzz", {
    d <- rapidfuzz_fixture()
    for (mode in c("raw", "proc")) {
        got <- fast.string::fuzz_partial_ratio_alignment(
            d$a, d$b, full_process = mode == "proc", nthreads = 1
        )
        column <- function(name) d[[paste0(name, "_", mode)]]
        expect_equal(got$score, as.numeric(column("align_score")),
                     tolerance = 1e-12)
        # rapidfuzz positions are 0-based and end-exclusive.
        expect_identical(got$src_start, as.integer(column("src_start")) + 1L)
        expect_identical(got$src_end, as.integer(column("src_end")))
        expect_identical(got$dest_start, as.integer(column("dest_start")) + 1L)
        expect_identical(got$dest_end, as.integer(column("dest_end")))
    }
})

test_that("the alignment reproduces the partial ratio through substr()", {
    a <- c("a certain string", "fuzzy wuzzy was a bear", "xaby", "")
    b <- c("cetain", "wuzzy fuzzy", "ab", "")
    al <- fast.string::fuzz_partial_ratio_alignment(a, b, full_process = FALSE)
    # rapidfuzz: src_start 2 and 1 (0-based).
    expect_identical(al$src_start[c(1L, 3L)], c(3L, 2L))
    expect_equal(
        fast.string::fuzz_ratio(substr(a, al$src_start, al$src_end),
                                substr(b, al$dest_start, al$dest_end),
                                full_process = FALSE),
        fast.string::fuzz_partial_ratio(a, b, full_process = FALSE)
    )
})

test_that("alignments below the cutoff, or of NA, have no positions", {
    al <- fast.string::fuzz_partial_ratio_alignment(
        c("abc", NA, "abc"), c("xyz", "abc", "abd"),
        full_process = FALSE, score_cutoff = 50
    )
    expect_identical(al$score[1:2], c(0, NA))
    expect_true(all(is.na(al$src_start[1:2])))
    expect_true(all(is.na(al$dest_end[1:2])))
    # "abc" against the window "ab" of "abd": 2 * 2 / 5.
    expect_identical(al$score[3], 80)
    expect_identical(c(al$dest_start[3], al$dest_end[3]), c(1L, 2L))
})

test_that("fuzz_default_process matches rapidfuzz.utils.default_process", {
    d <- rapidfuzz_fixture()
    expect_identical(fast.string::fuzz_default_process(d$a), d$a_processed)
    expect_identical(fast.string::fuzz_default_process(d$b), d$b_processed)
})

test_that("fuzz_default_process keeps names and NA and maps all of Unicode", {
    x <- c(first = "Hello, World!", second = "  O'Brien_Smith  ", third = NA)
    expect_identical(
        fast.string::fuzz_default_process(x),
        c(first = "hello  world", second = "o brien smith", third = NA)
    )
    # Dotted capital I lowercases to "i"; U+023A to U+2C65; Greek is kept.
    expect_identical(
        fast.string::fuzz_default_process(
            intToUtf8(c(0x130, 0x23a, 0x20, 0x3a3, 0x391, 0xa0, 0x5f, 0x31))
        ),
        intToUtf8(c(0x69, 0x2c65, 0x20, 0x3c3, 0x3b1, 0x20, 0x20, 0x31))
    )
    latin1 <- "caf\xe9"
    Encoding(latin1) <- "latin1"
    expect_identical(fast.string::fuzz_default_process(toupper(latin1)),
                     intToUtf8(c(0x63, 0x61, 0x66, 0xe9)))
    expect_identical(fast.string::fuzz_default_process(character()), character())
})

test_that("full_process applies default_process before scoring", {
    a <- c("Hello, World!", "O'Brien", "!!!")
    b <- c("hello world", "OBRIEN", "???")
    for (fn in scorer_functions) {
        expect_identical(
            fn(a, b),
            fn(fast.string::fuzz_default_process(a),
               fast.string::fuzz_default_process(b), full_process = FALSE)
        )
    }
    # Only a literal TRUE turns processing on.
    expect_identical(fast.string::fuzz_ratio("Hello!", "hello", full_process = NA),
                     fast.string::fuzz_ratio("Hello!", "hello", full_process = FALSE))
})

test_that("headline values match RapidFuzz's documentation", {
    expect_equal(
        fast.string::fuzz_ratio("this is a test", "this is a test!",
                                full_process = FALSE),
        96.55172413793103
    )
    expect_identical(
        fast.string::fuzz_token_set_ratio("fuzzy was a bear but not a dog",
                                          "fuzzy was a bear"),
        100
    )
    expect_equal(
        fast.string::fuzz_token_set_ratio("fuzzy was a bear but not a dog",
                                          "fuzzy was a bear but not a cat"),
        92.3076923076923
    )
    expect_equal(
        fast.string::fuzz_token_sort_ratio("fuzzy was a bear",
                                           "fuzzy fuzzy was a bear",
                                           full_process = FALSE),
        84.21052631578947
    )
})

test_that("empty strings follow RapidFuzz's rules", {
    empty <- vapply(scorer_functions,
                    function(fn) fn("", "", full_process = FALSE), 0)
    expect_identical(empty, c(
        ratio = 100, partial_ratio = 100, token_sort_ratio = 100,
        token_set_ratio = 0, token_ratio = 100, partial_token_sort_ratio = 100,
        partial_token_set_ratio = 0, partial_token_ratio = 100, wratio = 0,
        qratio = 0
    ))
    one_empty <- vapply(scorer_functions,
                        function(fn) fn("", "abc", full_process = FALSE), 0)
    expect_true(all(one_empty == 0))
})

test_that("a no-break space only splits words beside wider characters", {
    nbsp <- intToUtf8(0xa0)
    wide <- intToUtf8(0x100)
    expect_equal(
        fast.string::fuzz_token_sort_ratio(paste0("a", nbsp, "b"), "b a",
                                           full_process = FALSE),
        200 / 3
    )
    expect_identical(
        fast.string::fuzz_token_sort_ratio(paste0("a", nbsp, "b", wide),
                                           paste0("b", wide, " a"),
                                           full_process = FALSE),
        100
    )
})

test_that("score_cutoff zeroes lower scores and is validated", {
    a <- c("MARTHA", "abc", "fuzzy was a bear")
    b <- c("MARHTA", "xyz", "fuzzy fuzzy bear")
    for (fn in scorer_functions) {
        exact <- fn(a, b)
        expect_identical(fn(a, b, score_cutoff = 70),
                         ifelse(exact >= 70, exact, 0))
        expect_identical(fn(a, b, score_cutoff = 100),
                         ifelse(exact >= 100, exact, 0))
    }
    expect_error(fast.string::fuzz_ratio("a", "b", score_cutoff = 101),
                 "between 0 and 100")
    expect_error(fast.string::fuzz_ratio("a", "b", score_cutoff = NA),
                 "between 0 and 100")
    expect_error(fast.string::fuzz_ratio("a", "b", score_cutoff = c(1, 2)),
                 "between 0 and 100")
})

test_that("scorers are vectorised and NA-aware", {
    a <- c("hello world", NA, "test", "x")
    b <- c("hello world!", "x", NA, "x")
    for (fn in scorer_functions) {
        out <- fn(a, b)
        expect_length(out, 4L)
        expect_identical(is.na(out), c(FALSE, TRUE, TRUE, FALSE))
    }
    expect_identical(fast.string::fuzz_ratio(character(), character()),
                     numeric())
})

test_that("long strings use the multi-word LCS and exhaustive partial search", {
    set.seed(42)
    long <- paste(sample(letters, 300, TRUE), collapse = "")
    shifted <- paste0(substr(long, 41, 300), substr(long, 1, 40))
    expect_identical(
        fast.string::fuzz_partial_ratio(substr(long, 101, 230), long,
                                        full_process = FALSE),
        100
    )
    # 260 of 300 characters line up after the rotation.
    expect_equal(
        fast.string::fuzz_ratio(long, shifted, full_process = FALSE),
        100 * 2 * 260 / 600,
        tolerance = 0.02
    )
})

test_that("latin1 input is compared as text", {
    latin1 <- "caf\xe9"
    Encoding(latin1) <- "latin1"
    utf8 <- intToUtf8(c(0x63, 0x61, 0x66, 0xe9))
    for (fn in scorer_functions) {
        expect_identical(fn(latin1, "cafe", full_process = FALSE),
                         fn(utf8, "cafe", full_process = FALSE))
    }
    expect_equal(fast.string::fuzz_ratio(utf8, "cafe", full_process = FALSE), 75)
})

test_that("scorers reject bad input", {
    expect_error(fast.string::fuzz_ratio(c("a", "b"), "x"), "same length")
    expect_error(fast.string::fuzz_ratio(1, "x"), "character vectors")
    expect_error(fast.string::fuzz_wratio("a", "b", nthreads = 0), "nthreads")
    expect_error(fast.string::fuzz_default_process(1), "character vector")
})
