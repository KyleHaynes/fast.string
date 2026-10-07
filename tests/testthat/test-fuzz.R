test_that("fuzz_ratio matches fuzzywuzzy/difflib reference values", {
    expect_identical(fast.string::fuzz_ratio("this is a test", "this is a test!"), 100)
    expect_identical(fast.string::fuzz_ratio("MARTHA", "MARHTA"), 83)
    expect_identical(fast.string::fuzz_ratio("abc", "xyz"), 0)
    expect_identical(fast.string::fuzz_ratio("", "", full_process = FALSE), 100)
    # With full_process (fuzzywuzzy's QRatio), an empty string scores 0.
    expect_identical(fast.string::fuzz_ratio("", ""), 0)
})

test_that("fuzz_partial_ratio matches fuzzywuzzy reference values", {
    expect_identical(
        fast.string::fuzz_partial_ratio("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear"), 91)
    expect_identical(
        fast.string::fuzz_partial_ratio("fuzzy was a bear", "fuzzy fuzzy bear was a bear"), 69)
})

test_that("fuzz_token_sort_ratio is insensitive to word order", {
    expect_identical(
        fast.string::fuzz_token_sort_ratio("fuzzy was a bear", "bear was a fuzzy"), 100)
    expect_identical(
        fast.string::fuzz_token_sort_ratio(
            "New York Mets vs Atlanta Braves", "Atlanta Braves vs New York Mets"),
        100)
})

test_that("fuzz_token_set_ratio is robust to one side having extra tokens", {
    expect_identical(
        fast.string::fuzz_token_set_ratio("fuzzy was a bear", "fuzzy fuzzy bear was a bear"), 100)
})

test_that("fuzz_* are vectorised and NA-aware", {
    a <- c("hello world", NA, "test")
    b <- c("hello world!", "x", "test")
    expect_identical(fast.string::fuzz_ratio(a, b), c(100, NA, 100))
    expect_true(is.na(fast.string::fuzz_partial_ratio(a, b)[2]))
})

test_that("fuzz_* full_process lowercases and strips punctuation by default", {
    # Each separator becomes its own space, so ", " leaves two (QRatio: 96).
    expect_identical(fast.string::fuzz_ratio("Hello, World!", "hello world"), 96)
    expect_identical(fast.string::fuzz_ratio("Hello World!", "hello world"), 100)
    expect_identical(fast.string::fuzz_ratio("Hello, World!", "hello world", full_process = FALSE), 75)
})

test_that("fuzz_* survive the RcppParallel threshold (n >= 1000)", {
    set.seed(1)
    n <- 1500
    a <- replicate(n, paste(sample(letters, sample(3:15, 1), TRUE), collapse = " "))
    b <- rev(a)
    expect_length(fast.string::fuzz_ratio(a, b), n)
    expect_length(fast.string::fuzz_partial_ratio(a, b), n)
    expect_length(fast.string::fuzz_token_sort_ratio(a, b), n)
    expect_length(fast.string::fuzz_token_set_ratio(a, b), n)
    expect_false(anyNA(fast.string::fuzz_ratio(a, b)))
})

test_that("fuzz_* errors on mismatched lengths or non-character input", {
    expect_error(fast.string::fuzz_ratio(c("a", "b"), "x"), "same length")
    expect_error(fast.string::fuzz_ratio(1, "x"), "character vectors")
})

test_that("full_process matches fuzzywuzzy's preprocessing", {
    a <- c(
        "Hello, World!", "  A---B  ", "123..ABC", "", "!!!", "snake_case",
        "fuzzy\twuzzy\nwas", "caf\u00e9", "\u2019O'Brien", NA_character_
    )
    b <- c(
        "hello world", "a b", "123 abc", "", "???", "snake case",
        "FUZZY WUZZY was", "cafe", "obrien", "x"
    )
    # utils.full_process(force_ascii = TRUE): drop U+0080-U+00FF, one space
    # per non-word character, lowercase, trim.
    reference <- function(x) {
        x <- gsub("(*UTF)[\\x{80}-\\x{ff}]", "", x, perl = TRUE)
        x <- gsub("(*UTF)[^A-Za-z0-9_\\x{100}-\\x{10ffff}]", " ", x,
                  perl = TRUE)
        x <- chartr(paste(LETTERS, collapse = ""),
                    paste(letters, collapse = ""), x)
        sub(" +$", "", sub("^ +", "", x))
    }
    aa <- reference(a)
    bb <- reference(b)
    usable <- !is.na(aa) & !is.na(bb) & nzchar(aa) & nzchar(bb)

    functions <- list(
        fast.string::fuzz_ratio,
        fast.string::fuzz_partial_ratio,
        fast.string::fuzz_token_sort_ratio,
        fast.string::fuzz_token_set_ratio
    )
    for (fn in functions) {
        processed <- fn(a, b, full_process = TRUE, nthreads = 1)
        expect_identical(
            processed[usable],
            fn(aa, bb, full_process = FALSE, nthreads = 1)[usable]
        )
        expect_true(is.na(processed[is.na(a)]))
    }
})

test_that("full_process only runs for literal TRUE", {
    expect_identical(
        fast.string::fuzz_ratio("Hello!", "hello", full_process = NA),
        fast.string::fuzz_ratio("Hello!", "hello", full_process = FALSE)
    )
})

# Expected values below come from fuzzywuzzy 0.18 run on difflib (no
# python-Levenshtein), the reference these ports follow.
test_that("fuzz scores follow fuzzywuzzy's empty-string rules", {
    expect_identical(fast.string::fuzz_token_set_ratio("", "abc", full_process = FALSE), 0)
    expect_identical(fast.string::fuzz_token_set_ratio("!!!", "abc"), 0)
    expect_identical(fast.string::fuzz_token_set_ratio("!!!", "!!!"), 0)
    expect_identical(fast.string::fuzz_token_sort_ratio("!!!", "???"), 100)
})

test_that("fuzz_partial_ratio also scores the end-aligned window", {
    expect_identical(
        fast.string::fuzz_partial_ratio("adaaabBc ac ", "a,B,AA dda",
                                        full_process = FALSE),
        40
    )
})

test_that("full_process keeps underscores and does not collapse separators", {
    expect_identical(fast.string::fuzz_token_set_ratio("fuzzy_bear", "fuzzy bear"), 50)
    # full_process = TRUE makes fuzz_ratio() fuzzywuzzy's QRatio().
    expect_identical(fast.string::fuzz_ratio("Hello, World!", "hello world"), 96)
    expect_identical(fast.string::fuzz_ratio("a, b", "a b"), 86)
})

test_that("non-ASCII text is compared by character", {
    expect_identical(
        fast.string::fuzz_ratio(paste0("caf", intToUtf8(0xe9)), "cafe",
                                full_process = FALSE),
        75
    )
})
