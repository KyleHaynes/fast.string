test_that("fuzz_ratio matches fuzzywuzzy/difflib reference values", {
    expect_identical(fast.string::fuzz_ratio("this is a test", "this is a test!"), 100)
    expect_identical(fast.string::fuzz_ratio("MARTHA", "MARHTA"), 83)
    expect_identical(fast.string::fuzz_ratio("abc", "xyz"), 0)
    expect_identical(fast.string::fuzz_ratio("", ""), 100)
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
    expect_identical(fast.string::fuzz_ratio("Hello, World!", "hello world"), 100)
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
