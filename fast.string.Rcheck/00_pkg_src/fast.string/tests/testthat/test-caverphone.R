# Reference values are Apache Commons Codec's own published Caverphone2
# unit-test vectors (Caverphone2Test.java) -- the de facto reference
# implementation this package's algorithm is ported from.

test_that("caverphone matches the Commons Codec reference for grouped names", {
    expect_identical(
        fast.string::caverphone(c("add", "aid", "at", "art", "eat", "hold", "old")),
        rep("AT11111111", 7))
    expect_identical(
        fast.string::caverphone(c("Glen", "Glenn", "Klein", "Kline", "Xylon")),
        rep("KLN1111111", 5))
    expect_identical(
        fast.string::caverphone(c("Dan", "Dawn", "Don", "Tan", "Thin", "Town")),
        rep("TN11111111", 6))
})

test_that("caverphone matches the Commons Codec reference for specific examples", {
    expect_identical(fast.string::caverphone("Peter"), "PTA1111111")
    expect_identical(fast.string::caverphone("Stevenson"), "STFNSN1111")
    expect_identical(fast.string::caverphone("ready"), "RTA1111111")
    expect_identical(fast.string::caverphone("social"), "SSA1111111")
    expect_identical(fast.string::caverphone("able"), "APA1111111")
    expect_identical(fast.string::caverphone("Tedder"), "TTA1111111")
    expect_identical(fast.string::caverphone("Karleen"), "KLN1111111")
    expect_identical(fast.string::caverphone("Dyun"), "TN11111111")
    expect_identical(fast.string::caverphone("mb"), "M111111111")
    expect_identical(fast.string::caverphone("mbmb"), "MPM1111111")
})

test_that("caverphone always returns a 10-character code or NA", {
    res <- fast.string::caverphone(c("A", "Supercalifragilisticexpialidocious", "", "123"))
    expect_true(all(nchar(res) == 10))
})

test_that("caverphone survives the RcppParallel threshold (n >= 10000)", {
    set.seed(1)
    n <- 12000
    x <- replicate(n, paste(sample(LETTERS, sample(3:15, 1), TRUE), collapse = ""))
    res <- fast.string::caverphone(x)
    expect_length(res, n)
    expect_false(anyNA(res))
})

test_that("caverphone returns NA for NA and preserves names / coerces input", {
    expect_true(is.na(fast.string::caverphone(NA_character_)))
    expect_identical(names(fast.string::caverphone(c(a = "Peter"))), "a")
    expect_identical(fast.string::caverphone(factor("Peter")), "PTA1111111")
})
