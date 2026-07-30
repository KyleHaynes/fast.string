# Reference values are Apache Commons Codec's own published DoubleMetaphone
# unit-test vectors (DoubleMetaphoneTest.java, testDoubleMetaphone()) -- the
# de facto reference implementation this package's algorithm is ported from.

test_that("double_metaphone primary codes match the Commons Codec reference", {
    words    <- c("testing", "The", "quick", "brown", "fox", "jumped", "over",
                  "the", "lazy", "dogs", "MacCafferey", "Stephan", "Kuczewski",
                  "McClelland", "san jose", "xenophobia")
    expected <- c("TSTN", "0", "KK", "PRN", "FKS", "JMPT", "AFR",
                  "0", "LS", "TKS", "MKFR", "STFN", "KSSK",
                  "MKLL", "SNHS", "SNFP")
    expect_identical(fast.string::double_metaphone(words)$primary, expected)
})

test_that("double_metaphone secondary codes match the Commons Codec reference", {
    words    <- c("testing", "The", "quick", "brown", "fox", "jumped", "over",
                  "the", "lazy", "dogs", "MacCafferey", "Stephan", "Kutchefski",
                  "McClelland", "san jose", "xenophobia", "Fokker", "Joqqi",
                  "Hovvi", "Czerny")
    expected <- c("TSTN", "T", "KK", "PRN", "FKS", "AMPT", "AFR",
                  "T", "LS", "TKS", "MKFR", "STFN", "KXFS",
                  "MKLL", "SNHS", "SNFP", "FKR", "AK", "HF", "XRN")
    expect_identical(fast.string::double_metaphone(words)$secondary, expected)
})

test_that("double_metaphone returns a two-column data.frame the same length as x", {
    res <- fast.string::double_metaphone(c("Smith", "Schmidt", "Catherine"))
    expect_s3_class(res, "data.frame")
    expect_identical(names(res), c("primary", "secondary"))
    expect_identical(nrow(res), 3L)
})

test_that("double_metaphone survives the RcppParallel threshold (n >= 10000)", {
    set.seed(1)
    n <- 12000
    x <- replicate(n, paste(sample(LETTERS, sample(3:15, 1), TRUE), collapse = ""))
    res <- fast.string::double_metaphone(x)
    expect_identical(nrow(res), as.integer(n))
    expect_false(anyNA(res$primary))
})

test_that("double_metaphone returns NA for NA input and coerces non-character input", {
    res <- fast.string::double_metaphone(c("Smith", NA))
    expect_false(is.na(res$primary[1]))
    expect_true(is.na(res$primary[2]) && is.na(res$secondary[2]))
    expect_identical(fast.string::double_metaphone(factor("Smith"))$primary, "SM0")
})
