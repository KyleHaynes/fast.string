test_that("metaphone() matches Apache Commons Codec", {
    # Commons Codec 1.17.1 Metaphone, maxCodeLen 4 and 8.
    x <- c("Thompson", "Knight", "Wright", "Christopher", "Xavier", "Schmidt",
           "Dodge", "Science", "Agnes", "Whitehead", "Peter", "Stevenson")
    expect_identical(metaphone(x),
                     c("0MPS", "NT", "RT", "XRST", "SFR", "SKMT", "TJ", "SNS",
                       "ANS", "WTHT", "PTR", "STFN"))
    expect_identical(metaphone(x, max_length = 8),
                     c("0MPSN", "NT", "RT", "XRSTFR", "SFR", "SKMTT", "TJ", "SNS",
                       "ANS", "WTHT", "PTR", "STFNSN"))
    expect_identical(metaphone(c(a = "a", b = NA, c = "123", d = intToUtf8(c(0xc9, 0x6d)))),
                     c(a = "A", b = NA, c = NA, d = "EM"))
    expect_error(metaphone("x", max_length = 0), "max_length")
})

test_that("mra_encode() and mra_compare() follow the published rules", {
    # jellyfish's match_rating_codex() / match_rating_comparison() test data.
    expect_identical(
        mra_encode(c("Byrne", "Boern", "Smith", "Smyth", "Catherine", "Kathryn",
                     "Kathrynoglin", "William", NA, "123")),
        c("BYRN", "BRN", "SMTH", "SMYTH", "CTHRN", "KTHRYN", "KTHGLN", "WLM", NA, NA))
    expect_identical(
        mra_compare(c("Byrne", "Smith", "Catherine", "Michael", "Tim", "Ed", NA),
                    c("Boern", "Smyth", "Kathryn", "Mike", "Timothy", "Ad", "x")),
        c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, NA))
    # a codex encodes to itself, so codes can be compared too
    expect_identical(mra_encode(mra_encode(c("Catherine", "Stevenson"))),
                     mra_encode(c("Catherine", "Stevenson")))
    expect_error(mra_compare("a", c("a", "b")), "same length")
})

test_that("onca() is Soundex of the uncapped NYSIIS key", {
    # Commons Codec Soundex of Nysiis(strict = false).
    x <- c("Thompson", "Knight", "Wright", "Christopher", "Xavier", "Schmidt",
           "Dodge", "Science", "Agnes", "Whitehead", "Peter", "Stevenson", "Aess")
    expect_identical(onca(x),
                     c("T512", "N230", "W623", "C623", "X160", "S530", "D320",
                       "S520", "A250", "W330", "P360", "S315", ""))
})

test_that("statcan(), lein(), phonex() and roger_root() give phonics' codes", {
    x <- c("William", "Peter", "Peady", "Stevenson", "Stephenson", "Knight", "Schmidt")
    expect_identical(statcan(x), c("WLM", "PTR", "PD", "STVN", "STPH", "KNGH", "SCHM"))
    expect_identical(lein(x), c("W320", "P130", "P100", "S142", "S142", "K251", "S521"))
    expect_identical(phonex(x), c("W450", "B360", "B300", "S315", "S315", "N230", "S253"))
    expect_identical(roger_root(x), c("45300", "09140", "09100", "00182", "00182", "02710", "06310"))
    expect_identical(statcan(c(a = NA, b = "", c = "--")),
                     c(a = NA_character_, b = NA_character_, c = NA_character_))
})

test_that("the phonics-derived codes agree with phonics on letters-only names", {
    skip_if_not_installed("phonics")
    set.seed(5)
    syl <- c("a", "e", "i", "o", "u", "y", "ch", "sch", "ph", "th", "kn", "wr", "wh",
             "tsch", "ss", "ll", "m", "n", "r", "l", "d", "t", "s", "z", "g", "k", "b", "v")
    x <- unique(vapply(1:3000, function(i) {
        s <- paste(sample(syl, sample(1:5, 1), TRUE), collapse = "")
        paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
    }, ""))
    expect_identical(statcan(x), phonics::statcan(x))
    expect_identical(lein(x), phonics::lein(x))
    expect_identical(phonex(x), phonics::phonex(x))
    expect_identical(phonex(x, 6), phonics::phonex(x, 6))
    expect_identical(roger_root(x), phonics::rogerroot(x))
})

test_that("the new phonetic codes fold accents and survive parallel dispatch", {
    e <- intToUtf8(0xc9)
    expect_identical(statcan(paste0(e, "mile")), statcan("Emile"))
    x <- rep(c("Stevenson", NA, "Knight", ""), 3000)
    for (f in list(metaphone, mra_encode, statcan, lein, phonex, roger_root, onca))
        expect_identical(f(x, nthreads = 1), f(x))
})
