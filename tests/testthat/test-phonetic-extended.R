test_that("refined_soundex matches Apache Commons reference behavior", {
    expect_identical(
        fast.string::refined_soundex(
            c("Robert", "Rupert", "Ashcraft", "Pfister", "Tymczak", "Honeyman")
        ),
        c("R901096", "R901096", "A03039026", "P1203609", "T6083503", "H080808")
    )
    expect_identical(fast.string::refined_soundex(c("A", "AE", "AB")),
                     c("A0", "A0", "A01"))
})

test_that("cologne matches standard German examples", {
    expect_identical(
        fast.string::cologne(
            c("Müller-Lüdenscheidt", "Wikipedia", "Breschnew")
        ),
        c("65752682", "3412", "17863")
    )
    expect_identical(
        fast.string::cologne(c("Meier", "Mayr", "Maier", "Meyer")),
        rep("67", 4L)
    )
    expect_identical(fast.string::cologne("Honeyman"), "0666")
    expect_identical(fast.string::cologne(c("BHB", "HCL")), c("11", "45"))
})

test_that("extended phonetic encoders preserve names and missing behavior", {
    x <- c(valid = "Robert", missing = NA_character_, empty = "", digits = "123")
    refined <- fast.string::refined_soundex(x)
    cologne <- fast.string::cologne(x)
    expect_identical(names(refined), names(x))
    expect_identical(names(cologne), names(x))
    expect_false(is.na(refined[[1L]]) || is.na(cologne[[1L]]))
    expect_true(all(is.na(refined[-1L])))
    expect_true(all(is.na(cologne[-1L])))
    expect_identical(fast.string::refined_soundex(factor("Robert")), "R901096")
})

test_that("extended phonetic encoders survive parallel dispatch", {
    x <- rep(c("Müller", "Wikipedia", "Robert", NA_character_), 500L)
    expect_length(fast.string::refined_soundex(x), length(x))
    expect_length(fast.string::cologne(x), length(x))
    expect_identical(is.na(fast.string::refined_soundex(x)), is.na(x))
    expect_identical(is.na(fast.string::cologne(x)), is.na(x))
})
