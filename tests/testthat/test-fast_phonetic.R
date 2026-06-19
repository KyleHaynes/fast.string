test_that("soundex matches classic reference codes", {
    expect_identical(fgrepl::soundex("Robert"),   "R163")
    expect_identical(fgrepl::soundex("Rupert"),   "R163")
    expect_identical(fgrepl::soundex("Ashcraft"), "A261") # H/W transparency rule
    expect_identical(fgrepl::soundex("Pfister"),  "P236")
    expect_identical(fgrepl::soundex("Tymczak"),  "T522")
    expect_identical(fgrepl::soundex("Honeyman"), "H555")
})

test_that("soundex is vectorised and case-insensitive", {
    expect_identical(fgrepl::soundex(c("Robert", "ROBERT", "robert")), rep("R163", 3))
})

test_that("soundex is always exactly 4 characters or NA", {
    x <- c("A", "Robert", "Supercalifragilisticexpialidocious")
    res <- fgrepl::soundex(x)
    expect_true(all(nchar(res) == 4))
})

test_that("soundex returns NA for NA, empty, and non-alphabetic input", {
    expect_true(is.na(fgrepl::soundex(NA_character_)))
    expect_true(is.na(fgrepl::soundex("")))
    expect_true(is.na(fgrepl::soundex("12345")))
    expect_true(is.na(fgrepl::soundex("---")))
})

test_that("soundex preserves names and coerces non-character input", {
    expect_identical(names(fgrepl::soundex(c(a = "Robert"))), "a")
    expect_identical(fgrepl::soundex(factor("Robert")), "R163")
})

test_that("nysiis matches hand-traced reference codes for the implemented ruleset", {
    expect_identical(fgrepl::nysiis("MACDONALD"), "MCDANA")
    expect_identical(fgrepl::nysiis("KNIGHT"),    "NAGT")
    expect_identical(fgrepl::nysiis("PHILBERT"),  "FALBAD")
    expect_identical(fgrepl::nysiis("SCHMIDT"),   "SNAD")
    expect_identical(fgrepl::nysiis("WATSON"),    "WATSAN")
    expect_identical(fgrepl::nysiis("BROWNING"),  "BRANAN")
})

test_that("nysiis is capped at 6 characters", {
    res <- fgrepl::nysiis(c("A", "Supercalifragilisticexpialidocious"))
    expect_true(all(nchar(res) <= 6))
})

test_that("nysiis returns NA for NA, empty, and non-alphabetic input", {
    expect_true(is.na(fgrepl::nysiis(NA_character_)))
    expect_true(is.na(fgrepl::nysiis("")))
    expect_true(is.na(fgrepl::nysiis("12345")))
    expect_true(is.na(fgrepl::nysiis("---")))
})

test_that("nysiis preserves names and coerces non-character input", {
    expect_identical(names(fgrepl::nysiis(c(a = "Robert"))), "a")
    expect_identical(fgrepl::nysiis(factor("WATSON")), "WATSAN")
})

test_that("soundex/nysiis handle a vector with mixed NA and valid entries", {
    x <- c("Robert", NA, "Rupert", "")
    sx <- fgrepl::soundex(x)
    ny <- fgrepl::nysiis(x)
    expect_identical(sx, c("R163", NA, "R163", NA))
    expect_true(is.na(ny[2]) && is.na(ny[4]))
    expect_false(is.na(ny[1]) || is.na(ny[3]))
})
