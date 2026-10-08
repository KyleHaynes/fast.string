test_that("soundex matches classic reference codes", {
    expect_identical(fast.string::soundex("Robert"),   "R163")
    expect_identical(fast.string::soundex("Rupert"),   "R163")
    expect_identical(fast.string::soundex("Ashcraft"), "A261") # H/W transparency rule
    expect_identical(fast.string::soundex("Pfister"),  "P236")
    expect_identical(fast.string::soundex("Tymczak"),  "T522")
    expect_identical(fast.string::soundex("Honeyman"), "H555")
})

test_that("soundex is vectorised and case-insensitive", {
    expect_identical(fast.string::soundex(c("Robert", "ROBERT", "robert")), rep("R163", 3))
})

test_that("soundex is always exactly 4 characters or NA", {
    x <- c("A", "Robert", "Supercalifragilisticexpialidocious")
    res <- fast.string::soundex(x)
    expect_true(all(nchar(res) == 4))
})

test_that("soundex returns NA for NA, empty, and non-alphabetic input", {
    expect_true(is.na(fast.string::soundex(NA_character_)))
    expect_true(is.na(fast.string::soundex("")))
    expect_true(is.na(fast.string::soundex("12345")))
    expect_true(is.na(fast.string::soundex("---")))
})

test_that("soundex preserves names and coerces non-character input", {
    expect_identical(names(fast.string::soundex(c(a = "Robert"))), "a")
    expect_identical(fast.string::soundex(factor("Robert")), "R163")
})

test_that("nysiis matches hand-traced reference codes for the implemented ruleset", {
    expect_identical(fast.string::nysiis("MACDONALD"), "MCDANA")
    expect_identical(fast.string::nysiis("KNIGHT"),    "NAGT")
    expect_identical(fast.string::nysiis("PHILBERT"),  "FALBAD")
    expect_identical(fast.string::nysiis("SCHMIDT"),   "SNAD")
    expect_identical(fast.string::nysiis("WATSON"),    "WATSAN")
    expect_identical(fast.string::nysiis("BROWNING"),  "BRANAN")
})

test_that("nysiis is capped at 6 characters", {
    res <- fast.string::nysiis(c("A", "Supercalifragilisticexpialidocious"))
    expect_true(all(nchar(res) <= 6))
})

test_that("nysiis returns NA for NA, empty, and non-alphabetic input", {
    expect_true(is.na(fast.string::nysiis(NA_character_)))
    expect_true(is.na(fast.string::nysiis("")))
    expect_true(is.na(fast.string::nysiis("12345")))
    expect_true(is.na(fast.string::nysiis("---")))
})

test_that("nysiis preserves names and coerces non-character input", {
    expect_identical(names(fast.string::nysiis(c(a = "Robert"))), "a")
    expect_identical(fast.string::nysiis(factor("WATSON")), "WATSAN")
})

test_that("soundex/nysiis handle a vector with mixed NA and valid entries", {
    x <- c("Robert", NA, "Rupert", "")
    sx <- fast.string::soundex(x)
    ny <- fast.string::nysiis(x)
    expect_identical(sx, c("R163", NA, "R163", NA))
    expect_true(is.na(ny[2]) && is.na(ny[4]))
    expect_false(is.na(ny[1]) || is.na(ny[3]))
})

test_that("nysiis transcodes in place like Apache Commons Codec", {
    # The H and W rules copy the previous letter after it was transcoded,
    # so vowels other than A never reach the key.
    expect_identical(fast.string::nysiis(c("Johnson", "Jonson")),
                     c("JANSAN", "JANSAN"))
    # PH and SCH are also rewritten inside the name.
    expect_identical(fast.string::nysiis(c("Stephens", "Raphael", "Bischoff")),
                     c("STAFAN", "RAFAL", "BASAF"))
    # A final H after a vowel takes that vowel, which is then dropped.
    expect_identical(fast.string::nysiis(c("Sarah", "Hannah")), c("SAR", "HAN"))
    surnames <- c("Johnson", "Johnston", "Johns", "Gutierrez", "Heitschmidt",
                  "Westphal", "Rickert", "Carraway", "Yamada", "Macintosh")
    expect_false(any(grepl("^.+[EIOU]", fast.string::nysiis(surnames))))
    expect_identical(
        fast.string::nysiis(c("MACINTOSH", "KNUTH", "WESTERLUND", "CASSTEVENS",
                              "HEITSCHMIDT", "MCKNIGHT", "DEUTSCH", "CARRAWAY")),
        c("MCANT", "NAT", "WASTAR", "CASTAF", "HATSNA", "MCNAGT", "DAT", "CARY")
    )
})

test_that("phonetic codes fold accented Latin letters", {
    u <- function(...) intToUtf8(c(...))
    accented <- c(paste0(u(0xc9), "mile"), paste0("Nu", u(0xf1), "ez"),
                  paste0(u(0x141), "ukasz"), paste0("Dvo", u(0x159), u(0xe1), "k"),
                  paste0("M", u(0xfc), "ller"))
    plain <- c("Emile", "Nunez", "Lukasz", "Dvorak", "Muller")
    for (encode in list(fast.string::soundex, fast.string::refined_soundex,
                        fast.string::nysiis, fast.string::cologne,
                        fast.string::caverphone)) {
        expect_identical(encode(accented), encode(plain))
    }
    expect_identical(fast.string::double_metaphone(accented),
                     fast.string::double_metaphone(plain))
    # latin1-encoded input is folded too.
    latin1 <- iconv(accented[c(1, 2, 5)], "UTF-8", "latin1")
    expect_identical(fast.string::soundex(latin1), fast.string::soundex(plain[c(1, 2, 5)]))
})

test_that("phonetic codes take an nthreads cap", {
    x <- rep(c("Robert", "Rupert", NA), 50)
    for (encode in list(fast.string::soundex, fast.string::refined_soundex,
                        fast.string::nysiis, fast.string::cologne,
                        fast.string::caverphone)) {
        expect_identical(encode(x, nthreads = 1L), encode(x))
    }
    expect_identical(fast.string::double_metaphone(x, nthreads = 2L),
                     fast.string::double_metaphone(x))
    expect_error(fast.string::soundex("a", nthreads = -1), "positive integer")
})
