# The expected codes are the assertions of Apache Commons Codec's
# DaitchMokotoffSoundexTest, which this encoder is a port of. Several
# spellings code more than one way; "|" separates the codes as Commons
# Codec's own soundex() joins them.
dm <- function(x) {
    vapply(fast.string::daitch_mokotoff(x), paste, "", collapse = "|")
}

test_that("daitch_mokotoff matches the reference codes for one-code names", {
    expect_identical(dm("Straßburg"), "294795")
    expect_identical(dm("Strasburg"), "294795")
    expect_identical(dm("Éregon"), "095600")
    expect_identical(dm("Eregon"), "095600")
    expect_identical(dm("AKSSOL"), "054800")
    expect_identical(dm("GOLDEN"), "583600")
    expect_identical(dm("Alpert"), "087930")
    expect_identical(dm("Breuer"), "791900")
    expect_identical(dm("Haber"), "579000")
    expect_identical(dm("Mannheim"), "665600")
    expect_identical(dm("Mintz"), "664000")
    expect_identical(dm("Topf"), "370000")
    expect_identical(dm("Kleinmann"), "586660")
    expect_identical(dm("Ben Aron"), "769600")
    expect_identical(dm("Moskowitz"), "645740")
    expect_identical(dm("Moskovitz"), "645740")
    expect_identical(dm("LIPSHITZ"), "874400")
    expect_identical(dm("LEWINSKY"), "876450")
    expect_identical(dm("LEVINSKI"), "876450")
    expect_identical(dm("SZLAMAWICZ"), "486740")
    expect_identical(dm("SHLAMOVITZ"), "486740")
    expect_identical(dm("Tsenyuv"), "467000")
    expect_identical(dm("Golubitsa"), "587400")
    expect_identical(dm("Pshemeshil"), "746480")
    expect_identical(dm("Rosokhovatsets"), "945744")
})

test_that("daitch_mokotoff gives every code of an ambiguous spelling", {
    expect_identical(dm("GERSCHFELD"), "547830|545783|594783|594578")
    expect_identical(dm("AUERBACH"), "097400|097500")
    expect_identical(dm("OHRBACH"), "097400|097500")
    expect_identical(dm("LIPPSZYC"), "874400|874500")
    expect_identical(dm("Ceniow"), "467000|567000")
    expect_identical(dm("Holubica"), "587400|587500")
    expect_identical(dm("Przemysl"), "746480|794648")
    expect_identical(dm("Peters"), "734000|739400")
    expect_identical(dm("Peterson"), "734600|739460")
    expect_identical(dm("Jackson"), "154600|145460|454600|445460")
    expect_identical(
        dm("Rosochowaciec"),
        paste("944744", "944745", "944754", "944755", "945744", "945745",
              "945754", "945755", sep = "|")
    )
})

test_that("daitch_mokotoff ignores whitespace, punctuation and digits", {
    expect_identical(dm(" \t\n\r Washington \t\n\r "), "746536")
    expect_identical(dm("Washington"), "746536")
    joined <- dm("JacksonJackson")
    for (spelling in c("Jackson-Jackson", "Jackson--Jackson",
                       "Jackson_Jackson", "Jackson$Jackson",
                       "Jackson Jackson", "Jackson1Jackson")) {
        expect_identical(dm(spelling), joined, info = spelling)
    }
})

test_that("daitch_mokotoff folds accented letters as the rules list them", {
    # The Romanian t-cedilla and t-comma are coded alike.
    expect_identical(dm("ţamas"), "364000|464000")
    expect_identical(dm("țamas"), "364000|464000")
    # Upper case reaches the same rules.
    expect_identical(dm("ŢAMAS"), "364000|464000")
    expect_identical(dm("Éregon"), dm("Eregon"))
    expect_identical(dm("Łukasz"), dm("Lukasz"))
})

test_that("daitch_mokotoff gives NA where there is no letter to code", {
    # Commons Codec codes these as "000000"; this package reports no code,
    # as soundex() and the other encoders here do.
    expect_identical(fast.string::daitch_mokotoff(c("", " ", "123", NA)),
                     rep(list(NA_character_), 4))
    expect_identical(fast.string::soundex(c("", "123", NA)),
                     rep(NA_character_, 3))
})

test_that("daitch_mokotoff codes are always six digits", {
    x <- c("Smith", "Moskowitz", "a", "Przemysl", "Schwarzenegger",
           "Oesterreicher", "Nguyen", "O'Brien", "van der Berg")
    codes <- unlist(fast.string::daitch_mokotoff(x))
    expect_true(all(nchar(codes) == 6L))
    expect_true(all(grepl("^[0-9]{6}$", codes)))
})

test_that("daitch_mokotoff simplify gives an NA-padded matrix", {
    m <- fast.string::daitch_mokotoff(c("Peters", "Smith", NA),
                                      simplify = TRUE)
    expect_true(is.matrix(m))
    expect_identical(nrow(m), 3L)
    expect_identical(m[1, ], c("734000", "739400"))
    expect_identical(m[2, 2], NA_character_)
    expect_true(all(is.na(m[3, ])))
    # One column even when nothing has a code, so it still works as a key.
    expect_identical(dim(fast.string::daitch_mokotoff(c("", ""),
                                                      simplify = TRUE)),
                     c(2L, 1L))
    expect_identical(dim(fast.string::daitch_mokotoff(character(0),
                                                      simplify = TRUE)),
                     c(0L, 0L))
})

test_that("daitch_mokotoff keeps names and takes non-character input", {
    expect_identical(names(fast.string::daitch_mokotoff(c(a = "Smith"))), "a")
    expect_identical(
        rownames(fast.string::daitch_mokotoff(c(a = "Smith", b = "Jones"),
                                              simplify = TRUE)),
        c("a", "b")
    )
    expect_identical(fast.string::daitch_mokotoff(factor("Smith")),
                     fast.string::daitch_mokotoff("Smith"))
    expect_identical(fast.string::daitch_mokotoff(character(0)), list())
    expect_error(fast.string::daitch_mokotoff("x", simplify = NA),
                 "TRUE or FALSE")
})

test_that("daitch_mokotoff is thread-independent", {
    x <- rep(c("Moskowitz", "AUERBACH", "Rosochowaciec", NA, "Peters"), 20000)
    expect_identical(fast.string::daitch_mokotoff(x, nthreads = 1L),
                     fast.string::daitch_mokotoff(x))
    expect_identical(
        fast.string::daitch_mokotoff(x, simplify = TRUE, nthreads = 1L),
        fast.string::daitch_mokotoff(x, simplify = TRUE)
    )
})

test_that("daitch_mokotoff codes feed block_pairs as one pass per column", {
    x <- c("Moskowitz", "Moskovitz", "Auerbach", "Ohrbach", "Smith")
    keys <- fast.string::daitch_mokotoff(x, simplify = TRUE)
    pairs <- fast.string::block_pairs(as.data.frame(keys))
    found <- paste(x[pairs[[1]]], x[pairs[[2]]])
    expect_true("Moskowitz Moskovitz" %in% found)
    expect_true("Auerbach Ohrbach" %in% found)
    expect_false(any(grepl("Smith", found)))
})
