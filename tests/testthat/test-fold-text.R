u <- function(...) intToUtf8(c(...))

test_that("ascii_fold() spells out accented letters, ligatures and punctuation", {
    x <- c(paste0("Fran", u(0xe7), "ois M", u(0xfc), "ller"),
           paste0(u(0xc6), "sir ", u(0xd8), "rsted"),
           paste0("Stra", u(0xdf), "e"), u(0xde, 0x6f, 0x72),
           paste0(u(0x201c), "q", u(0x201d), " ", u(0x2013), " ", u(0xbd)),
           paste0("a", u(0xa0), "b", u(0x2026)), "plain", NA, "")
    expect_identical(ascii_fold(x),
                     c("Francois Muller", "AEsir Orsted", "Strasse", "THor",
                       "\"q\" -  1/2", "a b...", "plain", NA, ""))
})

test_that("ascii_fold() drops combining accents after Latin letters and digits", {
    expect_identical(ascii_fold(paste0("e", u(0x301), "te", u(0x301, 0x302))), "ete")
    expect_identical(ascii_fold(paste0("1", u(0x301))), "1")
    # ... but not after other scripts or symbols
    greek <- u(0x3b1, 0x301)
    expect_identical(ascii_fold(greek), greek)
    expect_identical(ascii_fold(paste0("-", u(0x301))), paste0("-", u(0x301)))
})

test_that("ascii_fold() keeps or replaces what it cannot fold", {
    x <- paste0(u(0x394, 0x3b9), " caf", u(0xe9), " ", u(0x20ac))
    expect_identical(ascii_fold(x), paste0(u(0x394, 0x3b9), " cafe ", u(0x20ac)))
    expect_identical(ascii_fold(x, sub = ""), " cafe ")
    expect_identical(ascii_fold(x, sub = "?"), "?? cafe ?")
    expect_identical(ascii_fold(c(a = u(0xe9))), c(a = "e"))
    expect_identical(ascii_fold(factor(u(0xe9))), "e")
    l1 <- "caf\xe9"
    Encoding(l1) <- "latin1"
    expect_identical(ascii_fold(l1), "cafe")
    expect_error(ascii_fold("a", sub = c("a", "b")), "single string")
})

test_that("ascii_fold() reproduces ICU's Latin-ASCII transform", {
    skip_if_not_installed("stringi")
    cp <- c(0xa0:0x24f, 0x1e00:0x1eff, 0x2000:0x206f, 0x2100:0x218f, 0xfb00:0xfb06,
            0xff01:0xff5e)
    ch <- intToUtf8(cp, multiple = TRUE)
    expect_identical(ascii_fold(ch), stringi::stri_trans_general(ch, "Latin-ASCII"))
    set.seed(7)
    pool <- c(cp, 0x20:0x7e, 0x300:0x36f, 0x391:0x3c9)
    x <- vapply(1:2000, function(i) intToUtf8(sample(pool, sample(0:10, 1), TRUE)), "")
    x <- stringi::stri_trans_nfc(x)
    expect_identical(ascii_fold(x), stringi::stri_trans_general(x, "Latin-ASCII"))
})

test_that("fingerprint() builds OpenRefine's key-collision keys", {
    x <- c("Smith, John", "john SMITH", "  John  Smith. ", "Jon Smith",
           "Smith-Jones", paste0("S", u(0xf8), "ren ", u(0xc6), "ble"), "a $ + b",
           "-- Smith", "", NA)
    expect_identical(fingerprint(x),
                     c("john smith", "john smith", "john smith", "jon smith",
                       "smithjones", "aeble soeren", "$ + a b", "smith", "", NA))
    expect_identical(fingerprint(c(k = "B a")), c(k = "a b"))
})

test_that("ngram_fingerprint() builds sorted, de-duplicated n-gram keys", {
    expect_identical(
        ngram_fingerprint(c("Acme Pty Ltd", "ACME PtyLtd", "Acme Pty. Ltd.")),
        rep("accmepltmepttdtyyl", 3)
    )
    expect_identical(ngram_fingerprint("abcab", 3), "abcbcacab")
    # shorter than n: the text itself (OpenRefine gives "")
    expect_identical(ngram_fingerprint(c("A", "", NA)), c("a", "", NA))
    expect_error(ngram_fingerprint("a", 0), "positive integer")
})

test_that("text folding is unaffected by the thread count", {
    set.seed(3)
    x <- vapply(1:3000, function(i) intToUtf8(sample(c(0x41:0x5a, 0xc0:0x17f, 0x20),
                                                     sample(1:12, 1), TRUE)), "")
    expect_identical(ascii_fold(x, nthreads = 1), ascii_fold(x))
    expect_identical(fingerprint(x, nthreads = 1), fingerprint(x))
    expect_identical(ngram_fingerprint(x, 2, nthreads = 1), ngram_fingerprint(x, 2))
})
