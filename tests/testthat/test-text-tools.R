u <- function(...) intToUtf8(c(...))

unhex <- function(h) {
    vapply(h, function(s) {
        if (!nzchar(s)) return("")
        bytes <- as.raw(strtoi(substring(s, seq(1, nchar(s), 2),
                                         seq(2, nchar(s), 2)), 16L))
        out <- rawToChar(bytes)
        Encoding(out) <- "UTF-8"
        out
    }, "", USE.NAMES = FALSE)
}

test_that("freplace_tokens() replaces whole tokens and squishes", {
    x <- c("12 SMITH ST", "3/45 NORTH RD", "  7   KING   ST  ", NA, "",
           "STAR ST.", "ST")
    expect_identical(
        freplace_tokens(x, c("ST", "RD"), c("STREET", "ROAD")),
        c("12 SMITH STREET", "3/45 NORTH ROAD", "7 KING STREET", NA, "",
          "STAR ST.", "STREET")
    )
    expect_identical(names(freplace_tokens(c(a = "ST"), "ST", "X")), "a")
    # Deletion, multi-word replacements, and no re-scanning
    expect_identical(freplace_tokens("MR JOHN SMITH", "MR", ""), "JOHN SMITH")
    expect_identical(freplace_tokens("MR", "MR", ""), "")
    expect_identical(freplace_tokens("STH BANK", "STH", "SOUTH BANK"),
                     "SOUTH BANK BANK")
    expect_identical(freplace_tokens("A B", c("A", "B"), c("B", "C")), "B C")
    # The first duplicate wins, as match()
    expect_identical(freplace_tokens("ST", c("ST", "ST"), c("1", "2")), "1")
    # Unicode whitespace separates tokens
    expect_identical(freplace_tokens(paste0("ST", u(0xa0), "RD", u(0x3000)),
                                     c("ST", "RD"), c("S", "R")), "S R")
})

test_that("freplace_tokens() folds ASCII case on request", {
    x <- c("Dr John Smith", "MRS JANE DOE", "mr x")
    expect_identical(
        freplace_tokens(x, c("dr", "mrs", "mr"), "", ignore_case = TRUE),
        c("John Smith", "JANE DOE", "x")
    )
    expect_identical(freplace_tokens(x, c("dr", "mrs", "mr"), ""),
                     c("Dr John Smith", "MRS JANE DOE", "x"))
    e <- u(0xc9)
    expect_identical(freplace_tokens(c(e, tolower(e)), e, "E", ignore_case = TRUE),
                     c("E", tolower(e)))
})

test_that("freplace_tokens() agrees with strsplit(), match() and paste()", {
    set.seed(5)
    words <- c("12", "SMITH", "ST", "st", "RD", "AVE", "MR", "JOHN", u(0xc9, 0x43))
    x <- vapply(1:2000, function(i) paste(sample(words, sample(0:6, 1), TRUE),
                                          collapse = sample(c(" ", "  ", "\t"), 1)), "")
    from <- c("ST", "RD", "AVE", "MR", u(0xc9, 0x43))
    to <- c("STREET", "ROAD", "AVENUE", "", "EC")
    ref <- vapply(strsplit(x, "[ \t]+"), function(t) {
        t <- t[nzchar(t)]
        m <- match(t, from)
        t[!is.na(m)] <- to[m[!is.na(m)]]
        paste(t[nzchar(t)], collapse = " ")
    }, "")
    expect_identical(freplace_tokens(x, from, to), ref)
    expect_identical(freplace_tokens(x, from, to, nthreads = 1), ref)
})

test_that("freplace_tokens() validates its dictionary", {
    expect_error(freplace_tokens("a", c("a", "b"), c("1", "2", "3")), "same length")
    expect_error(freplace_tokens("a", NA_character_, "x"), "NA")
    expect_error(freplace_tokens("a", "a b", "x"), "whitespace")
    expect_error(freplace_tokens("a", "", "x"), "empty")
    expect_error(freplace_tokens(1, "a", "x"), "character")
    expect_identical(freplace_tokens(NA, "a", "b"), NA_character_)
})

test_that("fpad() matches stringi::stri_pad()", {
    expect_identical(fpad(c("800", "4000", "2", NA, "", "12345"), 4, pad = "0"),
                     c("0800", "4000", "0002", NA, "0000", "12345"))
    expect_identical(fpad(c("a", "ab", "abc"), 6, "both", "*"),
                     c("**a***", "**ab**", "*abc**"))
    expect_identical(fpad("ab", 4, "right"), "ab  ")
    expect_identical(fpad(c(k = "ab"), 4), c(k = "  ab"))
    expect_identical(fpad(u(0xe9), 3, pad = u(0x4e2d)), u(0x4e2d, 0x4e2d, 0xe9))
    skip_if_not_installed("stringi")
    set.seed(2)
    x <- vapply(1:500, function(i) paste(sample(c(letters, u(0xe9), u(0x4e2d)),
                                                sample(0:8, 1), TRUE), collapse = ""), "")
    for (side in c("left", "right", "both")) {
        expect_identical(fpad(x, 6, side, "."),
                         stringi::stri_pad(x, 6, side, pad = ".", use_length = TRUE))
    }
})

test_that("fpad() validates its arguments", {
    expect_error(fpad("a", -1), "width")
    expect_error(fpad("a", 2, pad = "ab"), "single character")
    expect_error(fpad("a", 2, side = "middle"))
})

test_that("string_shape() maps letters, digits and keeps the rest", {
    x <- c("0412 345 678", "(07) 3123 4567", "Smith", "O'Brien", "QLD 4000", NA, "")
    expect_identical(string_shape(x),
                     c("9999 999 999", "(99) 9999 9999", "Aaaaa", "A'Aaaaa",
                       "AAA 9999", NA, ""))
    expect_identical(string_shape(x, collapse = TRUE),
                     c("9 9 9", "(9) 9 9", "Aa", "A'Aa", "A 9", NA, ""))
    expect_identical(string_shape("Smith", ignore_case = TRUE), "aaaaa")
    expect_identical(string_shape(c(n = "ab")), c(n = "aa"))
})

test_that("string_shape() classifies Unicode by general category", {
    # e-acute (precomposed and decomposed), Greek capital, CJK, Arabic-Indic 3
    expect_identical(string_shape(u(0x63, 0x61, 0x66, 0xe9)), "aaaa")
    expect_identical(string_shape(u(0x65, 0x301, 0x74, 0x65, 0x301)), "aaa")
    expect_identical(string_shape(u(0x394, 0x3b9)), "Aa")
    expect_identical(string_shape(u(0x4e2d, 0x6587)), "aa")
    expect_identical(string_shape(u(0x663, 0x20ac)), paste0("9", u(0x20ac)))
    l1 <- "caf\xe9"
    Encoding(l1) <- "latin1"
    expect_identical(string_shape(l1), "aaaa")
})

test_that("url_encode() matches utils::URLencode()", {
    set.seed(3)
    pool <- c(letters, LETTERS, 0:9,
              strsplit(" !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", "")[[1]],
              u(0xe9), u(0x4e2d), "%41")
    x <- vapply(1:3000, function(i) paste(sample(pool, sample(0:10, 1), TRUE),
                                          collapse = ""), "")
    for (reserved in c(FALSE, TRUE)) for (repeated in c(FALSE, TRUE)) {
        expect_identical(url_encode(x, reserved, repeated),
                         utils::URLencode(x, reserved, repeated))
    }
    expect_identical(url_encode(c("a b", NA)), c("a%20b", NA))
    expect_identical(url_encode(c(k = "a&b"), reserved = TRUE), c(k = "a%26b"))
})

test_that("url_decode() decodes escapes and leaves broken ones", {
    expect_identical(url_decode(c("a%20b", "%41%42", "%zz", "a%2", "100%", NA)),
                     c("a b", "AB", "%zz", "a%2", "100%", NA))
    expect_identical(url_decode("q=meat+pie%21", plus = TRUE), "q=meat pie!")
    expect_identical(url_decode("q=meat+pie"), "q=meat+pie")
    e <- url_decode("caf%C3%A9")
    expect_identical(e, u(0x63, 0x61, 0x66, 0xe9))
    expect_identical(Encoding(e), "UTF-8")
    bad <- url_decode("%E9")
    expect_identical(charToRaw(bad), as.raw(0xe9))
    expect_false(Encoding(bad) == "UTF-8")
    x <- c("fish & chips", u(0x63, 0x61, 0x66, 0xe9, 0x20, 0x2f), "100%")
    expect_identical(url_decode(url_encode(x, reserved = TRUE, repeated = TRUE)), x)
})

test_that("html_unescape() decodes named and numeric references", {
    x <- c("Fish &amp; Chips", "caf&eacute;", "&lt;b&gt;", "AT&T", NA, "",
           "&copy2024", "&#x1F600;", "&#0;", "&#150;", "&ampx;", "&#65",
           "&#;", "& amp;", "&notit;", "&NotEqualTilde;")
    expect_identical(
        html_unescape(x),
        c("Fish & Chips", u(0x63, 0x61, 0x66, 0xe9), "<b>", "AT&T", NA, "",
          paste0(u(0xa9), "2024"), u(0x1F600), u(0xFFFD), u(0x2013), "&x;", "A",
          "&#;", "& amp;", paste0(u(0xac), "it;"), u(0x2242, 0x338))
    )
    expect_identical(html_unescape(c(k = "&gt;")), c(k = ">"))
})

test_that("html_unescape() reproduces Python's html.unescape()", {
    ref <- utils::read.csv(test_path("fixtures", "html_unescape.csv"),
                           colClasses = "character")
    input <- unhex(ref$input)
    expect_identical(html_unescape(input), unhex(ref$expected))
    expect_identical(html_unescape(input, nthreads = 1), unhex(ref$expected))
})
