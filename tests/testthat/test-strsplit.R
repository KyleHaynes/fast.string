test_that("fstrsplit matches base::strsplit on a fixed separator", {
    cases <- list(
        "a,b,c", "a,b,", ",a", ",", ",,", "", NA_character_, "no-sep",
        c("a,b", NA, "", "x"), c("a", "b,c,d"), character(0)
    )
    for (x in cases) {
        expect_identical(fast.string::fstrsplit(x, ",", fixed = TRUE),
                         strsplit(x, ",", fixed = TRUE),
                         info = paste(deparse(x), collapse = ""))
    }
    expect_identical(fast.string::fstrsplit("aXXbXXc", "XX", fixed = TRUE),
                     strsplit("aXXbXXc", "XX", fixed = TRUE))
    # A literal dot is not a wildcard under fixed = TRUE.
    expect_identical(fast.string::fstrsplit("a.b.c", ".", fixed = TRUE),
                     strsplit("a.b.c", ".", fixed = TRUE))
})

test_that("fstrsplit matches base::strsplit on a regular expression", {
    cases <- list(
        list("a  b   c", "\\s+"),
        list("a1b22c", "[0-9]+"),
        list("a-b_c", "[-_]"),
        list("a.b", "."),
        list("2024-03-15", "-"),
        list(c("x1y", "p22q", NA, ""), "[0-9]+"),
        list("Smith, John; Doe, Jane", ";\\s*")
    )
    for (case in cases) {
        expect_identical(fast.string::fstrsplit(case[[1]], case[[2]]),
                         strsplit(case[[1]], case[[2]]),
                         info = case[[2]])
    }
    expect_identical(fast.string::fstrsplit("a\tb c", "[ \t]", perl = TRUE),
                     strsplit("a\tb c", "[ \t]", perl = TRUE))
})

test_that("fstrsplit splits into characters when split is empty", {
    expect_identical(fast.string::fstrsplit("abc", ""), strsplit("abc", ""))
    expect_identical(fast.string::fstrsplit(c("café", "é"), ""),
                     strsplit(c("café", "é"), ""))
    expect_identical(fast.string::fstrsplit("café", "", useBytes = TRUE),
                     strsplit("café", "", useBytes = TRUE))
    expect_identical(fast.string::fstrsplit(c("", NA), ""),
                     strsplit(c("", NA), ""))
})

test_that("fstrsplit takes ignore.case, which base::strsplit has no option for", {
    expect_identical(fast.string::fstrsplit("aXbxc", "x",
                                            ignore.case = TRUE)[[1]],
                     c("a", "b", "c"))
    expect_identical(fast.string::fstrsplit("aXbxc", "x", fixed = TRUE,
                                            ignore.case = TRUE)[[1]],
                     c("a", "b", "c"))
})

test_that("fstrsplit simplify gives an NA-padded matrix", {
    m <- fast.string::fstrsplit(c("12 Smith St", "4/7 Bay Rd", NA, "one"),
                                " ", fixed = TRUE, simplify = TRUE)
    expect_true(is.matrix(m))
    expect_identical(dim(m), c(4L, 3L))
    expect_identical(m[1, ], c("12", "Smith", "St"))
    expect_identical(m[3, ], c(NA_character_, NA_character_, NA_character_))
    expect_identical(m[4, ], c("one", NA, NA))
    # The same values do.call(rbind, strsplit(...)) would give, unpadded.
    expect_identical(
        unname(fast.string::fstrsplit(c("a b", "c d"), " ", fixed = TRUE,
                                      simplify = TRUE)),
        do.call(rbind, strsplit(c("a b", "c d"), " ", fixed = TRUE))
    )
    expect_identical(dim(fast.string::fstrsplit(character(0), ",",
                                                fixed = TRUE,
                                                simplify = TRUE)),
                     c(0L, 0L))
    expect_identical(dim(fast.string::fstrsplit(c("", ""), ",", fixed = TRUE,
                                                simplify = TRUE)),
                     c(2L, 0L))
})

test_that("fstrsplit keeps names on both result shapes", {
    expect_identical(names(fast.string::fstrsplit(c(a = "x,y"), ",",
                                                  fixed = TRUE)), "a")
    m <- fast.string::fstrsplit(c(a = "x,y", b = "z"), ",", fixed = TRUE,
                                simplify = TRUE)
    expect_identical(rownames(m), c("a", "b"))
    expect_null(colnames(m))
    # Unnamed input gives an unnamed matrix.
    expect_null(dimnames(fast.string::fstrsplit("x,y", ",", fixed = TRUE,
                                                simplify = TRUE)))
})

test_that("fstrsplit rejects a separator that can match nothing", {
    expect_error(fast.string::fstrsplit("abc", "x*"), "empty string")
    expect_error(fast.string::fstrsplit("abc", "\\b"), "empty string")
    expect_error(fast.string::fstrsplit("abc", "(|a)"), "empty string")
})

test_that("fstrsplit validates its arguments", {
    expect_error(fast.string::fstrsplit("abc", NA), "must not be `NA`")
    expect_error(fast.string::fstrsplit("abc", c(",", ";")),
                 "single character string")
    expect_error(fast.string::fstrsplit("abc", character(0)),
                 "single character string")
    expect_error(fast.string::fstrsplit("a", ",", simplify = NA),
                 "TRUE or FALSE")
    expect_error(fast.string::fstrsplit(list("a"), ","), "character vector")
})

test_that("fstrsplit marks the encoding of each piece", {
    x <- c("café-x", "plain-y")
    pieces <- unlist(fast.string::fstrsplit(x, "-", fixed = TRUE))
    expect_identical(pieces, c("café", "x", "plain", "y"))
    expect_identical(Encoding(pieces[1]), "UTF-8")
})

test_that("fstrsplit is thread-independent and matches base at scale", {
    big <- rep(c("12 Smith St", "4/7 Bay Rd", "a b c d e", NA), 10000)
    expect_identical(fast.string::fstrsplit(big, " ", fixed = TRUE),
                     strsplit(big, " ", fixed = TRUE))
    expect_identical(fast.string::fstrsplit(big, "\\s+"),
                     strsplit(big, "\\s+"))
    expect_identical(fast.string::fstrsplit(big, " ", fixed = TRUE,
                                            nthreads = 1L),
                     fast.string::fstrsplit(big, " ", fixed = TRUE))
    expect_identical(fast.string::fstrsplit(big, " ", fixed = TRUE,
                                            simplify = TRUE, nthreads = 1L),
                     fast.string::fstrsplit(big, " ", fixed = TRUE,
                                            simplify = TRUE))
})

test_that("fstrsplit and fsplit_field agree on the field they share", {
    x <- c("2024-03-15", "1999-12-31", "bad", NA)
    pieces <- fast.string::fstrsplit(x, "-", fixed = TRUE)
    expect_identical(vapply(pieces, `[`, "", 1L),
                     fast.string::fsplit_field(x, "-", 1, fixed = TRUE))
})
