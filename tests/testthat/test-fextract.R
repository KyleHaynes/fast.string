u <- function(...) intToUtf8(c(...))

base_extract <- function(pattern, x, perl = FALSE, fixed = FALSE) {
    m <- regexpr(pattern, x, perl = perl, fixed = fixed)
    out <- rep(NA_character_, length(x))
    hit <- !is.na(m) & m != -1L
    out[hit] <- regmatches(x, m)
    out
}

base_extract_all <- function(pattern, x, perl = FALSE, fixed = FALSE) {
    out <- regmatches(x, gregexpr(pattern, x, perl = perl, fixed = fixed))
    out[is.na(x)] <- list(NA_character_)
    out
}

set.seed(101)
alphabet <- c(letters[1:6], "A", "B", 0:9, " ", " ", ",", "-", u(0xe9), u(0x4e2d))
random_x <- c(vapply(1:400, function(i) paste(sample(alphabet, sample(0:14, 1), TRUE),
                                             collapse = ""), ""), NA, "", " ")

test_that("fextract() matches regmatches(regexpr())", {
    for (p in c("[0-9]+", "[a-c]{2,}", "\\w+", "[^ ]+", " +")) {
        expect_identical(fextract(p, random_x, perl = TRUE),
                         base_extract(p, random_x, perl = TRUE), info = p)
        expect_identical(fextract(p, random_x), base_extract(p, random_x), info = p)
    }
    expect_identical(fextract("a", random_x, fixed = TRUE),
                     base_extract("a", random_x, fixed = TRUE))
})

test_that("fextract_all() matches regmatches(gregexpr())", {
    for (p in c("[0-9]+", "[a-c]", "\\w+")) {
        expect_identical(fextract_all(p, random_x, perl = TRUE),
                         base_extract_all(p, random_x, perl = TRUE), info = p)
        expect_identical(fextract_all(p, random_x), base_extract_all(p, random_x),
                         info = p)
    }
    expect_identical(fextract_all("ab", random_x, fixed = TRUE),
                     base_extract_all("ab", random_x, fixed = TRUE))
    # Empty matches step one character, as fcount() counts them.
    expect_identical(lengths(fextract_all("a*", random_x, perl = TRUE))[!is.na(random_x)],
                     fcount("a*", random_x, perl = TRUE)[!is.na(random_x)])
})

test_that("fextract() extracts capture groups", {
    x <- c("Unit 4, 12 Smith St", "PO Box 99", NA, "7 King St")
    expect_identical(fextract("([0-9]+) ([A-Za-z]+) St", x, group = 1),
                     c("12", NA, NA, "7"))
    expect_identical(fextract("([0-9]+) ([A-Za-z]+) St", x, group = 2),
                     c("Smith", NA, NA, "King"))
    expect_identical(fextract("(a)|(b)", c("a", "b"), perl = TRUE, group = 2),
                     c(NA, "b"))
    expect_identical(fextract_all("(a)|(b)", "ab", perl = TRUE, group = 1),
                     list(c("a", NA)))
    expect_error(fextract("(a)", "a", group = 2), "does not exist")
    expect_error(fextract("a", "a", fixed = TRUE, group = 1), "must be 0")
})

test_that("extraction keeps names, handles NA patterns and latin1 input", {
    expect_identical(fextract("[0-9]", c(a = "x1", b = "y")), c(a = "1", b = NA))
    expect_identical(fextract(NA, c("a", "b")), c(NA_character_, NA_character_))
    expect_identical(fextract_all(NA, "a"), list(NA_character_))
    l1 <- "caf\xe9"
    Encoding(l1) <- "latin1"
    # A whole-string match returns the string itself; a part of it comes
    # back as UTF-8.
    expect_identical(fextract(paste0("[a-z", u(0xe9), "]+"), l1), l1)
    got <- fextract(paste0("f", u(0xe9)), l1)
    expect_identical(got, paste0("f", u(0xe9)))
    expect_identical(Encoding(got), "UTF-8")
})

test_that("fsplit_field() returns one field, counting empty fields", {
    split_ref <- function(x, split, k) vapply(x, function(s) {
        if (is.na(s)) return(NA_character_)
        p <- strsplit(paste0(s, "\001"), split, fixed = TRUE)[[1]]
        p[length(p)] <- sub("\001$", "", p[length(p)])
        i <- if (k > 0) k else length(p) + k + 1
        if (i < 1 || i > length(p)) NA_character_ else p[i]
    }, "", USE.NAMES = FALSE)
    for (k in c(1, 2, 3, -1, -2, 6)) {
        expect_identical(fsplit_field(random_x, ",", k, fixed = TRUE),
                         split_ref(random_x, ",", k), info = k)
        expect_identical(fsplit_field(random_x, " ", k, fixed = TRUE),
                         split_ref(random_x, " ", k), info = k)
    }
    expect_identical(fsplit_field(c("a,,b", "a,b,", NA), ",", 3, fixed = TRUE),
                     c("b", "", NA))
    expect_identical(fsplit_field("a  b   c", "\\s+", 2), "b")
    expect_identical(fsplit_field("a  b   c", "\\s+", -1, perl = TRUE), "c")
    expect_identical(fsplit_field(c(x = "2024-03-15"), "-", 2, fixed = TRUE),
                     c(x = "03"))
    expect_error(fsplit_field("a", "x*", 1), "must not match the empty string")
    expect_error(fsplit_field("a", ",", 0), "non-zero")
})

test_that("fsquish() trims and collapses Unicode whitespace", {
    x <- c("  12   Smith\tSt  ", "already clean", NA, "", "   ",
           paste0("a", u(0x2003, 0x3000, 0x85, 0x2028), "b ", u(0xa0), " c "),
           "\v\fa\r\nb", paste0(u(0xe9), "  ", u(0xe9)))
    expect_identical(fsquish(x),
                     c("12 Smith St", "already clean", NA, "", "", "a b c", "a b",
                       paste0(u(0xe9), " ", u(0xe9))))
    expect_identical(fsquish(c(n = " a ")), c(n = "a"))
    big <- rep(x, 200)
    expect_identical(fsquish(big, nthreads = 1), fsquish(big))
})
