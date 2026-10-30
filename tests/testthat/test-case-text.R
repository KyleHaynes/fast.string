u <- function(...) intToUtf8(c(...))

test_that("fupper and flower handle ASCII", {
    x <- c("Smith", "o'brien", "MiXeD 123", "", NA)
    expect_identical(fast.string::fupper(x), toupper(x))
    expect_identical(fast.string::flower(x), tolower(x))
})

test_that("fupper and flower apply the full Unicode mappings", {
    # One character can map to several, which base::toupper() cannot do.
    expect_identical(fast.string::fupper(u(0xDF)), "SS")
    expect_identical(fast.string::fupper(u(0xFB01)), "FI")
    expect_identical(fast.string::fupper(u(0x149)), u(0x2BC, 0x4E))
    expect_identical(fast.string::flower(u(0x130)), u(0x69, 0x307))
    expect_identical(fast.string::fupper(u(0xE9)), u(0xC9))
    expect_identical(fast.string::flower(u(0xC9)), u(0xE9))
    # Title case differs from upper case for the digraphs.
    expect_identical(fast.string::fupper(u(0x1C5)), u(0x1C4))
    expect_identical(fast.string::ftitle_case(u(0x1C6)), u(0x1C5))
})

test_that("flower applies Unicode's Final_Sigma rule", {
    expect_identical(fast.string::flower(u(0x39F, 0x394, 0x39F, 0x3A3)),
                     u(0x3BF, 0x3B4, 0x3BF, 0x3C2))
    # Not at the end of a word, so the medial form.
    expect_identical(fast.string::flower(u(0x3A3, 0x39F, 0x3A3)),
                     u(0x3C3, 0x3BF, 0x3C2))
    # Nothing cased before it, so the medial form again.
    expect_identical(fast.string::flower(u(0x3A3)), u(0x3C3))
    expect_identical(fast.string::fupper(u(0x3C2)), u(0x3A3))
})

test_that("case conversion is locale-independent", {
    # Not the Turkish tailoring, whatever the session's locale.
    expect_identical(fast.string::fupper(u(0x131)), "I")
    expect_identical(fast.string::flower("I"), "i")
})

test_that("fupper and flower agree with stringi's root locale", {
    skip_if_not_installed("stringi")
    cps <- c(0x41:0x7A, 0xC0:0x2FF, 0x370:0x3FF, 0x400:0x52F, 0x1E00:0x1EFF,
             0x1F00:0x1FFF, 0x2C60:0x2C7F)
    x <- vapply(cps, intToUtf8, "")
    for (pair in list(
        list(fast.string::fupper(x), stringi::stri_trans_toupper(x, locale = "root")),
        list(fast.string::flower(x), stringi::stri_trans_tolower(x, locale = "root"))
    )) {
        ours <- pair[[1]]
        theirs <- pair[[2]]
        # Where the two differ, this package's table (a newer Unicode) has a
        # mapping that stringi's ICU does not have at all.
        differs <- ours != theirs
        expect_true(all(theirs[differs] == x[differs]))
    }
})

test_that("fupper and flower keep names, NA and non-character input", {
    expect_identical(names(fast.string::fupper(c(a = "x", b = "y"))), c("a", "b"))
    expect_identical(fast.string::fupper(NA_character_), NA_character_)
    expect_identical(fast.string::flower(NA_character_), NA_character_)
    expect_identical(fast.string::fupper(factor("abc")), "ABC")
    expect_identical(fast.string::fupper(character(0)), character(0))
})

test_that("case conversion is idempotent and thread-independent", {
    x <- rep(c("mary-jane o'brien", u(0xDF), "VAN DER BERG", u(0x39F, 0x3A3)),
             20000)
    expect_identical(fast.string::fupper(fast.string::fupper(x)),
                     fast.string::fupper(x))
    expect_identical(fast.string::flower(fast.string::flower(x)),
                     fast.string::flower(x))
    expect_identical(fast.string::fupper(x, nthreads = 1L),
                     fast.string::fupper(x))
    expect_identical(fast.string::ftitle_case(x, nthreads = 1L),
                     fast.string::ftitle_case(x))
})

test_that("ftitle_case capitalises one letter per word", {
    expect_identical(fast.string::ftitle_case("the quick brown fox"),
                     "The Quick Brown Fox")
    expect_identical(fast.string::ftitle_case("MARY-JANE SMITH"),
                     "Mary-Jane Smith")
    # An apostrophe stays inside its word, a hyphen separates words.
    expect_identical(fast.string::ftitle_case("don't tread", style = "simple"),
                     "Don't Tread")
    expect_identical(fast.string::ftitle_case("jean-luc picard"),
                     "Jean-Luc Picard")
    # A word that starts with a digit keeps its letters lower case.
    expect_identical(fast.string::ftitle_case("3rd street"), "3rd Street")
    # Separators are copied through untouched.
    expect_identical(fast.string::ftitle_case("  a,b\t(c)  "),
                     "  A,B\t(C)  ")
    expect_identical(fast.string::ftitle_case(c("", NA)), c("", NA))
})

test_that("ftitle_case name rules capitalise after Mc and an apostrophe", {
    expect_identical(fast.string::ftitle_case("mcdonald"), "McDonald")
    expect_identical(fast.string::ftitle_case("MCKINNON"), "McKinnon")
    expect_identical(fast.string::ftitle_case("o'brien-smith"), "O'Brien-Smith")
    expect_identical(fast.string::ftitle_case("d'arcy"), "D'Arcy")
    # Mac is left alone: too many names start with those letters.
    expect_identical(fast.string::ftitle_case("machado"), "Machado")
    # Only a one-letter prefix takes the apostrophe rule.
    expect_identical(fast.string::ftitle_case("don't"), "Don't")
    # The curly apostrophe works the same way.
    expect_identical(fast.string::ftitle_case(paste0("o", u(0x2019), "brien")),
                     paste0("O", u(0x2019), "Brien"))
    expect_identical(fast.string::ftitle_case("mcdonald", style = "simple"),
                     "Mcdonald")
})

test_that("ftitle_case lower-cases particles between words only", {
    expect_identical(fast.string::ftitle_case("vincent van gogh"),
                     "Vincent van Gogh")
    expect_identical(fast.string::ftitle_case("van gogh, vincent"),
                     "Van Gogh, Vincent")
    expect_identical(fast.string::ftitle_case("juan de la cruz"),
                     "Juan de la Cruz")
    # A middle initial is never a particle: every entry is two letters or more.
    expect_identical(fast.string::ftitle_case("john e smith"),
                     "John E Smith")
    expect_identical(fast.string::ftitle_case("van der berg"),
                     "Van der Berg")
})

test_that("ftitle_case takes a particle list of its own", {
    expect_identical(
        fast.string::ftitle_case("juan de la cruz", particles = c("de", "la")),
        "Juan de la Cruz"
    )
    expect_identical(
        fast.string::ftitle_case("juan de la cruz", particles = character(0)),
        "Juan De La Cruz"
    )
    # Matching ignores case.
    expect_identical(
        fast.string::ftitle_case("vincent van gogh", particles = "VAN"),
        "Vincent van Gogh"
    )
    expect_error(
        fast.string::ftitle_case("x", style = "simple", particles = "de"),
        "applies to"
    )
    expect_error(fast.string::ftitle_case("x", particles = NA_character_),
                 "without `NA`")
})

test_that("ftitle_case upper-cases unambiguous Roman suffixes", {
    expect_identical(fast.string::ftitle_case("john smith iii"),
                     "John Smith III")
    expect_identical(fast.string::ftitle_case("henry viii"), "Henry VIII")
    # Numerals that are also names or words are left alone.
    expect_identical(fast.string::ftitle_case("tom mix"), "Tom Mix")
    expect_identical(fast.string::ftitle_case("dorothea dix"), "Dorothea Dix")
    # A numeral as the first word is a name, not a suffix.
    expect_identical(fast.string::ftitle_case("ii naomasa"), "Ii Naomasa")
    expect_identical(fast.string::ftitle_case("iii"), "Iii")
    expect_identical(fast.string::ftitle_case("john smith iii",
                                              style = "simple"),
                     "John Smith Iii")
})

test_that("ftitle_case keeps names and converts non-character input", {
    expect_identical(names(fast.string::ftitle_case(c(a = "bc"))), "a")
    expect_identical(fast.string::ftitle_case(factor("ab cd")), "Ab Cd")
    expect_identical(fast.string::ftitle_case(character(0)), character(0))
})

test_that("html_escape escapes the markup characters", {
    expect_identical(fast.string::html_escape("Fish & Chips"),
                     "Fish &amp; Chips")
    expect_identical(fast.string::html_escape("<b>bold</b>"),
                     "&lt;b&gt;bold&lt;/b&gt;")
    # Quotes and line breaks only in attribute mode.
    expect_identical(fast.string::html_escape('say "hi"'), 'say "hi"')
    expect_identical(fast.string::html_escape('say "hi"', attribute = TRUE),
                     "say &quot;hi&quot;")
    expect_identical(fast.string::html_escape("a'b", attribute = TRUE),
                     "a&#39;b")
    expect_identical(fast.string::html_escape("x\r\ny", attribute = TRUE),
                     "x&#13;&#10;y")
    expect_identical(fast.string::html_escape(c("plain", "", NA)),
                     c("plain", "", NA))
})

test_that("html_escape matches htmltools", {
    skip_if_not_installed("htmltools")
    x <- c("Fish & Chips", "<b>bold</b>", 'say "hi"', "a'b", "x\r\ny",
           "plain", "", "&amp;")
    expect_identical(fast.string::html_escape(x), htmltools::htmlEscape(x))
    expect_identical(fast.string::html_escape(x, attribute = TRUE),
                     htmltools::htmlEscape(x, attribute = TRUE))
})

test_that("html_unescape reverses html_escape", {
    x <- c("1 < 2 & 3 > 2", "<a href=\"x\">&</a>", "no markup")
    expect_identical(fast.string::html_unescape(fast.string::html_escape(x)), x)
    expect_identical(
        fast.string::html_unescape(
            fast.string::html_escape(x, attribute = TRUE)
        ),
        x
    )
})

test_that("html_escape keeps names and validates its flag", {
    expect_identical(names(fast.string::html_escape(c(a = "&"))), "a")
    expect_identical(fast.string::html_escape(character(0)), character(0))
    expect_error(fast.string::html_escape("&", attribute = NA), "TRUE or FALSE")
    expect_identical(fast.string::html_escape(rep("a&b", 20000),
                                              nthreads = 1L),
                     fast.string::html_escape(rep("a&b", 20000)))
})
