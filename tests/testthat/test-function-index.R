exported_functions <- function() {
    ns <- readLines(system.file("NAMESPACE", package = "fast.string"))
    sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
}

test_that("the function index lists every exported function exactly once", {
    indexed <- names(fast.string:::.index_functions())

    expect_identical(anyDuplicated(indexed), 0L)
    expect_setequal(indexed, exported_functions())
})

test_that("descriptions avoid brackets, which roxygen reads as markdown links", {
    descriptions <- unname(fast.string:::.index_functions())

    expect_false(any(grepl("[][]", descriptions)))
})

test_that("tree lines nest categories, close each level and align per group", {
    index <- list(
        A = list(short = "one", muchlongername = "two"),
        B = list(x = "three")
    )

    expect_identical(
        fast.string:::.index_tree_lines(index, utf8 = FALSE),
        c(
            "+- A",
            paste0("|  +- short()", strrep(" ", 11), "one"),
            "|  \\- muchlongername()  two",
            "\\- B",
            "   \\- x()  three"
        )
    )
})

test_that("tree lines use box-drawing glyphs when the console is UTF-8", {
    lines <- fast.string:::.index_tree_lines(list(A = list(f = "d")), utf8 = TRUE)

    expect_identical(lines[[1L]], "\u2514\u2500 A")
    expect_identical(lines[[2L]], "   \u2514\u2500 f()  d")
})

test_that("banner lines fit in 80 columns with either glyph set", {
    for (utf8 in c(TRUE, FALSE)) {
        banner <- fast.string:::.index_banner("0.3.0", utf8 = utf8)
        expect_lte(max(cli::ansi_nchar(banner, type = "width")), 80L)
    }
})

test_that("banner has a versioned header, the tree and a suppression hint", {
    banner <- fast.string:::.index_banner("1.2.3", utf8 = FALSE)

    expect_match(banner[[1L]], "^fast.string 1.2.3: ")
    expect_match(banner[[2L]], "+- Matching & substitution", fixed = TRUE)
    expect_match(banner[[length(banner)]], "fast.string.verbose = FALSE", fixed = TRUE)
})

test_that("colour never changes the visible text or the alignment", {
    old <- options(cli.num_colors = 1L)
    on.exit(options(old), add = TRUE)
    plain <- fast.string:::.index_banner("0.3.0")

    options(cli.num_colors = 256L)
    coloured <- fast.string:::.index_banner("0.3.0")

    expect_false(any(cli::ansi_has_any(plain)))
    expect_true(all(cli::ansi_has_any(coloured)))
    expect_identical(cli::ansi_strip(coloured), as.character(plain))
})

test_that("attaching prints the tree unless fast.string.verbose is FALSE", {
    old <- options(fast.string.verbose = TRUE)
    on.exit(options(old), add = TRUE)
    expect_message(
        fast.string:::.onAttach("", "fast.string"),
        "Matching & substitution", fixed = TRUE
    )

    options(fast.string.verbose = FALSE)
    expect_silent(fast.string:::.onAttach("", "fast.string"))
})

test_that("the markdown index links every function for the help page", {
    md <- fast.string:::.index_markdown()

    expect_identical(
        sum(grepl("^ *\\* \\[[^]]+\\(\\)\\] - ", md)),
        length(fast.string:::.index_functions())
    )
    expect_match(md[[1L]], "* **Matching & substitution**", fixed = TRUE)
})
