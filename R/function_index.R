# Single source of truth for the categorised function index. It drives the
# startup banner (.onAttach), the "Function index" section of ?fast.string
# (via @eval in fast.string-package.R), and a test that keeps it in step with
# NAMESPACE.
#
# A node is a named list. An element that is a string is a function (name ->
# short description); an element that is itself a list is a sub-category.
# Keep descriptions under ~36 characters so banner lines fit in 80 columns.
.function_index <- list(
    "Matching & substitution" = list(
        fgrepl   = "grepl() equivalent",
        fgrep    = "grep() equivalent",
        fcount   = "count matches per string",
        fsub     = "sub() equivalent",
        fgsub    = "gsub() equivalent",
        gsub_all = "many patterns in one pass"
    ),
    "String utilities" = list(
        ftrimws = "trimws() equivalent",
        fsubstr = "substr() equivalent",
        fnchar  = "nchar() equivalent",
        fchartr = "chartr() equivalent"
    ),
    "Dates & timestamps" = list(
        "Parse" = list(
            fas.Date      = "parse fixed-format dates",
            fas.POSIXct   = "parse fixed-format timestamps"
        ),
        "Format" = list(
            format_date     = "format dates",
            format_datetime = "format timestamps"
        ),
        "Parts" = list(
            date_parts        = "split a Date into y/m/d columns",
            format_date_parts = "join y/m/d fields into a string"
        )
    ),
    "Phonetic codes" = list(
        soundex          = "American Soundex",
        refined_soundex  = "Refined Soundex (finer blocks)",
        nysiis           = "NYSIIS",
        cologne          = "Cologne (German pronunciation)",
        double_metaphone = "Double Metaphone (two codes)",
        caverphone       = "Caverphone 2.0 (NZ/AU names)"
    ),
    "String similarity" = list(
        "Jaro-Winkler" = list(
            jaro_winkler         = "pairwise similarity",
            jaro_winkler_matrix  = "all-pairs similarity matrix",
            jaro_winkler_tokens  = "word-order-tolerant similarity"
        ),
        "Edit distance" = list(
            levenshtein                    = "insert/delete/substitute edits",
            levenshtein_similarity         = "normalised 0-1 similarity",
            levenshtein_matrix             = "all-pairs distance matrix",
            levenshtein_within             = "TRUE if distance <= cutoff",
            osa_distance                   = "adds adjacent transpositions",
            osa_similarity                 = "normalised 0-1 similarity",
            osa_distance_matrix            = "all-pairs distance matrix",
            damerau_levenshtein            = "unrestricted transpositions",
            damerau_levenshtein_similarity = "normalised 0-1 similarity",
            damerau_levenshtein_matrix     = "all-pairs distance matrix",
            hamming                        = "count of differing positions"
        ),
        "Q-gram overlap" = list(
            jaccard_index     = "Jaccard set overlap",
            jaccard_matrix    = "all-pairs matrix",
            dice_coefficient  = "Dice set overlap",
            dice_matrix       = "all-pairs matrix",
            tversky_index     = "asymmetric set overlap",
            tversky_matrix    = "all-pairs matrix",
            cosine_similarity = "frequency-weighted overlap",
            cosine_matrix     = "all-pairs matrix"
        ),
        "fuzzywuzzy ratios" = list(
            fuzz_ratio            = "overall ratio (0-100)",
            fuzz_partial_ratio    = "best-substring ratio",
            fuzz_token_sort_ratio = "ignores word order",
            fuzz_token_set_ratio  = "ignores extra/duplicate words"
        )
    ),
    "Fuzzy lookup" = list(
        fuzzy_match = "best table match per query",
        fuzzy_top_n = "top-N matches, no full matrix"
    )
)

# Flatten the index to a named character vector of function -> description.
.index_functions <- function(node = .function_index) {
    out <- lapply(seq_along(node), function(i) {
        if (is.list(node[[i]])) return(.index_functions(node[[i]]))
        fn <- node[[i]]
        names(fn) <- names(node)[i]
        fn
    })
    unlist(out)
}

.tree_glyphs <- function(utf8) {
    if (utf8) {
        c(tee = "\u251c\u2500 ", last = "\u2514\u2500 ",
          pipe = "\u2502  ", blank = "   ")
    } else {
        c(tee = "+- ", last = "\\- ", pipe = "|  ", blank = "   ")
    }
}

# Draw one node of the index as `tree`-style lines. Descriptions are aligned
# within each group of sibling functions, so short names stay close to their
# text even when another group has very long names.
#
# Widths are measured on the plain text and colour is applied afterwards, so
# ANSI codes never disturb the alignment. cli emits no codes at all when the
# console has no colour support (piped output, NO_COLOR, Rgui, ...).
.index_tree_lines <- function(node = .function_index,
                              utf8 = cli::is_utf8_output(),
                              prefix = "") {
    glyphs <- .tree_glyphs(utf8)
    n <- length(node)
    is_fn <- !vapply(node, is.list, logical(1L))
    branch <- rep(glyphs[["tee"]], n)
    branch[n] <- glyphs[["last"]]
    tree <- paste0(prefix, branch)
    name <- paste0(names(node), ifelse(is_fn, "()", ""))
    width <- nchar(tree, type = "width") + nchar(name, type = "width")
    pad <- if (any(is_fn)) max(width[is_fn]) else 0L
    # Top-level categories (empty prefix) are bold cyan, nested ones plain cyan.
    category <- if (nzchar(prefix)) cli::col_cyan
                else cli::combine_ansi_styles("bold", "cyan")

    lines <- character()
    for (i in seq_len(n)) {
        if (is_fn[i]) {
            lines <- c(lines, paste0(
                cli::col_grey(tree[i]), cli::col_green(name[i]),
                strrep(" ", pad - width[i]), "  ", node[[i]]
            ))
        } else {
            child_prefix <- paste0(
                prefix, glyphs[[if (i == n) "blank" else "pipe"]]
            )
            lines <- c(
                lines, paste0(cli::col_grey(tree[i]), category(name[i])),
                .index_tree_lines(node[[i]], utf8, child_prefix)
            )
        }
    }
    lines
}

.index_banner <- function(version, utf8 = cli::is_utf8_output()) {
    c(
        paste0(
            cli::style_bold(paste("fast.string", version)),
            ": parallel string, date & fuzzy-matching functions"
        ),
        .index_tree_lines(.function_index, utf8),
        cli::col_grey(
            "Help: ?fast.string  |  Silence: options(fast.string.verbose = FALSE)"
        )
    )
}

# The same index as nested markdown bullets, for the ?fast.string help page.
.index_markdown <- function(node = .function_index, depth = 0L) {
    indent <- strrep("  ", depth)
    unlist(lapply(seq_along(node), function(i) {
        if (is.list(node[[i]])) {
            label <- if (depth == 0L) "**%s**" else "*%s*"
            c(sprintf(paste0("%s* ", label), indent, names(node)[i]),
              .index_markdown(node[[i]], depth + 1L))
        } else {
            sprintf("%s* [%s()] - %s", indent, names(node)[i], node[[i]])
        }
    }), use.names = FALSE)
}

# Roxygen lines for the "Function index" section of ?fast.string, inserted by
# `@eval` in fast.string-package.R so the help page is built from the same
# registry as the startup banner.
.index_roxygen <- function() {
    c("@section Function index:", .index_markdown())
}
