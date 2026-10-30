#' Fast parallel match extraction
#'
#' `fextract()` returns the first match of `pattern` in each string, and
#' `fextract_all()` every non-overlapping match. They replace
#' `regmatches(x, regexpr(pattern, x))` and
#' `regmatches(x, gregexpr(pattern, x))` (or `stringr::str_extract()` and
#' `stringr::str_extract_all()`), using the PCRE2 engine of [fgrepl()] and
#' parallelised across CPU cores via Intel TBB.
#'
#' Patterns follow the same rules as [fgrepl()], including how
#' `perl = FALSE` patterns are interpreted. After an empty match the search
#' moves on by one character, as in [fcount()].
#'
#' @inheritParams fgrepl
#' @param group Non-negative integer. `0` (the default) extracts the whole
#'   match; `k` extracts capture group `k` of it, as `stringr::str_extract()`
#'   does with `group = k`. A group that did not take part in the match gives
#'   `NA`. Must be `0` when `fixed = TRUE`.
#' @return `fextract()` returns a character vector the same length as `x`,
#'   with `NA` where `x` is `NA` or has no match. `fextract_all()` returns a
#'   list the same length as `x` of character vectors: `character(0)` for no
#'   match and `NA_character_` for a missing string. Both keep `names(x)`.
#' @seealso [base::regmatches()], [fcount()] to count matches,
#'   [fextract_any()] to extract any of many literal strings.
#' @family matching and substitution functions
#' @examples
#' x <- c("Unit 4, 12 Smith St", "PO Box 99", NA, "no numbers")
#'
#' fextract("[0-9]+", x)
#' fextract_all("[0-9]+", x)
#' fextract("([0-9]+) ([A-Za-z]+) St", x, group = 2)
#' @export
fextract <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE, group = 0L,
                     nthreads = NULL) {
    args <- .extract_args(pattern, x, perl, fixed, useBytes, group, nthreads)
    if (is.na(args$pattern))
        return(.copy_names(rep(NA_character_, length(args$x)), x))
    .copy_names(fast_extract_impl(
        args$pattern, args$x, isTRUE(ignore.case), args$syntax,
        isTRUE(fixed), args$use_bytes, args$group, args$threads
    ), x)
}

#' @rdname fextract
#' @export
fextract_all <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE, group = 0L,
                         nthreads = NULL) {
    args <- .extract_args(pattern, x, perl, fixed, useBytes, group, nthreads)
    if (is.na(args$pattern))
        return(.copy_names(as.list(rep(NA_character_, length(args$x))), x))
    .copy_names(fast_extract_all_impl(
        args$pattern, args$x, isTRUE(ignore.case), args$syntax,
        isTRUE(fixed), args$use_bytes, args$group, args$threads
    ), x)
}

.extract_args <- function(pattern, x, perl, fixed, useBytes, group, nthreads) {
    pattern <- .na_as_character(pattern)
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    if (!is.numeric(group) || length(group) != 1L || is.na(group) ||
        group < 0 || group != floor(group) || group > .Machine$integer.max)
        stop("`group` must be a single non-negative integer.")
    x <- .character_x(x)
    threads <- .as_nthreads(nthreads)
    if (is.na(pattern)) return(list(pattern = pattern, x = x))
    spec <- .regex_spec(pattern, perl, fixed, useBytes)
    list(pattern = spec$pattern, x = x, syntax = spec$syntax,
         use_bytes = spec$use_bytes, group = as.integer(group),
         threads = threads)
}

#' Fast parallel field extraction from delimited strings
#'
#' Splits each string at `split` and returns field number `field`, like
#' `stringr::str_split_i()`, DuckDB's `split_part()` or
#' `sapply(strsplit(x, split), "[", field)`, but without building the
#' intermediate list. Strings are processed in parallel across CPU cores via
#' Intel TBB.
#'
#' Every field counts, including empty ones: `"a,,b"` has three fields and
#' `"a,b,"` ends with an empty one. (`strsplit()` drops a trailing empty
#' field.)
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param split Character scalar: the separator. A regular expression unless
#'   `fixed = TRUE`; it must not be able to match the empty string.
#' @param field Non-zero integer scalar. Positive values count fields from
#'   the start (`1` is the first); negative values count from the end (`-1`
#'   is the last).
#' @param fixed,perl,ignore.case,useBytes As in [fgrepl()].
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept and
#'   `NA` where the string has fewer fields.
#' @seealso [base::strsplit()]
#' @family string utilities
#' @examples
#' x <- c("2024-03-15", "1999-12-31", "bad", NA)
#' fsplit_field(x, "-", 1, fixed = TRUE)
#' fsplit_field(x, "-", -1, fixed = TRUE)   # last field
#' fsplit_field("a  b   c", "\\s+", 2)      # regular expression
#' @export
fsplit_field <- function(x, split, field, fixed = FALSE, perl = FALSE,
                         ignore.case = FALSE, useBytes = FALSE,
                         nthreads = NULL) {
    split <- .na_as_character(split)
    if (!is.character(split) || length(split) != 1L)
        stop("`split` must be a single character string.")
    if (!is.numeric(field) || length(field) != 1L || is.na(field) ||
        field == 0 || field != floor(field) ||
        abs(field) > .Machine$integer.max)
        stop("`field` must be a single non-zero integer.")
    x <- .character_x(x)
    threads <- .as_nthreads(nthreads)
    if (is.na(split)) return(.copy_names(rep(NA_character_, length(x)), x))
    spec <- .regex_spec(split, perl, fixed, useBytes)
    .copy_names(fast_split_field_impl(
        spec$pattern, x, as.double(field), isTRUE(ignore.case), spec$syntax,
        isTRUE(fixed), spec$use_bytes, threads
    ), x)
}

#' Fast parallel string splitting
#'
#' Splits every string at a separator, as [base::strsplit()] does, but
#' finds the separators in parallel across CPU cores via Intel TBB. The
#' separator is a literal string, a regular expression matched with PCRE2,
#' or `""` to cut between characters. Use [fsplit_field()] instead when
#' only one field is wanted: it does not build the other pieces at all.
#'
#' @section Where the time goes:
#' Only the search is parallel. Creating the result strings is serial,
#' because R's global string cache is -- the constraint [fsubstr()] also
#' documents -- and it is most of the work. So for a literal separator of
#' one or two characters the search is nearly free, base R is already at
#' that floor, and this function is no faster (on a 400,000-string vector
#' of short addresses it is about twice as slow). It pays off where the
#' search is the expensive part, or where the list is: a regular
#' expression separator runs about 2x faster than [base::strsplit()], and
#' `simplify = TRUE` about 4x faster than `do.call(rbind, strsplit(...))`,
#' which allocates every small vector before copying it away again.
#'
#' Fields are cut exactly where [base::strsplit()] cuts them, including the
#' corners: a trailing empty field is dropped (`"a,b,"` gives two fields),
#' a leading one is kept (`",a"` gives `""` and `"a"`), an empty string
#' gives `character(0)`, and `NA` gives `NA_character_`. A `split` that can
#' match the empty string -- `"x*"`, `"\\b"` -- is an error rather than
#' silently doing something different from base R; `split = ""` is the one
#' supported case, and it splits into characters, or into bytes under
#' `useBytes = TRUE`.
#'
#' With `simplify = TRUE` the result is a character matrix with one row per
#' string and one column per field, which is what `do.call(rbind, ...)` on
#' the list would give, but without the copying. Rows with fewer fields are
#' padded with `NA`, so a ragged split does not recycle silently as
#' `rbind()` would.
#'
#' @param x Character vector. `NA` elements give `NA_character_`.
#' @param split A single string: the separator. A literal string when
#'   `fixed = TRUE`, otherwise a regular expression; `""` cuts between
#'   characters.
#' @param simplify Logical. Return a character matrix padded with `NA`
#'   instead of a list.
#' @param fixed,perl,ignore.case,useBytes As in [fgrepl()].
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return A list of character vectors the same length as `x`, with
#'   `names(x)` kept; or, with `simplify = TRUE`, a character matrix whose
#'   rows are named by `names(x)`.
#' @seealso [fsplit_field()] for one field, [base::strsplit()],
#'   [freplace_tokens()] to rewrite whole words in place.
#' @family string utilities
#' @examples
#' x <- c("12 Smith St", "4/7 Bay Rd", NA)
#' fstrsplit(x, " ", fixed = TRUE)
#' fstrsplit(x, " ", fixed = TRUE, simplify = TRUE)
#'
#' # Regular expressions, and splitting into characters.
#' fstrsplit("a  b   c", "\\s+")
#' fstrsplit("abc", "")
#'
#' # Base R's corner cases are kept.
#' fstrsplit(c("a,b,", ",a", "", NA), ",", fixed = TRUE)
#' @export
fstrsplit <- function(x, split, simplify = FALSE, fixed = FALSE,
                      perl = FALSE, ignore.case = FALSE, useBytes = FALSE,
                      nthreads = NULL) {
    split <- .na_as_character(split)
    if (!is.character(split) || length(split) != 1L)
        stop("`split` must be a single character string.")
    x <- .character_x(x)
    .check_flag(simplify, "simplify")
    if (is.na(split))
        stop("`split` must not be `NA`.")
    spec <- .regex_spec(split, perl, fixed, useBytes)
    result <- fast_strsplit_impl(
        spec$pattern, x, isTRUE(ignore.case), spec$syntax, isTRUE(fixed),
        spec$use_bytes, simplify, .as_nthreads(nthreads)
    )
    if (simplify) {
        if (!is.null(names(x))) dimnames(result) <- list(names(x), NULL)
        return(result)
    }
    .copy_names(result, x)
}

#' Fast whitespace squishing
#'
#' Removes whitespace from both ends of each string and collapses every
#' internal run of whitespace to a single space, as `stringr::str_squish()`
#' does. Whitespace is Unicode's `White_Space` property: the ASCII space,
#' tab, line feed, vertical tab, form feed and carriage return, plus the
#' no-break space (U+00A0), U+0085, U+1680, U+2000 to U+200A, the line and
#' paragraph separators (U+2028, U+2029), U+202F, U+205F and the ideographic
#' space (U+3000). Strings marked as `"bytes"` only have ASCII whitespace
#' squished.
#'
#' Runs in parallel across CPU cores via Intel TBB; strings that need no
#' change are returned without being copied.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [ftrimws()] to trim the ends only.
#' @family string utilities
#' @examples
#' fsquish(c("  12   Smith\tSt  ", "already clean", NA))
#' @export
fsquish <- function(x, nthreads = NULL) {
    x <- .character_x(x)
    .copy_names(fast_squish_impl(x, .as_nthreads(nthreads)), x)
}
