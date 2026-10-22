#' Search for many literal strings at once
#'
#' These functions look for any of `patterns` -- literal strings, such as a
#' list of keywords, place names or offence descriptions -- in every element
#' of `x`, in a single pass over each string with an Aho-Corasick automaton,
#' parallelised across CPU cores via Intel TBB. The cost barely depends on
#' how many patterns there are, so thousands of keywords take about as long
#' as a few, where looping [fgrepl()] over the keywords, or one long
#' `"kw1|kw2|..."` regular expression, slows down with every keyword added.
#'
#' * `fgrepl_any()`: does the string contain any of the patterns?
#' * `fwhich_any()`: the index of the first pattern (in the order of
#'   `patterns`) that occurs anywhere in the string. With patterns ordered
#'   by priority this assigns each string to a category, as a chain of
#'   `if`/`else` keyword tests would.
#' * `fcount_any()`: the number of non-overlapping matches.
#' * `fextract_any()`: the first match (or, with `all = TRUE`, every
#'   non-overlapping match) as it appears in the string.
#'
#' Matches are found left to right; where several patterns match at the same
#' position, the one that comes first in `patterns` wins, as with
#' [gsub_all()]`(fixed = TRUE, sequential = FALSE)` and as with a regular
#' expression alternation. The search then continues after the match.
#'
#' @param patterns Character vector of literal strings to search for; no
#'   `NA` or empty strings. Duplicates are allowed.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Fold ASCII letters, as [fgrepl()] does with
#'   `fixed = TRUE`.
#' @param useBytes Logical. Match bytes rather than UTF-8 text, as in
#'   [fgrepl()].
#' @param all Logical. For `fextract_any()`: return every match (a list)
#'   rather than the first (a character vector).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return `fgrepl_any()` a logical vector, `fwhich_any()` and `fcount_any()`
#'   integer vectors, and `fextract_any()` a character vector (`NA` where
#'   nothing matches) or, with `all = TRUE`, a list of character vectors. All
#'   are the same length as `x`, with `names(x)` kept.
#' @seealso [fgrepl()] and [fextract()] for one pattern, [gsub_all()] to
#'   replace many literal strings in one pass.
#' @family matching and substitution functions
#' @examples
#' notes <- c("stolen bicycle", "break and enter, tools taken",
#'            "graffiti on wall", NA, "assault")
#' keywords <- c("stolen", "taken", "break and enter", "graffiti")
#'
#' fgrepl_any(keywords, notes)
#' fwhich_any(keywords, notes)
#' fcount_any(keywords, notes)
#' fextract_any(keywords, notes)
#' fextract_any(keywords, notes, all = TRUE)
#' @name fgrepl_any
NULL

.any_args <- function(patterns, x, useBytes) {
    if (!is.character(patterns) || length(patterns) == 0L)
        stop("`patterns` must be a non-empty character vector.")
    if (anyNA(patterns)) stop("`patterns` must not contain NA.")
    x <- .character_x(x)
    spec <- .regex_spec(patterns, fixed = TRUE, useBytes = useBytes)
    list(patterns = spec$pattern, x = x, use_bytes = spec$use_bytes)
}

.any_run <- function(patterns, x, ignore.case, useBytes, mode, nthreads) {
    args <- .any_args(patterns, x, useBytes)
    .copy_names(fast_any_impl(
        args$patterns, args$x, isTRUE(ignore.case), args$use_bytes, mode,
        .as_nthreads(nthreads)
    ), x)
}

#' @rdname fgrepl_any
#' @export
fgrepl_any <- function(patterns, x, ignore.case = FALSE, useBytes = FALSE,
                       nthreads = NULL) {
    .any_run(patterns, x, ignore.case, useBytes, 0L, nthreads)
}

#' @rdname fgrepl_any
#' @export
fcount_any <- function(patterns, x, ignore.case = FALSE, useBytes = FALSE,
                       nthreads = NULL) {
    .any_run(patterns, x, ignore.case, useBytes, 1L, nthreads)
}

#' @rdname fgrepl_any
#' @export
fwhich_any <- function(patterns, x, ignore.case = FALSE, useBytes = FALSE,
                       nthreads = NULL) {
    .any_run(patterns, x, ignore.case, useBytes, 2L, nthreads)
}

#' @rdname fgrepl_any
#' @export
fextract_any <- function(patterns, x, all = FALSE, ignore.case = FALSE,
                         useBytes = FALSE, nthreads = NULL) {
    if (!is.logical(all) || length(all) != 1L || is.na(all))
        stop("`all` must be TRUE or FALSE.")
    args <- .any_args(patterns, x, useBytes)
    .copy_names(fast_extract_any_impl(
        args$patterns, args$x, isTRUE(ignore.case), args$use_bytes, all,
        .as_nthreads(nthreads)
    ), x)
}
