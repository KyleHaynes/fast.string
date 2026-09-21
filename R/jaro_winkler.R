#' Jaro-Winkler string similarity
#'
#' Scores how alike two strings are on a `[0, 1]` scale, where `1` is an exact
#' match. Jaro similarity counts matching characters (within a sliding window)
#' and transpositions; the Winkler adjustment then boosts pairs that share a
#' common prefix of up to four characters, which suits names and other short
#' strings where the first characters are the most reliable.
#'
#' `jaro_winkler()` compares `a[i]` with `b[i]`; `jaro_winkler_matrix()`
#' scores every combination of `a` and `b`. Comparison is case-sensitive and
#' does no trimming or normalisation, so clean the inputs first (for example
#' with [ftrimws()] and [toupper()]). Two empty strings score `1`, and an
#' empty string against a non-empty one scores `0`. For strings whose words
#' may be reordered, see [jaro_winkler_tokens()].
#'
#' @param a,b Equal-length character vectors.
#' @param p Prefix scaling factor (default 0.1, the standard value). Keep it at
#'   or below 0.25 so that scores cannot exceed 1.
#' @param nthreads Integer thread cap, or `NULL` to use the
#'   RcppParallel default for this call.
#' @param use_bytes Logical scalar. Compare encoded bytes when `TRUE` (the
#'   compatibility default), or UTF-8 code points when `FALSE`.
#' @return Numeric vector of similarities between 0 and 1; `NA` where either
#'   `a[i]` or `b[i]` is `NA`.
#' @family Jaro-Winkler functions
#' @examples
#' jaro_winkler("MARTHA", "MARHTA")
#' jaro_winkler(c("JOHN", NA, "MARY"), c("JON", "MARIE", "MARIE"))
#'
#' # p = 0 gives plain Jaro similarity, without the common-prefix boost.
#' jaro_winkler("SMITH", "SMYTH", p = 0)
#' jaro_winkler("SMITH", "SMYTH")
#' @export
jaro_winkler <- function(a, b, p = 0.1, nthreads = NULL,
                         use_bytes = TRUE) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    .validate_use_bytes(use_bytes)
    fast_jaro_winkler_impl(
        a, b, as.double(p), .as_nthreads(nthreads), use_bytes
    )
}

#' Jaro-Winkler all-pairs similarity matrix
#'
#' Scores every element of `a` against every element of `b`, the usual shape
#' of a data-linkage comparison table. See [jaro_winkler()] for how the score
#' is defined.
#'
#' @param a Character vector of length n (rows).
#' @param b Character vector of length m (columns).
#' @param p Prefix scaling factor (default 0.1).
#' @param nthreads Integer thread cap, or `NULL` to use the
#'   RcppParallel default for this call.
#' @return Numeric matrix with n rows and m columns; a cell is `NA` where
#'   either input is `NA`.
#' @inheritParams jaro_winkler
#' @family Jaro-Winkler functions
#' @examples
#' round(jaro_winkler_matrix(
#'     c("John Smith", "Mary Jones"),
#'     c("Jon Smith", "Mary-Anne Jones", "Rob Brown")
#' ), 2)
#' @export
jaro_winkler_matrix <- function(a, b, p = 0.1, nthreads = NULL,
                                use_bytes = TRUE) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    .validate_use_bytes(use_bytes)
    fast_jaro_winkler_matrix_impl(
        a, b, as.double(p), .as_nthreads(nthreads), use_bytes
    )
}
