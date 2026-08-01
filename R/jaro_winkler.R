#' Vectorised Jaro-Winkler similarity.
#'
#' @param a,b Equal-length character vectors.
#' @param p Prefix scaling factor (default 0.1, the standard value).
#' @param nthreads Integer thread cap, or `NULL` to use the
#'   RcppParallel default for this call.
#' @return Numeric vector of similarities between 0 and 1.
#' @param use_bytes Logical scalar. Compare encoded bytes when `TRUE` (the
#'   compatibility default), or UTF-8 code points when `FALSE`.
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

#' Jaro-Winkler all-pairs similarity matrix.
#'
#' @param a Character vector of length n (rows).
#' @param b Character vector of length m (columns).
#' @param p Prefix scaling factor (default 0.1).
#' @param nthreads Integer thread cap, or `NULL` to use the
#'   RcppParallel default for this call.
#' @return Numeric matrix with n rows and m columns.
#' @inheritParams jaro_winkler
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
