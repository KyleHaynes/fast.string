#' Vectorised Jaro-Winkler similarity.
#'
#' @param a,b Equal-length character vectors.
#' @param p Prefix scaling factor (default 0.1, the standard value).
#' @return Numeric vector of similarities in [0, 1].
#' @export
jaro_winkler <- function(a, b, p = 0.1, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    fast_jaro_winkler_impl(a, b, as.double(p))
}

#' Jaro-Winkler all-pairs similarity matrix.
#'
#' @param a Character vector of length n (rows).
#' @param b Character vector of length m (columns).
#' @param p Prefix scaling factor (default 0.1).
#' @return Numeric matrix of dimensions n × m.
#' @export
jaro_winkler_matrix <- function(a, b, p = 0.1, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    fast_jaro_winkler_matrix_impl(a, b, as.double(p))
}
