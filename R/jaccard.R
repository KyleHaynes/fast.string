#' Vectorised q-gram Jaccard similarity
#'
#' The Jaccard index of two strings' q-gram sets: split each string into
#' overlapping length-`q` substrings, then `|intersection| / |union|` of the
#' two sets. Unlike [jaro_winkler()], this is symmetric in token order and
#' insensitive to where a difference occurs, which makes it a useful second
#' signal alongside edit-distance-style metrics for fuzzy matching.
#'
#' The q-gram set is built and deduplicated as packed integers rather than
#' allocated strings (for `q <= 8`, which covers the overwhelming majority of
#' use), and compared via sorted-merge intersection instead of a hash table —
#' faster than `stringdist::stringdist(method = "jaccard")` for the same
#' reason [jaro_winkler()] is faster than `stringdist`'s Jaro-Winkler: no
#' per-pair R-level dispatch, and the whole vector runs in parallel across
#' CPU cores via Intel TBB (through RcppParallel).
#'
#' @param a,b Equal-length character vectors.
#' @param q Q-gram length (default `2`, i.e. bigrams — the common default).
#'   Must be a single integer >= 1.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Numeric vector of similarities in `[0, 1]`, `length(a)` long.
#'   `NA` if either `a[i]` or `b[i]` is `NA`. Two strings shorter than `q`
#'   (so neither has any q-grams) compare equal (`1`).
#' @seealso [jaccard_matrix()], [jaro_winkler()]
#' @examples
#' jaccard_index("night", "nacht")
#' jaccard_index(c("Kyle Haynes", "John Smith"), c("Kyle Haynes", "Jon Smith"))
#' @export
jaccard_index <- function(a, b, q = 2, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    if (!is.numeric(q) || length(q) != 1L || q < 1)
        stop("`q` must be a single integer >= 1.")
    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    fast_jaccard_impl(a, b, as.integer(q))
}

#' Q-gram Jaccard all-pairs similarity matrix
#'
#' @param a Character vector of length n (rows).
#' @param b Character vector of length m (columns).
#' @param q Q-gram length (default `2`).
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Numeric matrix of dimensions n × m.
#' @seealso [jaccard_index()]
#' @export
jaccard_matrix <- function(a, b, q = 2, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (!is.numeric(q) || length(q) != 1L || q < 1)
        stop("`q` must be a single integer >= 1.")
    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    fast_jaccard_matrix_impl(a, b, as.integer(q))
}
