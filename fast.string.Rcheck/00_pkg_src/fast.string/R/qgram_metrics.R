#' Vectorised q-gram set-overlap similarity: Jaccard, Dice, Tversky
#'
#' Three related measures of how much two strings' sets of overlapping
#' length-`q` substrings ("q-grams") have in common. Split each string into
#' its q-gram set, then:
#'
#' * `jaccard_index()` — `|intersection| / |union|`.
#' * `dice_coefficient()` — `2*|intersection| / (|A| + |B|)`, the
#'   Sorensen-Dice coefficient; weights the shared q-grams more heavily than
#'   Jaccard does (it's always >= the Jaccard score for the same pair).
#' * `tversky_index()` — generalises both: `|intersection| / (|intersection|
#'   + alpha*|A only| + beta*|B only|)`. `alpha = beta = 1` reduces to
#'   Jaccard; `alpha = beta = 0.5` reduces to Dice. Asymmetric weights
#'   (`alpha != beta`) are useful when one side is treated as a query and
#'   the other as a reference and the two kinds of mismatch shouldn't be
#'   penalised equally — e.g. extra tokens on the reference side mattering
#'   less than missing tokens from the query.
#'
#' All three are symmetric in token order and insensitive to *where* a
#' difference occurs, which makes them a useful second signal alongside
#' edit-distance-style metrics like [jaro_winkler()] for fuzzy matching.
#'
#' The q-gram set is built and deduplicated as packed integers rather than
#' allocated strings (for `q <= 8`, which covers the overwhelming majority of
#' use), and the three measures share one sorted-merge intersection pass —
#' faster than `stringdist::stringdist(method = "jaccard")` for the same
#' reason [jaro_winkler()] is faster than `stringdist`'s Jaro-Winkler: no
#' per-pair R-level dispatch, and the whole vector runs in parallel across
#' CPU cores via Intel TBB (through RcppParallel).
#'
#' @param a,b Equal-length character vectors.
#' @param q Q-gram length (default `2`, i.e. bigrams — the common default).
#'   Must be a single integer >= 1.
#' @param alpha,beta Tversky asymmetry weights (default `0.5` each, matching
#'   Dice). Non-negative scalars.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Numeric vector of similarities in `[0, 1]`, `length(a)` long.
#'   `NA` if either `a[i]` or `b[i]` is `NA`. Two strings shorter than `q`
#'   (so neither has any q-grams) compare equal (`1`).
#' @seealso [jaccard_matrix()], [dice_matrix()], [tversky_matrix()], [jaro_winkler()]
#' @examples
#' jaccard_index("night", "nacht")
#' dice_coefficient("night", "nacht")
#' tversky_index("night", "nacht", alpha = 1, beta = 1) # == jaccard_index()
#' jaccard_index(c("Kyle Haynes", "John Smith"), c("Kyle Haynes", "Jon Smith"))
#' @name qgram_metrics
NULL

.qgram_validate_q <- function(q) {
    if (!is.numeric(q) || length(q) != 1L || q < 1)
        stop("`q` must be a single integer >= 1.")
}

.qgram_validate <- function(a, b, q) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    .qgram_validate_q(q)
}

.qgram_validate_matrix <- function(a, b, q) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    .qgram_validate_q(q)
}

#' @rdname qgram_metrics
#' @export
jaccard_index <- function(a, b, q = 2, nthreads = NULL) {
    .qgram_validate(a, b, q)
    fast_jaccard_impl(a, b, as.integer(q), .as_nthreads(nthreads))
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
    .qgram_validate_matrix(a, b, q)
    fast_jaccard_matrix_impl(a, b, as.integer(q), .as_nthreads(nthreads))
}

#' @rdname qgram_metrics
#' @export
dice_coefficient <- function(a, b, q = 2, nthreads = NULL) {
    .qgram_validate(a, b, q)
    fast_dice_impl(a, b, as.integer(q), .as_nthreads(nthreads))
}

#' Q-gram Sorensen-Dice all-pairs similarity matrix
#'
#' @inheritParams jaccard_matrix
#' @return Numeric matrix of dimensions n × m.
#' @seealso [dice_coefficient()]
#' @export
dice_matrix <- function(a, b, q = 2, nthreads = NULL) {
    .qgram_validate_matrix(a, b, q)
    fast_dice_matrix_impl(a, b, as.integer(q), .as_nthreads(nthreads))
}

#' @rdname qgram_metrics
#' @export
tversky_index <- function(a, b, q = 2, alpha = 0.5, beta = 0.5, nthreads = NULL) {
    .qgram_validate(a, b, q)
    if (!is.numeric(alpha) || length(alpha) != 1L || alpha < 0)
        stop("`alpha` must be a single non-negative number.")
    if (!is.numeric(beta) || length(beta) != 1L || beta < 0)
        stop("`beta` must be a single non-negative number.")
    fast_tversky_impl(
        a, b, as.integer(q), as.double(alpha), as.double(beta),
        .as_nthreads(nthreads)
    )
}

#' Q-gram Tversky all-pairs similarity matrix
#'
#' @inheritParams jaccard_matrix
#' @param alpha,beta Tversky asymmetry weights (default `0.5` each).
#' @return Numeric matrix of dimensions n × m.
#' @seealso [tversky_index()]
#' @export
tversky_matrix <- function(a, b, q = 2, alpha = 0.5, beta = 0.5, nthreads = NULL) {
    .qgram_validate_matrix(a, b, q)
    if (!is.numeric(alpha) || length(alpha) != 1L || alpha < 0)
        stop("`alpha` must be a single non-negative number.")
    if (!is.numeric(beta) || length(beta) != 1L || beta < 0)
        stop("`beta` must be a single non-negative number.")
    fast_tversky_matrix_impl(
        a, b, as.integer(q), as.double(alpha), as.double(beta),
        .as_nthreads(nthreads)
    )
}
