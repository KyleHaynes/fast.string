#' Vectorised Levenshtein, Damerau-Levenshtein, and Hamming distance
#'
#' Classic edit-distance metrics — `stringdist`'s bread and butter — backed
#' by a bit-parallel implementation rather than `stringdist`'s generic
#' R-level dispatch:
#'
#' * `levenshtein()` — minimum number of single-character insertions,
#'   deletions, and substitutions to turn `a[i]` into `b[i]`. Uses Myers'
#'   (1999) bit-vector algorithm — the same family of trick as this
#'   package's bit-parallel Jaro (see `jaro_winkler_core.h`) — turning the
#'   usual O(l1*l2) dynamic-programming table into O(l1) word operations
#'   whenever the shorter string is <= 64 bytes (the overwhelming majority
#'   of names/addresses); longer pairs fall back to an O(l1*l2)
#'   rolling-row DP.
#' * `damerau_levenshtein()` — like Levenshtein, but an adjacent-character
#'   transposition (`"ab"` <-> `"ba"`) also costs one edit, under the
#'   "optimal string alignment" (OSA) restriction that no substring is
#'   edited more than once. This matches `stringdist::stringdist(method =
#'   "dl")`, not full unrestricted Damerau-Levenshtein.
#' * `hamming()` — number of positions at which two equal-length strings
#'   differ. Returns `Inf` for a pair of unequal length, matching
#'   `stringdist::stringdist(method = "hamming")`.
#'
#' All three run a single parallelised C++ pass over the whole vector (via
#' Intel TBB through RcppParallel), the same as [jaro_winkler()].
#'
#' @param a,b Equal-length character vectors.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Numeric vector the same length as `a`, `NA` if either `a[i]` or
#'   `b[i]` is `NA`.
#' @seealso [levenshtein_matrix()], [damerau_levenshtein_matrix()], [jaro_winkler()]
#' @examples
#' levenshtein("kitten", "sitting")
#' damerau_levenshtein("ab", "ba")
#' hamming("karolin", "kathrin")
#' @name levenshtein
NULL

.editdist_validate <- function(a, b) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
}

#' @rdname levenshtein
#' @export
levenshtein <- function(a, b, nthreads = NULL) {
    .editdist_validate(a, b)
    fast_levenshtein_impl(a, b, .as_nthreads(nthreads))
}

#' Levenshtein all-pairs distance matrix
#'
#' @param a Character vector of length n (rows).
#' @param b Character vector of length m (columns).
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Numeric matrix of dimensions n × m.
#' @seealso [levenshtein()]
#' @export
levenshtein_matrix <- function(a, b, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    fast_levenshtein_matrix_impl(a, b, .as_nthreads(nthreads))
}

#' @rdname levenshtein
#' @export
damerau_levenshtein <- function(a, b, nthreads = NULL) {
    .editdist_validate(a, b)
    fast_damerau_levenshtein_impl(a, b, .as_nthreads(nthreads))
}

#' Damerau-Levenshtein (OSA) all-pairs distance matrix
#'
#' @inheritParams levenshtein_matrix
#' @return Numeric matrix of dimensions n × m.
#' @seealso [damerau_levenshtein()]
#' @export
damerau_levenshtein_matrix <- function(a, b, nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    fast_damerau_levenshtein_matrix_impl(a, b, .as_nthreads(nthreads))
}

#' @rdname levenshtein
#' @export
hamming <- function(a, b, nthreads = NULL) {
    .editdist_validate(a, b)
    fast_hamming_impl(a, b, .as_nthreads(nthreads))
}
