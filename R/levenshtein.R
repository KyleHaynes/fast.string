#' Parallel edit distances and normalized similarities
#'
#' `levenshtein()` counts insertions, deletions, and substitutions.
#' `osa_distance()` additionally permits adjacent transpositions under the
#' Optimal String Alignment restriction. `damerau_levenshtein()` implements
#' unrestricted Damerau-Levenshtein, where a substring can participate in
#' more than one edit. `hamming()` counts differing positions and returns
#' `Inf` for unequal-length strings.
#'
#' Existing functions default to byte comparison for compatibility. Set
#' `use_bytes = FALSE` to compare UTF-8 code points. This is code-point
#' comparison, not grapheme-cluster comparison or Unicode normalization.
#'
#' @param a,b Equal-length character vectors.
#' @param nthreads Positive integer per-call thread cap, or `NULL` for the
#'   RcppParallel default.
#' @param use_bytes Logical scalar. Compare encoded bytes when `TRUE`, or
#'   UTF-8 code points when `FALSE`.
#' @return A numeric vector the same length as `a`; `NA` when either input is
#'   `NA`. Distance functions return raw edit counts. Similarity functions
#'   return values in `[0, 1]`.
#' @seealso [fuzzy_match()] and [fuzzy_top_n()] to search a table with these
#'   metrics without building a full matrix.
#' @family edit distance functions
#' @examples
#' levenshtein("kitten", "sitting")
#' osa_distance("ca", "abc")
#' damerau_levenshtein("ca", "abc")
#' levenshtein_similarity("kitten", "sitting")
#' hamming("karolin", "kathrin")
#' hamming("abc", "abcd")   # unequal lengths are never within Hamming range
#' @name edit_distance
NULL

.editdist_validate <- function(a, b, use_bytes) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    .validate_use_bytes(use_bytes)
}

.editdist_validate_matrix <- function(a, b, use_bytes) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    .validate_use_bytes(use_bytes)
}

.validate_use_bytes <- function(use_bytes) {
    if (!is.logical(use_bytes) || length(use_bytes) != 1L || is.na(use_bytes))
        stop("`use_bytes` must be TRUE or FALSE.")
    invisible(use_bytes)
}

#' @rdname edit_distance
#' @export
levenshtein <- function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate(a, b, use_bytes)
    fast_levenshtein_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

#' Levenshtein all-pairs distance matrix
#'
#' @inheritParams edit_distance
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns.
#' @family edit distance functions
#' @examples
#' levenshtein_matrix(c("kitten", "flaw"), c("sitting", "lawn"))
#' @export
levenshtein_matrix <-function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate_matrix(a, b, use_bytes)
    fast_levenshtein_matrix_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

#' @rdname edit_distance
#' @export
osa_distance <- function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate(a, b, use_bytes)
    fast_osa_distance_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

#' Optimal String Alignment all-pairs distance matrix
#'
#' @inheritParams edit_distance
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns.
#' @family edit distance functions
#' @examples
#' osa_distance_matrix(c("ca", "abc"), c("abc", "ca"))
#' @export
osa_distance_matrix <-function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate_matrix(a, b, use_bytes)
    fast_osa_distance_matrix_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

#' @rdname edit_distance
#' @export
damerau_levenshtein <- function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate(a, b, use_bytes)
    fast_damerau_levenshtein_impl(
        a, b, .as_nthreads(nthreads), use_bytes
    )
}

#' Unrestricted Damerau-Levenshtein all-pairs distance matrix
#'
#' @inheritParams edit_distance
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns.
#' @family edit distance functions
#' @examples
#' # Unrestricted transposition scores "ca" -> "abc" as 2, OSA as 3.
#' damerau_levenshtein_matrix(c("ca", "abc"), c("abc", "ca"))
#' @export
damerau_levenshtein_matrix <-function(a, b, nthreads = NULL,
                                       use_bytes = TRUE) {
    .editdist_validate_matrix(a, b, use_bytes)
    fast_damerau_levenshtein_matrix_impl(
        a, b, .as_nthreads(nthreads), use_bytes
    )
}

#' @rdname edit_distance
#' @export
hamming <- function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate(a, b, use_bytes)
    fast_hamming_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

.edit_similarity <- function(a, b, method, nthreads, use_bytes) {
    .editdist_validate(a, b, use_bytes)
    fast_edit_similarity_impl(
        a, b, method, .as_nthreads(nthreads), use_bytes
    )
}

#' @rdname edit_distance
#' @export
levenshtein_similarity <- function(a, b, nthreads = NULL,
                                   use_bytes = TRUE) {
    .edit_similarity(a, b, 1L, nthreads, use_bytes)
}

#' @rdname edit_distance
#' @export
osa_similarity <- function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .edit_similarity(a, b, 2L, nthreads, use_bytes)
}

#' @rdname edit_distance
#' @export
damerau_levenshtein_similarity <- function(a, b, nthreads = NULL,
                                            use_bytes = TRUE) {
    .edit_similarity(a, b, 3L, nthreads, use_bytes)
}

#' Test whether Levenshtein distance is within a cutoff
#'
#' Uses a banded dynamic program and returns early when the distance cannot be
#' less than or equal to `max_distance`.
#'
#' @inheritParams edit_distance
#' @param max_distance Non-negative integer distance cutoff.
#' @return Logical vector the same length as `a`, with missing comparisons
#'   returned as `NA`.
#' @family edit distance functions
#' @examples
#' levenshtein_within(c("kitten", "kitten"), c("sitting", "kitchen"), 2)
#' @export
levenshtein_within <-function(a, b, max_distance, nthreads = NULL,
                               use_bytes = TRUE) {
    .editdist_validate(a, b, use_bytes)
    if (!is.numeric(max_distance) || length(max_distance) != 1L ||
        is.na(max_distance) || !is.finite(max_distance) ||
        max_distance < 0 || max_distance != floor(max_distance) ||
        max_distance > .Machine$integer.max)
        stop("`max_distance` must be a non-negative integer.")
    fast_levenshtein_within_impl(
        a, b, as.integer(max_distance), .as_nthreads(nthreads), use_bytes
    )
}
