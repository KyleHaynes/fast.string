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
#' `levenshtein()`, [levenshtein_matrix()] and `levenshtein_similarity()`
#' take `weights`, the costs of an insertion, a deletion and a
#' substitution (as in RapidFuzz's `Levenshtein`, and `costs` in
#' [utils::adist()]). With other weights than the default all-ones the
#' distance is the cheapest total cost, and the similarity divides it by the
#' cost of the most expensive way to do the job (RapidFuzz's
#' `levenshtein_maximum`).
#'
#' @param a,b Equal-length character vectors.
#' @param nthreads Positive integer per-call thread cap, or `NULL` for the
#'   RcppParallel default.
#' @param use_bytes Logical scalar. Compare encoded bytes when `TRUE`, or
#'   UTF-8 code points when `FALSE`.
#' @param weights Non-negative numeric vector of length 3: the cost of an
#'   insertion, a deletion and a substitution, in that order (or named
#'   `insert`, `delete` and `substitute`). Insertion and deletion are as seen
#'   from `a`: inserting a character of `b`, deleting one of `a`.
#' @return A numeric vector the same length as `a`; `NA` when either input is
#'   `NA`. Distance functions return raw edit counts. Similarity functions
#'   return values in `[0, 1]`.
#' @seealso [fuzzy_match()] and [fuzzy_top_n()] to search a table with these
#'   metrics without building a full matrix.
#' @family edit distance functions
#' @examples
#' # Surnames as they actually arrive in two files: a keying slip, a dropped
#' # letter, an anglicised spelling, a transposition.
#' file_1 <- c("NGUYEN", "OBRIEN",  "MACDONALD", "SCHMIDT", "CHOWDHURY")
#' file_2 <- c("NGUYNE", "O'BRIEN", "MCDONALD",  "SCHMITT", "CHOUDHURY")
#' levenshtein(file_1, file_2)
#'
#' # A substitution is usually a worse sign than an insertion in a name: an
#' # apostrophe or a missing vowel is a recording convention, a different
#' # letter is a different name. Weighting it says so.
#' levenshtein(file_1, file_2, weights = c(1, 1, 2))
#'
#' # Normalised, so one cut-off works across names of different lengths --
#' # 2 edits in "NGUYEN" is a long way, 2 in "CHOWDHURY" is not.
#' round(levenshtein_similarity(file_1, file_2), 3)
#'
#' # Transpositions are the commonest typing error, and plain Levenshtein
#' # charges two edits for one slip. OSA charges one.
#' levenshtein("NGUYEN", "NGUYNE")
#' osa_distance("NGUYEN", "NGUYNE")
#' round(osa_similarity("NGUYEN", "NGUYNE"), 3)
#'
#' # OSA allows each character only one transposition; the unrestricted form
#' # allows a further edit inside a transposed block. On real name typos the
#' # two almost always agree -- it takes two *overlapping* transpositions to
#' # separate them, which a hand slipping on a keyboard does not produce --
#' # so OSA is the one to reach for, and this is the pair that shows the
#' # difference at all:
#' osa_distance("ca", "abc")
#' damerau_levenshtein("ca", "abc")
#' round(damerau_levenshtein_similarity("ca", "abc"), 3)
#'
#' # Hamming compares position by position, which suits fixed-width codes --
#' # a Medicare number, an ICD-10 code, a postcode -- and nothing else.
#' hamming(c("4000", "E66.9"), c("4001", "E66.0"))
#' hamming("4000", "04000")   # unequal lengths are never within range
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

# Row and column names from names(a) and names(b), as outer() gives.
.matrix_dimnames <- function(m, a, b) {
    if (!is.null(names(a)) || !is.null(names(b)))
        dimnames(m) <- list(names(a), names(b))
    m
}

.validate_use_bytes <- function(use_bytes) {
    if (!is.logical(use_bytes) || length(use_bytes) != 1L || is.na(use_bytes))
        stop("`use_bytes` must be TRUE or FALSE.")
    invisible(use_bytes)
}

#' @rdname edit_distance
#' @export
levenshtein <- function(a, b, nthreads = NULL, use_bytes = TRUE,
                        weights = c(1, 1, 1)) {
    .editdist_validate(a, b, use_bytes)
    weights <- .levenshtein_weights(weights)
    if (!all(weights == 1))
        return(fast_weighted_levenshtein_impl(
            a, b, weights, FALSE, use_bytes, .as_nthreads(nthreads)
        ))
    fast_levenshtein_impl(a, b, .as_nthreads(nthreads), use_bytes)
}

# Validates `weights` and returns c(insert, delete, substitute).
.levenshtein_weights <- function(weights) {
    if (!is.numeric(weights) || length(weights) != 3L || anyNA(weights) ||
        any(!is.finite(weights)) || any(weights < 0))
        stop("`weights` must be three non-negative numbers: insert, delete, substitute.")
    nm <- names(weights)
    if (!is.null(nm) && all(nzchar(nm))) {
        idx <- c(pmatch("ins", nm), pmatch("del", nm), pmatch("sub", nm))
        if (anyNA(idx))
            stop("`weights` names must be `insert`, `delete` and `substitute`.")
        weights <- weights[idx]
    }
    as.double(unname(weights))
}

#' Levenshtein all-pairs distance matrix
#'
#' @inheritParams edit_distance
#' @param weights Costs of an insertion, a deletion and a substitution, as
#'   in [levenshtein()].
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns,
#'   with `names(a)` and `names(b)` as row and column names when present.
#' @family edit distance functions
#' @examples
#' # Every suburb in one file against the reference list, in one call: the
#' # smallest value in each row is the candidate to check.
#' entered <- c("BRISBANE", "TOOWONG", "INDOOROOPILY")
#' gazetteer <- c("BRISBANE", "TOOWONG", "INDOOROOPILLY", "TOOWOOMBA")
#' m <- levenshtein_matrix(entered, gazetteer)
#' dimnames(m) <- list(entered, gazetteer)
#' m
#' gazetteer[apply(m, 1, which.min)]
#' @export
levenshtein_matrix <-function(a, b, nthreads = NULL, use_bytes = TRUE,
                              weights = c(1, 1, 1)) {
    .editdist_validate_matrix(a, b, use_bytes)
    weights <- .levenshtein_weights(weights)
    if (!all(weights == 1))
        return(.matrix_dimnames(fast_weighted_levenshtein_matrix_impl(
            a, b, weights, use_bytes, .as_nthreads(nthreads)
        ), a, b))
    .matrix_dimnames(fast_levenshtein_matrix_impl(a, b, .as_nthreads(nthreads), use_bytes), a, b)
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
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns,
#'   with `names(a)` and `names(b)` as row and column names when present.
#' @family edit distance functions
#' @examples
#' # Transposed given names cost one edit here and two under Levenshtein,
#' # which is why OSA is the usual default for name matching.
#' osa_distance_matrix(c("MICHELLE", "STEPHANIE"),
#'                     c("MICHEELE", "MICHELLE", "STPEHANIE"))
#' @export
osa_distance_matrix <-function(a, b, nthreads = NULL, use_bytes = TRUE) {
    .editdist_validate_matrix(a, b, use_bytes)
    .matrix_dimnames(fast_osa_distance_matrix_impl(a, b, .as_nthreads(nthreads), use_bytes), a, b)
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
#' @return Numeric matrix with `length(a)` rows and `length(b)` columns,
#'   with `names(a)` and `names(b)` as row and column names when present.
#' @family edit distance functions
#' @examples
#' # Unrestricted transposition scores "ca" -> "abc" as 2, OSA as 3. Pairs
#' # that separate the two need overlapping transpositions and so are rare
#' # in practice: on name typos ([edit_distance()] shows a few) the two
#' # agree, and OSA is the cheaper of them.
#' damerau_levenshtein_matrix(c("ca", "abc"), c("abc", "ca"))
#' osa_distance_matrix(c("ca", "abc"), c("abc", "ca"))
#' @export
damerau_levenshtein_matrix <-function(a, b, nthreads = NULL,
                                       use_bytes = TRUE) {
    .editdist_validate_matrix(a, b, use_bytes)
    .matrix_dimnames(fast_damerau_levenshtein_matrix_impl(
        a, b, .as_nthreads(nthreads), use_bytes
    ), a, b)
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
                                   use_bytes = TRUE, weights = c(1, 1, 1)) {
    weights <- .levenshtein_weights(weights)
    if (!all(weights == 1)) {
        .editdist_validate(a, b, use_bytes)
        return(fast_weighted_levenshtein_impl(
            a, b, weights, TRUE, use_bytes, .as_nthreads(nthreads)
        ))
    }
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
#' # "Within k edits" is the question a blocking pass asks, and answering it
#' # without computing the distance is what makes it cheap over millions of
#' # pairs: the bit-parallel search gives up as soon as k is exceeded.
#' levenshtein_within(c("NGUYEN", "NGUYEN", "NGUYEN"),
#'                    c("NGUYNE", "NGYUEN", "PHAM"), 2)
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
