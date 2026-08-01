#' Memory-efficient fuzzy lookup
#'
#' Find the best or top-N matches for each element of `x` in `table` without
#' allocating a full `length(x)` by `length(table)` score matrix. Lookup is
#' parallelized over queries and retains only the requested matches.
#'
#' All methods return normalized similarity scores in `[0, 1]`, where one is
#' an exact match. New lookup APIs compare UTF-8 code points by default; set
#' `use_bytes = TRUE` for byte-wise comparison. Code-point comparison does not
#' perform Unicode normalization or grapheme-cluster segmentation.
#'
#' @param x Character vector of query strings.
#' @param table Character vector of candidate strings.
#' @param method One of `"jaro_winkler"`, `"levenshtein"`, `"osa"`, or
#'   `"damerau_levenshtein"`.
#' @param p Jaro-Winkler prefix scale in `[0, 0.25]`.
#' @param min_score Minimum normalized similarity in `[0, 1]`.
#' @param max_distance Optional non-negative integer raw-distance cutoff for
#'   edit-distance methods. Not available for Jaro-Winkler.
#' @param match_na Logical. Match an `NA` query to the first `NA` in `table`.
#' @param nomatch Integer scalar returned by [fuzzy_match()] when no candidate
#'   passes the cutoffs.
#' @param nthreads Positive integer per-call thread cap, or `NULL` for the
#'   RcppParallel default.
#' @param use_bytes Logical. Compare bytes when `TRUE`; compare UTF-8 code
#'   points when `FALSE`.
#' @return `fuzzy_match()` returns an integer vector of table positions.
#'   `fuzzy_top_n()` returns a data frame with `query_index`, `table_index`,
#'   `score`, and `rank`.
#' @examples
#' fuzzy_match(c("SMITH", "JONES"), c("SMYTH", "JONAS", "JONES"))
#' fuzzy_top_n("kitten", c("sitting", "mitten", "cat"), top_n = 2)
#' @name fuzzy_lookup
NULL

.fuzzy_lookup_validate <- function(x, table, method, p, min_score,
                                   max_distance, match_na, nthreads,
                                   use_bytes) {
    if (!is.character(x) || !is.character(table))
        stop("`x` and `table` must be character vectors.")
    method <- match.arg(
        method,
        c("jaro_winkler", "levenshtein", "osa", "damerau_levenshtein")
    )
    if (!is.numeric(p) || length(p) != 1L || is.na(p) ||
        !is.finite(p) || p < 0 || p > 0.25)
        stop("`p` must be a number between 0 and 0.25.")
    if (!is.numeric(min_score) || length(min_score) != 1L ||
        is.na(min_score) || !is.finite(min_score) ||
        min_score < 0 || min_score > 1)
        stop("`min_score` must be a number between 0 and 1.")
    if (!is.null(max_distance)) {
        if (method == "jaro_winkler")
            stop("`max_distance` is only available for edit-distance methods.")
        if (!is.numeric(max_distance) || length(max_distance) != 1L ||
            is.na(max_distance) || !is.finite(max_distance) ||
            max_distance < 0 || max_distance != floor(max_distance) ||
            max_distance > .Machine$integer.max)
            stop("`max_distance` must be NULL or a non-negative integer.")
        max_distance <- as.integer(max_distance)
    }
    if (!is.logical(match_na) || length(match_na) != 1L || is.na(match_na))
        stop("`match_na` must be TRUE or FALSE.")
    .validate_use_bytes(use_bytes)
    list(
        method = match(method, c(
            "jaro_winkler", "levenshtein", "osa", "damerau_levenshtein"
        )) - 1L,
        p = as.double(p),
        min_score = as.double(min_score),
        max_distance = if (is.null(max_distance)) -1L else max_distance,
        match_na = match_na,
        nthreads = .as_nthreads(nthreads),
        use_bytes = use_bytes
    )
}

.fuzzy_lookup_run <- function(x, table, method, top_n, p, min_score,
                              max_distance, match_na, nthreads, use_bytes) {
    validated <- .fuzzy_lookup_validate(
        x, table, method, p, min_score, max_distance,
        match_na, nthreads, use_bytes
    )
    fast_fuzzy_top_n_impl(
        x, table, validated$method, top_n, validated$p,
        validated$min_score, validated$max_distance,
        validated$match_na, validated$nthreads, validated$use_bytes
    )
}

#' @rdname fuzzy_lookup
#' @export
fuzzy_match <- function(x, table,
                        method = c("jaro_winkler", "levenshtein", "osa",
                                   "damerau_levenshtein"),
                        p = 0.1, min_score = 0, max_distance = NULL,
                        match_na = FALSE, nomatch = NA_integer_,
                        nthreads = NULL, use_bytes = FALSE) {
    if (!is.numeric(nomatch) || length(nomatch) != 1L ||
        (!is.na(nomatch) &&
         (!is.finite(nomatch) || nomatch != floor(nomatch) ||
          nomatch < -.Machine$integer.max ||
          nomatch > .Machine$integer.max)))
        stop("`nomatch` must be a single integer or NA.")
    result <- .fuzzy_lookup_run(
        x, table, method, 1L, p, min_score, max_distance,
        match_na, nthreads, use_bytes
    )$index[, 1L]
    result[is.na(result)] <- as.integer(nomatch)
    names(result) <- names(x)
    result
}

#' @rdname fuzzy_lookup
#' @param top_n Positive integer number of candidates retained per query.
#' @export
fuzzy_top_n <- function(x, table,
                        method = c("jaro_winkler", "levenshtein", "osa",
                                   "damerau_levenshtein"),
                        top_n = 5L, p = 0.1, min_score = 0,
                        max_distance = NULL, match_na = FALSE,
                        nthreads = NULL, use_bytes = FALSE) {
    if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
        !is.finite(top_n) || top_n < 1 || top_n != floor(top_n) ||
        top_n > .Machine$integer.max)
        stop("`top_n` must be a positive integer.")
    top_n <- as.integer(top_n)
    result <- .fuzzy_lookup_run(
        x, table, method, top_n, p, min_score, max_distance,
        match_na, nthreads, use_bytes
    )
    query_index <- rep(seq_along(x), each = top_n)
    rank <- rep(seq_len(top_n), times = length(x))
    table_index <- as.vector(t(result$index))
    score <- as.vector(t(result$score))
    keep <- !is.na(table_index)
    data.frame(
        query_index = query_index[keep],
        table_index = table_index[keep],
        score = score[keep],
        rank = rank[keep],
        row.names = NULL
    )
}
