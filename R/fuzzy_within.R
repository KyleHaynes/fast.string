#' Every table entry within a few edits of each query
#'
#' `fuzzy_within()` finds, for each element of `x`, every element of
#' `table` within `max_distance` edits, without comparing each query with
#' every table entry. It uses the symmetric-delete index of SymSpell (Wolf
#' Garbe): strings within `k` edits of each other always share a string
#' obtained by deleting at most `k` characters from each, so the table is
#' indexed by its deletion variants and each query only looks up its own.
#' Every candidate is then checked with the exact distance, so the result
#' is exactly the set of pairs within `max_distance` -- the same as
#' filtering a full [osa_distance_matrix()], at a fraction of the cost for
#' large tables and small distances.
#'
#' As in SymSpell, only the first `prefix_length` characters of each string
#' are indexed, which keeps the index small without losing matches (each
#' edit removes at most one character of either prefix from their longest
#' common subsequence). Longer prefixes make the index bigger but give
#' fewer candidates to check.
#'
#' The work grows quickly with `max_distance`; 1 or 2 is typical.
#'
#' @param x Character vector of queries.
#' @param table Character vector to search.
#' @param max_distance Non-negative integer (at most 8): the largest
#'   distance to report.
#' @param method `"osa"` (the default, as SymSpell: adjacent transpositions
#'   count as one edit), `"levenshtein"` or `"damerau_levenshtein"`.
#' @param prefix_length Positive integer (at most 64): how many leading
#'   characters are indexed (SymSpell's default is 7).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param use_bytes Logical. Compare UTF-8 code points (`FALSE`, the default)
#'   or bytes.
#' @return A data frame with one row per matching pair: `query_index`,
#'   `table_index` and `distance`, ordered by query, then distance, then
#'   table position. `NA` strings match nothing.
#' @seealso [fuzzy_match()] and [fuzzy_top_n()] for the best matches by
#'   similarity, and [osa_distance()] for the distance itself.
#' @family fuzzy lookup functions
#' @examples
#' streets <- c("SMITH", "SMYTH", "SMITHE", "SIMTH", "JONES", "SMITHS")
#' fuzzy_within(c("SMITH", "JONSE"), streets, max_distance = 1)
#' fuzzy_within("SMITH", streets, max_distance = 1, method = "levenshtein")
#' @export
fuzzy_within <- function(x, table, max_distance = 1L,
                         method = c("osa", "levenshtein", "damerau_levenshtein"),
                         prefix_length = 7L, nthreads = NULL,
                         use_bytes = FALSE) {
    if (!is.character(x) || !is.character(table))
        stop("`x` and `table` must be character vectors.")
    method <- match.arg(method)
    if (!is.numeric(max_distance) || length(max_distance) != 1L ||
        is.na(max_distance) || max_distance < 0 || max_distance > 8 ||
        max_distance != floor(max_distance))
        stop("`max_distance` must be a single integer from 0 to 8.")
    if (!is.numeric(prefix_length) || length(prefix_length) != 1L ||
        is.na(prefix_length) || prefix_length < 1 || prefix_length > 64 ||
        prefix_length != floor(prefix_length))
        stop("`prefix_length` must be a single integer from 1 to 64.")
    .validate_use_bytes(use_bytes)
    id <- match(method, c("jaro_winkler", "levenshtein", "osa",
                          "damerau_levenshtein")) - 1L
    res <- fast_fuzzy_within_impl(
        x, table, as.integer(max_distance), id, as.integer(prefix_length),
        use_bytes, .as_nthreads(nthreads)
    )
    data.frame(query_index = res$query_index, table_index = res$table_index,
               distance = res$distance)
}
