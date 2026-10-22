#' TF-IDF n-gram matching
#'
#' Finds each query's most similar table entries by the cosine similarity
#' of their TF-IDF weighted character n-grams -- the approach of ING's
#' `sparse_dot_topn` and the Python `string_grouper` and `PolyFuzz`
#' packages, popular for matching company names and addresses at scale.
#' Every string is broken into overlapping `q`-character n-grams; n-grams
#' that are rare in `table` get high weight and common ones (`"PTY"`,
#' `" ST"`, `"ING"`) little, so matches are driven by the distinctive part
#' of each string. Word order matters only within an n-gram, so
#' `"SMITH JOHN"` and `"JOHN SMITH"` score high.
#'
#' Weights follow scikit-learn's `TfidfVectorizer` defaults: raw n-gram
#' counts times `ln((1 + N) / (1 + df)) + 1`, where `N` is the number of
#' table entries and `df` the number containing the n-gram, then each vector
#' is scaled to unit length. The weights are learnt from `table` alone, so
#' a query's scores do not depend on the other queries. A non-empty string
#' shorter than `q` characters is a single n-gram (so `"LI"` still matches
#' `"LI"`). No case folding or cleaning is done; apply [toupper()],
#' [fuzz_default_process()], [ascii_fold()] or similar first.
#'
#' The table is indexed by n-gram, so each query is only scored against
#' entries it shares an n-gram with; queries run in parallel across CPU
#' cores via Intel TBB.
#'
#' @param x Character vector of queries.
#' @param table Character vector of candidates.
#' @param top_n Positive integer: candidates kept per query.
#' @param q Positive integer n-gram length (default `3`, as `string_grouper`).
#' @param min_score Minimum cosine similarity in `[0, 1]`.
#' @param nomatch Integer returned by `tfidf_match()` when no candidate
#'   passes `min_score`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param use_bytes Logical. Build n-grams from UTF-8 characters (`FALSE`,
#'   the default) or from bytes.
#' @return `tfidf_top_n()`: a data frame with `query_index`, `table_index`,
#'   `score` and `rank`, best first, ties broken by table position. Only
#'   entries sharing at least one n-gram with the query (score above 0) are
#'   listed. `tfidf_match()`: the table position of each query's best match.
#' @references Christen, P. (2012). Data Matching. Springer (TF-IDF and
#'   q-gram similarity for record linkage).
#' @seealso [fuzzy_top_n()] for edit-distance and Jaro-Winkler scoring,
#'   [cosine_similarity()] for unweighted q-gram cosine of two strings.
#' @family fuzzy lookup functions
#' @examples
#' companies <- c("ACME PTY LTD", "ACME HOLDINGS PTY LTD", "APEX PTY LTD",
#'                "ACME CORPORATION", "BETA INDUSTRIES PTY LTD")
#' tfidf_top_n(c("ACME LTD", "BETA IND"), companies, top_n = 3)
#' tfidf_match(c("ACME LTD", "BETA IND", "ZZZ"), companies)
#' @export
tfidf_top_n <- function(x, table, top_n = 5L, q = 3L, min_score = 0,
                        nthreads = NULL, use_bytes = FALSE) {
    if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) ||
        top_n < 1 || top_n != floor(top_n) || top_n > .Machine$integer.max)
        stop("`top_n` must be a positive integer.")
    res <- .tfidf_run(x, table, as.integer(top_n), q, min_score, nthreads,
                      use_bytes)
    top_n <- as.integer(top_n)
    query_index <- rep(seq_along(x), each = top_n)
    rank <- rep(seq_len(top_n), times = length(x))
    table_index <- as.vector(t(res$index))
    score <- as.vector(t(res$score))
    keep <- !is.na(table_index)
    data.frame(query_index = query_index[keep],
               table_index = table_index[keep],
               score = score[keep], rank = rank[keep], row.names = NULL)
}

#' @rdname tfidf_top_n
#' @export
tfidf_match <- function(x, table, q = 3L, min_score = 0,
                        nomatch = NA_integer_, nthreads = NULL,
                        use_bytes = FALSE) {
    if (!is.numeric(nomatch) || length(nomatch) != 1L ||
        (!is.na(nomatch) && nomatch != floor(nomatch)))
        stop("`nomatch` must be a single integer or NA.")
    result <- .tfidf_run(x, table, 1L, q, min_score, nthreads,
                         use_bytes)$index[, 1L]
    result[is.na(result)] <- as.integer(nomatch)
    names(result) <- names(x)
    result
}

.tfidf_run <- function(x, table, top_n, q, min_score, nthreads, use_bytes) {
    if (!is.character(x) || !is.character(table))
        stop("`x` and `table` must be character vectors.")
    .qgram_validate_q(q)
    if (!is.numeric(min_score) || length(min_score) != 1L ||
        is.na(min_score) || min_score < 0 || min_score > 1)
        stop("`min_score` must be a number between 0 and 1.")
    .validate_use_bytes(use_bytes)
    fast_tfidf_top_n_impl(x, table, top_n, as.integer(q), as.double(min_score),
                          use_bytes, .as_nthreads(nthreads))
}
