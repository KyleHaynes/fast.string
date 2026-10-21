#' Search and compare tables with the RapidFuzz scorers
#'
#' Ports of the Python RapidFuzz package's `process` module. They run any
#' of the [fuzz] scorers over many strings in one parallel C++ pass.
#'
#' * `fuzz_extract()` (`process.extract`): for each query in `x`, the
#'   `limit` best-scoring `choices`, best first. Tied choices keep their
#'   order in `choices`.
#' * `fuzz_extract_one()` (`process.extractOne`): each query's best choice,
#'   the first one on ties.
#' * `fuzz_cdist()` (`process.cdist`): the score of every query against
#'   every choice, as a matrix. `fuzz_cdist(x)` compares `x` with itself and
#'   computes each symmetric pair once.
#' * `fuzz_cpdist()` (`process.cpdist`): element-wise scores, `a[i]` against
#'   `b[i]`. This equals calling the scorer function directly, but the
#'   scorer is chosen by name.
#'
#' Each string is decoded (and processed, with `full_process = TRUE`) once,
#' not once per pair. `fuzz_extract()` and `fuzz_extract_one()` also tighten
#' the scorer's cutoff to the worst score still in a query's list, so most
#' poor candidates are rejected early. A choice that is `NA` is skipped.
#'
#' @param x Character vector of queries.
#' @param choices Character vector of candidate strings.
#' @param scorer The scorer, by name or as a function. Names are
#'   `"ratio"`, `"partial_ratio"`, `"token_sort_ratio"`, `"token_set_ratio"`,
#'   `"token_ratio"`, `"partial_token_sort_ratio"`,
#'   `"partial_token_set_ratio"`, `"partial_token_ratio"`, `"wratio"` and
#'   `"qratio"`, in any case, so RapidFuzz's `"WRatio"` works too. A
#'   function must be one of the scorers, for example [fuzz_wratio]. The
#'   defaults are RapidFuzz's: `WRatio` for extraction and `ratio` for
#'   `fuzz_cdist()` and `fuzz_cpdist()`.
#' @param limit Positive integer: at most this many matches per query
#'   (default 5). `NULL` returns every choice that reaches `score_cutoff`.
#' @param score_cutoff Number between 0 and 100 (default 0). Choices scoring
#'   below it are not returned. In `fuzz_cdist()` and `fuzz_cpdist()`,
#'   scores below it become 0.
#' @inheritParams fuzz
#' @return
#' * `fuzz_extract()`: a data frame with one row per match and columns
#'   `query_index`, `choice_index`, `choice`, `score`, and `rank` (1 for a
#'   query's best match). An `NA` query has no rows.
#' * `fuzz_extract_one()`: a data frame with one row per query and columns
#'   `choice`, `score` and `choice_index`. All three are `NA` when the query
#'   is `NA` or no choice reaches `score_cutoff`.
#' * `fuzz_cdist()`: a numeric matrix with `length(x)` rows and
#'   `length(choices)` columns, named by `names(x)` and `names(choices)`
#'   when present. A cell is `NA` where either string is `NA`.
#' * `fuzz_cpdist()`: a numeric vector, as from the scorer functions.
#' @seealso [fuzzy_match()] and [fuzzy_top_n()] for the same kind of search
#'   with Jaro-Winkler and edit-distance scores.
#' @examples
#' choices <- c("Atlanta Falcons", "New York Jets", "New York Giants",
#'              "Dallas Cowboys")
#' fuzz_extract("new york jets", choices, limit = 2)
#' fuzz_extract_one(c("cowboys", "falcon", "chargers"), choices,
#'                  score_cutoff = 50)
#' round(fuzz_cdist(c("new york", "dallas"), choices,
#'                  scorer = "token_set_ratio"), 1)
#' fuzz_cpdist("fuzzy was a bear", "bear was fuzzy", scorer = "token_sort_ratio")
#' @name fuzz_process
NULL

.fuzz_validate_search <- function(x, choices) {
    if (!is.character(x) || !is.character(choices))
        stop("`x` and `choices` must be character vectors.")
}

.fuzz_extract_run <- function(x, choices, scorer, limit, full_process,
                              score_cutoff, nthreads) {
    .fuzz_validate_search(x, choices)
    fast_fuzz_extract_impl(
        x, choices, .fuzz_scorer_id(scorer), limit, .fuzz_cutoff(score_cutoff),
        isTRUE(full_process), .as_nthreads(nthreads)
    )
}

#' @rdname fuzz_process
#' @export
fuzz_extract <- function(x, choices, scorer = "wratio", limit = 5L,
                         full_process = TRUE, score_cutoff = 0,
                         nthreads = NULL) {
    if (is.null(limit)) {
        limit <- 0L
    } else {
        if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
            !is.finite(limit) || limit < 1 || limit != floor(limit) ||
            limit > .Machine$integer.max)
            stop("`limit` must be NULL or a positive integer.")
        limit <- as.integer(limit)
    }
    found <- .fuzz_extract_run(x, choices, scorer, limit, full_process,
                               score_cutoff, nthreads)
    data.frame(
        query_index = found$query_index,
        choice_index = found$choice_index,
        choice = unname(choices[found$choice_index]),
        score = found$score,
        rank = found$rank,
        stringsAsFactors = FALSE
    )
}

#' @rdname fuzz_process
#' @export
fuzz_extract_one <- function(x, choices, scorer = "wratio",
                             full_process = TRUE, score_cutoff = 0,
                             nthreads = NULL) {
    found <- .fuzz_extract_run(x, choices, scorer, 1L, full_process,
                               score_cutoff, nthreads)
    index <- rep(NA_integer_, length(x))
    score <- rep(NA_real_, length(x))
    index[found$query_index] <- found$choice_index
    score[found$query_index] <- found$score
    data.frame(
        choice = unname(choices[index]),
        score = score,
        choice_index = index,
        stringsAsFactors = FALSE
    )
}

#' @rdname fuzz_process
#' @export
fuzz_cdist <- function(x, choices = x, scorer = "ratio", full_process = TRUE,
                       score_cutoff = 0, nthreads = NULL) {
    .fuzz_validate_search(x, choices)
    .matrix_dimnames(fast_fuzz_cdist_impl(
        x, choices, .fuzz_scorer_id(scorer), isTRUE(full_process),
        .fuzz_cutoff(score_cutoff), .as_nthreads(nthreads)
    ), x, choices)
}

#' @rdname fuzz_process
#' @inheritParams fuzz
#' @export
fuzz_cpdist <- function(a, b, scorer = "ratio", full_process = TRUE,
                        score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, scorer, full_process, score_cutoff, nthreads)
}
