#' fuzzywuzzy-style fuzzy string ratios
#'
#' Vectorised ports of the Python [fuzzywuzzy](https://github.com/seatgeek/fuzzywuzzy)
#' package's headline functions — `fuzz.ratio`, `fuzz.partial_ratio`,
#' `fuzz.token_sort_ratio`, and `fuzz.token_set_ratio` — implementing the same
#' Ratcliff/Obershelp matching-blocks algorithm fuzzywuzzy itself falls back
#' to (the one behind Python's `difflib.SequenceMatcher`, used whenever the
#' optional `python-Levenshtein` speedup isn't installed), so scores match
#' fuzzywuzzy's reference behaviour. All four return `0-100` like the Python
#' originals, run a single parallelised C++ pass over the whole vector (via
#' Intel TBB through RcppParallel), and have no Python/reticulate dependency.
#'
#' * `fuzz_ratio()` — overall similarity: `2*M / (len(a)+len(b))`, where `M`
#'   is the total length of the longest-common matching blocks (found
#'   recursively, Ratcliff/Obershelp-style — *not* edit distance).
#' * `fuzz_partial_ratio()` — best alignment of the shorter string against
#'   any equal-length window of the longer one; high when one string is a
#'   near-substring of the other regardless of what surrounds it.
#' * `fuzz_token_sort_ratio()` — splits each string into whitespace tokens,
#'   sorts them, rejoins, then runs `fuzz_ratio()` on the result — so word
#'   order stops mattering.
#' * `fuzz_token_set_ratio()` — splits into token *sets* and compares the
#'   shared-token core against each side's leftovers, taking the best of the
#'   three pairwise ratios — robust to one side simply having extra words.
#'
#' @param a,b Equal-length character vectors.
#' @param full_process Logical (default `TRUE`, matching fuzzywuzzy's
#'   default). Lowercases and replaces runs of non-alphanumeric characters
#'   with a single space before comparing, same as fuzzywuzzy's
#'   `full_process()` preprocessing step.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Numeric vector of scores in `[0, 100]`, `length(a)` long. `NA` if
#'   either `a[i]` or `b[i]` is `NA`.
#' @examples
#' fuzz_ratio("this is a test", "this is a test!")
#' fuzz_partial_ratio("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear")
#' fuzz_token_sort_ratio("fuzzy was a bear", "bear was a fuzzy")
#' fuzz_token_set_ratio("fuzzy was a bear", "fuzzy fuzzy bear was a bear")
#' @name fuzz
#' @aliases fuzz_ratio fuzz_partial_ratio fuzz_token_sort_ratio fuzz_token_set_ratio
NULL

.fuzz_validate <- function(a, b) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
}

#' @rdname fuzz
#' @export
fuzz_ratio <- function(a, b, full_process = TRUE, nthreads = NULL) {
    .fuzz_validate(a, b)
    round(fast_fuzz_ratio_impl(
        a, b, isTRUE(full_process), .as_nthreads(nthreads)
    ))
}

#' @rdname fuzz
#' @export
fuzz_partial_ratio <- function(a, b, full_process = TRUE, nthreads = NULL) {
    .fuzz_validate(a, b)
    round(fast_fuzz_partial_ratio_impl(
        a, b, isTRUE(full_process), .as_nthreads(nthreads)
    ))
}

#' @rdname fuzz
#' @export
fuzz_token_sort_ratio <- function(a, b, full_process = TRUE, nthreads = NULL) {
    .fuzz_validate(a, b)
    round(fast_fuzz_token_sort_ratio_impl(
        a, b, isTRUE(full_process), .as_nthreads(nthreads)
    ))
}

#' @rdname fuzz
#' @export
fuzz_token_set_ratio <- function(a, b, full_process = TRUE, nthreads = NULL) {
    .fuzz_validate(a, b)
    round(fast_fuzz_token_set_ratio_impl(
        a, b, isTRUE(full_process), .as_nthreads(nthreads)
    ))
}
