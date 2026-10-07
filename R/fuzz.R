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
#' Scores follow fuzzywuzzy's own rules for equal and empty strings: equal
#' inputs score 100, and an empty input scores 0. With `full_process = TRUE`
#' an input that processes to nothing (only punctuation, say) also scores 0.
#'
#' @param a,b Equal-length character vectors.
#' @param full_process Logical (default `TRUE`). Preprocess both strings as
#'   fuzzywuzzy's `full_process()` does: delete the characters U+0080 to
#'   U+00FF (its default `force_ascii`), turn every other character except
#'   letters, digits and `_` into a space, lowercase, and trim. Characters
#'   from U+0100 up are kept unchanged. This is fuzzywuzzy's default for
#'   `token_sort_ratio()` and `token_set_ratio()`; its `ratio()` and
#'   `partial_ratio()` never preprocess, so use `full_process = FALSE` to
#'   reproduce those (`fuzz_ratio()` with preprocessing is fuzzywuzzy's
#'   `QRatio()`).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Numeric vector of scores in `[0, 100]`, `length(a)` long. `NA` if
#'   either `a[i]` or `b[i]` is `NA`.
#' @seealso [jaro_winkler_tokens()] for a word-order-tolerant score built on
#'   Jaro-Winkler, and [edit_distance] for edit-based measures.
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
