#' Fast parallel string matching using RE2 and RcppParallel
#'
#' A drop-in replacement for [base::grepl()] that uses Google's RE2 regex
#' engine (linear-time, no catastrophic backtracking) parallelised across all
#' CPU cores via Intel TBB through RcppParallel.  Typically 5–50x faster than
#' `grepl()` on large character vectors.
#'
#' **RE2 vs PCRE differences**: RE2 does not support backreferences
#' (`\1`, `(?P=name)`) or lookahead/lookbehind assertions (`(?=...)`,
#' `(?!...)`).  All other common regex syntax works identically.
#' Use `grepl()` for patterns that require those PCRE-only features.
#'
#' @param pattern Character scalar. The pattern to search for.
#'   When `fixed = FALSE` this is an RE2 regular expression.
#'   When `fixed = TRUE` it is a literal string.
#' @param x Character vector to search in. `NA` elements return `NA`.
#' @param fixed Logical (default `FALSE`). If `TRUE`, treat `pattern` as a
#'   literal string — no regex parsing, maximum speed.
#' @param ignore.case Logical (default `FALSE`). Case-insensitive matching.
#'   For `fixed = TRUE` this applies ASCII case-folding only.
#' @param nthreads Integer or `NULL`. Number of threads to use.
#'   `NULL` (default) uses all available cores via
#'   [RcppParallel::defaultNumThreads()].
#'
#' @return A logical vector the same length as `x`.
#'
#' @seealso [base::grepl()], [RcppParallel::setThreadOptions()]
#'
#' @examples
#' x <- c("hello world", "foo bar", NA, "test pattern here")
#' fast_grepl("pattern", x)
#' fast_grepl("PATTERN", x, ignore.case = TRUE)
#' fast_grepl("foo", x, fixed = TRUE)
#'
#' @export
fast_grepl <- function(pattern, x, fixed = FALSE, ignore.case = FALSE,
                       nthreads = NULL) {
    if (!is.character(pattern) || length(pattern) != 1L) {
        stop("`pattern` must be a single character string.")
    }
    if (!is.character(x)) {
        if (all(is.na(x))) {
            x <- as.character(x)
        } else {
            stop("`x` must be a character vector.")
        }
    }
    if (!is.null(nthreads)) {
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    }
    if (isTRUE(fixed)) {
        fast_fixed_impl(pattern, x, isTRUE(ignore.case))
    } else {
        fast_grepl_impl(pattern, x, isTRUE(ignore.case))
    }
}
