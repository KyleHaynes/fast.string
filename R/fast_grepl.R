#' Fast parallel string matching
#'
#' Equivalent to [base::grepl()], using PCRE2 and Intel TBB.
#' Typically 5–20x faster on large character vectors.
#'
#' Patterns using PCRE-only syntax (lookaheads, lookbehinds, atomic groups,
#' possessive quantifiers, named backreferences) are automatically detected
#' when `perl = FALSE` and delegated to [base::grepl()] with `perl = TRUE`,
#' so results are always correct.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip the PCRE-only syntax check and run
#'   the parallel PCRE2 engine directly.
#' @param fixed Logical. Treat `pattern` as a literal string (fastest path).
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param nthreads Integer or `NULL`. Thread count; `NULL` uses all cores.
#'
#' @return Logical vector the same length as `x`.
#' @seealso [base::grepl()]
#' @export
fgrepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                  fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }
    if (!is.null(nthreads)) RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))

    if (isTRUE(fixed)) return(fast_fixed_impl(pattern, x, isTRUE(ignore.case)))

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::grepl} with {.code perl = TRUE}.")
        return(base::grepl(pattern, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_grepl_impl(pattern, x, isTRUE(ignore.case))
}
