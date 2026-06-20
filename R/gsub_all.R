#' Apply multiple substitutions in one pass (masks no base function)
#'
#' Replace several patterns in `x` either sequentially (one full pass per
#' pattern, like chaining [gsub()] calls) or in a single combined scan
#' (`sequential = FALSE`), parallelised across all CPU cores via Intel TBB.
#'
#' @param patterns Character vector of patterns to search for.
#' @param replacements Character scalar or vector the same length as
#'   `patterns`.
#' @param x Character vector. `NA` elements return `NA`.
#' @param fixed Logical. Treat each pattern as a literal string.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param sequential Logical. If `TRUE` (default), apply patterns one after
#'   another (later patterns can match text introduced by earlier
#'   replacements). If `FALSE`, match all patterns in a single left-to-right
#'   scan (first pattern to match at each position wins).
#' @param verbose Logical. One-time mask message. Defaults to
#'   `getOption("fast.string.verbose", TRUE)`.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Character vector the same length as `x`.
#' @export
gsub_all <- function(patterns, replacements, x,
                     fixed = FALSE, ignore.case = FALSE,
                     sequential = TRUE,
                     verbose = getOption("fast.string.verbose", TRUE),
                     nthreads = NULL) {
    if (isTRUE(verbose)) .show_mask_msg_once()

    if (!is.character(patterns) || length(patterns) == 0L)
        stop("`patterns` must be a non-empty character vector.")
    if (!is.character(replacements) ||
        !length(replacements) %in% c(1L, length(patterns)))
        stop("`replacements` must be length 1 or the same length as `patterns`.")
    if (length(replacements) == 1L)
        replacements <- rep_len(replacements, length(patterns))
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }

    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))

    if (isTRUE(fixed))
        return(fast_fixed_gsub_all_impl(patterns, replacements, x,
                                         isTRUE(ignore.case), isTRUE(sequential)))

    if (any(vapply(patterns, .has_pcre_only_syntax, logical(1L)))) {
        cli::cli_inform(
            "One or more patterns have PCRE-only syntax; delegating to {.fn base::gsub} with {.code perl = TRUE}."
        )
        return(.base_gsub_all_loop(patterns, replacements, x, ignore.case))
    }

    fast_regex_gsub_all_impl(patterns, replacements, x,
                              isTRUE(ignore.case), isTRUE(sequential))
}

.base_gsub_all_loop <- function(patterns, repls, x, ignore.case) {
    for (i in seq_along(patterns))
        x <- base::gsub(patterns[i], repls[i], x,
                        ignore.case = ignore.case, perl = TRUE)
    x
}
