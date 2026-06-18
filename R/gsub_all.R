gsub_all <- function(patterns, replacements, x,
                     fixed = FALSE, ignore.case = FALSE,
                     sequential = TRUE,
                     verbose = getOption("fgrepl.verbose", TRUE),
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
