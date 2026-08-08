#' Fast parallel string matching
#'
#' Equivalent to [base::grepl()], using PCRE2 and Intel TBB.
#' Large inputs can use multiple cores; the crossover depends on subject
#' length, pattern cost, match density, and the available cores.
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
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
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
    threads <- .as_nthreads(nthreads)

    if (isTRUE(fixed)) {
        return(fast_fixed_impl(
            pattern, x, isTRUE(ignore.case), threads
        ))
    }

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::grepl} with {.code perl = TRUE}.")
        return(base::grepl(pattern, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_grepl_impl(pattern, x, isTRUE(ignore.case), threads)
}

#' Fast parallel match counting
#'
#' Counts non-overlapping matches of one pattern in each element of `x`.
#' This is the counting counterpart to [fgrepl()], using the same PCRE2 and
#' prepared fixed-string engines and the same parallel dispatch policy.
#'
#' @inheritParams fgrepl
#' @return Integer vector the same length as `x`. Missing inputs return `NA`.
#' @seealso [fgrepl()], [base::gregexpr()]
#' @export
fcount <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern))
        stop("`pattern` must be a single non-missing character string.")
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }

    # Empty patterns have character-position semantics in base R, whereas the
    # fast fixed engine deliberately operates on bytes. Keep this rare edge
    # case exact instead of silently returning byte counts for UTF-8 strings.
    if (identical(pattern, "")) {
        return(.base_count_matches(
            pattern, x, ignore.case, perl, fixed, useBytes
        ))
    }

    threads <- .as_nthreads(nthreads)
    if (isTRUE(fixed)) {
        return(.copy_names(fast_fixed_count_impl(
            pattern, x, isTRUE(ignore.case), threads
        ), x))
    }

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::gregexpr} with {.code perl = TRUE}.")
        return(.base_count_matches(
            pattern, x, ignore.case, TRUE, FALSE, useBytes
        ))
    }

    .copy_names(fast_regex_count_impl(
        pattern, x, isTRUE(ignore.case), threads
    ), x)
}

.base_count_matches <- function(pattern, x, ignore.case, perl, fixed, useBytes) {
    matches <- base::gregexpr(
        pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes
    )
    result <- vapply(matches, function(hit) {
        if (length(hit) == 1L && is.na(hit)) return(NA_integer_)
        if (length(hit) == 1L && hit[[1L]] < 0L) return(0L)
        length(hit)
    }, integer(1L))
    .copy_names(result, x)
}
