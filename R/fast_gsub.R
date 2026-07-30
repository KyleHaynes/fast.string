.validate_sub_args <- function(pattern, replacement, x, nthreads) {
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    if (!is.character(replacement) || length(replacement) != 1L)
        stop("`replacement` must be a single character string.")
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }
    list(x = x, threads = .as_nthreads(nthreads))
}

#' Fast parallel string matching returning indices or values
#'
#' Equivalent to [base::grep()], using PCRE2 and Intel TBB.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param x Character vector.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param value Logical. Return matching elements instead of indices.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param invert Logical. Return non-matching indices/values.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Integer vector of indices (or character when `value = TRUE`).
#' @seealso [base::grep()]
#' @export
fgrep <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                 value = FALSE, fixed = FALSE, useBytes = FALSE,
                 invert = FALSE, nthreads = NULL) {
    m <- fgrepl(pattern, x, ignore.case = ignore.case, perl = perl,
               fixed = fixed, nthreads = nthreads)
    keep <- if (isTRUE(invert)) is.na(m) | !m else !is.na(m) & m
    if (isTRUE(value)) x[keep] else unname(which(keep))
}

#' Fast parallel first-match substitution
#'
#' Equivalent to [base::sub()], using PCRE2 and Intel TBB. Supports
#' `\\1`-`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param replacement Character scalar. Replacement string.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`.
#' @seealso [base::sub()], [fgsub()]
#' @export
fsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    validated <- .validate_sub_args(pattern, replacement, x, nthreads)
    x <- validated$x
    threads <- validated$threads

    if (isTRUE(fixed)) {
        if (identical(pattern, ""))
            stop("zero-length pattern")
        return(fast_fixed_sub_impl(
            pattern, replacement, x, isTRUE(ignore.case), FALSE,
            threads
        ))
    }

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::sub} with {.code perl = TRUE}.")
        return(base::sub(pattern, replacement, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_regex_sub_impl(
        pattern, replacement, x, isTRUE(ignore.case), FALSE, threads
    )
}

#' Fast parallel global substitution
#'
#' Equivalent to [base::gsub()], using PCRE2 and Intel TBB. Supports
#' `\\1`-`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param replacement Character scalar. Replacement string.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`.
#' @seealso [base::gsub()], [fsub()]
#' @export
fgsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    validated <- .validate_sub_args(pattern, replacement, x, nthreads)
    x <- validated$x
    threads <- validated$threads

    if (isTRUE(fixed)) {
        if (identical(pattern, ""))
            stop("zero-length pattern")
        return(fast_fixed_sub_impl(
            pattern, replacement, x, isTRUE(ignore.case), TRUE,
            threads
        ))
    }

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::gsub} with {.code perl = TRUE}.")
        return(base::gsub(pattern, replacement, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_regex_sub_impl(
        pattern, replacement, x, isTRUE(ignore.case), TRUE, threads
    )
}
