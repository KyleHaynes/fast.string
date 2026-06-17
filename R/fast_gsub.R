.validate_sub_args <- function(pattern, replacement, x, nthreads) {
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    if (!is.character(replacement) || length(replacement) != 1L)
        stop("`replacement` must be a single character string.")
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }
    if (!is.null(nthreads)) RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    x
}

#' Fast parallel string matching returning indices or values (masks base::grep)
#'
#' Drop-in replacement for [base::grep()] using PCRE2 and Intel TBB.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param x Character vector.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param value Logical. Return matching elements instead of indices.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param invert Logical. Return non-matching indices/values.
#' @param verbose Logical. One-time mask message. Defaults to
#'   `getOption("fgrepl.verbose", TRUE)`.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Integer vector of indices (or character when `value = TRUE`).
#' @seealso [base::grep()]
#' @export
grep <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                 value = FALSE, fixed = FALSE, useBytes = FALSE,
                 invert = FALSE,
                 verbose = getOption("fgrepl.verbose", TRUE),
                 nthreads = NULL) {
    if (isTRUE(verbose)) .show_mask_msg_once()

    m <- grepl(pattern, x, ignore.case = ignore.case, perl = perl,
               fixed = fixed, verbose = FALSE, nthreads = nthreads)
    keep <- if (isTRUE(invert)) is.na(m) | !m else !is.na(m) & m
    if (isTRUE(value)) x[keep] else which(keep)
}

#' Fast parallel first-match substitution (masks base::sub)
#'
#' Drop-in replacement for [base::sub()] using PCRE2 and Intel TBB.
#' Supports `\\1`–`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param replacement Character scalar. Replacement string.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param verbose Logical. One-time mask message. Defaults to
#'   `getOption("fgrepl.verbose", TRUE)`.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Character vector the same length as `x`.
#' @seealso [base::sub()], [gsub()]
#' @export
sub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE,
                verbose = getOption("fgrepl.verbose", TRUE),
                nthreads = NULL) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    x <- .validate_sub_args(pattern, replacement, x, nthreads)

    if (isTRUE(fixed))
        return(fast_fixed_sub_impl(pattern, replacement, x, isTRUE(ignore.case), FALSE))

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::sub} with {.code perl = TRUE}.")
        return(base::sub(pattern, replacement, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_regex_sub_impl(pattern, replacement, x, isTRUE(ignore.case), FALSE)
}

#' Fast parallel global substitution (masks base::gsub)
#'
#' Drop-in replacement for [base::gsub()] using PCRE2 and Intel TBB.
#' Supports `\\1`–`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion.
#'
#' @param pattern Character scalar. Pattern to search for.
#' @param replacement Character scalar. Replacement string.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param perl Logical. If `TRUE`, skip PCRE-only syntax check.
#' @param fixed Logical. Treat `pattern` as a literal string.
#' @param useBytes Logical. Ignored; included for signature compatibility.
#' @param verbose Logical. One-time mask message. Defaults to
#'   `getOption("fgrepl.verbose", TRUE)`.
#' @param nthreads Integer or `NULL`. Thread count.
#' @return Character vector the same length as `x`.
#' @seealso [base::gsub()], [sub()]
#' @export
gsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE,
                 verbose = getOption("fgrepl.verbose", TRUE),
                 nthreads = NULL) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    x <- .validate_sub_args(pattern, replacement, x, nthreads)

    if (isTRUE(fixed))
        return(fast_fixed_sub_impl(pattern, replacement, x, isTRUE(ignore.case), TRUE))

    if (!isTRUE(perl) && .has_pcre_only_syntax(pattern)) {
        cli::cli_inform("Pattern has PCRE-only syntax; delegating to {.fn base::gsub} with {.code perl = TRUE}.")
        return(base::gsub(pattern, replacement, x, ignore.case = ignore.case, perl = TRUE))
    }

    fast_regex_sub_impl(pattern, replacement, x, isTRUE(ignore.case), TRUE)
}
