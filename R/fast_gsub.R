.validate_sub_args <- function(pattern, replacement, x, nthreads) {
    pattern <- .na_as_character(pattern)
    replacement <- .na_as_character(replacement)
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    if (!is.character(replacement) || length(replacement) != 1L)
        stop("`replacement` must be a single character string.")
    list(pattern = pattern, replacement = replacement, x = .character_x(x),
         threads = .as_nthreads(nthreads))
}

# Shared body of fsub() and fgsub().
.substitute <- function(pattern, replacement, x, ignore.case, perl, fixed,
                        useBytes, nthreads, global) {
    args <- .validate_sub_args(pattern, replacement, x, nthreads)
    pattern <- args$pattern
    replacement <- args$replacement
    x <- args$x
    # As in base R: an NA pattern gives NA everywhere, and an NA replacement
    # gives NA wherever it would be used.
    if (is.na(pattern)) return(rep(NA_character_, length(x)))
    if (isTRUE(fixed) && identical(pattern, ""))
        stop("zero-length pattern")
    if (is.na(replacement)) {
        hit <- fgrepl(pattern, x, ignore.case = ignore.case, perl = perl,
                      fixed = fixed, useBytes = useBytes,
                      nthreads = nthreads)
        x[!is.na(hit) & hit] <- NA_character_
        return(unname(x))
    }

    spec <- .regex_spec(pattern, perl, fixed, useBytes, replacement)
    if (isTRUE(fixed)) {
        return(fast_fixed_sub_impl(
            spec$pattern, spec$replacement, x, isTRUE(ignore.case), global,
            spec$use_bytes, args$threads
        ))
    }
    fast_regex_sub_impl(
        spec$pattern, spec$replacement, x, isTRUE(ignore.case), global,
        spec$syntax, spec$use_bytes, args$threads
    )
}

#' Fast parallel string matching returning indices or values
#'
#' Equivalent to [base::grep()], using PCRE2 and Intel TBB. `NA` elements of
#' `x` never match (and are never returned, unless `invert = TRUE`).
#'
#' @inheritParams fgrepl
#' @param x Character vector.
#' @param value Logical. Return matching elements instead of indices.
#' @param invert Logical. Return non-matching indices/values.
#' @return Integer vector of indices (or character when `value = TRUE`).
#' @seealso [base::grep()]
#' @family matching and substitution functions
#' @examples
#' x <- c("apple pie", "banana split", NA, "cherry tart")
#'
#' fgrep("an", x)                  # positions
#' fgrep("an", x, value = TRUE)    # the matching strings
#' fgrep("an", x, invert = TRUE)   # everything else, including the NA
#' @export
fgrep <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                 value = FALSE, fixed = FALSE, useBytes = FALSE,
                 invert = FALSE, nthreads = NULL) {
    pattern <- .na_as_character(pattern)
    if (is.character(pattern) && length(pattern) == 1L && is.na(pattern))
        return(base::grep(pattern, .character_x(x), value = value,
                          invert = invert))
    m <- fgrepl(pattern, x, ignore.case = ignore.case, perl = perl,
               fixed = fixed, useBytes = useBytes, nthreads = nthreads)
    keep <- if (isTRUE(invert)) is.na(m) | !m else !is.na(m) & m
    if (isTRUE(value)) x[keep] else unname(which(keep))
}

#' Fast parallel first-match substitution
#'
#' Equivalent to [base::sub()], using PCRE2 and Intel TBB. Supports
#' `\\1`-`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion (the
#' latter with `perl = FALSE` too, unlike base R). See [fgrepl()] for how
#' `perl = FALSE` patterns are interpreted.
#'
#' @inheritParams fgrepl
#' @param replacement Character scalar. Replacement string. If `NA`, every
#'   element that matches becomes `NA`, as in base R.
#' @return Character vector the same length as `x`.
#' @seealso [base::sub()]
#' @family matching and substitution functions
#' @examples
#' x <- c("apple pie", "banana split", NA, "cherry tart")
#'
#' fsub("a", "_", x, fixed = TRUE)          # first "a" only
#' fsub("(\\w+) (\\w+)", "\\2 \\1", x)      # swap the two words
#' @export
fsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    .substitute(pattern, replacement, x, ignore.case, perl, fixed, useBytes,
                nthreads, global = FALSE)
}

#' Fast parallel global substitution
#'
#' Equivalent to [base::gsub()], using PCRE2 and Intel TBB. Supports
#' `\\1`-`\\9` capture groups and `\\U`/`\\L`/`\\E` case conversion (the
#' latter with `perl = FALSE` too, unlike base R). See [fgrepl()] for how
#' `perl = FALSE` patterns are interpreted.
#'
#' @inheritParams fsub
#' @return Character vector the same length as `x`.
#' @seealso [base::gsub()]
#' @family matching and substitution functions
#' @examples
#' x <- c("apple pie", "banana split", NA, "cherry tart")
#'
#' fgsub("[aeiou]", "_", x)                 # every vowel
#' fgsub("(^|\\s)(\\w)", "\\1\\U\\2", x)    # capitalise each word
#' @export
fgsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    .substitute(pattern, replacement, x, ignore.case, perl, fixed, useBytes,
                nthreads, global = TRUE)
}
