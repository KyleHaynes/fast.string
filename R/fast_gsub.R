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
#' x <- c("Unit 4, 12 Smith St", "12 SMITH STREET", NA, "PO Box 219")
#'
#' fgrep("Smith", x, ignore.case = TRUE)                # positions
#' fgrep("Smith", x, ignore.case = TRUE, value = TRUE)  # the strings
#' fgrep("Smith", x, ignore.case = TRUE, invert = TRUE) # the rest, with NA
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
#' # "SURNAME, GIVEN" in one file and "GIVEN SURNAME" in the other is the
#' # commonest shape mismatch in a name field, and one substitution fixes
#' # it: only the first comma, so a middle name is left alone.
#' x <- c("SMITH, JOHN", "NGUYEN, THI MINH", NA, "DE LA CRUZ, JUAN")
#' fsub("^([^,]+), (.*)$", "\\2 \\1", x)
#'
#' fsub("-", " ", c("SMITH-JONES", "A-B-C", NA), fixed = TRUE)
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
#' x <- c("O'Brien-Smith", "de la Cruz", NA, "MacDonald")
#'
#' # Stripping punctuation from a name before comparing it: an apostrophe
#' # and a hyphen are recording conventions, not information, and leaving
#' # them in makes two spellings of one name disagree.
#' fgsub("[^A-Za-z ]", "", x)
#'
#' # Upper-casing the first letter of each word. Note what this does to
#' # "de la Cruz" and "MacDonald" -- which is why ftitle_case() exists and
#' # is what you should reach for on a name field.
#' fgsub("(^|\\s)(\\w)", "\\1\\U\\2", x)
#' @export
fgsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    .substitute(pattern, replacement, x, ignore.case, perl, fixed, useBytes,
                nthreads, global = TRUE)
}
