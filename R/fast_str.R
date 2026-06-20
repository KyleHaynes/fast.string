# Reattach names(x) so these masked functions remain true drop-in
# replacements — base R's trimws/substr/nchar/chartr all preserve names.
.copy_names <- function(result, x) {
    nm <- names(x)
    if (!is.null(nm)) names(result) <- nm
    result
}

#' Fast parallel whitespace trimming (masks base::trimws)
#'
#' Drop-in replacement for [base::trimws()], parallelised across all CPU
#' cores via Intel TBB. Strips `" \t\r\n"` from the ends of each string.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param which Character scalar. One of `"both"`, `"left"`, or `"right"`.
#' @param whitespace Character scalar giving a regex of characters to strip.
#'   Only the default `"[ \t\r\n]"` uses the fast path; any other value
#'   delegates to [base::trimws()].
#' @param verbose Logical. Show a one-time message that base functions are
#'   masked. Defaults to `getOption("fast.string.verbose", TRUE)`.
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::trimws()]
#' @export
ftrimws <- function(x, which = c("both", "left", "right"),
                   whitespace = "[ \t\r\n]",
                   verbose = getOption("fast.string.verbose", TRUE)) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    which <- match.arg(which)
    if (!identical(whitespace, "[ \t\r\n]"))
        return(base::trimws(x, which = which, whitespace = whitespace))
    code <- switch(which, both = 0L, left = 1L, right = 2L)
    .copy_names(fast_trimws_impl(x, code), x)
}

#' Fast parallel substring extraction (masks base::substr)
#'
#' Drop-in replacement for [base::substr()], parallelised across all CPU
#' cores via Intel TBB. `start`/`stop` are 1-indexed, clamped to each
#' string's bounds, and recycled to `length(x)`, matching base R semantics.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param start,stop Integer (or numeric, coerced via [as.integer()]) vectors
#'   of length 1 or `length(x)`. `NA` in either produces `NA` for that element.
#' @param verbose Logical. Show a one-time message that base functions are
#'   masked. Defaults to `getOption("fast.string.verbose", TRUE)`.
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::substr()]
#' @export
fsubstr <- function(x, start, stop,
                   verbose = getOption("fast.string.verbose", TRUE)) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    n <- length(x)
    start_int <- as.integer(start)
    stop_int  <- as.integer(stop)
    if (length(start_int) != 1L && length(start_int) != n)
        start_int <- rep_len(start_int, n)
    if (length(stop_int) != 1L && length(stop_int) != n)
        stop_int <- rep_len(stop_int, n)
    .copy_names(fast_substr_impl(x, start_int, stop_int), x)
}

#' Fast parallel character/byte counting (masks base::nchar)
#'
#' Drop-in replacement for [base::nchar()], parallelised across all CPU
#' cores via Intel TBB.
#'
#' @param x Vector, coerced to character via [as.character()] if needed.
#' @param type Character scalar. One of `"bytes"`, `"chars"`, or `"width"`.
#'   `"width"` delegates to [base::nchar()] (display width is not
#'   parallelised).
#' @param allowNA Logical. Ignored; included for signature compatibility.
#'   The fast path never raises an encoding error, regardless of this value.
#' @param keepNA Logical or `NA` (the default). If `NA` or `TRUE`, `NA`
#'   elements of `x` return `NA`; if `FALSE`, they return `2L` (the length
#'   of the string `"NA"`), matching base R's legacy `keepNA = FALSE`
#'   behaviour.
#' @param verbose Logical. Show a one-time message that base functions are
#'   masked. Defaults to `getOption("fast.string.verbose", TRUE)`.
#'
#' @return Integer vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::nchar()]
#' @export
fnchar <- function(x, type = "chars", allowNA = FALSE, keepNA = NA,
                  verbose = getOption("fast.string.verbose", TRUE)) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    if (!is.character(x)) x <- as.character(x)
    type <- match.arg(type, c("bytes", "chars", "width"))
    if (type == "width")
        return(base::nchar(x, type = "width", allowNA = allowNA, keepNA = keepNA))
    # R 4.5 default: keepNA=NA -> return NA for NA input (same as keepNA=TRUE).
    # keepNA=FALSE -> return nchar("NA")=2 for NA input (legacy behaviour).
    allow_na <- if (is.na(keepNA)) TRUE else isTRUE(keepNA)
    code <- if (type == "bytes") 0L else 1L
    .copy_names(fast_nchar_impl(x, code, allow_na), x)
}

#' Fast parallel character translation (masks base::chartr)
#'
#' Drop-in replacement for [base::chartr()] using a flat 256-byte lookup
#' table, parallelised across all CPU cores via Intel TBB.
#'
#' @param old,new Single strings with the same number of characters (not
#'   bytes — `old`/`new` may differ in byte length, as in base R, as long as
#'   the character counts match). Each character of `old` is translated to
#'   the corresponding character of `new`.
#' @param x Character vector. `NA` elements return `NA`.
#' @param verbose Logical. Show a one-time message that base functions are
#'   masked. Defaults to `getOption("fast.string.verbose", TRUE)`.
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::chartr()]
#' @export
fchartr <- function(old, new, x,
                   verbose = getOption("fast.string.verbose", TRUE)) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    if (!is.character(old) || length(old) != 1L)
        base::stop("`old` must be a single string.")
    if (!is.character(new) || length(new) != 1L)
        base::stop("`new` must be a single string.")
    if (base::nchar(old, type = "chars") != base::nchar(new, type = "chars"))
        base::stop("`old` and `new` must have the same number of characters.")
    # Fall back to base R when either string contains multi-byte characters:
    # the fast path is a flat byte-for-byte lookup table, valid only when
    # every character in both `old` and `new` is a single byte (ASCII).
    if (base::nchar(old, type = "bytes") != base::nchar(old, type = "chars") ||
        base::nchar(new, type = "bytes") != base::nchar(new, type = "chars"))
        return(base::chartr(old, new, x))
    .copy_names(fast_chartr_impl(old, new, x), x)
}
