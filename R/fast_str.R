# base R's trimws/substr/nchar/chartr all preserve names(x); reattach them
# here so these functions match that behaviour exactly.
.copy_names <- function(result, x) {
    nm <- names(x)
    if (!is.null(nm)) names(result) <- nm
    result
}

.native_encoding_info <- function() {
    info <- l10n_info()
    list(
        utf8 = isTRUE(info[["UTF-8"]]),
        mbcs = isTRUE(info[["MBCS"]])
    )
}

#' Fast whitespace trimming
#'
#' Equivalent to [base::trimws()]. Strips `" \t\r\n"` from the ends of each
#' string.
#'
#' Creating each result string goes through R's global string cache, which
#' is single-threaded, so this runs as one serial pass that prefetches
#' upcoming strings, returns untouched strings as-is, and reuses the previous
#' result when adjacent trimmed strings are identical.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param which Character scalar. One of `"both"`, `"left"`, or `"right"`.
#' @param whitespace Character scalar giving a regex of characters to strip.
#'   Only the default `"[ \t\r\n]"` uses the fast path; any other value
#'   delegates to [base::trimws()].
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::trimws()]
#' @family string utilities
#' @examples
#' x <- c("  hi  ", "\tthere\n", NA)
#' ftrimws(x)
#' ftrimws(x, which = "left")
#' @export
ftrimws <- function(x, which = c("both", "left", "right"),
                   whitespace = "[ \t\r\n]") {
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

#' Fast substring extraction
#'
#' Equivalent to [base::substr()]. `start`/`stop` are 1-indexed, clamped to
#' each string's bounds, and recycled to `length(x)`, matching base R
#' semantics.
#'
#' Creating each result string goes through R's global string cache, which
#' is single-threaded, so the usual case is one serial pass that prefetches
#' upcoming strings, skips the UTF-8 character scan for ASCII prefixes, and
#' reuses the previous result when adjacent substrings are identical (as in
#' sorted or grouped data). When any `start` exceeds 256 characters, the
#' character scan runs in parallel across all CPU cores via Intel TBB
#' first.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param start,stop Integer (or numeric, coerced via [as.integer()]) vectors
#'   of length 1 or `length(x)`. `NA` in either produces `NA` for that element.
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::substr()]
#' @family string utilities
#' @examples
#' fsubstr(c("abcdef", "xyz", NA), 2, 4)   # out-of-range stops are clamped
#' @export
fsubstr <- function(x, start, stop) {
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    n <- length(x)
    start_int <- as.integer(start)
    stop_int  <- as.integer(stop)
    if (length(start_int) == 0L || length(stop_int) == 0L)
        return(base::substr(x, start_int, stop_int))
    if (length(start_int) != 1L && length(start_int) != n)
        start_int <- rep_len(start_int, n)
    if (length(stop_int) != 1L && length(stop_int) != n)
        stop_int <- rep_len(stop_int, n)
    encoding <- .native_encoding_info()
    if (encoding$mbcs && !encoding$utf8 &&
        any(!is.na(x) & Encoding(x) == "unknown"))
        return(base::substr(x, start_int, stop_int))
    .copy_names(
        fast_substr_impl(x, start_int, stop_int, encoding$utf8),
        x
    )
}

#' Fast character/byte counting
#'
#' Equivalent to [base::nchar()]. Byte counts, and character counts of ASCII
#' or single-byte-encoded strings, come straight from R's stored string
#' length in one serial pass; only non-ASCII UTF-8 strings need their
#' characters counted, which runs in parallel across all CPU cores via
#' Intel TBB when there is enough of it.
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
#'
#' @return Integer vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::nchar()]
#' @family string utilities
#' @examples
#' x <- c("abc", "h\u00e9llo", NA)
#' fnchar(x)                    # characters
#' fnchar(x, type = "bytes")    # the accented letter takes two bytes in UTF-8
#' @export
fnchar <- function(x, type = "chars", allowNA = FALSE, keepNA = NA) {
    if (!is.character(x)) x <- as.character(x)
    type <- match.arg(type, c("bytes", "chars", "width"))
    if (type == "width")
        return(base::nchar(x, type = "width", allowNA = allowNA, keepNA = keepNA))
    # R 4.5 default: keepNA=NA -> return NA for NA input (same as keepNA=TRUE).
    # keepNA=FALSE -> return nchar("NA")=2 for NA input (legacy behaviour).
    allow_na <- if (is.na(keepNA)) TRUE else isTRUE(keepNA)
    code <- if (type == "bytes") 0L else 1L
    encoding <- .native_encoding_info()
    if (type == "chars" && encoding$mbcs && !encoding$utf8 &&
        any(!is.na(x) & Encoding(x) == "unknown"))
        return(base::nchar(x, type = type, allowNA = allowNA, keepNA = keepNA))
    .copy_names(fast_nchar_impl(x, code, allow_na, encoding$utf8), x)
}

#' Fast parallel character translation
#'
#' Equivalent to [base::chartr()], using a flat 256-byte lookup table,
#' parallelised across all CPU cores via Intel TBB.
#'
#' @param old,new Single strings specifying the translation, as in
#'   [base::chartr()]: each character of `old` becomes the corresponding
#'   character of `new`, ranges such as `"a-z"` are expanded, and `new` may
#'   be longer than `old` (the extra characters are ignored) but not shorter.
#'   Specifications with non-ASCII characters are passed to [base::chartr()].
#' @param x Character vector. `NA` elements return `NA`.
#'
#' @return Character vector the same length as `x`, with `names(x)` preserved.
#' @seealso [base::chartr()]
#' @family string utilities
#' @examples
#' fchartr("abc", "xyz", c("aabbcc", NA))
#' fchartr("a-cx", "A-CX", "abcxyz")      # ranges, as in base R
#' @export
fchartr <- function(old, new, x) {
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    if (!is.character(old) || length(old) != 1L)
        base::stop("`old` must be a single string.")
    if (!is.character(new) || length(new) != 1L)
        base::stop("`new` must be a single string.")
    # The fast path is a byte-for-byte table, which only works when every
    # character of `old` and `new` is ASCII; anything else goes to base R.
    if (anyNA(c(old, new)) ||
        base::grepl("[^\\x01-\\x7f]", old, perl = TRUE, useBytes = TRUE) ||
        base::grepl("[^\\x01-\\x7f]", new, perl = TRUE, useBytes = TRUE))
        return(base::chartr(old, new, x))
    # base::chartr() builds the table by translating every ASCII character,
    # so ranges, repeated characters and argument errors are exactly base R's.
    translated <- base::chartr(old, new, .ascii_characters)
    .copy_names(fast_chartr_impl(.ascii_characters, translated, x), x)
}

# The 127 non-NUL ASCII characters, in byte order.
.ascii_characters <- rawToChar(as.raw(1:127))
