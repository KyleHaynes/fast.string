trimws <- function(x, which = c("both", "left", "right"),
                   whitespace = "[ \t\r\n]",
                   verbose = getOption("fgrepl.verbose", TRUE)) {
    if (isTRUE(verbose)) .show_mask_msg_once()
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    which <- match.arg(which)
    if (!identical(whitespace, "[ \t\r\n]"))
        return(base::trimws(x, which = which, whitespace = whitespace))
    code <- switch(which, both = 0L, left = 1L, right = 2L)
    fast_trimws_impl(x, code)
}

substr <- function(x, start, stop) {
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
    fast_substr_impl(x, start_int, stop_int)
}

nchar <- function(x, type = "chars", allowNA = FALSE, keepNA = NA) {
    if (!is.character(x)) x <- as.character(x)
    type <- match.arg(type, c("bytes", "chars", "width"))
    if (type == "width")
        return(base::nchar(x, type = "width", allowNA = allowNA, keepNA = keepNA))
    # R 4.5 default: keepNA=NA → return NA for NA input (same as keepNA=TRUE).
    # keepNA=FALSE → return nchar("NA")=2 for NA input (legacy behaviour).
    allow_na <- if (is.na(keepNA)) TRUE else isTRUE(keepNA)
    code <- if (type == "bytes") 0L else 1L
    fast_nchar_impl(x, code, allow_na)
}

chartr <- function(old, new, x) {
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else base::stop("`x` must be a character vector.")
    }
    if (!is.character(old) || length(old) != 1L)
        base::stop("`old` must be a single string.")
    if (!is.character(new) || length(new) != 1L)
        base::stop("`new` must be a single string.")
    if (base::nchar(old, type = "bytes") != base::nchar(new, type = "bytes"))
        base::stop("`old` and `new` must have the same number of characters.")
    # Fall back to base R for multi-byte characters (UTF-8 accented chars, etc.)
    if (base::nchar(old, type = "bytes") != base::nchar(old, type = "chars"))
        return(base::chartr(old, new, x))
    fast_chartr_impl(old, new, x)
}
