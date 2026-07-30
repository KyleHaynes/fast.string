#' Format a Date vector as strings without locale or timezone overhead.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @param format One of `"iso"` (YYYY-MM-DD), `"compact"` (YYYYMMDD),
#'   `"dmy"` (DD/MM/YYYY), or `"ymd_slash"` (YYYY/MM/DD).
#' @return Character vector the same length as `x`.
#' @export
format_date <- function(x, format = c("iso", "compact", "dmy", "ymd_slash")) {
    format <- match.arg(format)
    if (inherits(x, "Date")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be a Date or numeric vector of days since 1970-01-01.")
    code <- switch(format, iso = 0L, compact = 1L, dmy = 2L, ymd_slash = 3L)
    fast_format_date_impl(as.double(x), code)
}

#' Concatenate separate year/month/day fields into a formatted date string.
#'
#' Pure string-building (zero-padding plus punctuation): no calendar math
#' and no Date object involved, so it's faster than going through
#' [base::as.Date()] or [sprintf()] just to assemble a string. This is the
#' inverse of [date_parts()], which decomposes a Date into year/month/day
#' columns.
#'
#' @param year,month,day Numeric vectors, recycled to a common length.
#'   `NA`, non-integer-coercible, or out-of-width values (year outside
#'   0-9999, or month/day outside 0-99) produce `NA` for that element.
#'   There is no calendar validation — `month = 13` or `day = 99` are
#'   formatted as given, as long as they fit the field width.
#' @param format One of `"iso"` (YYYY-MM-DD), `"compact"` (YYYYMMDD),
#'   `"dmy"` (DD/MM/YYYY), or `"ymd_slash"` (YYYY/MM/DD).
#' @return Character vector recycled to the common length of `year`,
#'   `month`, and `day`.
#' @seealso [date_parts()] for the reverse direction.
#' @export
format_date_parts <- function(year, month, day,
                              format = c("iso", "compact", "dmy", "ymd_slash")) {
    if (!is.numeric(year) || !is.numeric(month) || !is.numeric(day))
        stop("`year`, `month`, and `day` must be numeric vectors.")
    format <- match.arg(format)
    n <- max(length(year), length(month), length(day))
    if (n > 0L) {
        year  <- rep_len(year,  n)
        month <- rep_len(month, n)
        day   <- rep_len(day,   n)
    }
    code <- switch(format, iso = 0L, compact = 1L, dmy = 2L, ymd_slash = 3L)
    fast_format_date_parts_impl(as.integer(year), as.integer(month), as.integer(day), code)
}

#' Decompose a Date vector into year, month, day integer columns.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @return A data.frame with integer columns `year`, `month`, `day`.
#' @export
date_parts <- function(x) {
    if (inherits(x, "Date")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be a Date or numeric vector of days since 1970-01-01.")
    fast_date_parts_impl(as.double(x))
}

#' Fast fixed-format date parsing (not a drop-in for base::as.Date())
#'
#' Parses a character vector into a `Date` object much faster than
#' [base::as.Date()] by skipping locale handling, [strptime()], and
#' multi-format auto-detection entirely. In exchange, `x` must be in
#' exactly one fixed `format` (the same four [format_date()] produces, so
#' the two are natural round-trip partners), and validation is minimal:
#' correct length, digit/separator positions, month in 1-12, day in 1-31.
#' There is no days-in-month or leap-year check, so e.g. `"2024-02-30"`
#' parses without error (unlike [base::as.Date()]) — this trades strictness
#' for speed, by design.
#'
#' @param x Character vector. `NA` elements, and elements that don't match
#'   `format` exactly (wrong length/separators/non-digits) or have an
#'   out-of-range month/day, become `NA`.
#' @param format One of `"iso"` (YYYY-MM-DD), `"compact"` (YYYYMMDD),
#'   `"dmy"` (DD/MM/YYYY), or `"ymd_slash"` (YYYY/MM/DD).
#' @return A `Date` vector the same length as `x`.
#' @seealso [format_date()] for the reverse direction.
#' @export
fas.Date <- function(x, format = c("iso", "compact", "dmy", "ymd_slash")) {
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }
    format <- match.arg(format)
    code <- switch(format, iso = 0L, compact = 1L, dmy = 2L, ymd_slash = 3L)
    result <- .copy_names(fast_parse_date_impl(x, code), x)
    class(result) <- "Date"
    result
}
