#' Format a Date vector as strings without locale or timezone overhead.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @param format One of `"iso"` (YYYY-MM-DD), `"compact"` (YYYYMMDD),
#'   `"dmy"` (DD/MM/YYYY), or `"ymd_slash"` (YYYY/MM/DD).
format_date <- function(x, format = c("iso", "compact", "dmy", "ymd_slash")) {
    format <- match.arg(format)
    if (inherits(x, "Date")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be a Date or numeric vector of days since 1970-01-01.")
    code <- switch(format, iso = 0L, compact = 1L, dmy = 2L, ymd_slash = 3L)
    fast_format_date_impl(as.double(x), code)
}

#' Decompose a Date vector into year, month, day integer columns.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @return A data.frame with integer columns `year`, `month`, `day`.
date_parts <- function(x) {
    if (inherits(x, "Date")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be a Date or numeric vector of days since 1970-01-01.")
    fast_date_parts_impl(as.double(x))
}
