#' Format a Date vector as strings without locale or timezone overhead.
#'
#' Converts days since 1970-01-01 to one of four fixed layouts using pure
#' integer calendar arithmetic, so it is faster than [base::format()] on a
#' `Date` but does not honour the locale or accept arbitrary formats.
#' Fractional days are floored; `NA`, `NaN` and infinite values give `NA`.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @param format One of `"iso"` (YYYY-MM-DD), `"compact"` (YYYYMMDD),
#'   `"dmy"` (DD/MM/YYYY), or `"ymd_slash"` (YYYY/MM/DD).
#' @return Character vector the same length as `x`.
#' @seealso [fas.Date()] for the reverse direction.
#' @family date and timestamp functions
#' @examples
#' d <- as.Date(c("2024-06-18", "1999-12-31", NA))
#' format_date(d)
#' format_date(d, "dmy")
#' format_date(d, "compact")
#' @export
format_date <-function(x, format = c("iso", "compact", "dmy", "ymd_slash")) {
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
#' @family date and timestamp functions
#' @examples
#' format_date_parts(c(2024, 1999), c(6, 12), c(18, 31))
#' format_date_parts(2024, 6, 18, format = "dmy")
#' @export
format_date_parts <-function(year, month, day,
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
#' Splits every date in a single pass, which makes it a cheap way to build
#' blocking keys (for example "same birth year and month") in record linkage.
#' Missing dates give `NA` in all three columns.
#'
#' @param x A `Date` object or numeric vector of days since 1970-01-01.
#' @return A data.frame with integer columns `year`, `month`, `day`.
#' @seealso [format_date_parts()] for the reverse direction.
#' @family date and timestamp functions
#' @examples
#' date_parts(as.Date(c("2024-06-18", "1999-12-31", NA)))
#' @export
date_parts <-function(x) {
    if (inherits(x, "Date")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be a Date or numeric vector of days since 1970-01-01.")
    fast_date_parts_impl(as.double(x))
}

#' Fast fixed-format date parsing and epoch day-count conversion
#'
#' For a character vector, parses dates much faster than [base::as.Date()]
#' by skipping locale handling, [strptime()], and multi-format
#' auto-detection entirely. In exchange, `x` must be in exactly one fixed
#' `format` (the same four [format_date()] produces, so the two are natural
#' round-trip partners), and validation is minimal: correct length,
#' digit/separator positions, month in 1-12, day in 1-31. There is no
#' days-in-month or leap-year check, so e.g. `"2024-02-30"` parses without
#' error (unlike [base::as.Date()]) — this trades strictness for speed, by
#' design.
#'
#' For a numeric vector, converts counts from a standard epoch (spreadsheet
#' serials, SAS/Stata/SPSS dates, MATLAB datenums, Julian days, ...) to
#' dates in one pass, with no origin string to parse. It replaces
#' `as.Date(x, origin = ...)`, and the `"excel"` epoch also handles Excel's
#' phantom 1900-02-29, which `as.Date(x, origin = "1899-12-30")` gets wrong
#' for serials below 61.
#'
#' @section Epochs:
#' | `origin` | Counts | Day 0 (or 1) |
#' |---|---|---|
#' | `"unix"` | days | 0 = 1970-01-01 (R's own `Date`) |
#' | `"excel"` | days | 1 = 1900-01-01, Excel 1900 date system (Windows, Lotus 1-2-3) |
#' | `"excel1904"` | days | 0 = 1904-01-01, Excel 1904 date system (old Mac Excel) |
#' | `"sas"`, `"stata"` | days | 0 = 1960-01-01 (SAS date values, Stata daily dates) |
#' | `"spss"` | seconds | 0 = 1582-10-14 |
#' | `"matlab"` | days | 1 = 0000-01-01 (`datenum`) |
#' | `"julian_day"` | days | 2440588 = 1970-01-01; days begin at noon |
#' | `"mjd"` | days | 0 = 1858-11-17 (Modified Julian Date) |
#' | `"rata_die"` | days | 1 = 0001-01-01 |
#'
#' Excel's 1900 system counts a 29 February 1900 that never existed, so
#' serial 60 gives `NA` and serials below it land one day later than a plain
#' 1899-12-30 origin would put them (as readxl and janitor do). Google
#' Sheets, LibreOffice and OLE automation dates have no such day: use
#' `origin = "1899-12-30"` for those.
#'
#' Fractional counts are floored to the day containing them, so an Excel
#' serial carrying a time of day gives that day, and a Julian date gives the
#' UTC calendar date. Unlike [base::as.Date()], which keeps the fraction
#' (and `NaN`/`Inf`), the result always holds whole days. Integer `x` on a
#' zero-offset epoch such as `"unix"` keeps integer storage, as
#' `base::as.Date(x)` does; every other epoch returns double days.
#'
#' @param x Character vector of dates, or numeric vector of counts from
#'   `origin`. Character `NA` elements, and elements that don't match
#'   `format` exactly (wrong length/separators/non-digits) or have an
#'   out-of-range month/day, become `NA`. Numeric `NA`, `NaN` and infinite
#'   values become `NA`.
#' @param format For character `x`: one of `"iso"` (YYYY-MM-DD),
#'   `"compact"` (YYYYMMDD), `"dmy"` (DD/MM/YYYY), or `"ymd_slash"`
#'   (YYYY/MM/DD).
#' @param origin For numeric `x`: a named epoch from the table under
#'   **Epochs** (case-insensitive), or any other day 0 as a `Date` or a
#'   `"YYYY-MM-DD"` string.
#' @return A `Date` vector with the same length and names as `x`.
#' @seealso [format_date()] for the reverse direction.
#' @family date and timestamp functions
#' @examples
#' fas.Date(c("2024-06-18", "not a date", NA))   # bad input becomes NA
#' fas.Date("18/06/2024", format = "dmy")
#'
#' # Day counts from standard epochs
#' fas.Date(c(1, 59, 60, 61, 45000.75), origin = "excel")
#' fas.Date(23000L, origin = "sas")
#' fas.Date(2460000.5, origin = "julian_day")
#' fas.Date(100, origin = "2000-01-01")
#' @export
fas.Date <-function(x, format = c("iso", "compact", "dmy", "ymd_slash"),
                    origin = "unix") {
    if (is.numeric(x)) {
        if (!missing(format))
            stop("`format` applies to character `x`; numeric `x` is read from an `origin`.")
        if (inherits(x, "integer64")) x <- as.double(x)
        spec <- if (is.character(origin) && length(origin) == 1L)
            .date_epochs[[origin]]
        if (is.null(spec)) spec <- .epoch_spec(origin)
        return(fast_epoch_date_impl(x, spec))
    }
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector, or a numeric vector of counts from an `origin`.")
    } else if (!missing(origin)) {
        stop("`origin` applies to numeric `x`; character `x` is read with `format`.")
    }
    format <- match.arg(format)
    code <- switch(format, iso = 0L, compact = 1L, dmy = 2L, ymd_slash = 3L)
    result <- .copy_names(fast_parse_date_impl(x, code), x)
    class(result) <- "Date"
    result
}

# A count x from each epoch is floor(x / divisor + frac) + shift days since
# 1970-01-01; `excel` = 1 also skips Excel's phantom 1900-02-29 (serial 60).
# Shifts are the 1970-based day number of each epoch's day 0, e.g.
# as.numeric(as.Date("1960-01-01")) = -3653.
.date_epochs <- list(
    unix       = c(divisor = 1,     frac = 0,   shift = 0,        excel = 0),
    excel      = c(divisor = 1,     frac = 0,   shift = -25569,   excel = 1),
    excel1904  = c(divisor = 1,     frac = 0,   shift = -24107,   excel = 0),
    sas        = c(divisor = 1,     frac = 0,   shift = -3653,    excel = 0),
    stata      = c(divisor = 1,     frac = 0,   shift = -3653,    excel = 0),
    spss       = c(divisor = 86400, frac = 0,   shift = -141428,  excel = 0),
    matlab     = c(divisor = 1,     frac = 0,   shift = -719529,  excel = 0),
    julian_day = c(divisor = 1,     frac = 0.5, shift = -2440588, excel = 0),
    mjd        = c(divisor = 1,     frac = 0,   shift = -40587,   excel = 0),
    rata_die   = c(divisor = 1,     frac = 0,   shift = -719163,  excel = 0)
)

# Slow path of the origin lookup in fas.Date(): other spellings of a named
# epoch, and custom day 0s given as a Date or "YYYY-MM-DD" string.
.epoch_spec <- function(origin) {
    days <- NA_real_
    if (inherits(origin, "Date")) {
        if (length(origin) == 1L) days <- floor(unclass(origin))
    } else if (is.character(origin) && length(origin) == 1L && !is.na(origin)) {
        spec <- .date_epochs[[tolower(origin)]]
        if (!is.null(spec)) return(spec)
        days <- fast_parse_date_impl(origin, 0L)
        # The parser skips days-in-month checks, so demand an exact round trip.
        if (!identical(fast_format_date_impl(days, 0L), origin)) days <- NA_real_
    }
    if (!is.finite(days))
        stop("`origin` must be one of ",
             paste0("\"", names(.date_epochs), "\"", collapse = ", "),
             ", a Date, or a \"YYYY-MM-DD\" string.")
    c(divisor = 1, frac = 0, shift = days, excel = 0)
}

#' Fast fixed-format timestamp parsing
#'
#' Parses strict, locale-free timestamps directly into UTC seconds since the
#' Unix epoch. Unlike [base::as.POSIXct()], this function does not consult a
#' timezone database or try multiple formats. Calendar dates are validated,
#' including month lengths and leap years.
#'
#' @param x Character vector. Malformed or out-of-range values become `NA`.
#' @param format One of `"iso"` (`YYYY-MM-DD HH:MM:SS`), `"rfc3339"`
#'   (`YYYY-MM-DDTHH:MM:SSZ`), `"compact"` (`YYYYMMDDHHMMSS`), or
#'   `"iso_offset"` (`YYYY-MM-DDTHH:MM:SS+HH:MM`).
#' @return A `POSIXct` vector in UTC with the same length and names as `x`.
#' @seealso [format_datetime()], [fas.Date()]
#' @family date and timestamp functions
#' @examples
#' # 2023 is not a leap year, so the second value is NA.
#' fas.POSIXct(c("2024-06-18 09:15:00", "2023-02-29 00:00:00"))
#' fas.POSIXct("20240618091500", format = "compact")
#' @export
fas.POSIXct <-function(
    x,
    format = c("iso", "rfc3339", "compact", "iso_offset")
) {
    if (!is.character(x)) {
        if (all(is.na(x))) x <- as.character(x)
        else stop("`x` must be a character vector.")
    }
    format <- match.arg(format)
    code <- switch(
        format, iso = 0L, rfc3339 = 1L, compact = 2L, iso_offset = 3L
    )
    result <- .copy_names(fast_parse_datetime_impl(x, code), x)
    structure(result, class = c("POSIXct", "POSIXt"), tzone = "UTC")
}

#' Fast fixed-format timestamp formatting
#'
#' Formats Unix-epoch seconds without locale or timezone-database overhead.
#' The `"iso_offset"` form applies one fixed numeric offset to every value;
#' other formats are emitted in UTC.
#'
#' @param x A `POSIXct` object or numeric vector of seconds since
#'   1970-01-01 00:00:00 UTC.
#' @param format One of the four formats accepted by [fas.POSIXct()].
#' @param offset Fixed offset written by `format = "iso_offset"`, as `"Z"`
#'   or a signed `"+HH:MM"`/`"-HH:MM"` string. It must be `"Z"` for other
#'   formats.
#' @return Character vector the same length as `x`.
#' @seealso [fas.POSIXct()], [format_date()]
#' @family date and timestamp functions
#' @examples
#' ts <- fas.POSIXct("2024-06-18 09:15:00")
#' format_datetime(ts, "rfc3339")
#' # The same instant, written with a +10:00 offset (wall-clock 19:15).
#' format_datetime(ts, "iso_offset", offset = "+10:00")
#' @export
format_datetime <-function(
    x,
    format = c("iso", "rfc3339", "compact", "iso_offset"),
    offset = "Z"
) {
    if (inherits(x, "POSIXct")) x <- unclass(x)
    if (!is.numeric(x))
        stop("`x` must be POSIXct or numeric Unix-epoch seconds.")
    format <- match.arg(format)
    offset_minutes <- .datetime_offset_minutes(offset)
    if (!identical(format, "iso_offset") && offset_minutes != 0L)
        stop("`offset` must be \"Z\" unless `format = \"iso_offset\"`.")
    code <- switch(
        format, iso = 0L, rfc3339 = 1L, compact = 2L, iso_offset = 3L
    )
    .copy_names(fast_format_datetime_impl(
        as.double(x), code, offset_minutes
    ), x)
}

.datetime_offset_minutes <- function(offset) {
    if (!is.character(offset) || length(offset) != 1L || is.na(offset))
        stop("`offset` must be one non-missing string.")
    if (identical(offset, "Z")) return(0L)
    if (!base::grepl("^[+-][0-9]{2}:[0-9]{2}$", offset))
        stop("`offset` must be \"Z\" or a signed \"+HH:MM\"/\"-HH:MM\" string.")
    hours <- as.integer(substr(offset, 2L, 3L))
    minutes <- as.integer(substr(offset, 5L, 6L))
    if (hours > 23L || minutes > 59L)
        stop("`offset` hour must be <= 23 and minute must be <= 59.")
    value <- hours * 60L + minutes
    if (substr(offset, 1L, 1L) == "-") -value else value
}
