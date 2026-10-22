#' Fast parallel approximate (fuzzy) matching
#'
#' Equivalents of [base::agrepl()] and [base::agrep()]: does each string
#' contain something within `max.distance` edits (insertions, deletions,
#' substitutions) of `pattern`? So `fagrepl("lasy", "1 lazy 2", 1)` is
#' `TRUE`. Strings are searched with Myers' bit-parallel algorithm (1999),
#' in parallel across CPU cores via Intel TBB.
#'
#' The fast path covers base R's default use: a literal pattern
#' (`fixed = TRUE`), unit costs, and `max.distance` given as a number or as
#' a list with only `cost` and/or `all`. As in base R, a `max.distance` below
#' 1 is a fraction of the pattern length, rounded up (so the default `0.1`
#' allows one edit for patterns of up to 10 characters). Other calls -- a
#' regular expression pattern, custom `costs`, or limits on insertions,
#' deletions or substitutions separately -- are passed to base R.
#'
#' Text is compared by UTF-8 character unless `useBytes = TRUE`; with
#' `ignore.case = TRUE` letters are lower-cased (Unicode-aware) on both
#' sides.
#'
#' @param pattern Non-empty character scalar. `NA` gives an all-`NA` result.
#' @param x Character vector. `NA` elements return `NA` (base R's `agrepl()`
#'   returns `FALSE` for them).
#' @param max.distance,costs As in [base::agrepl()].
#' @param ignore.case Logical. Case-insensitive matching.
#' @param fixed Logical. Treat `pattern` as a literal string (the fast path;
#'   `FALSE` passes the call to base R).
#' @param useBytes Logical. Compare bytes rather than characters.
#' @param value Logical. For `fagrep()`: return the matching elements
#'   instead of their indices.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return `fagrepl()`: a logical vector the same length as `x`, with
#'   `names(x)` kept. `fagrep()`: the indices of the matching elements, or
#'   with `value = TRUE` the elements themselves.
#' @seealso [fgrepl()] for exact matching, [levenshtein()] for whole-string
#'   distances.
#' @family matching and substitution functions
#' @examples
#' x <- c("1 lazy 2", "1 lasy 2", "1 LAZY 2", NA, "laser")
#' fagrepl("lasy", x)
#' fagrepl("lasy", x, ignore.case = TRUE)
#' fagrepl("lasy", x, max.distance = 0)
#' fagrep("lasy", x, value = TRUE)
#' @export
fagrepl <- function(pattern, x, max.distance = 0.1, costs = NULL,
                    ignore.case = FALSE, fixed = TRUE, useBytes = FALSE,
                    nthreads = NULL) {
    pattern <- .na_as_character(pattern)
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    x <- .character_x(x)
    if (is.na(pattern)) return(.copy_names(rep(NA, length(x)), x))
    threads <- .as_nthreads(nthreads)
    bound <- .agrep_bound(max.distance, costs)
    if (!isTRUE(fixed) || is.null(bound)) {
        result <- base::agrepl(pattern, x, max.distance = max.distance,
                               costs = costs, ignore.case = ignore.case,
                               fixed = fixed, useBytes = useBytes)
        result[is.na(x)] <- NA
        return(.copy_names(result, x))
    }
    if (!nzchar(pattern))
        stop("'pattern' must be a non-empty character string")
    spec <- .regex_spec(pattern, fixed = TRUE, useBytes = useBytes)
    .copy_names(fast_agrepl_impl(
        spec$pattern, x, bound, isTRUE(ignore.case), spec$use_bytes, threads
    ), x)
}

#' @rdname fagrepl
#' @export
fagrep <- function(pattern, x, max.distance = 0.1, costs = NULL,
                   ignore.case = FALSE, value = FALSE, fixed = TRUE,
                   useBytes = FALSE, nthreads = NULL) {
    hit <- fagrepl(pattern, x, max.distance = max.distance, costs = costs,
                   ignore.case = ignore.case, fixed = fixed,
                   useBytes = useBytes, nthreads = nthreads)
    keep <- !is.na(hit) & hit
    if (isTRUE(value)) x[keep] else unname(which(keep))
}

# The edit bound for the fast path, or NULL when base R must handle the
# call: costs other than all-ones, or per-operation limits.
.agrep_bound <- function(max.distance, costs) {
    if (!is.null(costs)) {
        values <- unlist(costs)
        if (!is.numeric(values) || !all(values == 1)) return(NULL)
    }
    if (is.numeric(max.distance) && length(max.distance) == 1L &&
        !is.na(max.distance) && max.distance >= 0)
        return(as.double(max.distance))
    if (!is.list(max.distance) || !length(max.distance)) return(NULL)
    table <- c("cost", "insertions", "deletions", "substitutions", "all")
    pos <- pmatch(names(max.distance), table)
    if (anyNA(pos) || !all(table[pos] %in% c("cost", "all"))) return(NULL)
    values <- unlist(max.distance)
    if (!is.numeric(values) || anyNA(values) || any(values < 0)) return(NULL)
    # With unit costs a lone `cost` or `all` bound is the plain edit bound
    # (a fraction is resolved against the pattern length in C++).
    if (length(values) == 1L) return(as.double(values))
    NULL
}
