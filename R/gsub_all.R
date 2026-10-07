#' Apply multiple substitutions in one pass
#'
#' Replace several patterns in `x` either sequentially (one full pass per
#' pattern, like chaining [fgsub()] calls) or, for fixed patterns, in a single
#' combined scan (`sequential = FALSE`), parallelised across CPU cores via
#' Intel TBB. Non-sequential regular-expression replacement is rejected
#' because combined capture/backreference semantics are not implemented.
#'
#' @param patterns Character vector of patterns to search for. An `NA`
#'   pattern makes every result `NA`, as chaining [base::gsub()] would.
#' @param replacements Character scalar or vector the same length as
#'   `patterns`. Elements that an `NA` replacement applies to become `NA`.
#' @param x Character vector. `NA` elements return `NA`.
#' @param fixed Logical. Treat each pattern as a literal string.
#' @param ignore.case Logical. Case-insensitive matching.
#' @param sequential Logical. If `TRUE` (default), apply patterns one after
#'   another (later patterns can match text introduced by earlier
#'   replacements). If `FALSE`, match all patterns in a single left-to-right
#'   scan (first pattern to match at each position wins). This mode requires
#'   `fixed = TRUE`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param perl,useBytes As in [fgsub()].
#' @return Character vector the same length as `x`.
#' @seealso [fgsub()] for a single pattern.
#' @family matching and substitution functions
#' @examples
#' x <- c("cat and hat", "a cat")
#' gsub_all(c("cat", "hat"), c("dog", "cap"), x, fixed = TRUE)
#'
#' # Sequential mode chains the patterns, so "a" -> "b" -> "c" cascades...
#' gsub_all(c("a", "b"), c("b", "c"), "ab", fixed = TRUE)
#' # ...whereas a single scan replaces each position once.
#' gsub_all(c("a", "b"), c("b", "c"), "ab", fixed = TRUE, sequential = FALSE)
#' @export
gsub_all <-function(patterns, replacements, x,
                     fixed = FALSE, ignore.case = FALSE,
                     sequential = TRUE,
                     nthreads = NULL, perl = FALSE, useBytes = FALSE) {
    if (!is.character(patterns) || length(patterns) == 0L)
        stop("`patterns` must be a non-empty character vector.")
    if (is.logical(replacements) && all(is.na(replacements)))
        replacements <- as.character(replacements)
    if (!is.character(replacements) ||
        !length(replacements) %in% c(1L, length(patterns)))
        stop("`replacements` must be length 1 or the same length as `patterns`.")
    if (length(replacements) == 1L)
        replacements <- rep_len(replacements, length(patterns))
    x <- .character_x(x)

    threads <- .as_nthreads(nthreads)
    if (anyNA(patterns)) return(rep(NA_character_, length(x)))

    if (isTRUE(fixed)) {
        if (any(patterns == ""))
            stop("zero-length pattern")
        spec <- .regex_spec(patterns, fixed = TRUE, useBytes = useBytes,
                            replacement = replacements)
        return(fast_fixed_gsub_all_impl(
            spec$pattern, spec$replacement, x, isTRUE(ignore.case),
            isTRUE(sequential), spec$use_bytes, threads
        ))
    }

    if (!isTRUE(sequential))
        stop("`sequential = FALSE` is not supported for regular-expression patterns; use `fixed = TRUE` or `sequential = TRUE`.")

    spec <- .regex_spec(patterns, perl, useBytes = useBytes,
                        replacement = replacements)
    fast_regex_gsub_all_impl(
        spec$pattern, spec$replacement, x, isTRUE(ignore.case),
        spec$syntax, spec$use_bytes, threads
    )
}
