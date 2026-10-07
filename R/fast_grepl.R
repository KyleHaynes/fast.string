#' Fast parallel string matching
#'
#' Equivalent to [base::grepl()], using PCRE2 and Intel TBB.
#' Large inputs can use multiple cores; the crossover depends on subject
#' length, pattern cost, match density, and the available cores.
#'
#' With `perl = FALSE` (the default) the pattern follows base R's default
#' regular expressions: `.` also matches a newline, `$` matches only at the
#' very end, a backslash inside a bracket expression is a literal, `\\<` and
#' `\\>` match word boundaries, and for non-ASCII text `\\w` and classes such
#' as `[[:alpha:]]` follow Unicode. One difference remains: where
#' alternatives can match at the same position (`"a|ab"`), the first
#' alternative wins, as with `perl = TRUE`, rather than the longest match.
#' Patterns that only PCRE understands (lookarounds, atomic groups,
#' possessive quantifiers, named groups and recursion) are run with
#' `perl = TRUE` semantics.
#'
#' Text is matched as UTF-8 characters, translating `latin1`-marked input,
#' unless `useBytes = TRUE`.
#'
#' @param pattern Character scalar. Pattern to search for. `NA` gives an all
#'   `NA` result, as in base R.
#' @param x Character vector. `NA` elements return `NA`.
#' @param ignore.case Logical. Case-insensitive matching. With
#'   `fixed = TRUE` only ASCII letters are folded.
#' @param perl Logical. Use PCRE (Perl-compatible) regular expression
#'   semantics instead of base R's default ones; see Details.
#' @param fixed Logical. Treat `pattern` as a literal string (fastest path).
#' @param useBytes Logical. If `TRUE`, match byte by byte rather than by
#'   UTF-8 character, as in base R.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#'
#' @return Logical vector the same length as `x`.
#' @seealso [base::grepl()]
#' @family matching and substitution functions
#' @examples
#' x <- c("apple pie", "banana split", NA, "cherry tart")
#'
#' fgrepl("an", x)                 # NA in, NA out
#' fgrepl("^[ab]", x)              # regular expression (PCRE2)
#' fgrepl("APPLE", x, fixed = TRUE, ignore.case = TRUE)
#' @export
fgrepl <-function(pattern, x, ignore.case = FALSE, perl = FALSE,
                  fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    pattern <- .na_as_character(pattern)
    if (!is.character(pattern) || length(pattern) != 1L)
        stop("`pattern` must be a single character string.")
    x <- .character_x(x)
    if (is.na(pattern)) return(rep(NA, length(x)))
    threads <- .as_nthreads(nthreads)
    spec <- .regex_spec(pattern, perl, fixed, useBytes)

    if (isTRUE(fixed)) {
        return(fast_fixed_impl(
            spec$pattern, x, isTRUE(ignore.case), spec$use_bytes, threads
        ))
    }
    fast_grepl_impl(
        spec$pattern, x, isTRUE(ignore.case), spec$syntax, spec$use_bytes,
        threads
    )
}

#' Fast parallel match counting
#'
#' Counts non-overlapping matches of one pattern in each element of `x`.
#' This is the counting counterpart to [fgrepl()], using the same PCRE2 and
#' prepared fixed-string engines and the same parallel dispatch policy.
#'
#' @inheritParams fgrepl
#' @return Integer vector the same length as `x`. Missing inputs return `NA`.
#' @seealso [base::gregexpr()]
#' @family matching and substitution functions
#' @examples
#' x <- c("apple pie", "banana split", NA, "cherry tart")
#'
#' fcount("a", x, fixed = TRUE)
#' fcount("[aeiou]", x)            # vowels per string
#' @export
fcount <- function(pattern, x, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE, nthreads = NULL) {
    if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern))
        stop("`pattern` must be a single non-missing character string.")
    x <- .character_x(x)

    # Empty patterns have character-position semantics in base R, whereas the
    # fast fixed engine deliberately operates on bytes. Keep this rare edge
    # case exact instead of silently returning byte counts for UTF-8 strings.
    if (identical(pattern, "")) {
        return(.base_count_matches(
            pattern, x, ignore.case, perl, fixed, useBytes
        ))
    }

    threads <- .as_nthreads(nthreads)
    spec <- .regex_spec(pattern, perl, fixed, useBytes)
    if (isTRUE(fixed)) {
        return(.copy_names(fast_fixed_count_impl(
            spec$pattern, x, isTRUE(ignore.case), spec$use_bytes, threads
        ), x))
    }
    .copy_names(fast_regex_count_impl(
        spec$pattern, x, isTRUE(ignore.case), spec$syntax, spec$use_bytes,
        threads
    ), x)
}

.base_count_matches <- function(pattern, x, ignore.case, perl, fixed, useBytes) {
    matches <- base::gregexpr(
        pattern, x, ignore.case = ignore.case, perl = perl,
        fixed = fixed, useBytes = useBytes
    )
    result <- vapply(matches, function(hit) {
        if (length(hit) == 1L && is.na(hit)) return(NA_integer_)
        if (length(hit) == 1L && hit[[1L]] < 0L) return(0L)
        length(hit)
    }, integer(1L))
    .copy_names(result, x)
}

# base R accepts a logical NA wherever it accepts a missing string.
.na_as_character <- function(value) {
    if (is.logical(value) && length(value) == 1L && is.na(value))
        NA_character_
    else
        value
}

.character_x <- function(x) {
    if (is.character(x)) return(x)
    if (all(is.na(x))) return(as.character(x))
    stop("`x` must be a character vector.")
}

# Prepares patterns (and replacements) for the C++ engines as base R does:
# text is matched as UTF-8, with latin1 or native strings translated, unless
# useBytes = TRUE, a pattern or replacement is marked "bytes", or one is not
# valid UTF-8 -- then everything is compared byte by byte. `syntax` is 0 for
# PCRE semantics (perl = TRUE, and patterns only PCRE understands) and 1 for
# base R's default extended regular expressions, emulated in C++.
.regex_spec <- function(pattern, perl = FALSE, fixed = FALSE,
                        useBytes = FALSE, replacement = NULL) {
    text <- c(pattern, replacement[!is.na(replacement)])
    use_bytes <- isTRUE(useBytes) || any(Encoding(text) == "bytes")
    if (!use_bytes) {
        pattern <- enc2utf8(pattern)
        if (!is.null(replacement)) replacement <- enc2utf8(replacement)
        text <- c(pattern, replacement[!is.na(replacement)])
        use_bytes <- !all(validUTF8(text))
    }
    syntax <- if (isTRUE(perl) || isTRUE(fixed)) {
        rep(0L, length(pattern))
    } else {
        ifelse(.has_pcre_only_syntax(pattern), 0L, 1L)
    }
    list(pattern = pattern, replacement = replacement, syntax = syntax,
         use_bytes = use_bytes)
}
