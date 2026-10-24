#' Fast token frequencies across a character vector
#'
#' Counts every whitespace-separated token in `x`: how often it occurs
#' (`n`) and in how many strings (`docs`). It is the quick way to see the
#' vocabulary of a name, address or free-text column -- the most common
#' tokens are the candidates for a stop-word or abbreviation dictionary
#' ([freplace_tokens()]), and their frequencies are the usual basis for
#' frequency-weighted name matching. It replaces
#' `table(unlist(strsplit(x, " ")))`, counting in parallel across CPU cores
#' via Intel TBB without building the list of tokens.
#'
#' Tokens are runs of characters other than Unicode `White_Space`, as in
#' [freplace_tokens()] and [token_sort()]; strip punctuation first when it
#' is attached to words. `NA` strings are skipped.
#'
#' @param x Character vector.
#' @param ignore_case Logical. Count tokens regardless of the case of ASCII
#'   letters; tokens are then reported in lower case.
#' @param min_count Only report tokens that occur at least this many times.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return A data frame with columns `token`, `n` (occurrences) and `docs`
#'   (strings containing the token), most frequent first and ties in byte
#'   order of the token.
#' @seealso [freplace_tokens()] to act on the result, [tfidf_top_n()] for
#'   n-gram weighting.
#' @family token functions
#' @examples
#' addr <- c("12 SMITH ST", "4 SMITH ST", "9 KING ST", "UNIT 2 12 KING RD", NA)
#' token_counts(addr)
#' token_counts(c("St Kilda Rd", "ST KILDA RD"), ignore_case = TRUE)
#' @export
token_counts <- function(x, ignore_case = FALSE, min_count = 1,
                         nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(ignore_case, "ignore_case")
    if (!is.numeric(min_count) || length(min_count) != 1L || is.na(min_count))
        stop("`min_count` must be a single number.")
    res <- fast_token_counts_impl(x, ignore_case, .as_nthreads(nthreads))
    out <- data.frame(token = res$token, n = res$n, docs = res$docs)
    if (min_count > 1) out <- out[out$n >= min_count, , drop = FALSE]
    rownames(out) <- NULL
    out
}

#' Fast sorting of the words within each string
#'
#' Splits each string at whitespace, sorts the tokens and joins them with
#' single spaces, so that `"SMITH JOHN"` and `"JOHN SMITH"` give the same
#' key. With `unique = TRUE` repeated tokens are dropped too. This is the
#' "token sort" step of fuzzy matching (as in RapidFuzz's
#' `token_sort_ratio`) and a cheap order-insensitive blocking or
#' deduplication key; unlike [fingerprint()] it changes nothing but the
#' order. Strings are processed in parallel across CPU cores via Intel TBB.
#'
#' Tokens are compared by their bytes, which is code-point order for UTF-8
#' (so all upper-case ASCII letters sort before lower-case ones), whatever
#' the locale.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param unique Logical. Drop repeated tokens.
#' @param decreasing Logical. Sort in decreasing order.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [fingerprint()], [fuzz_token_sort_ratio()].
#' @family token functions
#' @examples
#' token_sort(c("SMITH JOHN", "JOHN  SMITH", "SMITH SMITH JOHN", NA))
#' token_sort("SMITH SMITH JOHN", unique = TRUE)
#' @export
token_sort <- function(x, unique = FALSE, decreasing = FALSE, nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(unique, "unique")
    .check_flag(decreasing, "decreasing")
    .copy_names(fast_token_sort_impl(
        x, unique, decreasing, .as_nthreads(nthreads)
    ), x)
}

#' Fast initials of the words in each string
#'
#' Returns the first character of every whitespace-separated token, joined
#' by `sep`: `"JOHN PAUL SMITH"` gives `"JPS"`. Initials are a common
#' blocking key and comparison field in record linkage, where given names
#' are often recorded as initials only. Strings are processed in parallel
#' across CPU cores via Intel TBB.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param sep String placed between the initials (default none).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @family token functions
#' @examples
#' token_initials(c("JOHN PAUL SMITH", "Mary  Jane", "", NA))
#' token_initials("John Paul", sep = ".")
#' @export
token_initials <- function(x, sep = "", nthreads = NULL) {
    x <- .character_x(x)
    if (!is.character(sep) || length(sep) != 1L || is.na(sep))
        stop("`sep` must be a single string.")
    .copy_names(fast_token_initials_impl(
        x, enc2utf8(sep), .as_nthreads(nthreads)
    ), x)
}

#' Fast string reversal
#'
#' Reverses the characters of each string, as `stringi::stri_reverse()`
#' does, but keeps combining marks with the letter they belong to, so an
#' accent written as a separate code point stays on its letter (`stringi`
#' reverses code points, which moves it to the previous one). Reversed keys
#' let the sorted neighbourhood method catch errors at the start of a
#' string ([sorted_neighbourhood()]). Strings are processed in parallel
#' across CPU cores via Intel TBB.
#'
#' Other sequences of several code points (emoji with modifiers, flags) are
#' reversed code point by code point. Strings marked `"bytes"` are reversed
#' byte by byte.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @family token functions
#' @examples
#' freverse(c("SMITH", "caf\u00e9", NA))
#' @export
freverse <- function(x, nthreads = NULL) {
    x <- .character_x(x)
    .copy_names(fast_reverse_impl(x, .as_nthreads(nthreads)), x)
}

#' Longest matching prefix or suffix from a table
#'
#' For each string, the index of the longest element of `prefixes` that it
#' starts with (`longest_prefix()`), or of `suffixes` that it ends with
#' (`longest_suffix()`). It maps codes to categories wherever the category
#' is given by a leading or trailing part: phone numbers to area codes,
#' ICD or ANZSIC codes to their groups, postcodes to regions, email
#' addresses or host names to domains. One hashed probe per distinct affix
#' length replaces a loop of `startsWith()` over the table, and strings are
#' processed in parallel across CPU cores via Intel TBB.
#'
#' Matching is by bytes (of the UTF-8 text), and with `ignore_case` ASCII
#' letters match regardless of case. When `prefixes` contains duplicates the
#' first one wins.
#'
#' @param x Character vector. `NA` elements give `NA`.
#' @param prefixes,suffixes Character vector of non-empty strings, without
#'   `NA`.
#' @param ignore_case Logical. Fold ASCII letters.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Integer vector the same length as `x`: an index into `prefixes`
#'   (or `suffixes`), or `NA` where none matches. Use it to look up a
#'   category, as in `category[longest_prefix(x, prefixes)]`.
#' @family token functions
#' @examples
#' area <- data.frame(prefix = c("02", "03", "07", "08", "04", "13", "1300", "1800"),
#'                    type = c("NSW/ACT", "VIC/TAS", "QLD", "SA/WA/NT", "mobile",
#'                             "13 number", "local rate", "freecall"))
#' phone <- c("0732123456", "0412345678", "131450", "1300123456", "9999")
#' area$type[longest_prefix(phone, area$prefix)]
#'
#' email <- c("a@health.qld.gov.au", "b@gmail.com", "c@uq.edu.au")
#' domains <- c(".gov.au", ".edu.au", "gmail.com", ".qld.gov.au")
#' domains[longest_suffix(email, domains)]
#' @export
longest_prefix <- function(x, prefixes, ignore_case = FALSE, nthreads = NULL) {
    .longest_affix(x, prefixes, FALSE, ignore_case, nthreads, "prefixes")
}

#' @rdname longest_prefix
#' @export
longest_suffix <- function(x, suffixes, ignore_case = FALSE, nthreads = NULL) {
    .longest_affix(x, suffixes, TRUE, ignore_case, nthreads, "suffixes")
}

.longest_affix <- function(x, affixes, suffix, ignore_case, nthreads, arg) {
    x <- .character_x(x)
    if (!is.character(affixes) || anyNA(affixes) || any(!nzchar(affixes)))
        stop(sprintf("`%s` must be a character vector of non-empty strings.", arg))
    .check_flag(ignore_case, "ignore_case")
    .copy_names(fast_longest_affix_impl(
        x, affixes, suffix, ignore_case, .as_nthreads(nthreads)
    ), x)
}
