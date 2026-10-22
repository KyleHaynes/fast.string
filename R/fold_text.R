#' Fold accented and other Latin characters to ASCII
#'
#' Transliterates each string to ASCII the way ICU's `"Latin-ASCII"`
#' transform does (`stringi::stri_trans_general(x, "Latin-ASCII")`), but
#' from a built-in table and in parallel across CPU cores via Intel TBB.
#' Accented letters lose their accents (`"é"` to `"e"`), ligatures and
#' special letters are spelled out (`"Æ"` to `"AE"`, `"ß"` to `"ss"`,
#' `"Þ"` to `"TH"`), and typographic punctuation and spaces become their
#' ASCII equivalents (curly quotes, dashes, the no-break space, `"…"` to
#' `"..."`). Combining accents that follow a Latin letter or a digit are
#' removed, so decomposed text folds the same way as precomposed text.
#'
#' Characters the transform does not cover -- other scripts such as Greek,
#' Cyrillic or Chinese, and symbols such as `"€"` -- are kept unless `sub`
#' is given. Unlike `iconv(x, to = "ASCII//TRANSLIT")`, the result does not
#' depend on the platform.
#'
#' The table was generated from ICU 74.1 (Unicode 15.1) by
#' `tools/gen_ascii_fold.R`. For text in Unicode normal form NFC (the form
#' almost all text is in) the result is exactly ICU's. Otherwise the only
#' difference is that ICU also sorts stacked combining marks that it keeps
#' (on non-Latin letters) into canonical order.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). `NA` elements return `NA`.
#' @param sub `NULL` (the default) to keep characters that cannot be folded,
#'   or a single string to replace each of them with, such as `""` to drop
#'   them or `"?"` to mark them. The result is then always ASCII.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [fingerprint()], which folds text the way OpenRefine does before
#'   building a clustering key.
#' @family string utilities
#' @examples
#' ascii_fold(c("Fran\u00e7ois M\u00fcller", "\u00c6sir \u00d8rsted", "Stra\u00dfe"))
#' ascii_fold("\u201cquoted\u201d \u2013 \u00bd")
#' ascii_fold("\u0394\u03b9\u03bf\u03bd\u03cd\u03c3\u03b7\u03c2 caf\u00e9", sub = "")
#' @export
ascii_fold <- function(x, sub = NULL, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    if (!is.null(sub) &&
        (!is.character(sub) || length(sub) != 1L || is.na(sub)))
        stop("`sub` must be NULL or a single string.")
    .copy_names(fast_ascii_fold_impl(
        x, if (is.null(sub)) "" else enc2utf8(sub), !is.null(sub),
        .as_nthreads(nthreads)
    ), x)
}

#' OpenRefine clustering keys
#'
#' Key-collision keys from OpenRefine's "Cluster and edit" tool, for
#' grouping spelling variants of the same value (company names, street
#' names, free-text categories) or as blocking keys for record linkage.
#' Values with the same key are candidates for the same cluster. Keys are
#' built in parallel across CPU cores via Intel TBB.
#'
#' `fingerprint()` is OpenRefine's fingerprint keying method: the value is
#' lowercased, Unicode-decomposed (NFKD), and stripped of accents, modifier
#' letters and symbols, punctuation and control characters; a few letters
#' are spelled out (`"ß"` to `"ss"`, `"æ"` and `"ø"` to `"ae"` and `"oe"`,
#' `"þ"` to `"th"`); then the words are sorted, de-duplicated and joined
#' with single spaces. So `"Smith, John"`, `"john SMITH"` and
#' `"John Smith."` all key as `"john smith"`. Punctuation is deleted rather
#' than replaced by a space, so `"Smith-Jones"` keys as `"smithjones"`.
#' Symbols such as `"+"`, `"$"` and `"|"` are kept, as in OpenRefine.
#'
#' `ngram_fingerprint()` is OpenRefine's n-gram fingerprint: the same
#' normalisation, then all whitespace is removed and the key is the sorted,
#' de-duplicated character n-grams joined together. It also catches
#' misplaced spaces and some typos (`"Pty Ltd"` and `"PtyLtd"`) at the cost
#' of more false matches.
#'
#' The normalisation table was generated from OpenRefine's own rules with
#' Java's Unicode tables (`tools/GenFingerprintTable.java`). Differences from
#' OpenRefine: punctuation at the start of a value never leaves an empty
#' leading word (OpenRefine's key then starts with a space); a capital
#' sigma lowercases to the medial form even at the end of a word; and in
#' `ngram_fingerprint()` a value shorter than `n` characters keys as itself,
#' where OpenRefine gives every such value the same empty key.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). `NA` elements return `NA`.
#' @param n Positive integer n-gram length (default `2`, as in OpenRefine).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector of keys the same length as `x`, with `names(x)`
#'   kept.
#' @seealso [ascii_fold()] for transliteration alone, and the phonetic codes
#'   such as [soundex()] for keys that group by sound.
#' @family string utilities
#' @examples
#' x <- c("Smith, John", "john SMITH", "John  Smith.", "Jon Smith")
#' fingerprint(x)
#' split(x, fingerprint(x))
#'
#' ngram_fingerprint(c("Acme Pty Ltd", "ACME PtyLtd", "Acme Pty. Ltd."))
#' @export
fingerprint <- function(x, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_fingerprint_impl(x, .as_nthreads(nthreads)), x)
}

#' @rdname fingerprint
#' @export
ngram_fingerprint <- function(x, n = 2L, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 ||
        n != floor(n) || n > .Machine$integer.max)
        stop("`n` must be a single positive integer.")
    .copy_names(fast_ngram_fingerprint_impl(
        x, as.integer(n), .as_nthreads(nthreads)
    ), x)
}
