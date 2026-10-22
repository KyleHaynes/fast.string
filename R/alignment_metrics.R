#' Editex phonetic edit distance
#'
#' Editex (Zobel & Dart, 1996) is an edit distance for names that knows
#' which letters sound alike. Replacing a letter with one from the same
#' sound group (`AEIOUY`, `BP`, `CKQ`, `DT`, `LR`, `MN`, `GJ`, `FPV`, `SXZ`,
#' `CSZ`) costs 1 instead of 2, and an `H` or `W` that follows another
#' letter can be inserted or deleted for 1. So `editex("Phillips",
#' "Filips")` is smaller than their Levenshtein distance suggests it should
#' be, relative to unrelated names.
#'
#' The cost table and dynamic program follow the Python `abydos` and
#' `textdistance` implementations. Letters are compared case-insensitively
#' after folding accented Latin letters to ASCII (as the phonetic codes
#' such as [soundex()] do); other characters cost 2 to change unless
#' identical. `editex_similarity()` is `1 - editex / (2 * the longer
#' length)`, `textdistance`'s normalisation.
#'
#' @inheritParams lcs_seq
#' @return Numeric vector the same length as `a`; `NA` where either input is
#'   `NA`.
#' @references Zobel, J. and Dart, P. (1996). Phonetic string matching:
#'   lessons from information retrieval. SIGIR '96, 166-172.
#' @family edit distance functions
#' @examples
#' editex(c("Phillips", "Smith", "Catherine"), c("Filips", "Smyth", "Kathryn"))
#' levenshtein(c("Phillips", "Smith", "Catherine"), c("Filips", "Smyth", "Kathryn"))
#' editex_similarity("Niall", "Neil")
#' @export
editex <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_editex_impl(a, b, FALSE, use_bytes, .as_nthreads(nthreads))
}

#' @rdname editex
#' @export
editex_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_editex_impl(a, b, TRUE, use_bytes, .as_nthreads(nthreads))
}

#' Longest common substring
#'
#' `lcs_str()` returns the longest run of characters that `a[i]` and `b[i]`
#' share contiguously -- unlike the longest common *subsequence* of
#' [lcs_seq_distance()], whose characters may be spread out. Where several
#' are equally long, it is the one that starts earliest in `a`, then
#' earliest in `b`, as Python's `difflib.SequenceMatcher.find_longest_match()`
#' chooses. `lcs_str_similarity()` is its length divided by the longer
#' string's length (the `textdistance` package's `LCSStr` normalisation).
#'
#' @inheritParams lcs_seq
#' @return `lcs_str()`: a character vector the same length as `a`, `""`
#'   where the strings share no character. `lcs_str_similarity()`: numeric
#'   values in `[0, 1]`, `1` for two empty strings. Both are `NA` where
#'   either input is `NA`.
#' @family edit distance functions
#' @examples
#' lcs_str("Acme Holdings Pty Ltd", "ACME HOLDINGS LIMITED")
#' lcs_str(tolower("Acme Holdings Pty Ltd"), tolower("ACME HOLDINGS LIMITED"))
#' lcs_str_similarity("Unit 4, 12 Smith Street", "12 Smith St")
#' @export
lcs_str <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_lcs_str_impl(a, b, FALSE, use_bytes, .as_nthreads(nthreads))
}

#' @rdname lcs_str
#' @export
lcs_str_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    fast_lcs_str_impl(a, b, TRUE, use_bytes, .as_nthreads(nthreads))
}

#' Smith-Waterman-Gotoh local alignment similarity
#'
#' Scores the best *local* alignment of `a[i]` and `b[i]` (Smith &
#' Waterman, 1981): the best-matching stretch of each string, ignoring the
#' rest, so `"Dept of Health"` aligns well with `"Queensland Health"` on
#' `"Health"`. Matching characters score `match`, mismatches `mismatch`, and
#' a gap of `k` characters `gap_open + (k - 1) * gap_extend` (Gotoh's affine
#' gaps, 1982). The defaults reproduce the `SmithWatermanGotoh` metric of
#' the Java SimMetrics library, a common choice in record linkage: match 1,
#' mismatch -2 and every gap position -0.5.
#'
#' With `normalize = TRUE` (the default) the score is divided by the best
#' possible one, `match` times the shorter length, giving a similarity in
#' `[0, 1]`; two empty strings score `1` and one empty string `0`.
#'
#' @inheritParams lcs_seq
#' @param match Positive score for two equal characters.
#' @param mismatch Score (usually negative) for two different characters.
#' @param gap_open,gap_extend Scores (zero or negative) for the first and
#'   each further character of a gap. The default `gap_extend = gap_open`
#'   gives linear gaps.
#' @param normalize Logical. Divide by the best possible score.
#' @return Numeric vector the same length as `a`; `NA` where either input is
#'   `NA`.
#' @references Smith, T. F. and Waterman, M. S. (1981). Identification of
#'   common molecular subsequences. Journal of Molecular Biology 147,
#'   195-197.
#'
#'   Gotoh, O. (1982). An improved algorithm for matching biological
#'   sequences. Journal of Molecular Biology 162, 705-708.
#' @family edit distance functions
#' @examples
#' smith_waterman("Dept of Health", "Queensland Health")
#' smith_waterman("Dept of Health", "Queensland Health", normalize = FALSE)
#' smith_waterman("GATTACA", "GCATGCU", mismatch = -1, gap_open = -2,
#'                gap_extend = -1, normalize = FALSE)
#' @export
smith_waterman <- function(a, b, match = 1, mismatch = -2, gap_open = -0.5,
                           gap_extend = gap_open, normalize = TRUE,
                           nthreads = NULL, use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    num <- function(v, name) {
        if (!is.numeric(v) || length(v) != 1L || is.na(v) || !is.finite(v))
            stop("`", name, "` must be a single number.")
        as.double(v)
    }
    match <- num(match, "match")
    mismatch <- num(mismatch, "mismatch")
    gap_open <- num(gap_open, "gap_open")
    gap_extend <- num(gap_extend, "gap_extend")
    if (match <= 0) stop("`match` must be positive.")
    if (gap_open > 0 || gap_extend > 0)
        stop("`gap_open` and `gap_extend` must be zero or negative.")
    if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize))
        stop("`normalize` must be TRUE or FALSE.")
    fast_smith_waterman_impl(a, b, match, mismatch, gap_open, gap_extend,
                             normalize, use_bytes, .as_nthreads(nthreads))
}

#' Monge-Elkan token similarity
#'
#' The Monge-Elkan similarity (Monge & Elkan, 1996) compares multi-word
#' strings word by word: each word of `a[i]` is matched to its most
#' similar word of `b[i]`, and the scores are averaged. Word order does not
#' matter and a misspelt word still counts for most of its value, so it
#' suits names and addresses (`"Smith John"` against `"Jon Smyth"`). Words
#' are separated by whitespace and compared with `method`.
#'
#' Monge-Elkan is asymmetric: `b` may have extra words that `a` does not
#' need. With `symmetric = TRUE` the two directions are averaged. Two
#' strings without words score `1`; one string without words scores `0`.
#'
#' @inheritParams lcs_seq
#' @param method Word similarity: `"jaro_winkler"` (the default),
#'   `"levenshtein"`, `"osa"` or `"damerau_levenshtein"` (normalised edit
#'   similarities, as [levenshtein_similarity()]).
#' @param p Jaro-Winkler prefix scale in `[0, 0.25]`; `0` gives Jaro.
#' @param symmetric Logical. Average the similarities in both directions.
#' @return Numeric vector of similarities in `[0, 1]`, the same length as
#'   `a`; `NA` where either input is `NA`.
#' @references Monge, A. E. and Elkan, C. P. (1996). The field matching
#'   problem: algorithms and applications. KDD '96, 267-270.
#' @seealso [jaro_winkler_tokens()] and [fuzz_token_set_ratio()] for other
#'   word-order-tolerant scores.
#' @family edit distance functions
#' @examples
#' monge_elkan("Smith John", "Jon Smyth")
#' monge_elkan("Smith", "John Smith")                    # extra words in b are free
#' monge_elkan("Smith", "John Smith", symmetric = TRUE)
#' monge_elkan("12 Smith St", "12 Smyth Street", method = "levenshtein")
#' @export
monge_elkan <- function(a, b, method = c("jaro_winkler", "levenshtein", "osa",
                                         "damerau_levenshtein"),
                        p = 0.1, symmetric = FALSE, nthreads = NULL,
                        use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    method <- match.arg(method)
    .validate_prefix_scale(p)
    if (!is.logical(symmetric) || length(symmetric) != 1L || is.na(symmetric))
        stop("`symmetric` must be TRUE or FALSE.")
    id <- match(method, c("jaro_winkler", "levenshtein", "osa",
                          "damerau_levenshtein")) - 1L
    fast_monge_elkan_impl(a, b, id, as.double(p), symmetric, use_bytes,
                          .as_nthreads(nthreads))
}
