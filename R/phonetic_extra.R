.phonetic_x <- function(x) if (is.character(x)) x else as.character(x)

.phonetic_length <- function(max_length) {
    if (!is.numeric(max_length) || length(max_length) != 1L ||
        is.na(max_length) || max_length < 1 ||
        max_length != floor(max_length) || max_length > 1000)
        stop("`max_length` must be a single integer between 1 and 1000.")
    as.integer(max_length)
}

#' Metaphone phonetic code (Lawrence Philips, 1990)
#'
#' Encodes each string with the original Metaphone algorithm, the
#' predecessor of [double_metaphone()]: consonant sounds are coded with
#' letters (`"PH"` and `"V"` as `"F"`, a soft `"C"` as `"S"`, `"TH"` as
#' `"0"`), vowels are dropped except at the start, and the code is cut to
#' `max_length` characters.
#'
#' Metaphone has several published variants. This one follows Apache Commons
#' Codec's `Metaphone` (as do this package's [double_metaphone()],
#' [nysiis()] and [caverphone()]), which Solr and Elasticsearch also use.
#' `phonics::metaphone()` follows PHP's variant instead, so the two disagree
#' on some names. As in Commons Codec, characters other than letters are
#' kept while encoding (a space ends a word for rules such as "silent `W`
#' before a consonant"), and adjacent duplicate letters other than `"C"`
#' are skipped.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). Accented Latin letters are folded to ASCII
#'   first. `NA` elements, and elements with no letters, return `NA`.
#' @param max_length Positive integer: the longest code to return (default
#'   `4`, as in Commons Codec).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @family phonetic codes
#' @examples
#' metaphone(c("Thompson", "Tomson", "Knight", "Night", "Wright", "Rite"))
#' metaphone("Christopher", max_length = 8)
#' @export
metaphone <- function(x, max_length = 4L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_metaphone_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}

#' Match Rating Approach codes and comparison
#'
#' The Match Rating Approach (MRA) was developed by Western Airlines in 1977
#' for matching passenger names. `mra_encode()` builds the name's codex:
#' the first letter, then the consonants (dropping a consonant that repeats
#' the letter before it), shortened to its first and last three letters
#' when longer than six. `mra_compare()` decides whether two names match:
#' codexes whose lengths differ by three or more never match; otherwise the
#' letters that agree position by position are removed (first from the
#' left, then from the right), and the names match when six minus the
#' number of letters left over in the longer codex reaches a threshold that
#' depends on the codexes' combined length.
#'
#' Both follow the published rules as the Python `jellyfish` package
#' implements them (`match_rating_codex()` and
#' `match_rating_comparison()`). `jellyfish` returns `None` when the lengths
#' differ by three or more; `mra_compare()` returns `FALSE`.
#' `phonics::mra_compare()` compares one position fewer and counts the
#' shorter codex, so it reports more matches.
#'
#' @param x,a,b Character vectors (other vectors, such as factors, are
#'   converted with [as.character()]); `a` and `b` must have the same
#'   length. These are names, not codexes; passing codexes also works, as
#'   encoding a codex leaves it unchanged. Accented Latin letters are folded
#'   to ASCII and other characters are ignored.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return `mra_encode()`: a character vector the same length as `x`, with
#'   `names(x)` kept, and `NA` for `NA` or names without letters.
#'   `mra_compare()`: a logical vector the same length as `a`, `NA` where
#'   either name is `NA` or has no letters.
#' @family phonetic codes
#' @examples
#' mra_encode(c("Byrne", "Boern", "Catherine", "Kathryn", "Kathrynoglin"))
#' mra_compare(c("Byrne", "Smith", "Michael"), c("Boern", "Smyth", "Mike"))
#' @export
mra_encode <- function(x, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_mra_encode_impl(x, .as_nthreads(nthreads)), x)
}

#' @rdname mra_encode
#' @export
mra_compare <- function(a, b, nthreads = NULL) {
    a <- .phonetic_x(a)
    b <- .phonetic_x(b)
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    fast_mra_compare_impl(a, b, .as_nthreads(nthreads))
}

#' Statistics Canada, Lein, Phonex, Roger Root and ONCA name codes
#'
#' Five more phonetic name codes from the record-linkage literature, for
#' blocking and as comparison fields alongside [soundex()] and friends.
#' Each one strips the name to its letters (after folding accented Latin
#' letters to ASCII) before encoding.
#'
#' * `statcan()`: Statistics Canada's surname coding (Lynch & Arends, 1977):
#'   the first letter, then the remaining letters other than vowels and
#'   `"Y"`, with runs of one letter collapsed, cut to `max_length`.
#' * `lein()`: the Lein name code (also from Lynch & Arends, 1977): the first
#'   letter, then the remaining consonants other than `"H"`, `"W"` and `"Y"`
#'   (runs collapsed) coded as digits in five groups, padded with zeros to
#'   `max_length`.
#' * `phonex()`: Phonex (Lait & Randell, 1996), a combination of Soundex and
#'   Phonix: initial-letter rules (`"KN"` to `"N"`, `"PH"` to `"F"`, a
#'   leading vowel to `"A"`, ...) and trailing `"S"` removal, then Soundex-
#'   style digits.
#' * `roger_root()`: the Roger Root name code (Root, 1975): a numeric code
#'   whose first two digits encode the start of the name, padded with zeros
#'   to `max_length` (default `5`).
#' * `onca()`: the Oxford Name Compression Algorithm (Gill, 1997), used by
#'   the Oxford record linkage system: [nysiis()] (without its six-letter
#'   limit) followed by [soundex()] of the result.
#'
#' `statcan()`, `lein()`, `phonex()` and `roger_root()` reproduce the
#' `phonics` package's implementations (James P. Howard II, BSD-2-Clause)
#' rule for rule, quirks included: a name whose Phonex or Roger Root code
#' would be all zeros gets `""`. Unlike `phonics`, they ignore characters
#' other than letters instead of returning `NA` for them, fold accented
#' letters (`phonics` folds only some upper-case ones), and pad `lein()` to
#' the full `max_length`. `onca()` uses this package's [nysiis()], which
#' follows Apache Commons Codec, so it differs from `phonics::onca()`
#' wherever the two NYSIIS implementations do.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). `NA` elements, and elements with no letters,
#'   return `NA`.
#' @param max_length Positive integer code length (default `4`; `5` for
#'   `roger_root()`).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @references Lynch, B. T. and Arends, W. L. (1977). Selection of a surname
#'   coding procedure for the SRS record linkage system. US Department of
#'   Agriculture.
#'
#'   Lait, A. J. and Randell, B. (1996). An assessment of name matching
#'   algorithms. University of Newcastle upon Tyne.
#'
#'   Root, R. (1975). Name coding (unpublished).
#'
#'   Gill, L. (1997). OX-LINK: the Oxford medical record linkage system.
#'
#'   Howard, J. P. II (2020). Phonetic spelling algorithm implementations for
#'   R. Journal of Statistical Software 95(8).
#' @family phonetic codes
#' @examples
#' x <- c("William", "Peter", "Peady", "Stevenson", "Stephenson")
#' statcan(x)
#' lein(x)
#' phonex(x)
#' roger_root(x)
#' onca(x)
#' @name name_codes
NULL

#' @rdname name_codes
#' @export
statcan <- function(x, max_length = 4L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_statcan_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}

#' @rdname name_codes
#' @export
lein <- function(x, max_length = 4L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_lein_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}

#' @rdname name_codes
#' @export
phonex <- function(x, max_length = 4L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_phonex_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}

#' @rdname name_codes
#' @export
roger_root <- function(x, max_length = 5L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_roger_root_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}

#' @rdname name_codes
#' @export
onca <- function(x, max_length = 4L, nthreads = NULL) {
    x <- .phonetic_x(x)
    .copy_names(fast_onca_impl(
        x, .phonetic_length(max_length), .as_nthreads(nthreads)
    ), x)
}
