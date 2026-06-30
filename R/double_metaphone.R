#' Double Metaphone phonetic code (Lawrence Philips, 2000)
#'
#' An improvement on classic Metaphone that copes with names of non-English
#' origin by returning two codes per word — `primary` and `secondary` — when
#' the pronunciation is ambiguous (e.g. a `C` that could plausibly be a hard
#' `K` or a soft `S` sound). Two names are usually considered a phonetic
#' match if *any* of `primary`/`secondary` from one matches *any* of
#' `primary`/`secondary` from the other. More discriminating than
#' [soundex()] or [nysiis()] for names outside their English/American
#' design target, at the cost of a considerably more involved ruleset.
#'
#' Ported from the structure of the widely-used Apache Commons Codec Java
#' implementation and cross-checked against its published test vectors (see
#' `tests/testthat/test-double_metaphone.R`). Unlike [soundex()]/[nysiis()],
#' which strip the input down to letters only before encoding, this keeps
#' internal whitespace and punctuation (only trimming the ends and
#' uppercasing) — the algorithm itself keys off literal fragments like
#' `"VAN "`/`"SAN "`/`"SCH"`, including the space.
#'
#' @param x Character vector (coerced via [as.character()] if not already
#'   character). `NA` elements, and elements with no content after
#'   trimming, return `NA` in both columns.
#'
#' @return A `data.frame` with two character columns, `primary` and
#'   `secondary`, each `length(x)` long (`secondary` is empty `""`, not
#'   `NA`, when the algorithm found no plausible alternate pronunciation —
#'   only `NA` input produces `NA` output).
#' @seealso [soundex()], [nysiis()], [caverphone()]
#' @examples
#' double_metaphone(c("Smith", "Schmidt", "Catherine", "Kathryn"))
#' @export
double_metaphone <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    res <- fast_double_metaphone_impl(x)
    data.frame(primary = res$primary, secondary = res$secondary,
               stringsAsFactors = FALSE)
}
