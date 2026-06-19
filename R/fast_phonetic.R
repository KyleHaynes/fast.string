#' Soundex phonetic code (American Soundex / US Census Bureau rules)
#'
#' Encodes each string as a 4-character phonetic code: the first letter
#' kept verbatim, followed by up to 3 digits for the remaining consonants.
#' Letters mapping to the same digit collapse into one when adjacent (`H`
#' and `W` are transparent and don't break adjacency, e.g. `"Ashcraft"`
#' encodes as `"A261"`, not `"A2261"`). Has no [base] equivalent; intended
#' to complement [jaro_winkler()] / [jaro_winkler_matrix()] as a blocking
#' key for fuzzy name matching.
#'
#' @param x Character vector (coerced via [as.character()] if not already
#'   character, e.g. a factor of names read from a CSV). `NA` elements, and
#'   elements with no alphabetic characters, return `NA`.
#'
#' @return Character vector the same length as `x`, with `names(x)`
#'   preserved, each element either `NA` or exactly 4 characters.
#' @export
soundex <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_soundex_impl(x), x)
}

#' NYSIIS phonetic code (New York State Identification and Intelligence System)
#'
#' Encodes each string as a NYSIIS phonetic key (commonly-cited core
#' ruleset: leading/trailing letter transforms, a per-letter transform
#' table with adjacent-duplicate collapsing, and trailing key cleanup),
#' truncated to 6 characters. Has no [base] equivalent; intended to
#' complement [jaro_winkler()] / [jaro_winkler_matrix()] as a blocking key
#' for fuzzy name matching, typically alongside or instead of [soundex()].
#'
#' @param x Character vector (coerced via [as.character()] if not already
#'   character). `NA` elements, and elements with no alphabetic characters,
#'   return `NA`.
#'
#' @return Character vector the same length as `x`, with `names(x)`
#'   preserved, each element either `NA` or up to 6 characters.
#' @export
nysiis <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_nysiis_impl(x), x)
}
