#' Refined Soundex phonetic code
#'
#' Encodes names using the US-English Refined Soundex mapping from Apache
#' Commons Codec. Refined Soundex retains a code for every change in phonetic
#' class rather than truncating to the three digits used by classic
#' [soundex()], making it more discriminating for spelling comparison.
#'
#' @param x Character vector (coerced with [as.character()] when needed).
#'   `NA` values and elements with no ASCII letters return `NA`.
#' @return Character vector the same length as `x`, with names preserved.
#' @seealso [soundex()], [nysiis()], [cologne()]
#' @examples
#' refined_soundex(c("Robert", "Rupert", "Ashcraft"))
#' @export
refined_soundex <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_refined_soundex_impl(x), x)
}
