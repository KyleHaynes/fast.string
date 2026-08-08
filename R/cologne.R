#' Cologne phonetic code
#'
#' Encodes names with the Cologne phonetic algorithm, which is designed for
#' German pronunciation. Input is uppercased bytewise, German umlauts are
#' mapped to their base vowels, sharp s is mapped to `SS`, and other
#' non-letters are ignored before encoding.
#'
#' @param x Character vector (coerced with [as.character()] when needed).
#'   `NA` values and elements with no supported letters return `NA`.
#' @return Character vector the same length as `x`, with names preserved.
#' @seealso [soundex()], [refined_soundex()], [double_metaphone()]
#' @examples
#' cologne(c("Müller-Lüdenscheidt", "Meier", "Meyer"))
#' @export
cologne <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_cologne_impl(x), x)
}
