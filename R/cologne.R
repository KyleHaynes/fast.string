#' Cologne phonetic code
#'
#' Encodes names with the Cologne phonetic algorithm, which is designed for
#' German pronunciation. Accented Latin letters are folded to ASCII first
#' (umlauts to their base vowels, sharp s to `SS`), input is uppercased,
#' and other non-letters are ignored before encoding.
#'
#' @param x Character vector (coerced with [as.character()] when needed).
#'   `NA` values and elements with no supported letters return `NA`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with names preserved.
#' @family phonetic codes
#' @examples
#' cologne(c("Müller-Lüdenscheidt", "Meier", "Meyer"))
#' @export
cologne <- function(x, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_cologne_impl(x, .as_nthreads(nthreads)), x)
}
