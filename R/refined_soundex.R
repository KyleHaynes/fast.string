#' Refined Soundex phonetic code
#'
#' Encodes names using the US-English Refined Soundex mapping from Apache
#' Commons Codec. Refined Soundex retains a code for every change in phonetic
#' class rather than truncating to the three digits used by classic
#' [soundex()], making it more discriminating for spelling comparison.
#'
#' @param x Character vector (coerced with [as.character()] when needed).
#'   `NA` values and elements with no letters return `NA`. Accented Latin
#'   letters are folded to ASCII first, so `"Émile"` codes as `"Emile"`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with names preserved.
#' @family phonetic codes
#' @examples
#' refined_soundex(c("Robert", "Rupert", "Ashcraft"))
#' @export
refined_soundex <- function(x, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_refined_soundex_impl(x, .as_nthreads(nthreads)), x)
}
