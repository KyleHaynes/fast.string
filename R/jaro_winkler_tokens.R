#' Token-aware Jaro-Winkler similarity for multi-token strings
#'
#' [jaro_winkler()] compares two strings as single, ordered sequences of
#' characters, so `"Kyle John Haynes"` vs `"John Kylie Haynes"` scores badly
#' even though two of the three tokens are identical and the third is a
#' near-miss — the words just moved. `jaro_winkler_tokens()` fixes that by
#' scoring each pair two ways and keeping the higher result:
#'
#' 1. **Token alignment** — split both strings on whitespace, score every
#'    token in `a` against every token in `b`, then greedily pair off tokens
#'    in descending order of similarity (a cheap stand-in for an optimal
#'    assignment; accurate enough for the handful of tokens found in
#'    names/addresses). By default the summed score is divided by
#'    `max(n_tokens_a, n_tokens_b)`, so missing or extra tokens are
#'    penalised. This is what rescues `"Kyle John Haynes"` /
#'    `"John Kylie Haynes"`.
#' 2. **Collapsed** — both sides with internal whitespace removed and
#'    compared as one string. This rescues cases where punctuation/spacing
#'    alone differs, e.g. `"OBrien"` vs `"O Brien"`, which token alignment
#'    alone would under-score (one token vs two).
#'
#' Before either framing, `strip` characters (apostrophes and hyphens by
#' default) are removed so that `"O'Brien"`, `"O-Brien"`, and `"OBrien"`
#' tokenise and collapse identically.
#'
#' By default, unmatched tokens are penalised by dividing the matched total
#' by `max(n_tokens_a, n_tokens_b)` rather than the number actually matched.
#' Set `extra_penalty` to a number to switch to a different scheme: unmatched
#' tokens (e.g. a stray middle name/initial on one side) are dropped from the
#' average entirely, and each one then subtracts `extra_penalty` from the
#' result instead — `extra_penalty = 0` ignores extra tokens outright,
#' `extra_penalty = 0.1` lets a couple of strays through with a small dent in
#' score. This is what lets `"Kylie John ZZ Haynes"` still score highly
#' against `"Haynes John Kyle"` (3 of 4 tokens align well; `"ZZ"` is dropped
#' rather than dragging the average down).
#'
#' The normalisation (`toupper`/`strip`/whitespace collapse) runs in R using
#' the package's own [fgsub()]/[ftrimws()], but tokenising, the token x token
#' similarity matrix, the greedy assignment, and the collapsed comparison all
#' run in a single parallelised C++ pass (via Intel TBB through RcppParallel)
#' over the whole vector, the same as [jaro_winkler()] itself.
#'
#' @param a,b Equal-length character vectors.
#' @param p Prefix scaling factor (default 0.1, the standard value).
#' @param ignore_case Logical. Uppercase both sides before comparing
#'   (default `FALSE` — name/address tokens are rarely meaningfully
#'   case-sensitive).
#' @param strip Single regex string matching punctuation to remove before
#'   tokenising, or `NULL` to skip this step. Default `"['’-]"` strips
#'   straight/curly apostrophes and hyphens.
#' @param extra_penalty `NULL` (default) or a non-negative numeric scalar.
#'   `NULL` penalises unmatched tokens by diluting the average (divide by
#'   `max(n_tokens_a, n_tokens_b)`). A numeric value instead averages over
#'   only the matched tokens (`min(n_tokens_a, n_tokens_b)`) and subtracts
#'   `extra_penalty` per unmatched token from that average, floored at 0 —
#'   so unmatched tokens can be ignored (`0`) or lightly discounted (e.g.
#'   `0.05`-`0.2`) instead of fully diluting the score.
#' @param nthreads Integer or `NULL`. Thread count.
#'
#' @return Numeric vector of similarities in `[0, 1]`, `length(a)` long.
#'   `NA` if either `a[i]` or `b[i]` is `NA`.
#' @seealso [jaro_winkler()], [jaro_winkler_matrix()]
#' @examples
#' jaro_winkler_tokens("Kyle John Haynes", "John Kylie Haynes")
#' jaro_winkler_tokens(c("OBrien", "O'Brien"), c("O Brien", "O Brien"))
#'
#' # Penalised, not ignored: an extra token still costs something.
#' jaro_winkler_tokens("John Smith", "John Smith Jones")
#'
#' # Extra/junk token ("ZZ") dropped instead of diluting the score.
#' jaro_winkler_tokens("Kylie John ZZ Haynes", "Haynes John Kyle",
#'                     extra_penalty = 0)
#' @export
jaro_winkler_tokens <- function(a, b, p = 0.1, ignore_case = FALSE,
                                 strip = "['’-]", extra_penalty = NULL,
                                 nthreads = NULL) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
    if (!is.null(extra_penalty) &&
        (!is.numeric(extra_penalty) || length(extra_penalty) != 1L || extra_penalty < 0))
        stop("`extra_penalty` must be NULL or a single non-negative number.")

    if (isTRUE(ignore_case)) {
        a <- toupper(a)
        b <- toupper(b)
    }
    if (!is.null(strip)) {
        a <- fgsub(strip, "", a, nthreads = nthreads)
        b <- fgsub(strip, "", b, nthreads = nthreads)
    }
    a <- ftrimws(gsub("[ \t\r\n]+", " ", a))
    b <- ftrimws(gsub("[ \t\r\n]+", " ", b))

    if (!is.null(nthreads))
        RcppParallel::setThreadOptions(numThreads = as.integer(nthreads))
    fast_jaro_winkler_tokens_impl(
        a, b, as.double(p),
        if (is.null(extra_penalty)) NA_real_ else as.double(extra_penalty)
    )
}
