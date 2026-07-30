#' Token-aware Jaro-Winkler similarity for multi-token strings
#'
#' [jaro_winkler()] compares two strings as single, ordered sequences of
#' characters, so `"Kyle John Haynes"` vs `"John Kylie Haynes"` scores badly
#' even though two of the three tokens are identical and the third is a
#' near-miss -- the words just moved. `jaro_winkler_tokens()` fixes that by
#' scoring each pair two ways and keeping the higher result:
#'
#' 1. **Token alignment** -- split both strings on whitespace, score every
#'    token in `a` against every token in `b`, then greedily pair off tokens
#'    in descending order of similarity (a cheap stand-in for an optimal
#'    assignment; accurate enough for the handful of tokens found in
#'    names/addresses). By default the summed score is divided by
#'    `max(n_tokens_a, n_tokens_b)`, so missing or extra tokens are
#'    penalised. This is what rescues `"Kyle John Haynes"` /
#'    `"John Kylie Haynes"`.
#' 2. **Collapsed** -- both sides with internal whitespace removed and
#'    compared as one string. This rescues cases where punctuation/spacing
#'    alone differs, e.g. `"OBrien"` vs `"O Brien"`, which token alignment
#'    alone would under-score (one token vs two).
#'
#' Before either framing, `strip` characters (apostrophes and hyphens by
#' default) are removed so that `"O'Brien"`, `"O-Brien"`, and `"OBrien"`
#' tokenise and collapse identically.
#'
#' Set `contractions = TRUE` to also rescue tokens that are *split on one
#' side but joined on the other* -- `"KYLEJOHN HAYNES"` vs.
#' `"KYLE JOHN HAYNES"` scores perfectly, because `"KYLEJOHN"` is recognised
#' as `"KYLE"` + `"JOHN"` concatenated. This isn't limited to adjacent
#' tokens: `"KYLEHAYNES JOHN"` vs. `"KYLE JOHN HAYNES"` also scores
#' perfectly, because the two tokens that contracted (`"KYLE"`, `"HAYNES"`)
#' don't need to be next to each other in the original -- only in their
#' original left-to-right order (so `"HAYNESKYLE"` would *not* match). Every
#' pair of tokens on each side (not just adjacent ones) is tried as a
#' candidate contraction.
#'
#' `contractions = TRUE` also catches the case where *both* sides already
#' have two words fused into one token, just in opposite order --
#' `"HAYNES JOHNKYLE"` vs. `"KYLEJOHN HAYNES"` scores highly, because
#' `"JOHNKYLE"` and `"KYLEJOHN"` are equal-length and one is an exact cyclic
#' rotation of the other (plain Jaro-Winkler can't see this: swapping two
#' whole blocks moves every character further than its matching window
#' tolerates). Every rotation of an equal-length candidate pair is tried and
#' the best kept, so this needs no prior knowledge of where the fused word
#' boundary was.
#'
#' Both contraction mechanisms try `O(n^2)` extra candidate comparisons per
#' side, so `contractions = TRUE` is noticeably slower than the default --
#' off by default for that reason, and most useful when names/addresses are
#' known to have inconsistent word-splitting.
#'
#' By default, unmatched tokens are penalised by dividing the matched total
#' by `max(n_tokens_a, n_tokens_b)` rather than the number actually matched.
#' Set `extra_penalty` to a number to switch to a different scheme: unmatched
#' tokens (e.g. a stray middle name/initial on one side) are dropped from the
#' average entirely, and each one then subtracts `extra_penalty` from the
#' result instead -- `extra_penalty = 0` ignores extra tokens outright,
#' `extra_penalty = 0.1` lets a couple of strays through with a small dent in
#' score. This is what lets `"Kylie John ZZ Haynes"` still score highly
#' against `"Haynes John Kyle"` (3 of 4 tokens align well; `"ZZ"` is dropped
#' rather than dragging the average down).
#'
#' Case and `strip` normalisation run in R, while whitespace handling,
#' tokenising, the token x token similarity matrix, the greedy assignment,
#' and the collapsed comparison all run in a single parallelised C++ pass
#' (via Intel TBB through RcppParallel) over the whole vector, the same as
#' [jaro_winkler()] itself.
#'
#' @param a,b Equal-length character vectors.
#' @param p Prefix scaling factor (default 0.1, the standard value).
#' @param ignore_case Logical. Uppercase both sides before comparing
#'   (default `FALSE` -- name/address tokens are rarely meaningfully
#'   case-sensitive).
#' @param strip Single regex string matching punctuation to remove before
#'   tokenising, or `NULL` to skip this step. Default `"['\u2019-]"` strips
#'   straight/curly apostrophes and hyphens.
#' @param extra_penalty `NULL` (default) or a non-negative numeric scalar.
#'   `NULL` penalises unmatched tokens by diluting the average (divide by
#'   `max(n_tokens_a, n_tokens_b)`). A numeric value instead averages over
#'   only the matched tokens (`min(n_tokens_a, n_tokens_b)`) and subtracts
#'   `extra_penalty` per unmatched token from that average, floored at 0 --
#'   so unmatched tokens can be ignored (`0`) or lightly discounted (e.g.
#'   `0.05`-`0.2`) instead of fully diluting the score.
#' @param contractions Logical (default `FALSE`). If `TRUE`, also try every
#'   pair of tokens on each side concatenated together (in their original
#'   left-to-right order, not just adjacent pairs) as a candidate match for
#'   a single token on the other side -- see Details. Slower than the
#'   default since it considers `O(n^2)` extra candidates per side.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
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
#'
#' # Contractions: "KYLEJOHN" recognised as "KYLE" + "JOHN" (adjacent), and
#' # "KYLEHAYNES" recognised as "KYLE" + "HAYNES" (non-adjacent, "JOHN" sits
#' # between them in the original) -- both score perfectly with contractions on.
#' jaro_winkler_tokens("KYLEJOHN HAYNES", "KYLE JOHN HAYNES", contractions = TRUE)
#' jaro_winkler_tokens("KYLEHAYNES JOHN", "KYLE JOHN HAYNES", contractions = TRUE)
#'
#' # Both sides already fused two words into one token, just in opposite
#' # order ("JOHNKYLE" vs "KYLEJOHN") -- rescued by the rotation check.
#' jaro_winkler_tokens("HAYNES JOHNKYLE", "KYLEJOHN HAYNES", contractions = TRUE)
#' @export
jaro_winkler_tokens <- function(a, b, p = 0.1, ignore_case = FALSE,
                                 strip = "['\u2019-]", extra_penalty = NULL,
                                 contractions = FALSE, nthreads = NULL) {
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

    fast_jaro_winkler_tokens_impl(
        a, b, as.double(p),
        if (is.null(extra_penalty)) NA_real_ else as.double(extra_penalty),
        isTRUE(contractions), .as_nthreads(nthreads)
    )
}
