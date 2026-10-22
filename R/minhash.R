#' Candidate pairs by MinHash locality-sensitive hashing
#'
#' Finds pairs of strings whose character q-gram sets probably have a
#' Jaccard similarity of at least `threshold`, without comparing every pair
#' -- a blocking step for record linkage or deduplication of large
#' vectors, as in the Python `datasketch` package's `MinHashLSH` and the R
#' `zoomerjoin` package. Each string gets a MinHash signature of `n_hash`
#' values (Broder, 1997), split into `bands` bands of `rows` values; strings
#' that agree on every value of any band become a candidate pair. The
#' probability of that is `1 - (1 - J^rows)^bands` for a pair with Jaccard
#' similarity `J`, an S-curve that rises steeply around `threshold`.
#'
#' By default `bands` and `rows` are chosen as `datasketch` does, to
#' minimise `weights[1]` times the expected share of false positives below
#' `threshold` plus `weights[2]` times the false negatives above it. With
#' `verify = TRUE` (the default) every candidate's exact Jaccard similarity
#' is computed and pairs below `threshold` are dropped, so every pair
#' returned is a true match; some true matches can still be missed, more
#' often the closer their similarity is to `threshold`. With the equal
#' default weights that can be a quarter of the pairs just above the
#' threshold. Since verification removes the false positives anyway, a
#' weight such as `c(0.1, 0.9)` trades a longer run for fewer missed
#' matches. More hash functions (larger `n_hash`) make the curve steeper.
#' Results depend only on `seed`, not on the number of threads.
#'
#' Strings are sets of overlapping `q`-character substrings; a non-empty
#' string shorter than `q` is a set of one. Empty strings and `NA` are never
#' paired. The similarity is the same as [jaccard_index()] with
#' `use_bytes = FALSE` (or `TRUE`).
#'
#' @param x Character vector.
#' @param y Optional character vector. When given, pairs have one string from
#'   `x` and one from `y`; otherwise pairs are within `x`.
#' @param threshold Jaccard similarity in `(0, 1)` that pairs should reach.
#' @param q Positive integer q-gram length (default `3`).
#' @param n_hash Positive integer: the number of hash functions (default
#'   `128`).
#' @param bands Optional positive integer. The number of bands; `rows` is
#'   then `n_hash %/% bands`. `NULL` chooses both from `threshold`.
#' @param weights Two non-negative numbers: the weights of false positives
#'   and false negatives when `bands` is chosen (as `datasketch`'s
#'   `MinHashLSH`).
#' @param seed Number used to derive the hash functions.
#' @param verify Logical. Keep only pairs whose exact Jaccard similarity
#'   reaches `threshold`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param use_bytes Logical. Build q-grams from UTF-8 characters (`FALSE`,
#'   the default) or bytes.
#' @return A data frame with `x_index`, `y_index` (an index into `y`, or into
#'   `x` with `x_index < y_index` when `y` is `NULL`) and `jaccard`, the exact
#'   similarity, ordered by `x_index` then `y_index`. Attributes `bands` and
#'   `rows` record the banding used.
#' @references Broder, A. Z. (1997). On the resemblance and containment of
#'   documents. Compression and Complexity of Sequences, 21-29.
#'
#'   Leskovec, J., Rajaraman, A. and Ullman, J. D. (2014). Mining of
#'   Massive Datasets, chapter 3. Cambridge University Press.
#' @seealso [jaccard_index()], [fuzzy_within()] and [tfidf_top_n()] for
#'   other ways to find candidate matches.
#' @family fuzzy lookup functions
#' @examples
#' x <- c("12 SMITH STREET BRISBANE", "12 SMITH ST BRISBANE",
#'        "45 JONES ROAD IPSWICH", "45 JONES RD IPSWICH", "7 KING LANE")
#' minhash_pairs(x, threshold = 0.4)
#' minhash_pairs(x[c(1, 3)], x[c(2, 4, 5)], threshold = 0.4)
#' @export
minhash_pairs <- function(x, y = NULL, threshold = 0.5, q = 3L,
                          n_hash = 128L, bands = NULL, weights = c(0.5, 0.5),
                          seed = 1, verify = TRUE, nthreads = NULL,
                          use_bytes = FALSE) {
    if (!is.character(x)) stop("`x` must be a character vector.")
    if (!is.null(y) && !is.character(y))
        stop("`y` must be NULL or a character vector.")
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold) || threshold <= 0 || threshold >= 1)
        stop("`threshold` must be a number between 0 and 1.")
    .qgram_validate_q(q)
    if (!is.numeric(n_hash) || length(n_hash) != 1L || is.na(n_hash) ||
        n_hash < 1 || n_hash > 4096 || n_hash != floor(n_hash))
        stop("`n_hash` must be an integer from 1 to 4096.")
    if (!is.null(bands) &&
        (!is.numeric(bands) || length(bands) != 1L || is.na(bands) ||
         bands < 1 || bands > n_hash || bands != floor(bands)))
        stop("`bands` must be NULL or an integer from 1 to `n_hash`.")
    if (!is.numeric(weights) || length(weights) != 2L || anyNA(weights) ||
        any(weights < 0) || sum(weights) <= 0)
        stop("`weights` must be two non-negative numbers, not both zero.")
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed))
        stop("`seed` must be a single number.")
    if (!is.logical(verify) || length(verify) != 1L || is.na(verify))
        stop("`verify` must be TRUE or FALSE.")
    .validate_use_bytes(use_bytes)
    banding <- if (is.null(bands)) .minhash_banding(threshold, n_hash, weights)
               else c(bands = bands, rows = n_hash %/% bands)
    res <- fast_minhash_pairs_impl(
        x, if (is.null(y)) character() else y, !is.null(y), as.integer(q),
        as.integer(banding[["bands"]]), as.integer(banding[["rows"]]),
        abs(as.double(seed)), as.double(threshold), verify, use_bytes,
        .as_nthreads(nthreads)
    )
    out <- data.frame(x_index = res$x_index, y_index = res$y_index,
                      jaccard = res$jaccard)
    attr(out, "bands") <- as.integer(banding[["bands"]])
    attr(out, "rows") <- as.integer(banding[["rows"]])
    out
}

# datasketch's _optimal_param: the bands and rows (bands * rows <= n_hash)
# minimising the weighted false-positive area below the threshold plus the
# false-negative area above it.
.minhash_banding <- function(threshold, n_hash, weights = c(0.5, 0.5)) {
    best <- c(bands = 1, rows = 1)
    best_error <- Inf
    for (b in seq_len(n_hash)) {
        for (r in seq_len(n_hash %/% b)) {
            fp <- stats::integrate(function(s) 1 - (1 - s^r)^b,
                                   0, threshold)$value
            fn <- stats::integrate(function(s) (1 - s^r)^b,
                                   threshold, 1)$value
            error <- weights[1] * fp + weights[2] * fn
            if (error < best_error) {
                best_error <- error
                best <- c(bands = b, rows = r)
            }
        }
    }
    best
}
