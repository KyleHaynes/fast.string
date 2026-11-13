#' Candidate pairs by blocking on exact keys
#'
#' Returns every pair of records that agree exactly on at least one
#' blocking key -- the standard first step of record linkage and
#' deduplication, which keeps the number of detailed comparisons manageable.
#' Several keys (one per column of a data frame) give the union of several
#' blocking passes, such as "same Soundex of surname *or* same date of
#' birth", with each pair reported once. Pairs are generated in parallel
#' across CPU cores via Intel TBB.
#'
#' Keys can be character, numeric, logical, factor or `Date` vectors, or
#' combinations made with [paste()]. A missing key (`NA`) never matches;
#' convert placeholders such as `""` or `"UNKNOWN"` to `NA` first, or they
#' form one large block. `max_block_size` skips blocks too large to be
#' useful (for example the most common surname), with a warning saying how
#' many were skipped.
#'
#' @param x A vector of blocking keys, or a data frame or list of them (one
#'   per pass, all the same length).
#' @param y Optional keys for a second set of records, with the same number
#'   of passes as `x`. When given, pairs have one record from `x` and one from
#'   `y` (linkage); otherwise pairs are within `x` (deduplication).
#' @param max_block_size Number: blocks with more records than this (from `x`
#'   and `y` together) are skipped. `Inf` (the default) keeps every block.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return A data frame with one row per pair: `x_index`, `y_index` (an index
#'   into `y`, or into `x` with `x_index < y_index` when `y` is `NULL`) and
#'   `pass`, the first key that paired them, ordered by `x_index` then
#'   `y_index`.
#' @seealso [sorted_neighbourhood()] to also pair records with similar
#'   keys, [cluster_pairs()] to turn matched pairs into entity clusters, and
#'   [minhash_pairs()], [fuzzy_within()] and [tfidf_top_n()] for
#'   similarity-based candidates.
#' @family record linkage pair functions
#' @examples
#' people <- data.frame(
#'     surname = c("SMITH", "SMYTH", "JONES", "SMITH", "JONES"),
#'     dob = as.Date(c("1980-01-02", "1980-01-02", "1975-06-30",
#'                     "1990-12-01", "1975-06-30"))
#' )
#' # Pairs sharing a Soundex code or a date of birth
#' block_pairs(data.frame(soundex(people$surname), people$dob))
#'
#' # Linking two files on postcode
#' block_pairs(c("4000", "4101", NA), c("4101", "4000", "4000"))
#' @export
block_pairs <- function(x, y = NULL, max_block_size = Inf, nthreads = NULL) {
    .check_max_block_size(max_block_size)
    .candidate_pairs(x, y, 0L, max_block_size, nthreads)
}

.check_max_block_size <- function(max_block_size) {
    if (!is.numeric(max_block_size) || length(max_block_size) != 1L ||
        is.na(max_block_size) || max_block_size < 1)
        stop("`max_block_size` must be a single number of at least 1.",
             call. = FALSE)
    invisible(max_block_size)
}

# The half-window of a sorted neighbourhood: how many distinct key values
# either side of a record's own are paired with it.
.half_window <- function(window) {
    if (!is.numeric(window) || length(window) != 1L || is.na(window) ||
        window < 1 || window != floor(window) || window %% 2 != 1 ||
        window > .Machine$integer.max)
        stop("`window` must be a single odd positive integer.", call. = FALSE)
    as.integer((window - 1) %/% 2)
}

#' Candidate pairs by the sorted neighbourhood method
#'
#' Sorts the distinct values of a key and pairs records whose values are
#' within `(window - 1) / 2` places of each other in that order, as well as
#' records with equal values. Unlike exact blocking ([block_pairs()]) it
#' still pairs records whose keys differ slightly at the end -- `"SMITH"`
#' with `"SMITHE"`, or adjacent dates -- which is why it is a classic
#' blocking method for record linkage (Hernandez and Stolfo, 1995). Several
#' keys (one per column of a data frame) give the union of several passes,
#' with each pair reported once. Pairs are generated in parallel across CPU
#' cores via Intel TBB.
#'
#' The window moves over distinct key values, not records, as in Python's
#' `recordlinkage` package (`SortedNeighbourhood`), so ties are always
#' paired and the result does not depend on the order of the records.
#' Character keys are sorted by bytes (the C locale), numbers and dates
#' numerically. `window = 1` is exact blocking. A missing key (`NA`) is
#' never paired.
#'
#' @inheritParams block_pairs
#' @param window Odd positive integer: the width of the window over the
#'   sorted distinct key values (default `3`: each value and its neighbours
#'   on either side).
#' @return A data frame with one row per pair: `x_index`, `y_index` (an index
#'   into `y`, or into `x` with `x_index < y_index` when `y` is `NULL`) and
#'   `pass`, the first key that paired them, ordered by `x_index` then
#'   `y_index`.
#' @references Hernandez, M. A. and Stolfo, S. J. (1995). The merge/purge
#'   problem for large databases. ACM SIGMOD Record, 24(2), 127-138.
#' @family record linkage pair functions
#' @examples
#' surname <- c("SMITH", "SMITHE", "SMYTH", "JONES", "JONSON", "SMITH")
#' sorted_neighbourhood(surname, window = 3)
#'
#' # Two passes: surname, then surname reversed (catches typos at the start)
#' rev_str <- function(s) vapply(strsplit(s, ""), function(ch)
#'     paste(rev(ch), collapse = ""), "")
#' sorted_neighbourhood(data.frame(surname, rev_str(surname)), window = 3)
#' @export
sorted_neighbourhood <- function(x, y = NULL, window = 3L, nthreads = NULL) {
    .candidate_pairs(x, y, .half_window(window), Inf, nthreads)
}

.as_key_list <- function(keys, arg) {
    if (is.data.frame(keys)) keys <- as.list(keys)
    else if (!is.list(keys)) keys <- list(keys)
    if (length(keys) == 0L) stop(sprintf("`%s` must hold at least one key.", arg))
    lens <- lengths(keys)
    if (any(lens != lens[1L]))
        stop(sprintf("Every key in `%s` must have the same length.", arg))
    for (k in seq_along(keys)) {
        key <- keys[[k]]
        if (is.factor(key)) key <- as.character(key)
        if (!is.atomic(key) || !is.null(dim(key)))
            stop(sprintf("Keys in `%s` must be atomic vectors.", arg))
        keys[[k]] <- key
    }
    unname(keys)
}

.candidate_pairs <- function(x, y, half_window, max_block_size, nthreads) {
    codes <- .candidate_codes(x, y, half_window)
    res <- .candidate_run(codes, max_block_size, nthreads)
    data.frame(x_index = res$x_index, y_index = res$y_index, pass = res$pass)
}

# Finding the pairs is two steps, and they are separate so that the second
# can be run on one slice of the records at a time. The first turns every key
# into an integer code shared by both sides, which is the only part that
# touches the keys themselves; the second pairs records by code.
#
# rLink() runs the second step slice by slice when it filters while scoring,
# so that the candidates of one slice are scored and thinned before the next
# slice is generated.
.candidate_codes <- function(x, y, half_window) {
    xs <- .as_key_list(x, "x")
    dedupe <- is.null(y)
    ys <- if (dedupe) NULL else .as_key_list(y, "y")
    if (!dedupe && length(ys) != length(xs))
        stop("`x` and `y` must have the same number of keys.")
    x_codes <- y_codes <- vector("list", length(xs))
    n_codes <- integer(length(xs))
    n_x <- length(xs[[1L]])
    # One half-window for every pass, or one each: rLink() mixes exact passes
    # with sorted neighbourhoods, and the pair generator has always taken a
    # half-window per pass.
    if (!length(half_window) %in% c(1L, length(xs)))
        stop("There must be one half-window, or one per key.")
    half <- rep_len(as.integer(half_window), length(xs))
    for (k in seq_along(xs)) {
        all <- if (dedupe) xs[[k]] else c(xs[[k]], ys[[k]])
        u <- unique(all[!is.na(all)])
        # Exact blocking only ever compares codes for equality, so any 1..K
        # labelling of the distinct keys will do and the radix sort of what
        # can be millions of distinct keys is skipped. The sorted
        # neighbourhood does need the rank, because it pairs codes that are
        # near each other.
        if (half[k] > 0L) u <- sort(u, method = "radix")
        # One table built and probed once over both sides, rather than one
        # per side; chmatch() hashes R's cached strings by address.
        codes <- if (is.character(all) && is.character(u))
            data.table::chmatch(all, u) else match(all, u)
        x_codes[[k]] <- if (dedupe) codes else codes[seq_len(n_x)]
        y_codes[[k]] <- if (dedupe) x_codes[[k]] else
            codes[n_x + seq_along(ys[[k]])]
        n_codes[k] <- length(u)
    }
    list(x = x_codes, y = y_codes, n = n_codes, half = half, dedupe = dedupe,
         n_x = n_x)
}

# The pairs of the x records in (from, to], which is all of them by default.
# `warn = FALSE` is for a caller that runs several slices and wants the
# skipped blocks reported once rather than once per slice; the count is the
# same whichever slice is asked for, because it is a property of the keys.
.candidate_run <- function(codes, max_block_size, nthreads, from = 0,
                           to = codes$n_x, warn = TRUE) {
    # One limit for every key, or one each; 1e18 stands for "none".
    cap <- as.numeric(max_block_size)
    cap[!is.finite(cap)] <- 1e18
    res <- fast_candidate_pairs_impl(
        codes$x, codes$y, codes$n, codes$half, codes$dedupe, cap,
        .as_nthreads(nthreads), from, to
    )
    if (warn && res$skipped_blocks > 0)
        warning(sprintf(
            "Skipped %s block%s larger than `max_block_size`.",
            format(res$skipped_blocks, big.mark = ","),
            if (res$skipped_blocks == 1) "" else "s"
        ), call. = FALSE)
    res
}

# An upper bound on the partners each x record has, summed over the passes.
# It is what the slices are cut by: cumulate it and a slice ends wherever the
# running total crosses the next multiple of the target. A bound is enough --
# a pair found by two passes is counted twice here and reported once -- as
# long as it never falls short, because it is what stops a slice outgrowing
# memory.
.candidate_bound <- function(codes) {
    bound <- numeric(codes$n_x)
    for (k in seq_along(codes$x)) {
        partners <- .candidate_window(codes, k)[codes$x[[k]]]
        # A record with no key in this pass has no partners in it.
        partners[is.na(partners)] <- 0
        bound <- bound + partners
    }
    bound
}

# For each distinct key of pass `k`, how many y records hold a key within the
# pass's window of it -- the partners of any x record with that key. With a
# half-window of 0 that is the y records sharing the key. Deduplicating, the
# y records are the x records, so a record counts among its own partners.
.candidate_window <- function(codes, k) {
    n <- codes$n[k]
    counts <- as.numeric(tabulate(codes$y[[k]], n))
    if (codes$half[k] == 0L) return(counts)
    cum <- c(0, cumsum(counts))
    at <- seq_len(n)
    cum[pmin(n, at + codes$half[k]) + 1L] - cum[pmax(1L, at - codes$half[k])]
}

# Half-open (from, to] ranges of x records holding about `target` candidate
# pairs each. A single record with more partners than that gets a slice of
# its own rather than being split.
.candidate_slices <- function(bound, target) {
    n <- length(bound)
    if (!n) return(list(from = 0, to = 0))
    if (!is.finite(target) || sum(bound) <= target)
        return(list(from = 0, to = n))
    slice <- cumsum(bound) %/% target
    to <- c(which(diff(slice) != 0), n)
    to <- unique(to)
    list(from = c(0, to[-length(to)]), to = to)
}

#' Entity clusters from matched pairs
#'
#' Groups records into clusters so that any two records joined by a chain of
#' matched pairs share a cluster -- the transitive closure, or connected
#' components, of the match graph. It is the usual last step of
#' deduplication: after scoring candidate pairs and keeping the matches, it
#' gives every record an entity identifier. A union-find structure makes it
#' a near-linear pass over the pairs, with no graph package needed.
#'
#' Transitive closure can chain records that do not match directly (A
#' matches B, B matches C, but A and C differ); review large clusters, or
#' raise the match threshold, when that matters.
#'
#' To cluster the records of two files linked to each other, offset the
#' second file's indices: `cluster_pairs(p$x_index, p$y_index + length(x),
#' n = length(x) + length(y))`.
#'
#' @param i Integer vector of record indices (1-based), or a data frame of
#'   pairs, such as the result of [block_pairs()], [fuzzy_within()],
#'   [minhash_pairs()] or [rLink()], filtered to the matches. A data frame's
#'   `index_1` and `index_2` columns are used when it has them, then its
#'   `x_index` and `y_index`, and its first two columns otherwise --- so an
#'   [rLink()] result, whose first column is `pair_id`, goes straight in.
#' @param j Integer vector of the other record of each pair; omit when `i`
#'   is a data frame.
#' @param n Number of records. Defaults to the largest index; records that
#'   appear in no pair form clusters of their own.
#' @return Integer vector of length `n`: each record's cluster, numbered
#'   from 1 in the order of each cluster's first (lowest-indexed) record.
#' @family record linkage pair functions
#' @examples
#' # Records 1-2 and 2-4 match, as do 5-6; record 3 matches nothing
#' cluster_pairs(c(1, 2, 5), c(2, 4, 6))
#'
#' # From candidate pairs to entities
#' names <- c("JON SMITH", "JOHN SMITH", "MARY JONES", "JOHN SMYTH",
#'            "MARIE JONES")
#' p <- block_pairs(soundex(sub(".* ", "", names)))
#' p <- p[jaro_winkler(names[p$x_index], names[p$y_index]) > 0.85, ]
#' data.frame(names, entity = cluster_pairs(p, n = length(names)))
#' @export
cluster_pairs <- function(i, j = NULL, n = NULL) {
    if (is.data.frame(i)) {
        if (!is.null(j)) stop("Give `j` only when `i` is a vector.")
        if (ncol(i) < 2L) stop("`i` must have at least two columns.")
        # By name where the package's own pair functions provide one, as
        # select_greedy() and linkage_eval() do. Position alone would read
        # `pair_id` out of an rLink() result and cluster that.
        at <- .pair_columns(names(i))
        j <- i[[at[2L]]]
        i <- i[[at[1L]]]
    }
    if (is.null(j)) stop("`j` is missing.")
    if (!is.numeric(i) || !is.numeric(j) || length(i) != length(j))
        stop("`i` and `j` must be numeric vectors of the same length.")
    if (anyNA(i) || anyNA(j) || any(i < 1) || any(j < 1) ||
        any(i != floor(i)) || any(j != floor(j)))
        stop("Indices must be positive integers.")
    top <- if (length(i)) max(i, j) else 0
    if (is.null(n)) n <- top
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 ||
        n != floor(n) || n > .Machine$integer.max)
        stop("`n` must be a single non-negative integer.")
    if (top > n) stop("An index is larger than `n`.")
    fast_cluster_pairs_impl(as.integer(i), as.integer(j), as.integer(n))
}
