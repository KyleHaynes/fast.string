#' Linkage quality across candidate thresholds
#'
#' Reports, for every candidate cut-off, what keeping the pairs above it would
#' actually produce: how many pairs, how many records on each side gain a link,
#' how many of those links are a clean one-to-one, and how many records are left
#' holding two links or more. Choosing a threshold is the one step of a linkage
#' that scores alone cannot answer, and this is the table that answers it.
#'
#' Reading the result from the strictest cut downwards, `n_pairs` and
#' `n_linked_1` rise together while the threshold is still only admitting true
#' matches. The point where `n_conflict_1` and `n_conflict_2` start climbing ---
#' records acquiring a second competing link --- is where the cut has begun
#' admitting pairs that disagree with pairs already accepted, and it is the
#' usual place to stop. None of that needs a known truth.
#'
#' Given `probability` (the posterior from [match_weights()]) the table also
#' estimates precision and recall with no truth data at all, by summing
#' posteriors rather than counting: the expected number of true matches among
#' the pairs kept is the sum of their probabilities, and the expected number of
#' false ones the sum of their complements. `Splink` exposes the same
#' calculation. Given `truth`, the observed precision and recall are reported
#' beside the estimates, which is how the estimates themselves get checked.
#'
#' @param x Candidate pairs: a data frame with `index_1`/`index_2` columns (an
#'   [rLink()] result), or `x_index`/`y_index` (a [block_pairs()] one), or any
#'   data frame whose first two columns are the indices, or a list of two index
#'   vectors.
#' @param score The score to sweep: the name of a column of `x`, or a numeric
#'   vector with one value per pair. A pair whose score is missing is never
#'   kept, at any threshold.
#' @param thresholds Cut-offs to report, or `NULL` (the default) for every
#'   distinct score. Rows follow the order given; the default runs from the
#'   strictest cut to the loosest.
#' @param probability Optional match probability per pair, as a column name or
#'   a numeric vector in `[0, 1]`, such as the `probability` column of
#'   [match_weights()]. Adds the estimated columns.
#' @param truth Optional known matching pairs, in any of the shapes `x` takes.
#'   Adds the observed columns.
#' @param operator `">="` to keep pairs scoring at least the threshold, `"<="`
#'   to keep those scoring at most. `NULL` (the default) reads
#'   `attr(x, "compare")` when `score` names a column of an [rLink()] result,
#'   so a distance column is cut the right way round, and otherwise assumes
#'   `">="`.
#' @param ordered Whether a pair's two indices point into different inputs, as
#'   they do for a linkage. `FALSE` for a deduplication, where `(2, 7)` and
#'   `(7, 2)` are the same pair.
#' @return A data frame of class `fs_sweep`, one row per threshold:
#'
#'   * `threshold`, and `n_pairs`, the pairs kept at it.
#'   * `n_linked_1`, `n_linked_2`: records on each side holding at least one
#'     link.
#'   * `n_one_to_one`: kept pairs whose records each hold exactly one link.
#'   * `n_conflict_1`, `n_conflict_2`: records on each side holding more than
#'     one.
#'   * with `probability`: `est_tp`, `est_fp`, `est_fn`, `est_precision`,
#'     `est_recall` and `est_f_measure`.
#'   * with `truth`: `tp`, `fp`, `fn`, `precision`, `recall` and `f_measure`.
#'
#'   `attr(, "operator")` and `attr(, "n_missing")` record how the cut was
#'   applied and how many pairs had no score.
#' @seealso [plot.fs_sweep()] to see the curve, [review_order()] to put the
#'   undecided pairs first, [rLink_inspect()] to choose a threshold
#'   interactively, and [linkage_eval()] for one threshold against a truth.
#' @family record linkage model functions
#' @examples
#' set.seed(1)
#' # 60 true pairs scoring high, 240 non-matches scoring low.
#' pairs <- data.frame(
#'     index_1 = c(1:60, sample.int(60, 240, TRUE)),
#'     index_2 = c(1:60, sample.int(60, 240, TRUE)),
#'     score   = c(pmin(1, rnorm(60, 0.92, 0.05)),
#'                 pmax(0, rnorm(240, 0.45, 0.12)))
#' )
#'
#' sweep <- threshold_sweep(pairs, "score")
#' head(sweep)
#'
#' # Conflicts start climbing where the cut begins admitting non-matches.
#' sweep[sweep$threshold < 0.8 & sweep$threshold > 0.6,
#'       c("threshold", "n_pairs", "n_one_to_one", "n_conflict_1")]
#'
#' # Only the cut-offs worth reporting
#' threshold_sweep(pairs, "score", thresholds = seq(0.5, 0.95, by = 0.05))
#'
#' # A distance is cut the other way round
#' threshold_sweep(pairs, 1 - pairs$score, thresholds = c(0.05, 0.1, 0.2),
#'                 operator = "<=")
#'
#' # Against a known truth
#' threshold_sweep(pairs, "score", thresholds = c(0.7, 0.8, 0.9),
#'                 truth = data.frame(index_1 = 1:60, index_2 = 1:60))
#' @export
threshold_sweep <- function(x, score, thresholds = NULL, probability = NULL,
                            truth = NULL, operator = NULL, ordered = TRUE) {
    pairs <- .as_pair_set(x, "x", ordered)
    n <- length(pairs$i)
    sc <- .sweep_numeric(x, score, n, "score")
    op <- .sweep_operator(x, score, operator)
    prob <- if (is.null(probability)) NULL else
        .sweep_numeric(x, probability, n, "probability")
    if (!is.null(prob) && any(prob < 0 | prob > 1, na.rm = TRUE))
        stop("`probability` must lie between 0 and 1.")

    # Everything below reads "kept" as `value >= cut`. A `<=` cut becomes that
    # by negating both sides, which keeps one code path instead of two.
    flip <- identical(op, "<=")
    value <- if (flip) -sc else sc

    th <- .sweep_thresholds(thresholds, sc, flip)
    # order() with na.last = NA drops the pairs that have no score, which are
    # the pairs no threshold keeps.
    o <- order(value, decreasing = TRUE, na.last = NA)
    m <- length(o)
    i <- pairs$i[o]
    j <- pairs$j[o]
    sorted <- value[o]

    # Walking the pairs from the best score down, each record is seen for the
    # first time once and becomes contested once, so cumulative counts of those
    # two events are the whole answer -- no loop over thresholds.
    occ_i <- data.table::rowid(i)
    occ_j <- data.table::rowid(j)
    cum_linked_1 <- cumsum(occ_i == 1L)
    cum_linked_2 <- cumsum(occ_j == 1L)
    cum_conflict_1 <- cumsum(occ_i == 2L)
    cum_conflict_2 <- cumsum(occ_j == 2L)

    # A pair is one-to-one while both of its records are still unique. It
    # becomes so at its own position, and stops at the position where either
    # record turns up again, so tabulating those retirements turns the count
    # into another cumulative sum.
    eligible <- occ_i == 1L & occ_j == 1L
    second_i <- rep(m + 1L, if (m) max(i) else 0L)
    second_j <- rep(m + 1L, if (m) max(j) else 0L)
    at_2 <- which(occ_i == 2L)
    second_i[i[at_2]] <- at_2
    at_2 <- which(occ_j == 2L)
    second_j[j[at_2]] <- at_2
    expiry <- pmin(second_i[i], second_j[j])[eligible]
    retire <- tabulate(expiry, nbins = m + 1L)[seq_len(m)]
    cum_one_to_one <- cumsum(eligible) - cumsum(retire)

    # How many pairs each threshold keeps: -sorted is ascending, so this counts
    # the scores at or above the cut.
    cuts <- if (flip) -th else th
    k <- findInterval(-cuts, -sorted)
    at <- function(cum) {
        if (!m) return(rep(0L, length(k)))
        ifelse(k == 0L, 0L, cum[pmax(k, 1L)])
    }

    out <- data.frame(
        threshold = th,
        n_pairs = k,
        n_linked_1 = at(cum_linked_1),
        n_linked_2 = at(cum_linked_2),
        n_one_to_one = at(cum_one_to_one),
        n_conflict_1 = at(cum_conflict_1),
        n_conflict_2 = at(cum_conflict_2)
    )

    if (!is.null(prob)) {
        p <- prob[o]
        # A pair with no posterior is evidence neither way, so it contributes
        # nothing to either sum rather than counting as a false positive.
        tp_each <- ifelse(is.na(p), 0, p)
        fp_each <- ifelse(is.na(p), 0, 1 - p)
        total <- sum(tp_each)
        est_tp <- at(cumsum(tp_each))
        out$est_tp <- est_tp
        out$est_fp <- at(cumsum(fp_each))
        out$est_fn <- total - est_tp
        out$est_precision <- .sweep_ratio(est_tp, est_tp + out$est_fp)
        out$est_recall <- .sweep_ratio(est_tp, rep(total, length(est_tp)))
        out$est_f_measure <- .sweep_f(out$est_precision, out$est_recall)
    }

    if (!is.null(truth)) {
        known <- .as_pair_set(truth, "truth", ordered)
        # Membership is resolved once, so each threshold is a cumulative sum
        # rather than another pass over the truth.
        key_t <- unique(paste0(known$i, .rlink_sep, known$j))
        hit <- !is.na(data.table::chmatch(paste0(i, .rlink_sep, j), key_t))
        n_truth <- length(key_t)
        tp <- at(cumsum(hit))
        out$tp <- tp
        out$fp <- k - tp
        out$fn <- n_truth - tp
        out$precision <- .sweep_ratio(tp, k)
        out$recall <- .sweep_ratio(tp, rep(n_truth, length(tp)))
        out$f_measure <- .sweep_f(out$precision, out$recall)
    }

    structure(out, class = c("fs_sweep", "data.frame"), operator = op,
              n_missing = n - m,
              score = if (is.character(score) && length(score) == 1L) score
                      else NA_character_)
}

# A score given either as a column name or as one value per pair.
.sweep_numeric <- function(x, value, n, arg) {
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
        if (!is.data.frame(x) || !value %in% names(x))
            stop(sprintf("`%s` names `%s`, which is not a column of `x`.",
                         arg, value), call. = FALSE)
        value <- x[[value]]
    }
    if (is.logical(value)) value <- as.integer(value)
    if (!is.numeric(value) || !is.null(dim(value)))
        stop(sprintf("`%s` must be a numeric vector or the name of one column.",
                     arg), call. = FALSE)
    if (length(value) != n)
        stop(sprintf("`%s` has %s values, but there are %s pairs.", arg,
                     format(length(value), big.mark = ","),
                     format(n, big.mark = ",")), call. = FALSE)
    as.double(value)
}

# Which way round the cut goes. A score where 0 means an exact match has to be
# cut with `<=`, or the sweep comes out upside down.
#
# rLink() records this as the `direction` column of attr(x, "compare"), which
# is read rather than inferred from the comparator or from `scale`. `scale`
# would be the wrong thing to read: it describes what the *comparator*
# returned, and under the default `normalise = TRUE` an edit distance has
# already been converted so that 1 is an exact match --- so a column whose
# `scale` says "edit distance" is nonetheless cut with `>=`. Only
# `normalise = FALSE` leaves a score low-is-better, and `direction` is what
# says so.
.sweep_operator <- function(x, score, operator) {
    if (!is.null(operator)) {
        if (!is.character(operator) || length(operator) != 1L ||
            !operator %in% c(">=", "<="))
            stop('`operator` must be ">=" or "<=".', call. = FALSE)
        return(operator)
    }
    cmp <- attr(x, "compare")
    if (is.null(cmp) || is.null(cmp$direction) || !is.character(score) ||
        length(score) != 1L)
        return(">=")
    at <- match(score, as.character(cmp$score))
    if (is.na(at)) return(">=")
    if (identical(as.character(cmp$direction)[at], "lower")) "<=" else ">="
}

.sweep_thresholds <- function(thresholds, score, flip) {
    if (is.null(thresholds)) {
        th <- sort(unique(score[!is.na(score)]), decreasing = !flip)
        if (!length(th)) th <- numeric()
        return(th)
    }
    if (!is.numeric(thresholds) || !length(thresholds) || anyNA(thresholds))
        stop("`thresholds` must be a numeric vector without NA.", call. = FALSE)
    as.double(thresholds)
}

.sweep_ratio <- function(num, den) ifelse(den > 0, num / den, NA_real_)

.sweep_f <- function(precision, recall) {
    total <- precision + recall
    ifelse(!is.na(total) & total > 0, 2 * precision * recall / total, NA_real_)
}


#' Order candidate pairs by how undecided they are
#'
#' Returns the ordering that puts the pairs closest to a decision boundary
#' first --- the ones a person should look at, because they are where a judgement
#' changes the outcome. Pairs scoring far above or far below the threshold are
#' already settled by the threshold itself, and reviewing them in score order
#' means spending the first hour on the obvious ones.
#'
#' Give it a match probability and the default `threshold = 0.5` is the model's
#' own point of indifference. Give it a comparator score and a threshold you are
#' considering, and it ranks by distance from that cut instead. Ties are broken
#' by the higher score, so of two equally borderline pairs the likelier match
#' comes first, and the order is therefore stable rather than arbitrary.
#'
#' @param x Numeric vector: a match probability from [match_weights()], or any
#'   comparator score.
#' @param threshold The decision boundary to measure distance from, `0.5` by
#'   default.
#' @param na_last Whether pairs with no score go last. They are undecidable
#'   rather than undecided, so they are not usually where review should start.
#' @return Integer vector, an ordering of `x` as [order()] returns one.
#' @seealso [threshold_sweep()] for choosing the threshold in the first place,
#'   and [rLink_inspect()], which uses this to order its review queue.
#' @family record linkage model functions
#' @examples
#' probability <- c(0.99, 0.52, 0.03, 0.47, NA, 0.80)
#' review_order(probability)
#'
#' # The two pairs either side of 0.5 come first, the certain ones last.
#' probability[review_order(probability)]
#'
#' # Around a threshold on a comparator score instead
#' score <- c(0.95, 0.88, 0.40, 0.86, 0.91)
#' score[review_order(score, threshold = 0.87)]
#' @export
review_order <- function(x, threshold = 0.5, na_last = TRUE) {
    if (!is.numeric(x) && !is.logical(x))
        stop("`x` must be a numeric vector.")
    threshold <- .one_finite_number(threshold, "threshold")
    .check_flag(na_last, "na_last")
    x <- as.double(x)
    order(abs(x - threshold), -x, na.last = na_last)
}


# --- describing a table of candidate pairs ---------------------------------

# The columns rLink() puts in front of the data: everything else is either a
# carried source column or a score.
.fs_lead_columns <- function(nms) {
    at <- .pair_columns(nms)
    c("pair_id", if (is.character(at)) at else nms[at], "pass", "block_key")
}

# "_1"/"_2", or whatever `suffixes` renamed them to, read back off the index
# columns so that carried columns can be told from scores.
.fs_suffixes <- function(nms) {
    at <- .pair_columns(nms)
    if (!is.character(at)) return(NULL)
    sub("^index", "", at)
}

# Which columns hold scores. rLink() records this in attr(x, "compare"), and
# that is used when it survives; a table that has lost the attribute -- which
# `as.data.frame()` and a hand-built frame both do -- falls back to "numeric,
# and neither an index nor a carried column".
.fs_score_columns <- function(x, cmp = attr(x, "compare")) {
    nms <- names(x)
    if (!is.null(cmp) && !is.null(cmp$score))
        return(intersect(as.character(cmp$score), nms))
    cand <- setdiff(nms, .fs_lead_columns(nms))
    sfx <- .fs_suffixes(nms)
    if (length(sfx) == 2L)
        cand <- cand[!(endsWith(cand, sfx[1L]) | endsWith(cand, sfx[2L]))]
    keep <- vapply(cand, function(nm) {
        v <- x[[nm]]
        (is.numeric(v) || is.logical(v)) && is.null(dim(v))
    }, logical(1L))
    cand[keep]
}

#' Describe a table of candidate pairs
#'
#' Reports what an [rLink()] result contains: how many pairs, how many records
#' of each input they reach, how the blocking passes contributed, and the
#' distribution of every score column. It is the first thing to run on a result
#' and the quickest way to see whether the blocking found too little or far too
#' much.
#'
#' Score columns are read from `attr(object, "compare")`, so each one is
#' reported with the comparator that produced it and the scale that comparator
#' was found to use. A table that has lost the attribute still works: numeric
#' columns that are neither indices nor carried values are taken as the scores.
#'
#' @param object An [rLink()] result, or any pair table (see [threshold_sweep()]
#'   for the shapes accepted).
#' @param ... Ignored.
#' @return A list of class `summary_fs_pairs`, with a `print()` method:
#'   `n_pairs`, `n_distinct_1` and `n_distinct_2` (records reached on each
#'   side), `passes` (a table of pairs per blocking pass, or `NULL`),
#'   `pair_id` (`"identifier"` or `"number"`), `scores`, a data frame with
#'   one row per score column, and `filter`, what a `filter` in [rLink()]
#'   removed (or `NULL`).
#' @seealso [plot.fs_pairs()] for the distributions as histograms, and
#'   [threshold_sweep()] for what a cut-off would do.
#' @family record linkage model functions
#' @examples
#' enrol <- data.frame(
#'     surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
#'     given   = c("JOHN", "MARY", "ROB", "JON")
#' )
#' claims <- data.frame(
#'     surname = c("SMITH", "JONSE", "BROWNE"),
#'     given   = c("JON", "MARY", "ROBERT")
#' )
#' out <- rLink(enrol, claims, block_1 = list(soundex(surname)),
#'              compare = list(jaro_winkler(given), sn = jaro_winkler(surname)))
#' summary(out)
#'
#' # The summary is a list, so the pieces can be used directly
#' summary(out)$scores$median
#' @export
summary.fs_pairs <- function(object, ...) {
    nms <- names(object)
    cmp <- attr(object, "compare")
    at <- .pair_columns(nms)
    idx <- if (is.character(at)) at else nms[at]
    scores <- .fs_score_columns(object, cmp)

    rows <- lapply(scores, function(s) {
        v <- as.double(object[[s]])
        ok <- v[!is.na(v)]
        q <- if (length(ok))
            stats::quantile(ok, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
        else rep(NA_real_, 5L)
        k <- if (is.null(cmp)) NA_integer_ else match(s, as.character(cmp$score))
        data.frame(
            score = s,
            comparator = if (is.na(k)) NA_character_
                         else as.character(cmp$comparator)[k],
            scale = if (is.na(k)) NA_character_ else as.character(cmp$scale)[k],
            n_missing = sum(is.na(v)),
            min = q[1L], q1 = q[2L], median = q[3L], q3 = q[4L], max = q[5L],
            n_at_min = if (length(ok)) sum(ok == q[1L]) else 0L,
            n_at_max = if (length(ok)) sum(ok == q[5L]) else 0L,
            stringsAsFactors = FALSE)
    })

    structure(list(
        n_pairs = nrow(object),
        n_distinct_1 = length(unique(object[[idx[1L]]])),
        n_distinct_2 = length(unique(object[[idx[2L]]])),
        passes = if ("pass" %in% nms && !all(is.na(object[["pass"]])))
            table(pass = object[["pass"]]) else NULL,
        pair_id = if (!"pair_id" %in% nms) "none"
                  else if (is.character(object[["pair_id"]])) "identifier"
                  else "number",
        scores = if (length(rows)) do.call(rbind, rows) else NULL,
        # What a `filter` in rLink() removed, when there was one: these are
        # then the pairs that were kept, not the candidates.
        filter = attr(object, "filter")
    ), class = "summary_fs_pairs")
}

#' @export
print.summary_fs_pairs <- function(x, ...) {
    cat(sprintf("%s candidate pair%s\n", format(x$n_pairs, big.mark = ","),
                if (x$n_pairs == 1L) "" else "s"))
    cat(sprintf("Records reached: %s on side 1, %s on side 2\n",
                format(x$n_distinct_1, big.mark = ","),
                format(x$n_distinct_2, big.mark = ",")))
    cat(sprintf("Pair identifier: %s\n", x$pair_id))
    if (!is.null(x$filter))
        cat(sprintf("Kept by `filter` (%s): %s of %s candidates\n",
                    x$filter$rule, format(x$filter$n_kept, big.mark = ","),
                    format(x$filter$n_candidates, big.mark = ",",
                           scientific = FALSE)))
    if (!is.null(x$passes)) {
        cat("\nPairs per blocking pass\n")
        print(x$passes)
    }
    if (is.null(x$scores)) {
        cat("\nNo score columns.\n")
        return(invisible(x))
    }
    cat("\nScores\n")
    shown <- x$scores
    for (nm in c("min", "q1", "median", "q3", "max"))
        shown[[nm]] <- signif(shown[[nm]], 3)
    print(shown, row.names = FALSE)
    invisible(x)
}

#' Distribution of each comparator score
#'
#' Draws one histogram per score column. A linkage that is going to work shows
#' two humps --- the non-matches low down and the matches up near an exact
#' agreement --- and the dip between them is where the threshold belongs. A
#' single hump means the comparator is not separating the two populations, which
#' is worth knowing before tuning a cut-off that cannot exist.
#'
#' @param x An [rLink()] result, or any pair table.
#' @param score Score columns to draw, or `NULL` (the default) for all of them.
#' @param breaks Passed to [graphics::hist()].
#' @param threshold Optional cut-off, or one per score, drawn as a vertical
#'   line so a candidate threshold can be seen against the distribution.
#' @param ... Passed to [graphics::hist()].
#' @return `x`, invisibly.
#' @seealso [summary.fs_pairs()] for the same distributions as numbers, and
#'   [threshold_sweep()] for what a cut-off would do.
#' @family record linkage model functions
#' @examples
#' set.seed(1)
#' pairs <- data.frame(
#'     index_1 = 1:300, index_2 = 1:300,
#'     score = c(pmin(1, rnorm(60, 0.92, 0.05)),
#'               pmax(0, rnorm(240, 0.45, 0.12)))
#' )
#' plot(structure(pairs, class = c("fs_pairs", "data.frame")),
#'      threshold = 0.75)
#' @export
plot.fs_pairs <- function(x, score = NULL, breaks = 50, threshold = NULL, ...) {
    scores <- .fs_score_columns(x)
    if (!is.null(score)) {
        if (!is.character(score)) stop("`score` must be a character vector.")
        bad <- setdiff(score, names(x))
        if (length(bad))
            stop(sprintf("`score` names columns that are not there: %s.",
                         paste0("`", bad, "`", collapse = ", ")))
        scores <- score
    }
    if (!length(scores))
        stop("There are no score columns to plot.")
    if (!is.null(threshold)) {
        if (!is.numeric(threshold))
            stop("`threshold` must be numeric.")
        if (length(threshold) == 1L)
            threshold <- rep(threshold, length(scores))
        if (length(threshold) != length(scores))
            stop("`threshold` must be one value, or one per score.")
    }
    cmp <- attr(x, "compare")
    old <- graphics::par(mfrow = grDevices::n2mfrow(length(scores)))
    on.exit(graphics::par(old), add = TRUE)
    for (k in seq_along(scores)) {
        s <- scores[k]
        v <- as.double(x[[s]])
        at <- if (is.null(cmp)) NA_integer_
              else match(s, as.character(cmp$score))
        graphics::hist(v[!is.na(v)], breaks = breaks, main = s,
                       xlab = if (is.na(at)) "score" else
                           sprintf("%s (%s)", as.character(cmp$comparator)[at],
                                   as.character(cmp$scale)[at]),
                       ...)
        if (!is.null(threshold))
            graphics::abline(v = threshold[k], lwd = 2, lty = 2)
    }
    invisible(x)
}

#' Plot a threshold sweep
#'
#' Draws what [threshold_sweep()] tabulated: the pairs kept and the clean
#' one-to-one links against the cut-off, with the contested records on the same
#' axes. The threshold to choose is where the one-to-one line has levelled off
#' but the conflict lines have not yet lifted.
#'
#' @param x A [threshold_sweep()] result.
#' @param what Which series to draw: any of `"n_pairs"`, `"n_one_to_one"`,
#'   `"n_conflict_1"`, `"n_conflict_2"`, `"n_linked_1"`, `"n_linked_2"`.
#'   Defaults to the first four.
#' @param threshold Optional cut-off, drawn as a vertical line.
#' @param ... Passed to [graphics::matplot()].
#' @return `x`, invisibly.
#' @family record linkage model functions
#' @examples
#' set.seed(1)
#' pairs <- data.frame(
#'     index_1 = c(1:60, sample.int(60, 240, TRUE)),
#'     index_2 = c(1:60, sample.int(60, 240, TRUE)),
#'     score   = c(pmin(1, rnorm(60, 0.92, 0.05)),
#'                 pmax(0, rnorm(240, 0.45, 0.12)))
#' )
#' plot(threshold_sweep(pairs, "score"))
#' @export
plot.fs_sweep <- function(x, what = NULL, threshold = NULL, ...) {
    choices <- c("n_pairs", "n_one_to_one", "n_conflict_1", "n_conflict_2",
                 "n_linked_1", "n_linked_2")
    what <- if (is.null(what)) choices[1:4] else what
    if (!is.character(what) || !length(what))
        stop("`what` must be a character vector.")
    bad <- setdiff(what, choices)
    if (length(bad))
        stop(sprintf("`what` must name sweep columns: %s.",
                     paste0("`", choices, "`", collapse = ", ")))
    if (nrow(x) < 2L)
        stop("A sweep of fewer than two thresholds has nothing to plot.")
    o <- order(x$threshold)
    graphics::matplot(
        x$threshold[o], as.matrix(as.data.frame(x)[o, what, drop = FALSE]),
        type = "l", lty = 1, lwd = 2, col = seq_along(what),
        xlab = sprintf("threshold (keeping score %s cut)",
                       attr(x, "operator")),
        ylab = "pairs or records", ...)
    graphics::legend("topright", legend = what, lty = 1, lwd = 2,
                     col = seq_along(what), bty = "n", cex = 0.8)
    if (!is.null(threshold)) {
        if (!is.numeric(threshold) || length(threshold) != 1L)
            stop("`threshold` must be a single number.")
        graphics::abline(v = threshold, lty = 2)
    }
    invisible(x)
}
