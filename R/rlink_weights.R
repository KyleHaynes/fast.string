#' Match weights and probabilities for scored pairs, in one call
#'
#' Turns the several comparator scores of an [rLink()] result into the one
#' number a linkage is actually decided on: a **match weight** in bits, and
#' the **match probability** it implies. It cuts each score into agreement
#' levels, fits the Fellegi-Sunter model to them by EM --- no training data
#' is needed --- and adds `weight` and `probability` columns to the pairs.
#'
#' It is [compare_levels()], [agreement_patterns()], [em_linkage()] and
#' [match_weights()] run in the order they are always run in, with the
#' bookkeeping between them done for you: which columns are scores, which way
#' round each one counts, and which pair each weight belongs to.
#'
#' ```
#' pairs  <- rLink(enrol, claims, block_1 = ..., compare = ...)
#' pairs  <- rLink_weights(pairs)
#' rLink_inspect(pairs)                 # one slider, on the probability
#' ```
#'
#' The two new columns are registered as scores, so [summary()], [plot()],
#' [threshold_sweep()] and [rLink_inspect()] treat them as such; the review
#' app then thresholds the probability alone and shows the field scores as
#' the evidence behind it, and `threshold_sweep(pairs, "weight",
#' probability = "probability")` estimates precision and recall with no truth
#' set.
#'
#' @section Agreement levels:
#' The model does not read a Jaro-Winkler score of 0.93 as such, but as
#' "reached the close-match level". `levels` says where the levels are cut:
#'
#' * a named list gives each score its own cut points,
#'   `list(sn = c(0.85, 0.94, 1), dob = c(0.5, 1))`;
#' * one unnamed numeric vector is used for every score;
#' * `NULL`, and any score a list does not name, is cut automatically. A
#'   score with four or fewer distinct values --- an exact comparison, or a
#'   comparator that already returns steps --- uses those values as its
#'   levels. A similarity on 0--1 is cut at `c(0.8, 0.92, 1)`: different,
#'   partial, close and exact. Anything else has no sensible default and is
#'   an error asking for cut points.
#'
#' Cut points are always given increasing. A score where low means a match
#' --- one left on its own scale by `normalise = FALSE`, or an unbounded
#' distance between numbers --- is recognised from `attr(x, "compare")` and
#' numbered the other way round.
#'
#' @section What to check:
#' Print `attr(out, "model")$fit`. Every `m` should exceed its `u` at the
#' agreement levels, `lambda` should be a plausible share of true matches
#' among the candidates, and the fit should have converged.
#'
#' EM finds *a* split of the candidate pairs into two populations, and it is
#' the blocking that decides whether that split is matches against
#' non-matches. **Block on something you are not also scoring.** If the pairs
#' come from a pass on surname and a pass on date of birth, and both are
#' compared, then every candidate agrees on one or the other by construction;
#' the two populations the model finds are the two passes, a date of birth
#' that agrees looks like proof, and a weight of thirty bits on one field is
#' the sign of it. Blocked on a region, a postcode or a year of birth that is
#' not among the scores, the same fields give a model that separates the
#' matches cleanly. A `filter` in [rLink()] likewise changes which pairs the
#' model sees, and with them the `lambda` it estimates. Scores that measure
#' nearly the same thing --- a surname and its Soundex --- make every weight
#' too confident.
#'
#' Where the pairs you have are not the pairs a model should be fitted on,
#' fit it on ones that are and apply it with `model =`.
#'
#' @section Term frequencies:
#' Agreeing on `"ZELINSKY"` is far stronger evidence than agreeing on
#' `"SMITH"`. `tf` names the scores to adjust for that: where a pair reaches
#' the top level of such a score, its weight becomes
#'
#' ```
#' weight + weight_power * log2(sum(f^2) / f[value])
#' ```
#'
#' where `f` is the relative frequency of each value and `sum(f^2)` the
#' chance that two records taken at random agree on the field at all. A
#' value as common as that average is left alone, a rarer one gains weight
#' and a commoner one loses it, and a field whose values are all equally
#' common is not adjusted at all.
#'
#' The frequencies are counted over the records the pairs reach, once per
#' record and over both files together, so the compared column must have
#' been carried (see `keep` in [rLink()]). Use it on a score whose top level
#' is exact agreement; `weight_power` below 1 damps it where the top level is
#' only a close match.
#'
#' @param x Scored candidate pairs: an [rLink()] result, or any data frame of
#'   pairs with score columns.
#' @param levels Where to cut each score into agreement levels. See
#'   *Agreement levels*.
#' @param scores Score columns to use, or `NULL` (the default) for every one
#'   recorded in `attr(x, "compare")`.
#' @param tf Optional names of scores to term-frequency adjust. See *Term
#'   frequencies*.
#' @param model An `fs_model` from [em_linkage()], or the `attr(, "model")` of
#'   an earlier result, to apply instead of fitting one. Use it to score new
#'   pairs with the weights of a previous run, so that the same pair gets the
#'   same weight whatever else is in the file.
#' @param lambda Starting guess at the share of candidate pairs that are
#'   matches, passed to [em_linkage()].
#' @param weight_power How much of the term-frequency adjustment to apply,
#'   passed to [tf_adjust()].
#' @param ... Further arguments to [em_linkage()], such as `max_iter`,
#'   `m_init` and `u_init`.
#' @return A copy of `x` with two columns added: `weight`, the match weight
#'   in bits, and `probability`, the match probability. `attr(, "model")`
#'   holds `fit` (the `fs_model`), `breaks` (the cut points used for each
#'   score), `decreasing` (which scores were numbered low-is-a-match) and
#'   `tf`, and `attr(, "compare")` gains a row for each new column.
#' @seealso [em_linkage()] for the model, [rLink_inspect()] to choose a cut
#'   and review the pairs near it, [threshold_sweep()] for what a cut would
#'   do, and [select_greedy()] to resolve the pairs kept into links.
#' @family record linkage model functions
#' @examples
#' set.seed(1)
#' surnames <- c(rep("SMITH", 8), rep("JONES", 4), rep("BROWN", 3), "NGUYEN",
#'               "TAYLOR", "WILSON", "MARTIN", "ANDERSON", "THOMAS", "WHITE",
#'               "HARRIS", "CLARK", "LEWIS", "WALKER", "ZELINSKY")
#' givens <- c("JOHN", "MARY", "ROBERT", "ANNA", "DAVID", "LINDA", "PETER",
#'             "SUSAN", "JAMES", "KAREN", "PAUL", "HELEN")
#' n <- 400
#' enrol <- data.frame(
#'     frame_id = sprintf("E%03d", 1:n),
#'     person   = 1:n,
#'     region   = sample(25, n, TRUE),
#'     surname  = sample(surnames, n, TRUE),
#'     given    = sample(givens, n, TRUE),
#'     dob      = as.Date("1980-01-01") + sample(0:199, n, TRUE)
#' )
#' # Two thirds of the claims are people in the enrolment file: some with a
#' # typing error in the surname, some under another given name, some with a
#' # date of birth a day out.
#' claims <- enrol[sample(n, 266), ]
#' claims$frame_id <- sprintf("C%03d", 1:266)
#' typo <- sample(266, 38)
#' claims$surname[typo] <- paste0(claims$surname[typo], "E")
#' nick <- sample(266, 26)
#' claims$given[nick] <- sample(givens, 26, TRUE)
#' slip <- sample(266, 22)
#' claims$dob[slip] <- claims$dob[slip] + 1
#'
#' # Blocked on region, which is not one of the fields being scored.
#' pairs <- rLink(enrol, claims,
#'                block_1 = list(region),
#'                compare = list(sn = jaro_winkler(surname),
#'                               gn = jaro_winkler(given),
#'                               dob = date_compare(dob)))
#'
#' scored <- rLink_weights(pairs)
#' head(scored[order(-weight), c("sn", "gn", "dob", "weight", "probability")])
#'
#' # What each field turned out to be worth, level by level
#' attr(scored, "model")$fit
#'
#' # `person` was never compared, so it can mark the answer: how did a cut
#' # at an even chance do?
#' table(same_person = scored$person_1 == scored$person_2,
#'       linked = scored$probability > 0.5)
#'
#' # Chosen cut points instead of the automatic ones
#' own <- rLink_weights(pairs, levels = list(sn = c(0.9, 1), dob = c(0.5, 1)))
#' attr(own, "model")$breaks
#'
#' # Agreeing on a rare surname is worth more than agreeing on SMITH
#' tf <- rLink_weights(pairs, tf = "sn")
#' agree <- pairs$sn == 1 & pairs$gn == 1 & pairs$dob == 1
#' tapply(tf$weight[agree] - scored$weight[agree],
#'        pairs$surname_1[agree], function(d) round(d[1], 2))
#'
#' # Score a later run with this run's weights
#' again <- rLink_weights(pairs, model = attr(scored, "model"))
#' all.equal(again$weight, scored$weight)
#'
#' # Estimated precision and recall at a few cut-offs, with no truth set
#' threshold_sweep(scored, "probability", probability = "probability",
#'                 thresholds = c(0.5, 0.9, 0.99))[
#'     , c("threshold", "n_pairs", "est_precision", "est_recall")]
#' @export
rLink_weights <- function(x, levels = NULL, scores = NULL, tf = NULL,
                          model = NULL, lambda = 0.1, weight_power = 1, ...) {
    if (!is.data.frame(x)) stop("`x` must be a data frame of scored pairs.")
    if (!nrow(x)) stop("`x` has no pairs to weigh.")
    cmp <- attr(x, "compare")
    # A second call must not feed the first one's weight back in as evidence.
    ours <- .weights_columns(cmp)
    if (is.null(scores)) {
        scores <- setdiff(.fs_score_columns(x, cmp), ours)
        if (!length(scores))
            stop(paste0(
                "Cannot tell which columns hold scores. Give `scores`, or ",
                "pass an rLink() result with its `compare` attribute intact."),
                call. = FALSE)
    } else {
        if (!is.character(scores) || !length(scores) || anyNA(scores) ||
            anyDuplicated(scores))
            stop("`scores` must be a character vector of distinct column names.",
                 call. = FALSE)
        bad <- setdiff(scores, names(x))
        if (length(bad))
            stop(sprintf("`scores` names columns that are not there: %s.",
                         paste0("`", bad, "`", collapse = ", ")),
                 call. = FALSE)
    }
    wrong <- scores[!vapply(scores, function(s)
        is.numeric(x[[s]]) || is.logical(x[[s]]), NA)]
    if (length(wrong))
        stop(sprintf("These score columns are not numeric: %s.",
                     paste0("`", wrong, "`", collapse = ", ")), call. = FALSE)
    clash <- setdiff(intersect(c("weight", "probability"), names(x)), ours)
    if (length(clash))
        stop(sprintf(paste0(
            "`x` already has a column called `%s` that this did not make, ",
            "and it would be overwritten. Rename it first."), clash[1L]),
            call. = FALSE)

    # A model handed back from an earlier call carries its cut points; a bare
    # em_linkage() fit does not, and is then cut like a fresh one.
    held <- NULL
    if (!is.null(model)) {
        if (is.list(model) && inherits(model$fit, "fs_model")) {
            held <- model
            model <- model$fit
        }
        if (!is.list(model) || is.null(model$m) || is.null(model$u) ||
            is.null(model$lambda))
            stop(paste0("`model` must be an fs_model, or the `model` ",
                        "attribute of an rLink_weights() result."),
                 call. = FALSE)
        fitted_on <- names(model$m)
        if (!is.null(fitted_on) && !setequal(fitted_on, scores))
            stop(sprintf(paste0(
                "`model` was fitted on %s, but the scores here are %s."),
                paste0("`", fitted_on, "`", collapse = ", "),
                paste0("`", scores, "`", collapse = ", ")), call. = FALSE)
    }

    decreasing <- .weights_decreasing(scores, cmp, held)
    breaks <- .weights_breaks(x, scores, levels, decreasing, held)
    level <- lapply(scores, function(s) compare_levels(
        as.double(x[[s]]), breaks[[s]], decreasing = decreasing[[s]]))
    names(level) <- scores

    if (is.null(model)) {
        patterns <- agreement_patterns(level)
        fit <- em_linkage(patterns, lambda = lambda, ...)
        at <- attr(patterns, "index")
        weight <- fit$patterns$weight[at]
        probability <- fit$patterns$probability[at]
    } else {
        fit <- model
        got <- match_weights(fit, level)
        weight <- got$weight
        probability <- got$probability
    }

    tf <- .weights_tf_names(tf, scores)
    if (length(tf)) {
        for (s in tf)
            weight <- .weights_tf(x, s, cmp, level[[s]], fit, weight,
                                  weight_power)
        # The weight moved, so the probability it implies moves with it:
        # Bayes' rule with the model's own prevalence.
        probability <- 1 / (1 + (1 - fit$lambda) / fit$lambda * 2^(-weight))
    }

    out <- if (data.table::is.data.table(x)) data.table::copy(x) else x
    out[["weight"]] <- weight
    out[["probability"]] <- probability
    described <- .weights_compare(cmp, scores, decreasing)
    info <- list(fit = fit, breaks = breaks, decreasing = decreasing,
                 scores = scores, tf = tf, weight_power = weight_power)
    if (data.table::is.data.table(out)) {
        data.table::setattr(out, "compare", described)
        data.table::setattr(out, "model", info)
    } else {
        attr(out, "compare") <- described
        attr(out, "model") <- info
    }
    out
}

# The columns an earlier rLink_weights() added, read off the compare attribute
# rather than assumed from their names: a caller's own column called `weight`
# is theirs.
.weights_columns <- function(cmp) {
    if (is.null(cmp) || is.null(cmp$comparator)) return(character())
    as.character(cmp$score)[!is.na(cmp$comparator) &
                            as.character(cmp$comparator) == "em_linkage"]
}

# Which scores count the other way round. rLink() records it; a table that
# lost the attribute is read as higher-is-a-match, which is what rLink()
# produces unless told otherwise.
.weights_decreasing <- function(scores, cmp, held) {
    out <- vapply(scores, function(s) {
        if (!is.null(held) && !is.null(held$decreasing[[s]]))
            return(isTRUE(held$decreasing[[s]]))
        if (is.null(cmp) || is.null(cmp$direction)) return(FALSE)
        at <- match(s, as.character(cmp$score))
        !is.na(at) && identical(as.character(cmp$direction)[at], "lower")
    }, NA)
    as.list(out)
}

.weights_breaks <- function(x, scores, levels, decreasing, held) {
    given <- list()
    every <- NULL
    if (is.list(levels)) {
        if (is.null(names(levels)) || !all(nzchar(names(levels))))
            stop(paste0("`levels` given as a list must be named by score, ",
                        "e.g. list(sn = c(0.85, 0.94, 1))."), call. = FALSE)
        bad <- setdiff(names(levels), scores)
        if (length(bad))
            stop(sprintf("`levels` names scores that are not being used: %s.",
                         paste0("`", bad, "`", collapse = ", ")),
                 call. = FALSE)
        given <- levels
    } else if (!is.null(levels)) {
        if (!is.numeric(levels))
            stop(paste0("`levels` must be NULL, a numeric vector of cut ",
                        "points, or a list of them named by score."),
                 call. = FALSE)
        every <- levels
    }
    out <- lapply(scores, function(s) {
        cut <- if (!is.null(given[[s]])) given[[s]]
               else if (!is.null(every)) every
               else if (!is.null(held) && !is.null(held$breaks[[s]]))
                   held$breaks[[s]]
               else .weights_auto_breaks(x[[s]], s, decreasing[[s]])
        if (!is.numeric(cut) || !length(cut) || anyNA(cut) ||
            (length(cut) > 1L && any(diff(cut) <= 0)))
            stop(sprintf(paste0(
                "The cut points for `%s` must be numbers, strictly ",
                "increasing."), s), call. = FALSE)
        as.double(cut)
    })
    names(out) <- scores
    out
}

# Cut points when none were given. A score that only takes a few values is
# already a set of levels; a 0-1 similarity gets the conventional different /
# partial / close / exact. Nothing else has a default worth guessing at.
.weights_auto_breaks <- function(v, name, decreasing) {
    v <- as.double(v)
    # Looked at in a sample first, so a continuous score of fifty million
    # values is not sorted to find out that it is continuous.
    few <- function(values) {
        u <- unique(values[!is.na(values)])
        if (length(u) <= 4L) sort(u) else NULL
    }
    steps <- if (length(v) > 200000L &&
                 is.null(few(v[seq.int(1L, length(v), length.out = 100000L)])))
        NULL else few(v)
    if (!is.null(steps)) {
        if (length(steps) < 2L)
            stop(sprintf(paste0(
                "`%s` has the same value for every pair, so it tells matches ",
                "from non-matches not at all. Leave it out with `scores`."),
                name), call. = FALSE)
        # compare_levels() counts a value as reaching a level when it is at
        # or above the cut. Numbered downwards, the cut has to sit just
        # above each value instead, which the midpoints do either way.
        return((steps[-1L] + steps[-length(steps)]) / 2)
    }
    lo <- min(v, na.rm = TRUE)
    hi <- max(v, na.rm = TRUE)
    if (!decreasing && lo >= 0 && hi <= 1) return(c(0.8, 0.92, 1))
    stop(sprintf(paste0(
        "There is no default set of agreement levels for `%s`: it is not a ",
        "similarity between 0 and 1 (it runs from %.4g to %.4g%s). Give its ",
        "cut points, e.g. `levels = list(%s = c(...))`."), name, lo, hi,
        if (decreasing) ", low meaning a match" else "", name), call. = FALSE)
}

.weights_tf_names <- function(tf, scores) {
    if (is.null(tf)) return(character())
    if (!is.character(tf) || anyNA(tf))
        stop("`tf` must be a character vector of score names.", call. = FALSE)
    bad <- setdiff(tf, scores)
    if (length(bad))
        stop(sprintf("`tf` names scores that are not being used: %s.",
                     paste0("`", bad, "`", collapse = ", ")), call. = FALSE)
    unique(tf)
}

# One score's term-frequency adjustment, applied to the pairs at its top
# level. Frequencies are counted once per record the pairs reach, not once
# per pair: a record in a block of five hundred would otherwise be counted
# five hundred times, and the common values would look commoner still.
.weights_tf <- function(x, score, cmp, level, fit, weight, weight_power) {
    at <- if (is.null(cmp)) NA_integer_
          else match(score, as.character(cmp$score))
    sfx <- .fs_suffixes(names(x))
    idx <- .pair_columns(names(x))
    cols <- if (is.na(at) || length(sfx) != 2L) c(NA_character_, NA_character_)
            else paste0(c(as.character(cmp$column_1)[at],
                          as.character(cmp$column_2)[at]), sfx)
    if (anyNA(cols) || !all(cols %in% names(x)))
        stop(sprintf(paste0(
            "`tf` needs the values `%s` was computed from, and they are not ",
            "in `x`%s. Carry the compared column with `keep` in rLink()."),
            score, if (anyNA(cols)) "" else
                sprintf(" (looked for %s)",
                        paste0("`", cols, "`", collapse = " and "))),
            call. = FALSE)
    prepare <- function(v) {
        if (is.factor(v)) v <- as.character(v)
        if (is.character(v)) {
            v <- fupper(v)
            v[!nzchar(v)] <- NA_character_
        }
        v
    }
    v1 <- prepare(x[[cols[1L]]])
    v2 <- prepare(x[[cols[2L]]])
    if (!.rlink_same_type(v1, v2)) {
        v1 <- as.character(v1)
        v2 <- as.character(v2)
    }
    pool <- c(v1[!duplicated(x[[idx[1L]]])], v2[!duplicated(x[[idx[2L]]])])
    pool <- pool[!is.na(pool)]
    share <- value_frequency(pool)
    top <- which(!is.na(level) & level == length(fit$u[[score]]) - 1L)
    if (!length(top)) return(weight)
    # The chance that two records taken at random agree on this field at
    # all, which is what agreeing on one particular value is measured
    # against. A field with a single value has nothing to adjust.
    reference <- sum(share[!duplicated(pool)]^2)
    if (!is.finite(reference) || reference <= 0 || reference >= 1)
        return(weight)
    # At the top level the two records agree, so one value's frequency
    # serves both sides.
    f <- share[match(v1[top], pool)]
    known <- !is.na(f) & !is.na(weight[top])
    # Among pairs that agree, a non-match holds value v with chance
    # proportional to f(v)^2 and a match with chance proportional to f(v),
    # so the evidence in the value itself is reference / f(v): one power of
    # the frequency, not two. The reference is the frequencies' own, not the
    # model's `u`, because `u` is estimated on the candidate pairs -- and
    # where a pass blocks on this very field, nearly every candidate agrees
    # on it and `u` says so. tf_adjust() divides by the product of two
    # frequencies; handing it 1 for the second is what makes it this.
    weight[top[known]] <- tf_adjust(weight[top[known]], u = reference,
                                    frequency_1 = f[known], frequency_2 = 1,
                                    weight_power = weight_power)
    weight
}

# The compare attribute with the two new columns described in it. A table
# that arrived without one gets a row per score used, so that the scores are
# still found as scores afterwards rather than being hidden by the new rows.
.weights_compare <- function(cmp, scores, decreasing) {
    if (is.null(cmp)) {
        cmp <- data.frame(
            score = scores, comparator = NA_character_,
            column_1 = NA_character_, column_2 = NA_character_,
            scale = NA_character_,
            direction = ifelse(unlist(decreasing[scores]), "lower", "higher"),
            stringsAsFactors = FALSE)
    } else {
        cmp <- as.data.frame(cmp, stringsAsFactors = FALSE)
        cmp <- cmp[!as.character(cmp$score) %in% c("weight", "probability"), ,
                   drop = FALSE]
    }
    added <- data.frame(
        score = c("weight", "probability"), comparator = "em_linkage",
        column_1 = NA_character_, column_2 = NA_character_,
        scale = c("bits", "probability"), direction = "higher",
        stringsAsFactors = FALSE)
    for (nm in setdiff(names(cmp), names(added))) added[[nm]] <- NA
    for (nm in setdiff(names(added), names(cmp))) cmp[[nm]] <- NA
    out <- rbind(cmp, added[names(cmp)])
    rownames(out) <- NULL
    out
}
