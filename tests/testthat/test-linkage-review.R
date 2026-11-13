# The sweep the slow and obvious way: for every threshold, take the pairs that
# pass it and count from scratch. threshold_sweep() instead walks the pairs once
# and reads the counts off cumulative sums, so this is the independent oracle
# for that rewrite.
ref_sweep <- function(i, j, score, thresholds, operator = ">=") {
    keep_at <- function(t) {
        if (operator == ">=") which(!is.na(score) & score >= t)
        else which(!is.na(score) & score <= t)
    }
    do.call(rbind, lapply(thresholds, function(t) {
        k <- keep_at(t)
        t1 <- tabulate(i[k], max(c(i, 1L)))
        t2 <- tabulate(j[k], max(c(j, 1L)))
        data.frame(
            threshold = t,
            n_pairs = length(k),
            n_linked_1 = sum(t1 > 0L),
            n_linked_2 = sum(t2 > 0L),
            n_one_to_one = sum(t1[i[k]] == 1L & t2[j[k]] == 1L),
            n_conflict_1 = sum(t1 > 1L),
            n_conflict_2 = sum(t2 > 1L))
    }))
}

# Random pair grids with deliberately heavy score ties, which is where a
# cumulative formulation goes wrong if it gets findInterval()'s boundary
# convention backwards.
random_pairs <- function(n_pair, n_1, n_2, digits = 1L) {
    i <- sample.int(n_1, n_pair, TRUE)
    j <- sample.int(n_2, n_pair, TRUE)
    keep <- !duplicated(data.frame(i, j))
    list(i = i[keep], j = j[keep],
         score = round(runif(sum(keep)), digits))
}

# Compare the numbers only: threshold_sweep() also carries `operator`,
# `n_missing` and `score` attributes that the reference does not.
plain <- function(x) {
    out <- as.data.frame(x)
    attributes(out) <- list(names = names(out), class = "data.frame",
                            row.names = seq_len(nrow(out)))
    out
}


test_that("threshold_sweep() agrees with a brute-force sweep", {
    set.seed(11)
    for (rep in seq_len(30)) {
        p <- random_pairs(sample(2:80, 1L), sample(3:15, 1L), sample(3:12, 1L))
        pairs <- data.frame(index_1 = p$i, index_2 = p$j, score = p$score)
        th <- sort(unique(c(p$score, 0, 1, 0.55)), decreasing = TRUE)
        got <- threshold_sweep(pairs, "score", thresholds = th)
        want <- ref_sweep(p$i, p$j, p$score, th)
        expect_equal(plain(got), plain(want))
    }
})

test_that("threshold_sweep() defaults to every distinct score", {
    pairs <- data.frame(index_1 = 1:5, index_2 = 1:5,
                        score = c(0.9, 0.5, 0.9, 0.2, 0.7))
    out <- threshold_sweep(pairs, "score")
    # Strictest cut first, one row per distinct score, no duplicates.
    expect_identical(out$threshold, c(0.9, 0.7, 0.5, 0.2))
    expect_identical(out$n_pairs, c(2L, 3L, 4L, 5L))
})

test_that("threshold_sweep() cuts a distance the other way round", {
    set.seed(12)
    p <- random_pairs(40L, 10L, 8L)
    pairs <- data.frame(index_1 = p$i, index_2 = p$j, d = p$score)
    th <- sort(unique(p$score))
    got <- threshold_sweep(pairs, "d", thresholds = th, operator = "<=")
    want <- ref_sweep(p$i, p$j, p$score, th, operator = "<=")
    expect_equal(plain(got), plain(want))
    expect_identical(attr(got, "operator"), "<=")
    # And the default sweep runs from the strictest cut, which is the lowest.
    expect_identical(threshold_sweep(pairs, "d", operator = "<=")$threshold,
                     sort(unique(p$score)))
})

test_that("threshold_sweep() reads the cut direction from rLink()", {
    enrol <- data.frame(surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
                        stringsAsFactors = FALSE)
    claims <- data.frame(surname = c("SMITH", "JONSE", "BROWNE"),
                         stringsAsFactors = FALSE)
    link <- function(...) rLink(
        enrol, claims, block_1 = list(fsubstr(surname, 1, 1)),
        compare = list(sim = jaro_winkler(surname),
                       dist = levenshtein(surname)), ...)

    # normalise = TRUE converts the edit distance into a similarity, so both
    # columns are higher-is-better and both are cut with `>=` -- even though
    # `scale` still records what the comparator itself returned.
    norm <- link()
    cmp <- attr(norm, "compare")
    expect_identical(cmp$scale, c("similarity", "edit distance"))
    expect_identical(cmp$direction, c("higher", "higher"))
    expect_identical(attr(threshold_sweep(norm, "sim"), "operator"), ">=")
    expect_identical(attr(threshold_sweep(norm, "dist"), "operator"), ">=")

    # normalise = FALSE leaves the distance on its own scale, where low is the
    # match, and `direction` is the only record of that.
    raw <- link(normalise = FALSE)
    cmp <- attr(raw, "compare")
    expect_identical(cmp$scale, c("as is", "as is"))
    expect_identical(cmp$direction, c("higher", "lower"))
    expect_identical(attr(threshold_sweep(raw, "sim"), "operator"), ">=")
    expect_identical(attr(threshold_sweep(raw, "dist"), "operator"), "<=")

    # An explicit operator wins over the attribute.
    expect_identical(
        attr(threshold_sweep(raw, "dist", operator = ">="), "operator"), ">=")
    # And a sweep of the raw distance really does count the near matches.
    low <- threshold_sweep(raw, "dist", thresholds = c(0, 1, 9))
    expect_identical(low$n_pairs,
                     c(sum(raw$dist <= 0), sum(raw$dist <= 1),
                       sum(raw$dist <= 9)))
})

test_that("threshold_sweep() never keeps a pair with no score", {
    pairs <- data.frame(index_1 = 1:4, index_2 = 1:4,
                        score = c(0.9, NA, 0.5, NA))
    out <- threshold_sweep(pairs, "score", thresholds = c(0.9, 0.5, 0))
    expect_identical(out$n_pairs, c(1L, 2L, 2L))
    expect_identical(attr(out, "n_missing"), 2L)
    # Every score missing: nothing is ever kept.
    none <- data.frame(index_1 = 1:3, index_2 = 1:3, score = rep(NA_real_, 3))
    flat <- threshold_sweep(none, "score", thresholds = c(0, 1))
    expect_identical(flat$n_pairs, c(0L, 0L))
    expect_identical(flat$n_one_to_one, c(0L, 0L))
    expect_identical(nrow(threshold_sweep(none, "score")), 0L)
})

test_that("threshold_sweep() handles the degenerate shapes", {
    one <- data.frame(index_1 = 1L, index_2 = 1L, score = 0.5)
    out <- threshold_sweep(one, "score")
    expect_identical(nrow(out), 1L)
    expect_identical(out$n_pairs, 1L)
    expect_identical(out$n_one_to_one, 1L)
    # A threshold above every score keeps nothing.
    high <- threshold_sweep(one, "score", thresholds = 0.9)
    expect_identical(high$n_pairs, 0L)
    expect_identical(high$n_linked_1, 0L)
    expect_identical(high$n_one_to_one, 0L)
})

test_that("threshold_sweep() estimates quality from posteriors", {
    pairs <- data.frame(index_1 = 1:4, index_2 = 1:4,
                        score = c(0.9, 0.8, 0.4, 0.2),
                        p = c(0.95, 0.90, 0.10, 0.05))
    out <- threshold_sweep(pairs, "score", probability = "p",
                           thresholds = c(0.8, 0.2))
    # At 0.8 the first two pairs are kept.
    expect_equal(out$est_tp[1L], 0.95 + 0.90)
    expect_equal(out$est_fp[1L], 0.05 + 0.10)
    expect_equal(out$est_fn[1L], 0.10 + 0.05)
    expect_equal(out$est_precision[1L], 1.85 / 2)
    expect_equal(out$est_recall[1L], 1.85 / 2.00)
    # At 0.2 everything is kept, so recall is 1 and nothing is missed.
    expect_equal(out$est_fn[2L], 0)
    expect_equal(out$est_recall[2L], 1)
    expect_equal(out$est_precision[2L], 2.00 / 4)
    expect_error(threshold_sweep(pairs, "score", probability = c(0.5, 2, 0, 0)),
                 "between 0 and 1")
})

test_that("threshold_sweep() agrees with linkage_eval() against a truth", {
    set.seed(13)
    p <- random_pairs(60L, 12L, 12L)
    pairs <- data.frame(index_1 = p$i, index_2 = p$j, score = p$score)
    truth <- data.frame(index_1 = 1:12, index_2 = 1:12)
    th <- c(0.8, 0.5, 0.2)
    out <- threshold_sweep(pairs, "score", truth = truth, thresholds = th)
    for (k in seq_along(th)) {
        kept <- pairs[!is.na(pairs$score) & pairs$score >= th[k], ]
        want <- linkage_eval(kept, truth)
        expect_identical(out$tp[k], as.integer(want$tp))
        expect_identical(out$fp[k], as.integer(want$fp))
        expect_identical(out$fn[k], as.integer(want$fn))
        expect_equal(out$precision[k], want$precision)
        expect_equal(out$recall[k], want$recall)
        expect_equal(out$f_measure[k], want$f_measure)
    }
})

test_that("threshold_sweep() reads the index columns by name", {
    # pair_id first, so position alone would sweep the wrong columns.
    named <- data.frame(pair_id = paste0("k", 1:4), index_1 = c(1L, 1L, 2L, 3L),
                        index_2 = c(1L, 2L, 3L, 4L), score = c(1, .9, .8, .7))
    by_name <- threshold_sweep(named, "score", thresholds = 0.85)
    expect_identical(by_name$n_pairs, 2L)
    expect_identical(by_name$n_linked_1, 1L)
    expect_identical(by_name$n_conflict_1, 1L)
    # x_index/y_index too, as block_pairs() names them.
    other <- data.frame(x_index = c(1L, 1L), y_index = c(1L, 2L),
                        score = c(1, 0.9))
    expect_identical(threshold_sweep(other, "score",
                                     thresholds = 0.5)$n_conflict_1, 1L)
})

test_that("threshold_sweep() rejects bad input", {
    pairs <- data.frame(index_1 = 1:3, index_2 = 1:3, score = c(1, .5, 0))
    expect_error(threshold_sweep(pairs, "nope"), "not a column of `x`")
    expect_error(threshold_sweep(pairs, c(1, 2)), "but there are 3 pairs")
    expect_error(threshold_sweep(pairs, "score", thresholds = numeric()),
                 "numeric vector without NA")
    expect_error(threshold_sweep(pairs, "score", thresholds = c(1, NA)),
                 "numeric vector without NA")
    expect_error(threshold_sweep(pairs, "score", operator = "gt"),
                 'must be ">=" or "<="')
    expect_error(threshold_sweep(pairs, list(1, 2, 3)), "must be a numeric")
})


# --- review_order ----------------------------------------------------------

test_that("review_order() puts the most undecided pairs first", {
    p <- c(0.99, 0.52, 0.03, 0.47, 0.80)
    # Distances from 0.5: 0.49, 0.02, 0.47, 0.03, 0.30. So 0.03 (0.47 away)
    # is a touch more undecided than 0.99 (0.49 away), and comes first.
    expect_identical(review_order(p), c(2L, 4L, 5L, 3L, 1L))
    expect_equal(p[review_order(p)], c(0.52, 0.47, 0.80, 0.03, 0.99))
})

test_that("review_order() measures distance from the threshold given", {
    s <- c(0.95, 0.88, 0.40, 0.86, 0.91)
    expect_identical(review_order(s, threshold = 0.87), c(2L, 4L, 5L, 1L, 3L))
})

test_that("review_order() breaks ties by the higher score", {
    # 0.6 and 0.4 are both 0.1 from the cut; the likelier match comes first.
    expect_identical(review_order(c(0.4, 0.6), threshold = 0.5), c(2L, 1L))
    expect_identical(review_order(c(0.6, 0.4), threshold = 0.5), c(1L, 2L))
})

test_that("review_order() places the unscorable pairs as asked", {
    p <- c(0.9, NA, 0.5, NA)
    expect_identical(review_order(p), c(3L, 1L, 2L, 4L))
    expect_identical(review_order(p, na_last = FALSE), c(2L, 4L, 3L, 1L))
})

test_that("review_order() rejects bad input", {
    expect_error(review_order("a"), "must be a numeric vector")
    expect_error(review_order(0.5, threshold = c(0.1, 0.2)),
                 "single finite number")
    expect_error(review_order(0.5, na_last = NA), "TRUE or FALSE")
})


# --- summary() and plot() --------------------------------------------------

scored <- function(...) {
    enrol <- data.frame(surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
                        given = c("JOHN", "MARY", "ROB", "JON"),
                        stringsAsFactors = FALSE)
    claims <- data.frame(surname = c("SMITH", "JONSE", "BROWNE"),
                         given = c("JON", "MARY", "ROBERT"),
                         stringsAsFactors = FALSE)
    rLink(enrol, claims, block_1 = list(fsubstr(surname, 1, 1)),
          compare = list(gn = jaro_winkler(given), sn = jaro_winkler(surname)),
          ...)
}

test_that("summary() describes the pairs and every score", {
    out <- scored()
    s <- summary(out)
    expect_s3_class(s, "summary_fs_pairs")
    expect_identical(s$n_pairs, nrow(out))
    expect_identical(s$n_distinct_1, length(unique(out$index_1)))
    expect_identical(s$n_distinct_2, length(unique(out$index_2)))
    expect_identical(s$pair_id, "number")
    expect_identical(s$scores$score, c("gn", "sn"))
    expect_identical(s$scores$comparator, c("jaro_winkler", "jaro_winkler"))
    expect_identical(s$scores$scale, c("similarity", "similarity"))
    expect_equal(s$scores$median[1L], stats::median(out$gn))
    expect_equal(s$scores$max[2L], max(out$sn))
    expect_identical(s$scores$n_missing, c(0L, 0L))
    expect_output(print(s), "candidate pair")
    expect_output(print(s), "Scores")
})

test_that("summary() reports a composite pair_id as an identifier", {
    enrol <- data.frame(frame_id = c("E1", "E2"), surname = c("SMITH", "JONES"),
                        stringsAsFactors = FALSE)
    claims <- data.frame(frame_id = c("C1"), surname = "SMITH",
                         stringsAsFactors = FALSE)
    out <- rLink(enrol, claims, block_1 = list(fsubstr(surname, 1, 1)))
    expect_identical(summary(out)$pair_id, "identifier")
    expect_output(print(summary(out)), "identifier")
})

test_that("summary() still finds the scores without the compare attribute", {
    out <- scored()
    bare <- as.data.frame(out)
    attr(bare, "compare") <- NULL
    class(bare) <- c("fs_pairs", "data.frame")
    s <- summary(bare)
    # Without the attribute the scores are found by elimination, so they come
    # out in column order: score_position = "grouped" puts `sn` beside the
    # surname columns, which precede the given-name ones.
    expect_identical(s$scores$score, c("sn", "gn"))
    # Unknown comparator, because only the attribute knew it.
    expect_true(all(is.na(s$scores$comparator)))
    # Carried columns are not mistaken for scores.
    expect_false(any(c("surname_1", "surname_2") %in% s$scores$score))
})

test_that("summary() survives a filter, and reports no scores when there are none", {
    out <- scored()
    kept <- out[out$gn > 0.5, ]
    expect_identical(summary(kept)$n_pairs, nrow(kept))
    expect_identical(summary(kept)$scores$score, c("gn", "sn"))

    unscored <- rLink(
        data.frame(surname = c("SMITH", "JONES"), stringsAsFactors = FALSE),
        data.frame(surname = "SMITH", stringsAsFactors = FALSE),
        block_1 = list(fsubstr(surname, 1, 1)))
    expect_null(summary(unscored)$scores)
    expect_output(print(summary(unscored)), "No score columns")
})

test_that("summary() counts the pairs each blocking pass found", {
    out <- scored()
    expect_identical(as.integer(sum(summary(out)$passes)), nrow(out))
})

test_that("plot() draws one panel per score and accepts a threshold", {
    out <- scored()
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path), add = TRUE)
    grDevices::pdf(path)
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    expect_identical(plot(out), out)
    expect_identical(plot(out, score = "gn", threshold = 0.8), out)
    expect_error(plot(out, score = "nope"), "not there")
    expect_error(plot(out, score = "gn", threshold = c(1, 2)),
                 "one value, or one per score")
    expect_error(plot(out, threshold = "x"), "must be numeric")
})

test_that("plot() on a sweep draws the chosen series", {
    set.seed(14)
    p <- random_pairs(80L, 20L, 20L)
    pairs <- data.frame(index_1 = p$i, index_2 = p$j, score = p$score)
    sw <- threshold_sweep(pairs, "score")
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path), add = TRUE)
    grDevices::pdf(path)
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    expect_identical(plot(sw), sw)
    expect_identical(plot(sw, what = "n_pairs", threshold = 0.5), sw)
    expect_error(plot(sw, what = "nope"), "must name sweep columns")
    expect_error(plot(sw, what = "n_pairs", threshold = c(1, 2)),
                 "single number")
    expect_error(plot(threshold_sweep(pairs, "score", thresholds = 0.5)),
                 "fewer than two thresholds")
})


# --- cluster_pairs() reads the index columns by name -----------------------

test_that("cluster_pairs() uses the index columns, not the first two", {
    # pair_id first: taking columns 1 and 2 would cluster the identifier.
    pairs <- data.frame(pair_id = 101:103, index_1 = c(1L, 2L, 5L),
                        index_2 = c(2L, 4L, 6L))
    expect_identical(cluster_pairs(pairs, n = 6L),
                     cluster_pairs(c(1L, 2L, 5L), c(2L, 4L, 6L), n = 6L))
    # x_index/y_index as block_pairs() names them.
    other <- data.frame(x_index = c(1L, 2L), y_index = c(2L, 4L))
    expect_identical(cluster_pairs(other, n = 4L),
                     cluster_pairs(c(1L, 2L), c(2L, 4L), n = 4L))
    # Unnamed columns still fall back to position.
    plain_pairs <- data.frame(a = c(1L, 2L), b = c(2L, 4L))
    expect_identical(cluster_pairs(plain_pairs, n = 4L),
                     cluster_pairs(c(1L, 2L), c(2L, 4L), n = 4L))
})

test_that("cluster_pairs() takes an rLink() result straight in", {
    out <- scored()
    keep <- out[out$sn > 0.7, ]
    expect_silent(cl <- cluster_pairs(keep, n = 10L))
    expect_length(cl, 10L)
})

test_that("summary() reports the records there were and the reduction", {
    out <- scored()
    s <- summary(out)
    expect_identical(s$n_records, c(4L, 3L))
    expect_equal(s$n_candidates, nrow(out))
    expect_equal(s$reduction_ratio, 1 - nrow(out) / 12)
    expect_output(print(s), "Reduction ratio")
    expect_output(print(s), "of 4 on side 1")
    # It is a measure of the blocking, so a subset on a score leaves it be.
    kept <- out[out$gn > 0.9, ]
    expect_lt(nrow(kept), nrow(out))
    expect_equal(summary(kept)$reduction_ratio, s$reduction_ratio)
    # A pair table from elsewhere has no record counts to report.
    plain <- structure(data.frame(index_1 = 1:2, index_2 = 1:2),
                       class = c("fs_pairs", "data.frame"))
    expect_true(is.na(summary(plain)$reduction_ratio))
    expect_output(print(summary(plain)), "Records reached: 2 on side 1")
})

test_that("summary() names the passes that were named", {
    enrol <- data.frame(surname = c("SMITH", "JONES"), yob = c(1980, 1975),
                        stringsAsFactors = FALSE)
    out <- rLink(enrol, enrol[2:1, ],
                 block_1 = list(name = surname, born = yob))
    expect_output(print(summary(out)), "1 (name)", fixed = TRUE)
    expect_output(print(attr(out, "blocking")), "name")
})
