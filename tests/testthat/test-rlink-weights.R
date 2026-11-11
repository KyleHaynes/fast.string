# Two files with a known overlap: two thirds of the claims are people in the
# enrolment file, with a typing error in some surnames, another given name in
# some and a date of birth a day out in others. `person` is the truth and is
# never compared. SMITH is common and ZELINSKY rare, which is what the
# term-frequency tests need.
#
# The pairs are blocked on `region`, which is not one of the fields compared.
# That matters to the model: block on a field that is also scored and the two
# populations EM finds are the two blocking passes, not matches and
# non-matches.
linked_files <- function(seed = 1, n = 400) {
    set.seed(seed)
    surnames <- c(rep("SMITH", 8), rep("JONES", 4), rep("BROWN", 3), "NGUYEN",
                  "TAYLOR", "WILSON", "MARTIN", "ANDERSON", "THOMAS", "WHITE",
                  "HARRIS", "CLARK", "LEWIS", "WALKER", "ZELINSKY")
    givens <- c("JOHN", "MARY", "ROBERT", "ANNA", "DAVID", "LINDA", "PETER",
                "SUSAN", "JAMES", "KAREN", "PAUL", "HELEN")
    enrol <- data.frame(
        frame_id = sprintf("E%03d", seq_len(n)),
        person = seq_len(n),
        region = sample(25L, n, TRUE),
        surname = sample(surnames, n, TRUE),
        given = sample(givens, n, TRUE),
        dob = as.Date("1980-01-01") + sample(0:199, n, TRUE),
        stringsAsFactors = FALSE)
    m <- 2 * n %/% 3
    claims <- enrol[sample(n, m), ]
    claims$frame_id <- sprintf("C%03d", seq_len(m))
    typo <- sample(m, m %/% 7)
    claims$surname[typo] <- paste0(claims$surname[typo], "E")
    nick <- sample(m, m %/% 10)
    claims$given[nick] <- sample(givens, length(nick), TRUE)
    slip <- sample(m, m %/% 12)
    claims$dob[slip] <- claims$dob[slip] + 1L
    rownames(claims) <- NULL
    list(enrol = enrol, claims = claims)
}

scored_pairs <- function(seed = 1, ...) {
    f <- linked_files(seed)
    rLink(f$enrol, f$claims, block_1 = list(region),
          compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                         dob = date_compare(dob)), ...)
}

test_that("rLink_weights() is the manual pipeline, done in order", {
    pairs <- scored_pairs()
    out <- rLink_weights(pairs)

    cut <- c(0.8, 0.92, 1)
    levels <- data.frame(sn = compare_levels(pairs$sn, cut),
                         gn = compare_levels(pairs$gn, cut),
                         dob = compare_levels(pairs$dob, cut))
    fit <- em_linkage(levels)
    want <- match_weights(fit, levels)
    expect_equal(out$weight, want$weight)
    expect_equal(out$probability, want$probability)

    info <- attr(out, "model")
    expect_s3_class(info$fit, "fs_model")
    expect_equal(info$fit$lambda, fit$lambda)
    expect_identical(info$scores, c("sn", "gn", "dob"))
    expect_identical(unname(info$breaks), rep(list(cut), 3L))
    expect_identical(unlist(info$decreasing),
                     c(sn = FALSE, gn = FALSE, dob = FALSE))
})

test_that("the pairs that are the same person get the high weights", {
    out <- rLink_weights(scored_pairs())
    same <- out$person_1 == out$person_2
    expect_true(all(out$probability >= 0 & out$probability <= 1))
    expect_gt(mean(out$probability[same] > 0.5), 0.95)
    expect_lt(mean(out$probability[!same] > 0.5), 0.01)
    expect_gt(stats::median(out$weight[same]),
              stats::median(out$weight[!same]) + 10)
    # The fitted prevalence is near the share of pairs that really match.
    expect_equal(attr(out, "model")$fit$lambda, mean(same), tolerance = 0.2)
})

test_that("the new columns are scores, and the input is left alone", {
    pairs <- scored_pairs()
    before <- data.table::copy(pairs)
    out <- rLink_weights(pairs)
    expect_identical(names(pairs), names(before))
    expect_null(attr(pairs, "model"))
    expect_identical(attr(pairs, "compare"), attr(before, "compare"))

    expect_s3_class(out, "fs_pairs")
    expect_true(data.table::is.data.table(out))
    expect_identical(names(out), c(names(pairs), "weight", "probability"))
    cmp <- attr(out, "compare")
    expect_identical(cmp$score, c("sn", "gn", "dob", "weight", "probability"))
    expect_identical(cmp$comparator[4:5], rep("em_linkage", 2L))
    expect_identical(cmp$scale[4:5], c("bits", "probability"))
    expect_identical(cmp$direction[4:5], rep("higher", 2L))
    expect_identical(fast.string:::.fs_score_columns(out), cmp$score)
    expect_identical(summary(out)$scores$score, cmp$score)
    # And it survives a subset, which is how a threshold is applied.
    expect_false(is.null(attr(out[probability > 0.5], "model")))
})

test_that("a second call does not weigh the first call's weight", {
    pairs <- scored_pairs()
    once <- rLink_weights(pairs)
    twice <- rLink_weights(once)
    expect_equal(twice$weight, once$weight)
    expect_identical(attr(twice, "model")$scores, c("sn", "gn", "dob"))
    expect_identical(names(twice), names(once))
    expect_identical(attr(twice, "compare")$score, attr(once, "compare")$score)

    # But a `weight` column that is the caller's own is not written over.
    theirs <- data.table::copy(pairs)
    theirs[["weight"]] <- 1
    expect_error(rLink_weights(theirs, scores = c("sn", "gn")),
                 "already has a column called `weight`")
})

test_that("levels may be given per score, for all scores, or not at all", {
    pairs <- scored_pairs()
    own <- rLink_weights(pairs, levels = list(sn = c(0.9, 1)))
    expect_identical(attr(own, "model")$breaks$sn, c(0.9, 1))
    expect_identical(attr(own, "model")$breaks$gn, c(0.8, 0.92, 1))
    expect_length(attr(own, "model")$fit$m$sn, 3L)

    every <- rLink_weights(pairs, levels = c(0.9, 1))
    expect_identical(unname(attr(every, "model")$breaks),
                     rep(list(c(0.9, 1)), 3L))

    expect_error(rLink_weights(pairs, levels = list(c(0.9, 1))),
                 "named by score")
    expect_error(rLink_weights(pairs, levels = list(nope = c(0.9, 1))),
                 "not being used")
    expect_error(rLink_weights(pairs, levels = list(sn = c(1, 0.9))),
                 "strictly increasing")
    expect_error(rLink_weights(pairs, levels = "high"), "cut points")
})

test_that("a score of a few values is its own set of levels", {
    pairs <- scored_pairs()
    pairs[["same_given"]] <- pairs$given_1 == pairs$given_2
    pairs[["step"]] <- ifelse(pairs$sn == 1, 2, ifelse(pairs$sn > 0.9, 1, 0))
    out <- rLink_weights(pairs, scores = c("same_given", "step", "dob"))
    info <- attr(out, "model")
    expect_identical(info$breaks$same_given, 0.5)
    expect_identical(info$breaks$step, c(0.5, 1.5))
    expect_length(info$fit$m$step, 3L)
    expect_identical(info$breaks$dob, c(0.8, 0.92, 1))

    pairs[["flat"]] <- 1
    expect_error(rLink_weights(pairs, scores = c("sn", "flat")),
                 "same value for every pair")
})

test_that("a score with no natural levels asks for them", {
    f <- linked_files()
    raw <- rLink(f$enrol, f$claims, block_1 = list(region),
                 compare = list(lev = levenshtein(surname),
                                gn = jaro_winkler(given)),
                 normalise = FALSE)
    expect_identical(attr(raw, "compare")$direction[1L], "lower")
    raw[["big"]] <- raw$gn * 100 + seq_len(nrow(raw)) / nrow(raw)
    expect_error(rLink_weights(raw, scores = c("big", "gn")),
                 "no default set of agreement levels for `big`")

    # A distance is numbered the other way round: no edits is the top level.
    out <- rLink_weights(raw, scores = c("lev", "gn"),
                         levels = list(lev = c(1, 3)))
    info <- attr(out, "model")
    expect_true(info$decreasing$lev)
    expect_false(info$decreasing$gn)
    exact <- out$lev == 0 & out$gn == 1
    expect_gt(min(out$weight[exact]), max(out$weight[!exact]))
    lev_weight <- log2(info$fit$m$lev / info$fit$u$lev)
    expect_gt(lev_weight[3L], lev_weight[1L])
})

test_that("a model can be applied instead of fitted", {
    pairs <- scored_pairs()
    first <- rLink_weights(pairs)
    # To a later run: the same weights, and the cut points come with it.
    again <- rLink_weights(pairs, model = attr(first, "model"))
    expect_equal(again$weight, first$weight)
    expect_equal(again$probability, first$probability)
    expect_identical(attr(again, "model")$breaks, attr(first, "model")$breaks)

    # To other pairs: a pair with the same scores gets the same weight.
    other <- scored_pairs(seed = 2)
    applied <- rLink_weights(other, model = attr(first, "model"))
    key <- function(x) paste(x$sn, x$gn, x$dob)
    at <- match(key(applied), key(first))
    expect_true(any(!is.na(at)))
    expect_equal(applied$weight[!is.na(at)], first$weight[at[!is.na(at)]])
    expect_equal(attr(applied, "model")$fit$lambda,
                 attr(first, "model")$fit$lambda)

    # A bare em_linkage() fit works too, cut as the caller says.
    cut <- c(0.8, 0.92, 1)
    levels <- data.frame(sn = compare_levels(pairs$sn, cut),
                         gn = compare_levels(pairs$gn, cut))
    fit <- em_linkage(levels)
    bare <- rLink_weights(pairs, scores = c("sn", "gn"), model = fit,
                          levels = cut)
    expect_equal(bare$weight, match_weights(fit, levels)$weight)

    expect_error(rLink_weights(pairs, model = fit),
                 "was fitted on `sn`, `gn`")
    expect_error(rLink_weights(pairs, model = list(1)), "must be an fs_model")
})

test_that("a term-frequency adjustment favours the rare value", {
    pairs <- scored_pairs()
    plain <- rLink_weights(pairs)
    tf <- rLink_weights(pairs, tf = "sn")
    expect_identical(attr(tf, "model")$tf, "sn")

    # Only pairs at the top level of the adjusted score move.
    top <- pairs$sn == 1
    expect_equal(tf$weight[!top], plain$weight[!top])
    expect_false(isTRUE(all.equal(tf$weight[top], plain$weight[top])))

    # Two pairs with the same agreement pattern, one on SMITH and one on
    # ZELINSKY, had the same weight; now the rare name is worth more.
    full <- top & pairs$gn == 1 & pairs$dob == 1
    smith <- which(full & pairs$surname_1 == "SMITH")[1L]
    rare <- which(full & pairs$surname_1 == "ZELINSKY")[1L]
    expect_false(is.na(smith))
    expect_false(is.na(rare))
    expect_equal(plain$weight[smith], plain$weight[rare])
    expect_gt(tf$weight[rare], tf$weight[smith])
    expect_gt(tf$weight[rare], plain$weight[rare])
    expect_lt(tf$weight[smith], plain$weight[smith])

    # The probability is the adjusted weight through Bayes' rule.
    lambda <- attr(tf, "model")$fit$lambda
    expect_equal(tf$probability,
                 1 / (1 + (1 - lambda) / lambda * 2^(-tf$weight)))
    # Half the adjustment moves the weight half as far.
    half <- rLink_weights(pairs, tf = "sn", weight_power = 0.5)
    expect_equal(half$weight[rare] - plain$weight[rare],
                 (tf$weight[rare] - plain$weight[rare]) / 2)
})

test_that("a term-frequency adjustment needs the compared values", {
    slim <- scored_pairs(keep = character())
    expect_error(rLink_weights(slim, tf = "sn"), "Carry the compared column")
    pairs <- scored_pairs()
    expect_error(rLink_weights(pairs, tf = "nope"), "not being used")
    expect_error(rLink_weights(pairs, tf = 1), "character vector")
})

test_that("a plain data frame of pairs can be weighed", {
    pairs <- scored_pairs()
    plain <- data.frame(index_1 = pairs$index_1, index_2 = pairs$index_2,
                        sn = pairs$sn, gn = pairs$gn)
    out <- rLink_weights(plain)
    expect_s3_class(out, "data.frame")
    expect_false(data.table::is.data.table(out))
    expect_identical(attr(out, "compare")$score,
                     c("sn", "gn", "weight", "probability"))
    expect_equal(out$weight,
                 rLink_weights(pairs, scores = c("sn", "gn"))$weight)

    expect_error(rLink_weights(plain[0, ]), "no pairs")
    expect_error(rLink_weights("nope"), "data frame")
    expect_error(rLink_weights(plain, scores = "nope"), "not there")
    bad <- plain
    bad$txt <- "a"
    expect_error(rLink_weights(bad, scores = c("sn", "txt")), "not numeric")
})

test_that("further arguments reach em_linkage()", {
    pairs <- scored_pairs()
    expect_warning(rLink_weights(pairs, max_iter = 1), "did not converge")
    low <- rLink_weights(pairs, lambda = 0.01)
    expect_s3_class(attr(low, "model")$fit, "fs_model")
})
