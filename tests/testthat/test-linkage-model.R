test_that("compare_levels() cuts scores into contiguous levels", {
    score <- c(1, 0.95, 0.88, 0.60, NA)
    expect_equal(as.integer(compare_levels(score, 1)), c(1L, 0L, 0L, 0L, NA))
    expect_equal(as.integer(compare_levels(score, c(0.8, 0.92, 1))),
                 c(3L, 2L, 1L, 0L, NA))
    expect_equal(attr(compare_levels(score, c(0.8, 0.92, 1)), "n_levels"), 4L)
    # A break is the bottom of its level, not the top
    expect_equal(as.integer(compare_levels(c(0.8, 0.7999), 0.8)), c(1L, 0L))
})

test_that("compare_levels(decreasing = TRUE) reads a distance", {
    expect_equal(as.integer(compare_levels(c(0, 1, 2, 4), c(1, 3),
                                           decreasing = TRUE)),
                 c(2L, 1L, 1L, 0L))
    expect_equal(attr(compare_levels(c(0, 4), c(1, 3), decreasing = TRUE),
                      "n_levels"), 3L)
})

test_that("compare_levels() rejects bad input", {
    expect_error(compare_levels("a", 1), "numeric")
    expect_error(compare_levels(1, numeric()), "at least one cut point")
    expect_error(compare_levels(1, c(2, 1)), "increasing")
    expect_error(compare_levels(1, c(1, NA)), "numeric vector")
    expect_error(compare_levels(1, 1, decreasing = NA), "TRUE or FALSE")
})

test_that("numeric_compare() follows each documented curve", {
    d <- c(0, 1, 2, 5, 10, 20)
    expect_equal(numeric_compare(d, 0, method = "step", offset = 2),
                 as.double(d <= 2))
    expect_equal(numeric_compare(d, 0, method = "linear", offset = 1,
                                 scale = 5),
                 1 - pmin(pmax(d - 1, 0), 10) / 10)
    expect_equal(numeric_compare(d, 0, method = "squared", scale = 5),
                 1 - pmin(d, sqrt(2) * 5)^2 / 50)
    expect_equal(numeric_compare(d, 0, method = "exp", scale = 5),
                 2^(-d / 5))
    expect_equal(numeric_compare(d, 0, method = "gauss", scale = 5),
                 2^(-(d / 5)^2))
    # Every curve is 1 at no difference and within [0, 1] everywhere
    for (method in c("step", "linear", "squared", "exp", "gauss")) {
        s <- numeric_compare(d, 0, method = method, scale = 5)
        expect_equal(s[1], 1)
        expect_true(all(s >= 0 & s <= 1))
        expect_true(all(diff(s) <= 0))
    }
})

test_that("numeric_compare() handles origin, missing values and types", {
    expect_equal(numeric_compare(52300, 52000, method = "gauss",
                                 origin = 300, scale = 100), 1)
    expect_equal(numeric_compare(c(1, NA), c(1, 1)), c(1, NA))
    expect_equal(numeric_compare(c(1, NA), c(1, 1), missing_value = 0.5),
                 c(1, 0.5))
    # Dates are compared in days
    expect_equal(numeric_compare(as.Date("2020-01-11"), as.Date("2020-01-01"),
                                 method = "exp", scale = 10), 0.5)
    expect_equal(numeric_compare(c(1, 2, 3), 2, method = "step"),
                 c(0, 1, 0))
    expect_length(numeric_compare(numeric(), numeric()), 0)
    expect_error(numeric_compare(c(1, 2), c(1, 2, 3)), "same length")
    expect_error(numeric_compare("a", 1), "numeric")
    expect_error(numeric_compare(1, 1, scale = 0), "positive")
    expect_error(numeric_compare(1, 1, offset = -1), "negative")
})

test_that("date_compare() recognises day/month swaps and year typos", {
    swap <- date_compare(as.Date("2001-03-07"), as.Date("2001-07-03"))
    expect_equal(swap, 0.5)
    expect_equal(date_compare(as.Date("2001-03-07"), as.Date("2001-07-03"),
                              swap_score = 0.9), 0.9)
    # A swap that is also the same date is still 1
    expect_equal(date_compare(as.Date("2001-05-05"), as.Date("2001-05-05")), 1)

    # One digit of the year wrong, then two of them transposed
    expect_equal(date_compare(as.Date("1980-06-18"), as.Date("1985-06-18")),
                 0.5)
    expect_equal(date_compare(as.Date("1980-06-18"), as.Date("1908-06-18")),
                 0.5)
    # Two digits wrong is not a typo, so it falls back to the day difference
    expect_lt(date_compare(as.Date("1980-06-18"), as.Date("1955-06-18")), 0.01)

    # Turning the rules off leaves only the difference in days
    plain <- date_compare(as.Date("2001-03-07"), as.Date("2001-07-03"),
                          swap_score = 0, year_typo_score = 0)
    expect_equal(plain, numeric_compare(as.Date("2001-03-07"),
                                        as.Date("2001-07-03"),
                                        method = "exp", scale = 30))
})

test_that("date_compare() scores near dates on the day difference", {
    a <- as.Date("2001-03-07")
    expect_equal(date_compare(a, a), 1)
    expect_equal(date_compare(a, a + 30, method = "exp", scale = 30), 0.5)
    expect_equal(date_compare(a, a + 30, method = "step", offset = 60), 1)
    # The rule only wins when it beats the day difference
    expect_equal(date_compare(a, a + 1, method = "step", offset = 2), 1)
})

test_that("date_compare() handles missing values and bad input", {
    expect_equal(date_compare(as.Date(c(NA, "2001-01-01")),
                              as.Date(c("2001-01-01", NA))),
                 c(NA_real_, NA_real_))
    expect_equal(date_compare(as.Date(c(NA, "2001-01-01")),
                              as.Date(c("2001-01-01", NA)),
                              missing_value = 0), c(0, 0))
    expect_equal(date_compare("2001-03-07", "2001-07-03"), 0.5)
    expect_length(date_compare(as.Date(character()), as.Date(character())), 0)
    expect_error(date_compare(as.Date(c("2001-01-01", "2002-01-01")),
                              as.Date(c("2001-01-01", "2002-01-01",
                                        "2003-01-01"))), "same length")
    expect_error(date_compare(as.Date("2001-01-01"), as.Date("2001-01-01"),
                              swap_score = 2), "between 0 and 1")
})

test_that("agreement_patterns() collapses pairs and keeps the index", {
    levels <- data.frame(
        surname = c(1L, 1L, 0L, 1L, NA),
        given   = c(1L, 0L, 0L, 1L, 1L),
        dob     = c(1L, 1L, 0L, 1L, 1L)
    )
    ap <- agreement_patterns(levels)
    expect_s3_class(ap, "agreement_patterns")
    expect_equal(names(ap), c("surname", "given", "dob", "n"))
    expect_equal(nrow(ap), 4L)
    expect_equal(sum(ap$n), nrow(levels))
    expect_equal(ap$n, c(2L, 1L, 1L, 1L))
    index <- attr(ap, "index")
    expect_equal(index, c(1L, 2L, 3L, 1L, 4L))
    # Every pair's pattern really is the row the index points at
    for (k in seq_len(nrow(levels)))
        expect_equal(unname(unlist(ap[index[k], 1:3])),
                     unname(unlist(levels[k, ])))
    expect_equal(attr(ap, "n_levels"), c(2L, 2L, 2L))
})

test_that("agreement_patterns() accepts logicals, lists and odd names", {
    ap <- agreement_patterns(list(c(TRUE, FALSE), c(FALSE, FALSE)))
    expect_equal(names(ap), c("comparison_1", "comparison_2", "n"))
    expect_equal(nrow(ap), 2L)
    expect_equal(agreement_patterns(c(1L, 1L, 0L))$n, c(2L, 1L))
    expect_error(agreement_patterns(list()), "at least one comparison")
    expect_error(agreement_patterns(list(a = 1L, a = 1L)), "distinct names")
    expect_error(agreement_patterns(list(1L, c(1L, 2L))), "same length")
    expect_error(agreement_patterns(list(c(-1L, 0L))), "whole numbers from 0")
    expect_error(agreement_patterns(list("a")), "integer, logical or factor")
})

# One generated linkage, used by the model tests below: 300 matching pairs
# whose fields usually agree, and 3000 non-matching pairs whose fields rarely
# do. The m and u probabilities are known, so EM can be held to them.
make_levels <- function(seed = 42, n_match = 300, n_other = 3000) {
    set.seed(seed)
    m <- c(surname = 0.95, given = 0.88, dob = 0.97, sex = 0.98)
    u <- c(surname = 0.02, given = 0.04, dob = 0.005, sex = 0.5)
    draw <- function(n, p) rbinom(n, 1L, p)
    levels <- as.data.frame(lapply(names(m), function(f)
        c(draw(n_match, m[[f]]), draw(n_other, u[[f]]))))
    names(levels) <- names(m)
    list(levels = levels, m = m, u = u, n_match = n_match,
         n_other = n_other, is_match = rep(c(TRUE, FALSE),
                                           c(n_match, n_other)))
}

test_that("em_linkage() recovers the probabilities it was generated from", {
    d <- make_levels()
    fit <- em_linkage(d$levels)
    expect_s3_class(fit, "fs_model")
    expect_true(fit$converged)
    expect_equal(fit$lambda, d$n_match / (d$n_match + d$n_other),
                 tolerance = 0.03)
    for (f in names(d$m)) {
        expect_equal(fit$m[[f]][2], d$m[[f]], tolerance = 0.1)
        expect_equal(fit$u[[f]][2], d$u[[f]], tolerance = 0.2)
        # Each comparison's probabilities are a distribution over its levels
        expect_equal(sum(fit$m[[f]]), 1, tolerance = 1e-6)
        expect_equal(sum(fit$u[[f]]), 1, tolerance = 1e-6)
    }
    expect_equal(names(fit$m), names(d$levels))
    expect_equal(unname(fit$n_levels), rep(2L, 4L))
    expect_equal(sum(fit$patterns$n), nrow(d$levels))
})

test_that("em_linkage() separates the two populations it was given", {
    d <- make_levels()
    fit <- em_linkage(d$levels)
    scored <- match_weights(fit, d$levels)
    called <- scored$probability > 0.5
    # Almost every true match is called, and almost no non-match is
    expect_gt(mean(called[d$is_match]), 0.95)
    expect_lt(mean(called[!d$is_match]), 0.01)
    # Agreeing on everything beats disagreeing on everything
    expect_gt(max(scored$weight), 0)
    expect_lt(min(scored$weight), 0)
})

test_that("em_linkage() gives the same fit from a pattern table", {
    d <- make_levels()
    direct <- em_linkage(d$levels)
    from_table <- em_linkage(agreement_patterns(d$levels))
    expect_equal(direct$m, from_table$m)
    expect_equal(direct$u, from_table$u)
    expect_equal(direct$lambda, from_table$lambda)
    expect_equal(direct$loglik, from_table$loglik)
})

test_that("em_linkage() leaves a missing comparison out of the weight", {
    levels <- data.frame(
        a = c(1L, 1L, 0L, 0L, 1L, NA),
        b = c(1L, 0L, 1L, 0L, NA, 1L)
    )
    fit <- em_linkage(levels, lambda = 0.4)
    scored <- match_weights(fit, levels)
    # The pair missing `b` is worth exactly what agreeing on `a` alone is
    weight_a <- log2(fit$m$a[2] / fit$u$a[2])
    expect_equal(scored$weight[5], weight_a)
    weight_b <- log2(fit$m$b[2] / fit$u$b[2])
    expect_equal(scored$weight[6], weight_b)
    expect_equal(scored$weight[1], weight_a + weight_b)
})

test_that("em_linkage() handles more than two levels", {
    set.seed(7)
    draw <- function(n, p) sample(0:2, n, replace = TRUE, prob = p)
    levels <- data.frame(
        surname = c(draw(300, c(0.02, 0.1, 0.88)),
                    draw(3000, c(0.9, 0.08, 0.02))),
        dob     = c(rbinom(300, 1L, 0.97), rbinom(3000, 1L, 0.005))
    )
    fit <- em_linkage(levels)
    expect_equal(unname(fit$n_levels), c(3L, 2L))
    expect_length(fit$m$surname, 3L)
    # A close match is worth less than an exact one but still more than nothing
    weight <- log2(fit$m$surname / fit$u$surname)
    expect_true(weight[3] > weight[2])
    expect_true(weight[2] > weight[1])
})

test_that("em_linkage() validates its arguments and reports non-convergence", {
    levels <- data.frame(a = c(1L, 0L, 1L), b = c(1L, 0L, 0L))
    expect_error(em_linkage(levels, lambda = 0), "between 0 and 1")
    expect_error(em_linkage(levels, lambda = 1), "between 0 and 1")
    expect_error(em_linkage(levels, max_iter = 0), "positive integer")
    expect_error(em_linkage(levels, tol = 0), "positive number")
    expect_error(em_linkage(levels, min_prob = 0.9), "between 0 and 0.5")
    expect_error(em_linkage(levels, m_init = list(c(0.5, 0.5))),
                 "one element per comparison")
    expect_error(em_linkage(levels, m_init = list(c(0.5, 0.5), 0.5)),
                 "one probability per level")
    expect_error(em_linkage(levels, u_init = list(c(0, 1), c(0.5, 0.5))),
                 "probability in \\(0, 1\\)")
    expect_warning(em_linkage(levels, max_iter = 1), "did not converge")
    # A supplied start is used
    fit <- em_linkage(levels, m_init = list(a = c(0.2, 0.8), b = c(0.3, 0.7)))
    expect_s3_class(fit, "fs_model")
})

test_that("print.fs_model() reports the weights per level", {
    d <- make_levels()
    fit <- em_linkage(d$levels)
    out <- capture.output(print(fit))
    expect_true(any(grepl("Fellegi-Sunter model", out)))
    expect_true(any(grepl("lambda", out)))
    expect_true(any(grepl("converged", out)))
    for (f in names(d$m)) expect_true(any(grepl(f, out, fixed = TRUE)))
})

test_that("match_weights() adds up the per-level weights", {
    model <- list(
        m = list(a = c(0.1, 0.9), b = c(0.2, 0.8)),
        u = list(a = c(0.95, 0.05), b = c(0.6, 0.4)),
        lambda = 0.1
    )
    levels <- data.frame(a = c(1L, 0L, 1L, NA), b = c(1L, 0L, 0L, 1L))
    out <- match_weights(model, levels)
    expect_equal(names(out), c("weight", "probability"))
    expect_equal(out$weight, c(
        log2(0.9 / 0.05) + log2(0.8 / 0.4),
        log2(0.1 / 0.95) + log2(0.2 / 0.6),
        log2(0.9 / 0.05) + log2(0.2 / 0.6),
        log2(0.8 / 0.4)
    ))
    # The probability is the weight read through the prior odds
    odds <- 0.1 / 0.9 * 2^out$weight
    expect_equal(out$probability, odds / (1 + odds))
})

test_that("match_weights() matches comparisons by name", {
    model <- list(m = list(a = c(0.1, 0.9), b = c(0.2, 0.8)),
                  u = list(a = c(0.9, 0.1), b = c(0.8, 0.2)), lambda = 0.1)
    straight <- match_weights(model, data.frame(a = 1L, b = 0L))
    swapped <- match_weights(model, data.frame(b = 0L, a = 1L))
    expect_equal(straight, swapped)
    expect_error(match_weights(model, data.frame(a = 1L, c = 0L)),
                 "comparisons the model was fitted on")
    expect_error(match_weights(model, data.frame(a = 1L)),
                 "as many comparisons as the model")
    expect_error(match_weights(list(m = 1), data.frame(a = 1L)), "fs_model")
})

test_that("match_weights() warns about a level the model has no probability for", {
    model <- list(m = list(a = c(0.1, 0.9)), u = list(a = c(0.9, 0.1)),
                  lambda = 0.1)
    expect_warning(out <- match_weights(model, data.frame(a = c(1L, 2L))),
                   "no probability for")
    expect_equal(out$weight[2], NA_real_)
})

test_that("value_frequency() measures the share of each value", {
    surname <- c("SMITH", "SMITH", "SMITH", "NGUYEN", "ZELINSKY", NA)
    f <- value_frequency(surname)
    expect_equal(as.vector(f), c(0.6, 0.6, 0.6, 0.2, 0.2, NA))
    expect_equal(attr(f, "n"), 5)
    # A pool widens the distribution without being scored
    pooled <- value_frequency(surname, pool = c("SMITH", "SMITH", "JONES"))
    expect_length(pooled, length(surname))
    expect_equal(as.vector(pooled), c(rep(5 / 8, 3), 1 / 8, 1 / 8, NA))
    # The same distribution seen from the other side
    other <- value_frequency(c("SMITH", "SMITH", "JONES"), pool = surname)
    expect_equal(as.vector(other), c(5 / 8, 5 / 8, 1 / 8))
})

test_that("value_frequency() handles na_strings, types and bad input", {
    expect_equal(as.vector(value_frequency(c("a", ""))), c(1, NA))
    expect_equal(as.vector(value_frequency(c("a", ""), na_strings = character())),
                 c(0.5, 0.5))
    expect_equal(as.vector(value_frequency(c(1, 1, 2))), c(2 / 3, 2 / 3, 1 / 3))
    expect_equal(as.vector(value_frequency(factor(c("a", "a", "b")))),
                 c(2 / 3, 2 / 3, 1 / 3))
    expect_length(value_frequency(character()), 0)
    expect_error(value_frequency(list(1)), "atomic")
    expect_error(value_frequency("a", pool = 1), "same type")
})

test_that("tf_adjust() rewards a rare value and penalises a common one", {
    # A value as likely as the pooled u implies is left alone
    expect_equal(tf_adjust(6, u = 0.01, frequency_1 = 0.1), 6)
    expect_gt(tf_adjust(6, u = 0.001, frequency_1 = 1e-6), 6)
    expect_lt(tf_adjust(6, u = 0.001, frequency_1 = 0.5), 6)
    expect_equal(tf_adjust(6, u = 0.001, frequency_1 = 0.01),
                 6 + log2(0.001 / 1e-4))
    # The two sides can carry different distributions
    expect_equal(tf_adjust(0, u = 0.5, frequency_1 = 0.1, frequency_2 = 0.2),
                 log2(0.5 / 0.02))
    # weight_power damps it, and 0 turns it off
    full <- tf_adjust(6, u = 0.001, frequency_1 = 1e-6)
    expect_equal(tf_adjust(6, u = 0.001, frequency_1 = 1e-6,
                           weight_power = 0.5), 6 + (full - 6) / 2)
    expect_equal(tf_adjust(6, u = 0.001, frequency_1 = 1e-6,
                           weight_power = 0), 6)
    expect_equal(tf_adjust(c(1, NA), u = 0.1, frequency_1 = c(0.1, 0.1))[2],
                 NA_real_)
    expect_error(tf_adjust(1, u = 0, frequency_1 = 0.1), "between 0 and 1")
    expect_error(tf_adjust(1, u = 0.1, frequency_1 = 0), "relative frequencies")
    expect_error(tf_adjust(1, u = 0.1, frequency_1 = 0.1, weight_power = -1),
                 "negative")
})

test_that("select_greedy() keeps the best pair per record", {
    pairs <- data.frame(index_1 = c(1L, 1L, 2L), index_2 = c(1L, 2L, 2L))
    expect_equal(select_greedy(pairs, weight = c(12, 9, 8)),
                 c(TRUE, FALSE, TRUE))
    # When the middle pair wins, both others are blocked
    expect_equal(select_greedy(pairs, weight = c(1, 9, 8)),
                 c(FALSE, TRUE, FALSE))
    expect_equal(select_greedy(pairs, weight = c(12, 9, 8), n = 2),
                 c(TRUE, TRUE, FALSE))
    # m raises the cap on the second file only, so record 1 is still capped
    expect_equal(select_greedy(pairs, weight = c(12, 9, 8), m = 2),
                 c(TRUE, FALSE, TRUE))
    expect_equal(select_greedy(pairs, weight = c(12, 9, 8), n = 2, m = 2),
                 c(TRUE, TRUE, TRUE))
    # Vectors work as well as a data frame
    expect_equal(select_greedy(pairs[[1]], pairs[[2]], weight = c(12, 9, 8)),
                 c(TRUE, FALSE, TRUE))
})

test_that("select_greedy() respects the caps it is given", {
    set.seed(3)
    i <- sample(1:40, 400, replace = TRUE)
    j <- sample(1:40, 400, replace = TRUE)
    weight <- runif(400)
    for (n in 1:3) for (m in 1:3) {
        keep <- select_greedy(i, j, weight = weight, n = n, m = m)
        expect_true(all(table(i[keep]) <= n))
        expect_true(all(table(j[keep]) <= m))
        # Nothing selectable was left behind
        for (at in which(!keep))
            expect_true(sum(keep & i == i[at]) == n ||
                            sum(keep & j == j[at]) == m)
    }
})

test_that("select_greedy() breaks ties in the order given and skips NA", {
    expect_equal(select_greedy(c(1L, 1L), c(1L, 2L), weight = c(5, 5)),
                 c(TRUE, FALSE))
    expect_equal(select_greedy(c(1L, 1L), c(1L, 2L), weight = c(NA, 5)),
                 c(NA, TRUE))
    expect_equal(select_greedy(integer(), integer(), weight = numeric()),
                 logical())
    expect_error(select_greedy(data.frame(a = 1L)), "at least two columns")
    expect_error(select_greedy(1L, 1L), "`weight` is missing")
    expect_error(select_greedy(1L, 1L, weight = c(1, 2)), "one per pair")
    expect_error(select_greedy(1L, 1L, weight = 1, n = 0), "positive integer")
    expect_error(select_greedy(data.frame(a = 1L, b = 1L), 1L, weight = 1),
                 "only when")
})

test_that("linkage_eval() counts links against the truth", {
    truth <- data.frame(i = c(1L, 2L, 3L, 4L), j = c(1L, 2L, 3L, 4L))
    links <- data.frame(i = c(1L, 2L, 3L, 5L), j = c(1L, 2L, 9L, 5L))
    out <- linkage_eval(links, truth)
    expect_equal(out$n_predicted, 4)
    expect_equal(out$n_truth, 4)
    expect_equal(out$tp, 2)
    expect_equal(out$fp, 2)
    expect_equal(out$fn, 2)
    expect_equal(out$precision, 0.5)
    expect_equal(out$recall, 0.5)
    expect_equal(out$f_measure, 0.5)
    expect_false("tn" %in% names(out))

    with_space <- linkage_eval(links, truth, n_pairs = 100)
    expect_equal(with_space$tn, 94)
    expect_equal(with_space$accuracy, 96 / 100)
    expect_equal(with_space$specificity, 94 / 96)
    expect_error(linkage_eval(links, truth, n_pairs = 2), "smaller than")
})

test_that("linkage_eval() counts duplicate and reversed pairs once", {
    truth <- data.frame(i = 2L, j = 7L)
    expect_equal(linkage_eval(data.frame(i = c(2L, 2L), j = c(7L, 7L)),
                              truth)$n_predicted, 1)
    # Order matters when linking two files, but not when deduplicating one
    expect_equal(linkage_eval(data.frame(i = 7L, j = 2L), truth)$tp, 0)
    expect_equal(linkage_eval(data.frame(i = 7L, j = 2L), truth,
                              ordered = FALSE)$tp, 1)
})

test_that("linkage_eval() is perfect on a perfect linkage and 0 on a wrong one", {
    truth <- data.frame(i = 1:5, j = 1:5)
    perfect <- linkage_eval(truth, truth)
    expect_equal(perfect$precision, 1)
    expect_equal(perfect$recall, 1)
    expect_equal(perfect$f_measure, 1)
    wrong <- linkage_eval(data.frame(i = 1:5, j = 6:10), truth)
    expect_equal(wrong$tp, 0)
    expect_equal(wrong$precision, 0)
    expect_equal(wrong$f_measure, NA_real_)
    expect_error(linkage_eval(1:5, truth), "data frame, matrix or list")
    expect_error(linkage_eval(data.frame(i = 1L), truth), "two columns")
    expect_error(linkage_eval(data.frame(i = 0L, j = 1L), truth), "positive")
})

test_that("blocking_eval() measures reduction, completeness and quality", {
    candidates <- data.frame(x_index = c(1L, 1L, 2L, 3L, 4L, 5L),
                             y_index = c(1L, 3L, 2L, 3L, 1L, 5L))
    truth <- data.frame(x_index = 1:4, y_index = 1:4)
    out <- blocking_eval(candidates, truth, n_1 = 5, n_2 = 5)
    expect_equal(out$n_candidates, 6)
    expect_equal(out$n_pairs, 25)
    expect_equal(out$n_truth, 4)
    expect_equal(out$n_found, 3)            # (1,1), (2,2), (3,3)
    expect_equal(out$reduction_ratio, 1 - 6 / 25)
    expect_equal(out$pairs_completeness, 3 / 4)
    expect_equal(out$pairs_quality, 3 / 6)
    expect_equal(out$f_measure, 2 * 0.75 * 0.5 / 1.25)

    # Deduplication: unordered pairs over n * (n - 1) / 2 of them
    dedupe <- blocking_eval(candidates, truth, n_1 = 10)
    expect_equal(dedupe$n_pairs, 45)
    expect_equal(dedupe$reduction_ratio, 1 - 6 / 45)
    expect_error(blocking_eval(candidates, truth, n_1 = -1), "non-negative")
})

test_that("the pair columns are found by name, not by position", {
    truth <- data.frame(index_1 = 1:3, index_2 = 1:3)
    # rLink() puts `pair_id` first, so position alone would read the wrong
    # columns
    links <- data.frame(pair_id = 101:103, index_1 = 1:3, index_2 = c(1L, 2L, 9L))
    expect_equal(linkage_eval(links, truth)$tp, 2)
    expect_equal(blocking_eval(links, truth, n_1 = 3, n_2 = 9)$n_found, 2)
    expect_equal(select_greedy(links, weight = c(3, 2, 1)),
                 c(TRUE, TRUE, TRUE))
    # x_index / y_index, as block_pairs() gives them
    blocked <- data.frame(x_index = 1:3, y_index = c(1L, 2L, 9L), pass = 1L)
    expect_equal(linkage_eval(blocked, truth)$tp, 2)
    # Without either name, the first two columns are used
    expect_equal(linkage_eval(data.frame(a = 1:3, b = 1:3), truth)$tp, 3)
})

test_that("blocking_eval() shows a full index keeping everything", {
    n <- 6L
    full <- expand.grid(x = seq_len(n), y = seq_len(n))
    truth <- data.frame(x = 1:3, y = 1:3)
    out <- blocking_eval(full[c("x", "y")], truth, n_1 = n, n_2 = n)
    expect_equal(out$reduction_ratio, 0)
    expect_equal(out$pairs_completeness, 1)
    expect_equal(out$pairs_quality, 3 / 36)
})

test_that("the probabilistic pipeline runs end to end from rLink()", {
    enrol <- data.frame(
        id      = 1:6,
        surname = c("SMITH", "JONES", "BROWN", "SMYTH", "NGUYEN", "TAYLOR"),
        given   = c("JOHN", "MARY", "ROB", "JON", "LINH", "SARAH"),
        dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01",
                            "1980-01-02", "1968-03-14", "1955-09-09"))
    )
    claims <- data.frame(
        ref     = LETTERS[1:5],
        surname = c("SMITH", "JONSE", "BROWNE", "NGUYEN", "TAILOR"),
        given   = c("JON", "MARY", "ROBERT", "LIN", "SARA"),
        dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01",
                            "1968-03-14", "1955-09-09"))
    )
    out <- rLink(
        enrol, claims,
        block_1 = list(dob, soundex(surname)),
        compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                       db = (function(a, b) as.numeric(a == b))(dob))
    )
    expect_gt(nrow(out), 0)

    levels <- data.frame(
        sn = compare_levels(out$sn, c(0.85, 0.94, 1)),
        gn = compare_levels(out$gn, c(0.85, 0.94, 1)),
        db = compare_levels(out$db, 1)
    )
    fit <- suppressWarnings(em_linkage(levels, lambda = 0.3))
    scored <- match_weights(fit, levels)
    expect_equal(nrow(scored), nrow(out))
    expect_false(anyNA(scored$weight))

    keep <- scored$weight > 0
    chosen <- select_greedy(out$index_1[keep], out$index_2[keep],
                            weight = scored$weight[keep])
    links <- data.frame(i = out$index_1[keep][chosen],
                        j = out$index_2[keep][chosen])
    # One-to-one, as asked for
    expect_false(anyDuplicated(links$i) > 0)
    expect_false(anyDuplicated(links$j) > 0)

    truth <- data.frame(i = c(1L, 2L, 3L, 5L, 6L), j = 1:5)
    quality <- linkage_eval(links, truth)
    expect_gte(quality$precision, 0.8)
    expect_gte(quality$recall, 0.6)

    blocking <- blocking_eval(out, truth, n_1 = nrow(enrol),
                              n_2 = nrow(claims))
    expect_gt(blocking$reduction_ratio, 0)
    expect_equal(blocking$pairs_completeness, 1)
})


# --- level codes must be in range before anything indexes by them ----------
#
# `m` and `u` are indexed as offset[comparison] + level, so a level outside
# 0..n_levels-1 was an out-of-bounds write into the heap: it did not fail
# where it happened, it corrupted an unrelated allocation and brought the
# session down later with no message. Reachable from R, because
# agreement_patterns() keeps the level count in an attribute that survives
# edits to the table which invalidate it.

test_that("em_linkage() re-reads the levels rather than trusting the attribute", {
    levels <- data.frame(surname = c(1L, 1L, 0L, 1L, NA),
                         given   = c(1L, 0L, 0L, 1L, 1L))
    p <- agreement_patterns(levels)
    expect_identical(unname(attr(p, "n_levels")), c(2L, 2L))
    # A level the attribute does not cover. The attribute is now a floor, so
    # the model grows to fit the data instead of writing past its own
    # parameter vectors.
    p$surname[1L] <- 4L
    fit <- suppressWarnings(em_linkage(p))
    expect_identical(unname(fit$n_levels[["surname"]]), 5L)
    expect_length(fit$m$surname, 5L)
    expect_length(fit$u$surname, 5L)
    # Still every probability, and still probabilities.
    expect_true(all(fit$m$surname > 0 & fit$m$surname < 1))
})

test_that("a negative level is refused rather than indexed with", {
    levels <- data.frame(surname = c(1L, 0L), given = c(1L, 0L))
    p <- agreement_patterns(levels)
    p$surname[1L] <- -1L
    expect_error(em_linkage(p), "whole numbers from 0 up")
})

test_that("the EM kernel itself refuses an out-of-range level", {
    impl <- fast.string:::fast_em_linkage_impl
    # Called directly, bypassing the R-side check: the kernel must not trust
    # its caller, because it is the one doing the indexing.
    pattern <- matrix(c(0L, 1L, 7L, 0L), 2L, 2L)
    expect_error(
        impl(pattern, c(1, 1), c(2L, 2L), rep(0.5, 4), rep(0.5, 4),
             0.1, 10L, 1e-8, 1e-10),
        "only 2 level")
    expect_error(
        impl(pattern, c(1, 1), c(NA_integer_, 2L), rep(0.5, 4), rep(0.5, 4),
             0.1, 10L, 1e-8, 1e-10),
        "at least one level")
    # A well-formed call still fits.
    ok <- impl(matrix(c(0L, 1L, 1L, 0L), 2L, 2L), c(1, 1), c(2L, 2L),
               rep(0.5, 4), rep(0.5, 4), 0.1, 10L, 1e-8, 1e-10)
    expect_length(ok$m, 4L)
})

test_that("a pattern table with nothing to fit is refused by name", {
    levels <- data.frame(surname = c(1L, 0L))
    p <- agreement_patterns(levels)
    p$n <- NULL
    expect_error(em_linkage(p), "no `n` column")
})

test_that("blocking_eval() reads the record counts off an rLink() result", {
    a <- data.frame(k = c("x", "x", "y", "z"), stringsAsFactors = FALSE)
    b <- data.frame(k = c("x", "y", "q"), stringsAsFactors = FALSE)
    truth <- data.frame(index_1 = c(1L, 3L, 4L), index_2 = c(1L, 2L, 3L))
    out <- rLink(a, b, block_1 = list(k))
    expect_identical(blocking_eval(out, truth),
                     blocking_eval(out, truth, n_1 = 4, n_2 = 3))
    expect_equal(blocking_eval(out, truth)$n_pairs, 12)
    # One file: the comparison space is the unordered pairs.
    dup <- rLink(a, block_1 = list(k))
    expect_identical(blocking_eval(dup, truth[1L, ]),
                     blocking_eval(dup, truth[1L, ], n_1 = 4))
    expect_equal(blocking_eval(dup, truth[1L, ])$n_pairs, 6)
    # A value given still wins over the one recorded.
    expect_equal(blocking_eval(out, truth, n_1 = 10, n_2 = 10)$n_pairs, 100)
    expect_error(
        blocking_eval(data.frame(x_index = 1L, y_index = 1L), truth),
        "`n_1` is needed")
})
