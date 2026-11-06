#' Agreement levels from a comparator score
#'
#' Cuts a continuous comparator score into a small number of ordered
#' agreement *levels*, which is what the Fellegi-Sunter model of
#' [em_linkage()] consumes: it does not read a Jaro-Winkler score of 0.93 as
#' such, but as "reached the close-match level". Levels are numbered from `0`
#' (least agreement) upwards, and `NA` stays `NA` -- the model then leaves
#' that comparison out of the pair's weight rather than guessing at it.
#'
#' Two levels (`breaks` of length 1) is agree/disagree, as the classic model
#' assumes. Three or four let a near match count for something without
#' counting as much as an exact one, which is where most of the accuracy of a
#' modern linkage comes from, and is how `Splink` configures its comparisons.
#'
#' @param x Numeric vector of comparator scores.
#' @param breaks Numeric vector of cut points, strictly increasing. A score
#'   lands in level `k` when it is at or above `breaks[k]` and below
#'   `breaks[k + 1]`.
#' @param decreasing Set this to `TRUE` when `x` is a distance rather than a
#'   similarity, so that small values mean *more* agreement; the numbering is
#'   then reversed and `breaks` are still given increasing.
#' @return Integer vector the same length as `x`, with values in
#'   `0:length(breaks)` and `NA` where `x` is `NA`. `attr(, "n_levels")` is
#'   `length(breaks) + 1`.
#' @seealso [em_linkage()], which fits a model to these levels, and
#'   [numeric_compare()] and [date_compare()] for scoring non-text fields.
#' @family record linkage model functions
#' @examples
#' score <- c(1, 0.95, 0.88, 0.60, NA)
#'
#' # Agree / disagree
#' compare_levels(score, 1)
#'
#' # Exact, close, partial, different
#' compare_levels(score, c(0.8, 0.92, 1))
#'
#' # An edit distance counts the other way round
#' compare_levels(c(0, 1, 4), c(1, 3), decreasing = TRUE)
#' @export
compare_levels <- function(x, breaks, decreasing = FALSE) {
    if (!is.numeric(x)) stop("`x` must be a numeric vector.")
    if (!is.numeric(breaks) || length(breaks) < 1L || anyNA(breaks))
        stop("`breaks` must be a numeric vector of at least one cut point.")
    if (length(breaks) > 1L && any(diff(breaks) <= 0))
        stop("`breaks` must be strictly increasing.")
    if (!isTRUE(decreasing) && !isFALSE(decreasing))
        stop("`decreasing` must be TRUE or FALSE.")
    level <- findInterval(as.double(x), as.double(breaks))
    if (decreasing) level <- length(breaks) - level
    level[is.na(x)] <- NA_integer_
    structure(as.integer(level), n_levels = length(breaks) + 1L)
}

#' Agreement score for a numeric field
#'
#' Turns the difference between two numbers into an agreement score in
#' `[0, 1]`, so that an age, an income, a house number or a
#' [geo_distance()] can be compared with the same vocabulary as a name. The
#' five curves are those of the `Numeric` comparator in Python's
#' `recordlinkage` toolkit.
#'
#' Write `d` for `abs(abs(a - b) - origin)`. Then, clipping every score into
#' `[0, 1]`:
#'
#' * `"step"`: `1` when `d <= offset`, `0` beyond it -- a tolerance, not a
#'   gradient.
#' * `"linear"`: falls straight from `1` at `d = offset` to `0` at
#'   `d = offset + 2 * scale`.
#' * `"squared"`: `1 - (d - offset)^2 / (2 * scale^2)`, reaching `0` at
#'   `d = offset + sqrt(2) * scale`; flat near `offset`, so small differences
#'   are forgiven more than by `"linear"`.
#' * `"exp"`: `2^(-(d - offset) / scale)`, halving every `scale` units and
#'   never quite reaching `0`.
#' * `"gauss"`: `2^(-((d - offset) / scale)^2)`, which is `0.5` at
#'   `d = offset + scale` but falls away far faster beyond it.
#'
#' @param a,b Numeric vectors of the same length (or length 1, recycled).
#'   `Date` and `POSIXct` vectors are accepted and compared in days and
#'   seconds respectively.
#' @param method One of `"linear"` (the default), `"exp"`, `"gauss"`,
#'   `"squared"` or `"step"`.
#' @param offset Difference tolerated at full score; `0` by default, so only
#'   equal values score `1`.
#' @param scale How fast the score falls away past `offset`; see the formulas
#'   above. `1` by default.
#' @param origin The difference at which agreement is greatest, `0` by
#'   default. Set it when the two sources are known to differ by a constant,
#'   such as a rounding or a reporting lag.
#' @param missing_value Score for a pair where either value is `NA`; `NA` by
#'   default, which leaves the comparison missing for [em_linkage()] to skip.
#' @return Numeric vector of scores in `[0, 1]`, as long as the longer input.
#' @seealso [compare_levels()] to cut the score into agreement levels,
#'   [date_compare()] for dates with their characteristic errors, and
#'   [geo_distance()] for coordinates.
#' @family record linkage comparators
#' @examples
#' numeric_compare(c(40, 40, 40), c(40, 42, 55), method = "exp", scale = 5)
#'
#' # One year of tolerance, then a linear fall over ten more
#' numeric_compare(c(40, 41, 46), 40, method = "linear",
#'                 offset = 1, scale = 5)
#'
#' # A step function is a plain tolerance test
#' numeric_compare(c(40, 42), 40, method = "step", offset = 2)
#'
#' # Two sources whose incomes differ by a known rounding
#' numeric_compare(52300, 52000, method = "gauss", origin = 300, scale = 100)
#' @export
numeric_compare <- function(a, b, method = c("linear", "exp", "gauss",
                                             "squared", "step"),
                            offset = 0, scale = 1, origin = 0,
                            missing_value = NA_real_) {
    method <- match.arg(method)
    a <- .compare_numeric(a, "a")
    b <- .compare_numeric(b, "b")
    n <- max(length(a), length(b))
    if (length(a) != n) {
        if (length(a) != 1L) stop("`a` and `b` must have the same length.")
        a <- rep_len(a, n)
    }
    if (length(b) != n) {
        if (length(b) != 1L) stop("`a` and `b` must have the same length.")
        b <- rep_len(b, n)
    }
    offset <- .one_finite_number(offset, "offset")
    origin <- .one_finite_number(origin, "origin")
    if (offset < 0) stop("`offset` must not be negative.")
    if (method != "step") {
        scale <- .one_finite_number(scale, "scale")
        if (scale <= 0) stop("`scale` must be a positive number.")
    }
    if (!is.numeric(missing_value) && !is.logical(missing_value) ||
        length(missing_value) != 1L)
        stop("`missing_value` must be a single number or NA.")

    d <- abs(abs(a - b) - origin)
    past <- pmax(d - offset, 0)
    out <- switch(
        method,
        step    = as.double(d <= offset),
        linear  = 1 - pmin(past, 2 * scale) / (2 * scale),
        squared = 1 - pmin(past, sqrt(2) * scale)^2 / (2 * scale^2),
        exp     = 2^(-past / scale),
        gauss   = 2^(-(past / scale)^2)
    )
    # The linear and squared curves reach 0 by subtraction, which rounding can
    # take a hair past.
    out <- pmin(pmax(out, 0), 1)
    out[is.na(a) | is.na(b)] <- as.double(missing_value)
    out
}

.compare_numeric <- function(x, name) {
    if (inherits(x, "Date")) return(as.double(unclass(x)))
    if (inherits(x, "POSIXct")) return(as.double(unclass(x)))
    if (!is.numeric(x))
        stop(sprintf("`%s` must be numeric, a Date or a POSIXct.", name))
    as.double(x)
}

#' Agreement score for a date, allowing for the usual date errors
#'
#' Scores two dates the way [numeric_compare()] scores two numbers -- on the
#' difference in days -- but first recognises the two mistakes that dates in
#' administrative data actually suffer from, and which a difference in days
#' reads as a wild disagreement:
#'
#' * **the day and the month swapped**, as when a `DD/MM` form is read as
#'   `MM/DD`: `2001-03-07` against `2001-07-03`;
#' * **a typed year**, one digit of the year wrong or two of them
#'   transposed, with the day and month intact: `1980-06-18` against
#'   `1985-06-18` or `1908-06-18`.
#'
#' Each gets a score of its own, used when it beats the score the day
#' difference would have earned. Equal dates always score `1`.
#'
#' @param a,b `Date` vectors of the same length (or length 1, recycled).
#'   Anything [as.Date()] accepts is converted.
#' @param method,offset,scale,origin,missing_value Passed to
#'   [numeric_compare()], which scores the difference in days. The default
#'   `method = "exp"` with `scale = 30` halves the score for every month
#'   apart.
#' @param swap_score Score for a day/month swap, `0.5` by default. `0` turns
#'   the rule off.
#' @param year_typo_score Score for a mistyped year, `0.5` by default. `0`
#'   turns the rule off.
#' @return Numeric vector of scores in `[0, 1]`, as long as the longer input.
#' @seealso [numeric_compare()], [compare_levels()] and [date_parts()], which
#'   this uses to split the dates. [fas.Date()] parses fixed-format dates
#'   quickly.
#' @family record linkage comparators
#' @examples
#' a <- as.Date(c("2001-03-07", "2001-03-07", "2001-03-07", "2001-03-07"))
#' b <- as.Date(c("2001-03-07", "2001-03-09", "2001-07-03", "1974-11-22"))
#' data.frame(a, b, score = round(date_compare(a, b), 3))
#'
#' # Day and month exchanged, then a single mistyped year digit
#' date_compare(as.Date("2001-03-07"), as.Date("2001-07-03"))
#' date_compare(as.Date("1980-06-18"), as.Date("1985-06-18"))
#'
#' # Turn the rules off to score purely on the day difference
#' date_compare(as.Date("2001-03-07"), as.Date("2001-07-03"),
#'              swap_score = 0, year_typo_score = 0)
#' @export
date_compare <- function(a, b, method = c("exp", "linear", "gauss",
                                          "squared", "step"),
                         offset = 0, scale = 30, origin = 0,
                         swap_score = 0.5, year_typo_score = 0.5,
                         missing_value = NA_real_) {
    method <- match.arg(method)
    a <- .compare_date(a, "a")
    b <- .compare_date(b, "b")
    n <- max(length(a), length(b))
    if (length(a) != n) {
        if (length(a) != 1L) stop("`a` and `b` must have the same length.")
        a <- rep_len(a, n)
    }
    if (length(b) != n) {
        if (length(b) != 1L) stop("`a` and `b` must have the same length.")
        b <- rep_len(b, n)
    }
    swap_score <- .one_finite_number(swap_score, "swap_score")
    year_typo_score <- .one_finite_number(year_typo_score, "year_typo_score")
    if (swap_score < 0 || swap_score > 1 || year_typo_score < 0 ||
        year_typo_score > 1)
        stop("`swap_score` and `year_typo_score` must be between 0 and 1.")

    out <- numeric_compare(a, b, method = method, offset = offset,
                           scale = scale, origin = origin,
                           missing_value = missing_value)
    if (n == 0L) return(out)
    known <- !is.na(a) & !is.na(b)
    equal <- known & a == b
    if (swap_score > 0 || year_typo_score > 0) {
        pa <- date_parts(a)
        pb <- date_parts(b)
        if (swap_score > 0) {
            swap <- known & !equal & pa$year == pb$year &
                pa$day == pb$month & pa$month == pb$day
            swap[is.na(swap)] <- FALSE
            out[swap] <- pmax(out[swap], swap_score, na.rm = TRUE)
        }
        if (year_typo_score > 0) {
            same_day <- known & !equal & pa$day == pb$day &
                pa$month == pb$month & pa$year != pb$year
            same_day[is.na(same_day)] <- FALSE
            if (any(same_day)) {
                ya <- format_date_parts(pa$year[same_day], format = "y")
                yb <- format_date_parts(pb$year[same_day], format = "y")
                typo <- osa_distance(ya, yb) == 1
                typo[is.na(typo)] <- FALSE
                hit <- which(same_day)[typo]
                out[hit] <- pmax(out[hit], year_typo_score, na.rm = TRUE)
            }
        }
    }
    out[equal] <- 1
    out
}

.compare_date <- function(x, name) {
    if (inherits(x, "Date")) return(x)
    out <- try(as.Date(x), silent = TRUE)
    if (inherits(out, "try-error"))
        stop(sprintf("`%s` must be a Date, or something as.Date() accepts.",
                     name))
    out
}

#' Distinct agreement patterns and how often each occurs
#'
#' Collapses scored candidate pairs into the distinct *agreement patterns*
#' they show -- which comparisons agreed, at which level, and which were
#' missing -- with a count of the pairs having each. Millions of pairs
#' compared on half a dozen fields reduce to a few thousand patterns at most,
#' and every Fellegi-Sunter calculation then runs over the patterns instead
#' of the pairs.
#'
#' @param x A data frame or list of integer level vectors, one per
#'   comparison, all the same length -- typically the result of
#'   [compare_levels()] applied to each score column of [rLink()]. Logical
#'   vectors are read as two levels, `FALSE` and `TRUE`.
#' @return A data frame with one row per distinct pattern: one integer column
#'   per comparison, named as `x` is, and `n`, the number of pairs with that
#'   pattern. Patterns appear in the order they are first seen.
#'   `attr(, "index")` gives, for each pair, the row of this table it belongs
#'   to, and `attr(, "n_levels")` the number of levels per comparison.
#' @seealso [em_linkage()] to fit a model to these patterns, and
#'   [match_weights()] to score pairs with it.
#' @family record linkage model functions
#' @examples
#' levels <- data.frame(
#'     surname = c(1L, 1L, 0L, 1L, NA),
#'     given   = c(1L, 0L, 0L, 1L, 1L),
#'     dob     = c(1L, 1L, 0L, 1L, 1L)
#' )
#' agreement_patterns(levels)
#' attr(agreement_patterns(levels), "index")
#' @export
agreement_patterns <- function(x) {
    levels <- .as_level_list(x)
    res <- fast_agreement_patterns_impl(levels$codes)
    out <- as.data.frame(res$patterns)
    names(out) <- levels$names
    out$n <- as.integer(res$n)
    structure(out, index = res$index, n_levels = levels$n_levels,
              class = c("agreement_patterns", "data.frame"))
}

# Validates the per-comparison level vectors and works out how many levels
# each holds. Levels must be 0-based and contiguous for the model to have a
# probability for every one of them.
.as_level_list <- function(x, arg = "x") {
    if (is.data.frame(x)) x <- as.list(x)
    else if (!is.list(x)) x <- list(x)
    if (length(x) == 0L)
        stop(sprintf("`%s` must hold at least one comparison.", arg))
    nms <- names(x)
    if (is.null(nms)) nms <- rep("", length(x))
    blank <- !nzchar(nms)
    if (any(blank)) nms[blank] <- paste0("comparison_", which(blank))
    if (anyDuplicated(nms))
        stop(sprintf("Comparisons in `%s` must have distinct names.", arg))
    lens <- lengths(x)
    if (any(lens != lens[1L]))
        stop(sprintf("Every comparison in `%s` must have the same length.",
                     arg))
    codes <- vector("list", length(x))
    n_levels <- integer(length(x))
    for (k in seq_along(x)) {
        v <- x[[k]]
        if (is.logical(v)) v <- as.integer(v)
        if (is.factor(v)) v <- as.integer(v) - 1L
        if (!is.numeric(v))
            stop(sprintf("Comparison `%s` must be integer, logical or factor levels.",
                         nms[k]))
        if (any(v != floor(v), na.rm = TRUE) || any(v < 0, na.rm = TRUE))
            stop(sprintf("Levels of `%s` must be whole numbers from 0 up; see compare_levels().",
                         nms[k]))
        v <- as.integer(v)
        declared <- attr(x[[k]], "n_levels")
        top <- suppressWarnings(max(v, na.rm = TRUE))
        top <- if (is.finite(top)) top + 1L else 1L
        n_levels[k] <- if (is.null(declared)) top else
            max(as.integer(declared), top)
        codes[[k]] <- v
    }
    list(codes = unname(codes), names = nms, n_levels = n_levels)
}

#' Fit the Fellegi-Sunter model by EM
#'
#' Estimates, without any training data, how much each comparison is worth as
#' evidence that a candidate pair is the same person. The model behind every
#' probabilistic linkage system (Fellegi and Sunter 1969; `fastLink`,
#' `RecordLinkage`, `Splink`) says that candidate pairs come from two hidden
#' populations -- matches and non-matches -- and that within each, the
#' comparisons are independent. Fitting it by EM gives, for every comparison
#' and every agreement level:
#'
#' * `m`, the chance a true match reaches that level, which is about how
#'   reliably the field is recorded; and
#' * `u`, the chance a non-match reaches it, which is about how
#'   discriminating the field is.
#'
#' Their ratio is the evidence. `log2(m / u)` summed over a pair's
#' comparisons is its **match weight**: agreeing on a date of birth is worth
#' several bits, agreeing on sex is worth about one, and disagreeing on a
#' surname counts against the pair. [match_weights()] applies the fitted
#' model to pairs, and [select_greedy()] and [cluster_pairs()] turn the
#' weights into links.
#'
#' A comparison that is missing for a pair (`NA`) is left out of that pair's
#' weight rather than treated as a disagreement, so a pair missing a field is
#' judged on the fields it has.
#'
#' @section What to check:
#' EM on two classes is not guaranteed to find the split you have in mind. It
#' is worth checking that `lambda` is plausible, that every `m` is larger
#' than its `u` at the agreement levels, and that the model converged.
#' Comparisons that are not in fact independent -- a surname and a surname
#' Soundex, a suburb and a postcode -- make weights too confident, which
#' matters when the weight is read as a probability.
#'
#' @param x Agreement levels: a data frame or list of integer level vectors
#'   as [compare_levels()] returns, or a table from
#'   [agreement_patterns()].
#' @param lambda Starting guess at the proportion of candidate pairs that are
#'   matches, `0.1` by default. EM is sensitive to its start; a guess within
#'   an order of magnitude is enough.
#' @param m_init,u_init Optional starting probabilities, as a list of numeric
#'   vectors in the shape of the fitted `m` and `u`. The defaults start every
#'   comparison at `0.9` for the top level under a match and `0.1` under a
#'   non-match, spreading the rest evenly.
#' @param max_iter Maximum EM iterations (default `500`).
#' @param tol Convergence tolerance: EM stops when no parameter moves further
#'   than this (default `1e-8`).
#' @param min_prob Smallest probability any parameter is allowed to take
#'   (default `1e-10`), which keeps a level no pair ever reaches from giving
#'   an infinite weight.
#' @return An object of class `fs_model`: a list with `m` and `u` (lists of
#'   per-level probabilities, one element per comparison), `lambda`,
#'   `n_levels`, `comparisons`, `loglik`, `iterations`, `converged` and
#'   `patterns`, the distinct patterns with their counts, match weights and
#'   match probabilities. `print()` shows the weights per level.
#' @references Fellegi, I. P. and Sunter, A. B. (1969). A theory for record
#'   linkage. Journal of the American Statistical Association, 64(328),
#'   1183-1210.
#'
#'   Winkler, W. E. (1988). Using the EM algorithm for weight computation in
#'   the Fellegi-Sunter model of record linkage. Proceedings of the Section
#'   on Survey Research Methods, American Statistical Association, 667-671.
#' @seealso [compare_levels()], [agreement_patterns()], [match_weights()],
#'   [tf_adjust()], [select_greedy()] and [linkage_eval()].
#' @family record linkage model functions
#' @examples
#' set.seed(1)
#' # 200 matching pairs that usually agree, 2000 non-matching pairs that
#' # usually do not
#' agree <- function(n, p) rbinom(n, 1L, p)
#' levels <- data.frame(
#'     surname = c(agree(200, 0.95), agree(2000, 0.02)),
#'     given   = c(agree(200, 0.88), agree(2000, 0.03)),
#'     dob     = c(agree(200, 0.97), agree(2000, 0.001)),
#'     sex     = c(agree(200, 0.99), agree(2000, 0.5))
#' )
#'
#' fit <- em_linkage(levels)
#' fit
#'
#' # Sex agreeing is worth about a bit; a date of birth agreeing is worth ten
#' round(log2(sapply(fit$m, `[`, 2) / sapply(fit$u, `[`, 2)), 1)
#'
#' # The fitted prevalence recovers the 200 / 2200 the data were built with
#' round(fit$lambda, 3)
#' @export
em_linkage <- function(x, lambda = 0.1, m_init = NULL, u_init = NULL,
                       max_iter = 500L, tol = 1e-8, min_prob = 1e-10) {
    prepared <- .as_pattern_table(x)
    n_levels <- prepared$n_levels
    lambda <- .one_finite_number(lambda, "lambda")
    if (lambda <= 0 || lambda >= 1)
        stop("`lambda` must be between 0 and 1, exclusive.")
    min_prob <- .one_finite_number(min_prob, "min_prob")
    if (min_prob <= 0 || min_prob >= 0.5)
        stop("`min_prob` must be between 0 and 0.5, exclusive.")
    tol <- .one_finite_number(tol, "tol")
    if (tol <= 0) stop("`tol` must be a positive number.")
    if (!is.numeric(max_iter) || length(max_iter) != 1L || is.na(max_iter) ||
        max_iter < 1 || max_iter != floor(max_iter))
        stop("`max_iter` must be a single positive integer.")

    m_start <- .fs_start(m_init, n_levels, 0.9, "m_init", prepared$names)
    u_start <- .fs_start(u_init, n_levels, 0.1, "u_init", prepared$names)

    res <- fast_em_linkage_impl(
        prepared$patterns, as.double(prepared$n), as.integer(n_levels),
        m_start, u_start, lambda, as.integer(max_iter), tol, min_prob
    )
    if (!res$converged)
        warning(sprintf(
            "EM did not converge in %d iterations; the parameters are where it stopped.",
            as.integer(max_iter)
        ), call. = FALSE)

    patterns <- prepared$table
    patterns$weight <- res$weight
    patterns$probability <- res$probability
    structure(
        list(m = .fs_split(res$m, n_levels, prepared$names),
             u = .fs_split(res$u, n_levels, prepared$names),
             lambda = res$lambda,
             n_levels = .named(n_levels, prepared$names),
             comparisons = prepared$names, loglik = res$loglik,
             iterations = res$iterations, converged = res$converged,
             patterns = patterns),
        class = "fs_model"
    )
}

# Accepts either raw levels or an agreement_patterns table and returns the
# distinct-pattern matrix the C++ side wants, plus the bookkeeping around it.
.as_pattern_table <- function(x) {
    if (inherits(x, "agreement_patterns")) {
        table <- x
        nms <- setdiff(names(table), "n")
        n_levels <- attr(table, "n_levels")
        if (is.null(n_levels))
            n_levels <- vapply(table[nms], function(v) {
                top <- suppressWarnings(max(v, na.rm = TRUE))
                if (is.finite(top)) as.integer(top) + 1L else 1L
            }, integer(1))
        patterns <- as.matrix(as.data.frame(lapply(table[nms], as.integer)))
        return(list(patterns = patterns, n = table$n, n_levels = n_levels,
                    names = nms, table = as.data.frame(table[c(nms, "n")])))
    }
    table <- agreement_patterns(x)
    nms <- setdiff(names(table), "n")
    list(patterns = as.matrix(as.data.frame(lapply(table[nms], as.integer))),
         n = table$n, n_levels = attr(table, "n_levels"), names = nms,
         table = as.data.frame(table[c(nms, "n")]))
}

# Flattened starting probabilities: the top level gets `top`, the rest share
# what is left, so the model starts out believing the fields mean something.
.fs_start <- function(init, n_levels, top, arg, nms) {
    if (is.null(init)) {
        return(unlist(lapply(n_levels, function(levels) {
            if (levels == 1L) return(1)
            rest <- (1 - top) / (levels - 1L)
            c(rep(rest, levels - 1L), top)
        }), use.names = FALSE))
    }
    if (is.list(init)) {
        if (length(init) != length(n_levels))
            stop(sprintf("`%s` must have one element per comparison.", arg))
        if (!is.null(names(init)) && !identical(names(init), nms))
            init <- init[nms]
        lens <- lengths(init)
        if (any(lens != n_levels))
            stop(sprintf("`%s` must give one probability per level.", arg))
        init <- unlist(init, use.names = FALSE)
    }
    if (!is.numeric(init) || length(init) != sum(n_levels) || anyNA(init) ||
        any(init <= 0) || any(init >= 1))
        stop(sprintf("`%s` must hold one probability in (0, 1) per level.", arg))
    as.double(init)
}

.named <- function(x, nms) {
    names(x) <- nms
    x
}

.fs_split <- function(flat, n_levels, nms) {
    ends <- cumsum(n_levels)
    starts <- ends - n_levels + 1L
    .named(lapply(seq_along(n_levels), function(k) flat[starts[k]:ends[k]]),
           nms)
}

#' @export
print.fs_model <- function(x, ...) {
    cat(sprintf("Fellegi-Sunter model: %d comparison%s, %d pattern%s\n",
                length(x$comparisons),
                if (length(x$comparisons) == 1L) "" else "s",
                nrow(x$patterns), if (nrow(x$patterns) == 1L) "" else "s"))
    cat(sprintf("Match prevalence (lambda): %.4g\n", x$lambda))
    cat(sprintf("Log-likelihood: %.6g  (%s after %d iteration%s)\n", x$loglik,
                if (x$converged) "converged" else "stopped",
                x$iterations, if (x$iterations == 1L) "" else "s"))
    for (k in seq_along(x$comparisons)) {
        cat(sprintf("\n%s\n", x$comparisons[k]))
        m <- x$m[[k]]
        u <- x$u[[k]]
        print(data.frame(level = seq_along(m) - 1L, m = signif(m, 3),
                         u = signif(u, 3),
                         weight = round(log2(m / u), 2)),
              row.names = FALSE)
    }
    invisible(x)
}

#' Match weights and match probabilities for candidate pairs
#'
#' Applies a fitted [em_linkage()] model (or `m` and `u` probabilities from
#' anywhere else) to agreement levels, giving each pair:
#'
#' * its **match weight**, `log2(prod(m) / prod(u))` over the comparisons the
#'   pair has a value for -- the evidence in bits, positive for pairs the
#'   fields favour and negative for pairs they argue against; and
#' * its **match probability**, the weight combined with the model's
#'   prevalence `lambda` through Bayes' rule.
#'
#' The weight is what a threshold is usually set on, because it adds up
#' across fields in a way that can be read field by field; the probability is
#' what to report. A [tf_adjust()]ed weight can be turned back into a
#' probability with `lambda` and the same formula.
#'
#' @param object An `fs_model` from [em_linkage()], or a list with `m`, `u`
#'   and `lambda` in the same shape.
#' @param x Agreement levels for the pairs to score: a data frame or list in
#'   the same shape and order as the model was fitted on. The comparisons are
#'   matched by name when `x` has names.
#' @return A data frame with one row per pair: `weight` and `probability`.
#'   `NA` where a level falls outside the ones the model was fitted on.
#' @seealso [em_linkage()], [tf_adjust()] and [select_greedy()].
#' @family record linkage model functions
#' @examples
#' set.seed(2)
#' agree <- function(n, p) rbinom(n, 1L, p)
#' levels <- data.frame(
#'     surname = c(agree(150, 0.95), agree(1500, 0.02)),
#'     dob     = c(agree(150, 0.97), agree(1500, 0.001))
#' )
#' fit <- em_linkage(levels)
#' scored <- match_weights(fit, levels)
#' head(scored)
#'
#' # Agreeing on both is worth many bits; disagreeing on both, very few
#' unique(round(scored$weight, 1))
#' @export
match_weights <- function(object, x) {
    if (!is.list(object) || is.null(object$m) || is.null(object$u) ||
        is.null(object$lambda))
        stop("`object` must be an fs_model, or a list with `m`, `u` and `lambda`.")
    n_levels <- lengths(object$m)
    if (!identical(unname(n_levels), unname(lengths(object$u))))
        stop("`m` and `u` must have the same shape.")
    nms <- names(object$m)
    levels <- .as_level_list(x)
    if (length(levels$codes) != length(n_levels))
        stop("`x` must have as many comparisons as the model.")
    if (!is.null(nms) && all(nzchar(nms)) && !identical(levels$names, nms)) {
        if (!all(nms %in% levels$names))
            stop("`x` must hold the comparisons the model was fitted on: ",
                 paste(nms, collapse = ", "), ".")
        levels$codes <- levels$codes[match(nms, levels$names)]
        levels$names <- nms
    }
    patterns <- agreement_patterns(.named(levels$codes, levels$names))
    index <- attr(patterns, "index")
    matrix <- as.matrix(as.data.frame(
        lapply(patterns[setdiff(names(patterns), "n")], as.integer)
    ))
    res <- fast_match_weight_impl(
        matrix, unlist(object$m, use.names = FALSE),
        unlist(object$u, use.names = FALSE), as.integer(n_levels),
        object$lambda
    )
    if (anyNA(res$weight))
        warning("Some pairs have an agreement level the model has no probability for.",
                call. = FALSE)
    data.frame(weight = res$weight[index], probability = res$probability[index])
}

#' Relative frequency of each value
#'
#' The share of records holding each element's own value: `"SMITH"` in an
#' Australian surname column comes back as about 0.01, `"ZELINSKY"` as a few
#' millionths. Agreeing on a rare value is far stronger evidence than
#' agreeing on a common one, and [tf_adjust()] uses these frequencies to say
#' how much stronger.
#'
#' @param x Vector of values, of any atomic type (factors are compared as
#'   their labels).
#' @param pool Optional further values to count towards the distribution
#'   without being scored -- the other file's column, so that both sides of a
#'   linkage are measured against one distribution.
#' @param na_strings Values in `x` and `pool` to read as missing, `""` by
#'   default. Missing values are counted in neither the numerator nor the
#'   denominator.
#' @return Numeric vector the same length as `x`: each element's count
#'   divided by the number of non-missing values in `x` and `pool` together.
#'   `NA` where `x` is missing. `attr(, "n")` is that denominator.
#' @seealso [tf_adjust()], [token_counts()] for document frequencies of words
#'   rather than whole values, and [em_linkage()].
#' @family record linkage model functions
#' @examples
#' surname <- c("SMITH", "SMITH", "SMITH", "NGUYEN", "ZELINSKY", NA)
#' value_frequency(surname)
#'
#' # One distribution across two files
#' other <- c("SMITH", "SMITH", "JONES")
#' value_frequency(surname, pool = other)
#' @export
value_frequency <- function(x, pool = NULL, na_strings = "") {
    if (!is.atomic(x) || !is.null(dim(x)))
        stop("`x` must be an atomic vector.")
    if (is.factor(x)) x <- as.character(x)
    if (!is.null(pool)) {
        if (is.factor(pool)) pool <- as.character(pool)
        if (!is.atomic(pool) || !is.null(dim(pool)))
            stop("`pool` must be an atomic vector.")
        if (!.rlink_same_type(x, pool))
            stop("`x` and `pool` must be of the same type.")
    } else {
        pool <- x[0L]
    }
    if (is.character(x) && length(na_strings)) {
        if (!is.character(na_strings)) stop("`na_strings` must be character.")
        x[x %in% na_strings] <- NA_character_
        pool[pool %in% na_strings] <- NA_character_
    }
    all <- c(x, pool)
    u <- unique(all[!is.na(all)])
    codes <- if (is.character(all) && is.character(u))
        data.table::chmatch(all, u) else match(all, u)
    res <- fast_value_frequency_impl(
        codes[seq_along(x)], codes[length(x) + seq_along(pool)], length(u)
    )
    structure(res$x, n = res$total)
}

#' Adjust a match weight for how common the agreed value is
#'
#' A fitted `u` probability says how often a non-matching pair agrees on a
#' field *on average*. But two records agreeing on `"SMITH"` is much weaker
#' evidence than two agreeing on `"ZELINSKY"`, and averaging over the
#' distribution throws that away. This replaces the pooled `u` with the one
#' implied by the value actually agreed on, `p1 * p2`, so the weight becomes
#'
#' ```
#' weight + weight_power * log2(u / (frequency_1 * frequency_2))
#' ```
#'
#' A value exactly as likely as the pooled `u` implies is left alone; rarer
#' values gain weight and commoner ones lose it. This is the frequency-based
#' weight of Fellegi and Sunter (1969), which `Splink` exposes as its term
#' frequency adjustments, and it is usually the largest single accuracy gain
#' available once the model is fitted.
#'
#' Apply it only to pairs that *agree* -- the frequency of a value neither
#' record has says nothing -- and only at an exact-agreement level, where the
#' two records really do share one value. `weight_power` below 1 damps the
#' adjustment, which is what to reach for when the level is a fuzzy match and
#' so the two frequencies are not quite measuring the same thing.
#'
#' @param weight Numeric vector of match weights, from [match_weights()].
#' @param u The `u` probability of the agreement level being adjusted, from
#'   [em_linkage()] (`fit$u[["surname"]][2]` for the top level of two).
#' @param frequency_1 Relative frequency of the agreed value, from
#'   [value_frequency()].
#' @param frequency_2 The same frequency measured in the second file;
#'   defaults to `frequency_1`, which is right when one pooled distribution
#'   was used for both sides.
#' @param weight_power How much of the adjustment to apply, `1` by default
#'   (all of it) and `0` for none.
#' @return Numeric vector of adjusted weights, the same length as `weight`.
#'   `NA` where any input is `NA`.
#' @references Fellegi, I. P. and Sunter, A. B. (1969). A theory for record
#'   linkage. Journal of the American Statistical Association, 64(328),
#'   1183-1210, section 3.3.
#'
#'   Winkler, W. E. (1989). Frequency-based matching in the Fellegi-Sunter
#'   model of record linkage. Proceedings of the Section on Survey Research
#'   Methods, American Statistical Association, 778-783.
#' @seealso [value_frequency()], [em_linkage()] and [match_weights()].
#' @family record linkage model functions
#' @examples
#' # A pair agreeing on a surname, worth 6 bits on the pooled u of 0.001
#' weight <- c(6, 6)
#' frequency <- c(0.01, 0.000002)      # SMITH, then a rare name
#' tf_adjust(weight, u = 0.001, frequency_1 = frequency)
#'
#' # Half the adjustment, for a level that is a fuzzy rather than exact match
#' tf_adjust(weight, u = 0.001, frequency_1 = frequency, weight_power = 0.5)
#' @export
tf_adjust <- function(weight, u, frequency_1, frequency_2 = frequency_1,
                      weight_power = 1) {
    if (!is.numeric(weight)) stop("`weight` must be a numeric vector.")
    u <- .one_finite_number(u, "u")
    if (u <= 0 || u >= 1) stop("`u` must be between 0 and 1, exclusive.")
    weight_power <- .one_finite_number(weight_power, "weight_power")
    if (weight_power < 0) stop("`weight_power` must not be negative.")
    for (nm in c("frequency_1", "frequency_2")) {
        value <- if (nm == "frequency_1") frequency_1 else frequency_2
        if (!is.numeric(value))
            stop(sprintf("`%s` must be a numeric vector.", nm))
        if (any(value <= 0 | value > 1, na.rm = TRUE))
            stop(sprintf("`%s` must hold relative frequencies in (0, 1].", nm))
    }
    weight + weight_power * log2(u / (frequency_1 * frequency_2))
}

#' Resolve candidate pairs into links, greedily
#'
#' Chooses which of the scored pairs to keep as links, taking the
#' highest-weighted pair first and keeping it whenever neither of its two
#' records has already been linked as often as allowed. With the defaults
#' `n = 1` and `m = 1` the result is a one-to-one linkage: each record of
#' each file ends up in at most one link, which is what to want whenever both
#' files are known to hold each person once.
#'
#' Blocking and scoring on their own will happily link one record to several,
#' and three records in a chain to each other; resolving the pairs is the step
#' that stops that. Greedy selection is not guaranteed to maximise the total
#' weight -- that is an assignment problem -- but it is near-linear in the
#' number of pairs after the sort, and on linkage weights the difference is
#' rarely material.
#'
#' @param i Integer vector of indices into the first file, or a data frame of
#'   pairs such as [rLink()] or [block_pairs()] returns. From a data frame the
#'   `index_1`/`index_2` or `x_index`/`y_index` columns are used when it has
#'   them, and its first two columns otherwise.
#' @param j Integer vector of indices into the second file; omit when `i` is
#'   a data frame.
#' @param weight Numeric vector of match weights or scores, one per pair,
#'   higher meaning a better match. Pairs of equal weight are taken in the
#'   order given.
#' @param n How many times a record of the first file may be linked, `1` by
#'   default.
#' @param m How many times a record of the second file may be linked, `1` by
#'   default.
#' @return Logical vector the same length as the input pairs: `TRUE` for the
#'   pairs kept. `NA` for a pair with a missing index or weight, which is
#'   never kept.
#' @seealso [cluster_pairs()], which instead groups records joined by chains
#'   of matches, and [match_weights()] for the weights to feed in.
#' @family record linkage pair functions
#' @examples
#' # Record 1 of the first file matches records 1 and 2 of the second;
#' # the better pair wins and the other is dropped
#' pairs <- data.frame(index_1 = c(1L, 1L, 2L), index_2 = c(1L, 2L, 2L))
#' weight <- c(12, 9, 8)
#' select_greedy(pairs, weight = weight)
#'
#' # Allow each record of the first file two links
#' select_greedy(pairs, weight = weight, n = 2)
#'
#' # The usual last step of a linkage: score, threshold, then resolve
#' keep <- weight > 5
#' pairs[keep, ][select_greedy(pairs[keep, ], weight = weight[keep]), ]
#' @export
select_greedy <- function(i, j = NULL, weight, n = 1L, m = 1L) {
    if (is.data.frame(i)) {
        if (!is.null(j)) stop("Give `j` only when `i` is a vector.")
        if (ncol(i) < 2L) stop("`i` must have at least two columns.")
        at <- .pair_columns(names(i))
        j <- i[[at[2L]]]
        i <- i[[at[1L]]]
    }
    if (is.null(j)) stop("`j` is missing.")
    if (!is.numeric(i) || !is.numeric(j) || length(i) != length(j))
        stop("`i` and `j` must be numeric vectors of the same length.")
    if (missing(weight)) stop("`weight` is missing.")
    if (!is.numeric(weight) || length(weight) != length(i))
        stop("`weight` must be a numeric vector, one per pair.")
    for (nm in c("n", "m")) {
        value <- if (nm == "n") n else m
        if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
            value < 1 || value != floor(value))
            stop(sprintf("`%s` must be a single positive integer.", nm))
    }
    fast_select_greedy_impl(as.integer(i), as.integer(j), as.double(weight),
                            as.integer(n), as.integer(m))
}

#' Score a linkage against a known truth
#'
#' Precision, recall and F-measure for a set of links, given the pairs that
#' really match. It is the measurement that tells you whether a change to the
#' blocking, the comparators or the threshold helped, and it needs a sample
#' of pairs whose truth is known -- clerically reviewed, or carried by a
#' shared identifier that the linkage itself did not use.
#'
#' * **precision** is the share of links that are true matches: one minus the
#'   false-link rate.
#' * **recall** (sensitivity) is the share of true matches that were linked.
#' * **f_measure** is their harmonic mean.
#'
#' Precision and recall trade off against each other as the threshold moves,
#' so report both; the F-measure is for choosing between runs, not for
#' describing one.
#'
#' @param predicted The links: a data frame of pairs, or a list or matrix of
#'   two index vectors. From a data frame the `index_1`/`index_2` or
#'   `x_index`/`y_index` columns are used when it has them -- so the result of
#'   [rLink()], [block_pairs()] or [minhash_pairs()] can be passed straight in
#'   -- and its first two columns otherwise.
#' @param truth The pairs that really match, in the same form.
#' @param n_pairs Optional total number of candidate pairs considered, which
#'   adds the true negatives and `specificity` and `accuracy` with them. For
#'   a linkage with no blocking it is `nrow(data_1) * nrow(data_2)`.
#' @param ordered Whether the two indices of a pair mean different files
#'   (`TRUE`, the default, for linking two files). `FALSE` sorts each pair, so
#'   that `(2, 7)` and `(7, 2)` are the same pair -- what deduplication
#'   needs.
#' @return A one-row data frame: `n_predicted`, `n_truth`, `tp`, `fp`, `fn`,
#'   `precision`, `recall` and `f_measure`, plus `tn`, `specificity` and
#'   `accuracy` when `n_pairs` is given. Duplicate pairs are counted once.
#' @seealso [blocking_eval()] for whether the candidate pairs were the right
#'   ones in the first place, and [em_linkage()] for the model whose threshold
#'   this measures.
#' @family record linkage model functions
#' @examples
#' truth <- data.frame(i = c(1L, 2L, 3L, 4L), j = c(1L, 2L, 3L, 4L))
#' links <- data.frame(i = c(1L, 2L, 3L, 5L), j = c(1L, 2L, 9L, 5L))
#' linkage_eval(links, truth)
#'
#' # With the comparison space, specificity and accuracy as well
#' linkage_eval(links, truth, n_pairs = 100)
#' @export
linkage_eval <- function(predicted, truth, n_pairs = NULL, ordered = TRUE) {
    p <- .as_pair_set(predicted, "predicted", ordered)
    known <- .as_pair_set(truth, "truth", ordered)
    overlap <- fast_pair_overlap_impl(p$i, p$j, known$i, known$j)
    n_predicted <- overlap$n
    n_truth <- overlap$n_other
    tp <- overlap$n_common
    fp <- n_predicted - tp
    fn <- n_truth - tp
    out <- data.frame(
        n_predicted = n_predicted, n_truth = n_truth, tp = tp, fp = fp,
        fn = fn,
        precision = if (n_predicted > 0) tp / n_predicted else NA_real_,
        recall = if (n_truth > 0) tp / n_truth else NA_real_
    )
    out$f_measure <- if (is.na(out$precision) || is.na(out$recall) ||
                         out$precision + out$recall == 0) NA_real_ else
        2 * out$precision * out$recall / (out$precision + out$recall)
    if (!is.null(n_pairs)) {
        n_pairs <- .one_finite_number(n_pairs, "n_pairs")
        if (n_pairs < n_predicted || n_pairs < n_truth)
            stop("`n_pairs` is smaller than the pairs given.")
        out$tn <- n_pairs - tp - fp - fn
        out$specificity <- if (out$tn + fp > 0) out$tn / (out$tn + fp) else
            NA_real_
        out$accuracy <- (tp + out$tn) / n_pairs
    }
    out
}

#' Score a blocking scheme against a known truth
#'
#' How well the candidate pairs did their job: did blocking throw away enough
#' pairs to be worth doing, and did it keep the true matches? The two pull
#' against each other, and the standard measures of the trade-off are
#'
#' * **reduction_ratio**: the share of all possible pairs that blocking
#'   removed. A linkage of two files of 200,000 records has 40 billion
#'   possible pairs; a usable blocking scheme leaves a few million, for a
#'   reduction ratio above 0.9999.
#' * **pairs_completeness**: the share of true matches that survived into the
#'   candidates. Every one lost here is a match no comparator can ever find,
#'   which is why extra blocking passes are cheaper than they look.
#' * **pairs_quality**: the share of candidates that are true matches --
#'   how much of the comparison work was not wasted.
#'
#' @param candidates The candidate pairs, as [block_pairs()],
#'   [sorted_neighbourhood()], [minhash_pairs()] or [rLink()] return them, or a
#'   list or matrix of two index vectors. Column names are read as
#'   [linkage_eval()] reads them.
#' @param truth The pairs that really match, in the same form.
#' @param n_1 Number of records in the first file.
#' @param n_2 Number of records in the second file, or `NULL` (the default)
#'   for deduplication within one file, where the comparison space is
#'   `n_1 * (n_1 - 1) / 2` and pairs are unordered.
#' @return A one-row data frame: `n_candidates`, `n_pairs` (the whole
#'   comparison space), `n_truth`, `n_found`, `reduction_ratio`,
#'   `pairs_completeness`, `pairs_quality` and `f_measure`, the harmonic mean
#'   of the last two. Duplicate pairs are counted once.
#' @references Christen, P. (2012). Data Matching. Springer, chapter 7.
#' @seealso [linkage_eval()] for the links themselves, and [block_pairs()]
#'   and [sorted_neighbourhood()] for the schemes being measured.
#' @family record linkage model functions
#' @examples
#' # 5 x 5 records, so 25 possible pairs; blocking leaves 6 of them
#' candidates <- data.frame(x_index = c(1L, 1L, 2L, 3L, 4L, 5L),
#'                          y_index = c(1L, 3L, 2L, 3L, 1L, 5L))
#' truth <- data.frame(x_index = c(1L, 2L, 3L, 4L), y_index = c(1L, 2L, 3L, 4L))
#' blocking_eval(candidates, truth, n_1 = 5, n_2 = 5)
#' @export
blocking_eval <- function(candidates, truth, n_1, n_2 = NULL) {
    ordered <- !is.null(n_2)
    c_pairs <- .as_pair_set(candidates, "candidates", ordered)
    t_pairs <- .as_pair_set(truth, "truth", ordered)
    n_1 <- .one_finite_number(n_1, "n_1")
    if (n_1 < 0 || n_1 != floor(n_1))
        stop("`n_1` must be a non-negative whole number.")
    if (ordered) {
        n_2 <- .one_finite_number(n_2, "n_2")
        if (n_2 < 0 || n_2 != floor(n_2))
            stop("`n_2` must be a non-negative whole number.")
        space <- n_1 * n_2
    } else {
        space <- n_1 * (n_1 - 1) / 2
    }
    overlap <- fast_pair_overlap_impl(c_pairs$i, c_pairs$j, t_pairs$i,
                                      t_pairs$j)
    n_candidates <- overlap$n
    n_truth <- overlap$n_other
    found <- overlap$n_common
    pc <- if (n_truth > 0) found / n_truth else NA_real_
    pq <- if (n_candidates > 0) found / n_candidates else NA_real_
    data.frame(
        n_candidates = n_candidates, n_pairs = space, n_truth = n_truth,
        n_found = found,
        reduction_ratio = if (space > 0) 1 - n_candidates / space else NA_real_,
        pairs_completeness = pc, pairs_quality = pq,
        f_measure = if (is.na(pc) || is.na(pq) || pc + pq == 0) NA_real_ else
            2 * pc * pq / (pc + pq)
    )
}

# Which two columns hold the pair indices. The names this package's own pair
# functions use win over position, because rLink() puts `pair_id` first and
# taking the first two columns of its result would silently read that.
.pair_columns <- function(nms) {
    for (pair in list(c("index_1", "index_2"), c("x_index", "y_index")))
        if (!is.null(nms) && all(pair %in% nms)) return(pair)
    c(1L, 2L)
}

# Two index vectors from a data frame, matrix or list of two, sorted within
# each pair when the pair is unordered.
.as_pair_set <- function(x, arg, ordered) {
    if (is.data.frame(x) || is.matrix(x)) {
        if (ncol(x) < 2L)
            stop(sprintf("`%s` must have at least two columns.", arg))
        at <- .pair_columns(colnames(x))
        if (is.matrix(x)) {
            i <- x[, at[1L]]
            j <- x[, at[2L]]
        } else {
            i <- x[[at[1L]]]
            j <- x[[at[2L]]]
        }
    } else if (is.list(x)) {
        if (length(x) < 2L)
            stop(sprintf("`%s` must hold two index vectors.", arg))
        i <- x[[1L]]
        j <- x[[2L]]
    } else {
        stop(sprintf("`%s` must be a data frame, matrix or list of two index vectors.",
                     arg))
    }
    if (!is.numeric(i) || !is.numeric(j) || length(i) != length(j))
        stop(sprintf("`%s` must hold two numeric index vectors of the same length.",
                     arg))
    if (any(i < 1, na.rm = TRUE) || any(j < 1, na.rm = TRUE))
        stop(sprintf("Indices in `%s` must be positive.", arg))
    i <- as.integer(i)
    j <- as.integer(j)
    if (!ordered) {
        swap <- !is.na(i) & !is.na(j) & i > j
        if (any(swap)) {
            tmp <- i[swap]
            i[swap] <- j[swap]
            j[swap] <- tmp
        }
    }
    list(i = i, j = j)
}
