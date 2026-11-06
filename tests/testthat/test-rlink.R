# Every pair the slow and obvious way: all i x j, the first pass whose key
# agrees wins, a missing key agrees with nothing, ordered as block_pairs()
# orders its result.
brute_link <- function(keys_1, keys_2, dedupe = FALSE) {
    n1 <- length(keys_1[[1L]])
    n2 <- length(keys_2[[1L]])
    x <- y <- pass <- integer()
    for (i in seq_len(n1)) {
        for (j in seq_len(n2)) {
            if (dedupe && i >= j) next
            for (k in seq_along(keys_1)) {
                a <- keys_1[[k]][i]
                b <- keys_2[[k]][j]
                if (!is.na(a) && !is.na(b) && a == b) {
                    x <- c(x, i); y <- c(y, j); pass <- c(pass, k)
                    break
                }
            }
        }
    }
    o <- order(x, y)
    data.frame(x_index = x[o], y_index = y[o], pass = pass[o])
}

enrol <- function() data.frame(
    id      = 1:4,
    surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
    given   = c("JOHN", "MARY", "ROB", "JON"),
    dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01",
                        "1980-01-02")),
    stringsAsFactors = FALSE
)

claims <- function() data.frame(
    ref     = c("A", "B", "C"),
    surname = c("SMITH", "JONSE", "BROWNE"),
    given   = c("JON", "MARY", "ROBERT"),
    dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01")),
    stringsAsFactors = FALSE
)

test_that("rLink() reproduces a brute-force link", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(list(soundex(surname), dob),
                                    fsubstr(given, 1, 2)))
    want <- brute_link(
        list(paste(soundex(a$surname), a$dob), substr(a$given, 1, 2)),
        list(paste(soundex(b$surname), b$dob), substr(b$given, 1, 2))
    )
    expect_identical(out$index_1, want$x_index)
    expect_identical(out$index_2, want$y_index)
    expect_identical(out$pass, want$pass)
    expect_true(data.table::is.data.table(out))
    expect_identical(out$pair_id, seq_len(nrow(out)))
})

test_that("rLink() deduplicates one file without self-pairs", {
    a <- enrol()
    out <- rLink(a, block_1 = list(soundex(surname)))
    want <- brute_link(list(soundex(a$surname)), list(soundex(a$surname)),
                       dedupe = TRUE)
    expect_identical(out$index_1, want$x_index)
    expect_identical(out$index_2, want$y_index)
    expect_true(all(out$index_1 < out$index_2))
})

test_that("blocking terms may be any expression", {
    a <- enrol()
    b <- claims()
    my_threads <- 1
    expect_identical(
        rLink(a, b, block_1 = list(soundex(surname, nthreads = my_threads)))$index_1,
        rLink(a, b, block_1 = list(soundex(surname)))$index_1)
    v <- "surname"
    expect_identical(
        rLink(a, b, block_1 = list(soundex(!!rlang::sym(v))))$index_1,
        rLink(a, b, block_1 = list(soundex(surname)))$index_1)
    # A length-1 term is recycled, which pairs everything with everything.
    expect_identical(nrow(rLink(a, b, block_1 = list(1))), 12L)
})

test_that("the pass grammar reads list() as passes and c() as terms", {
    a <- enrol()
    b <- claims()
    # One pass of two terms, written three ways.
    n <- nrow(rLink(a, b, block_1 = list(list(surname, dob))))
    expect_identical(nrow(rLink(a, b, block_1 = c("surname", "dob"))), n)
    expect_identical(nrow(rLink(a, b, block_1 = list(c(surname, dob)))), n)
    # Two passes, each one term.
    expect_identical(max(rLink(a, b, block_1 = list(surname, dob))$pass), 2L)
    expect_error(rLink(a, b, block_1 = list(list())), "no terms")
})

test_that("a missing key term pairs with nothing unless match_missing", {
    a <- data.frame(k = c("X", NA, ""), stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(a, a, block_1 = list(k))), 1L)
    # "" is in na_strings, so both missing rows share one key.
    expect_identical(nrow(rLink(a, a, block_1 = list(k),
                                match_missing = TRUE)), 5L)
    expect_identical(nrow(rLink(a, a, block_1 = list(k),
                                na_strings = character())), 2L)

    # A missing value in any one term makes the whole pass key missing.
    m <- data.frame(s = c("SMITH", "JONES"), t = c(NA, NA),
                    stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(m, m, block_1 = list(list(s, t)))), 0L)
    # With match_missing the terms stay distinct, so SMITH does not reach
    # JONES through their shared blank.
    expect_identical(nrow(rLink(m, m, block_1 = list(list(s, t)),
                                match_missing = TRUE)), 2L)
})

test_that("a value that looks like a sentinel survives untouched", {
    # The function this replaces wrote paste0("1", colname, seq_len(n)) into
    # the data and stripped it afterwards with grepl("^1colname"), which
    # destroyed any real value of that shape.
    a <- data.frame(x = c("1x1", "1x2"), stringsAsFactors = FALSE)
    out <- rLink(a, a, block_1 = list(x), match_missing = TRUE)
    expect_identical(sort(unique(c(out$x_1, out$x_2))), c("1x1", "1x2"))
})

test_that("key terms are joined without colliding", {
    # Joined with "~", c("A~B", "C") and c("A", "B~C") shared a key.
    tl <- data.frame(p = c("A~B", "A"), q = c("C", "B~C"),
                     stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(tl, tl, block_1 = list(list(p, q)))), 2L)
    # The separator actually used is rejected in the data rather than hoped
    # against.
    sep <- data.frame(p = c("A\u001fB", "A"), q = c("C", "B"),
                      stringsAsFactors = FALSE)
    expect_error(rLink(sep, sep, block_1 = list(list(p, q))),
                 "control character")
})

test_that("block_2 maps columns named differently", {
    a <- enrol()
    b <- claims()
    cl2 <- data.frame(ref = b$ref, family_name = b$surname,
                      firstname = b$given, dob = b$dob,
                      stringsAsFactors = FALSE)
    expect_identical(
        rLink(a, cl2, block_1 = list(soundex(surname)),
              block_2 = list(soundex(family_name)))$index_2,
        rLink(a, b, block_1 = list(soundex(surname)))$index_2)
    # A NULL pass reuses block's own expression.
    expect_identical(
        nrow(rLink(a, b, block_1 = list(soundex(surname)), block_2 = list(NULL))),
        4L)
    expect_error(
        rLink(a, cl2, block_1 = list(soundex(surname)),
              block_2 = list(soundex(family_name), dob)),
        "matched up")
    expect_error(
        rLink(a, cl2, block_1 = list(list(soundex(surname), dob)),
              block_2 = list(soundex(family_name))),
        "can never agree")
})

test_that("block_2 is an error when deduplicating", {
    expect_error(rLink(enrol(), block_1 = list(soundex(surname)),
                       block_2 = list(soundex(surname))),
                 "there is none")
})

test_that("block_key = FALSE leaves out the block_key column", {
    a <- enrol()
    b <- claims()
    with_key <- rLink(a, b, block_1 = list(soundex(surname)))
    out <- rLink(a, b, block_1 = list(soundex(surname)), block_key = FALSE)
    expect_false("block_key" %in% names(out))
    expect_identical(names(out), setdiff(names(with_key), "block_key"))
    expect_identical(out$index_2, with_key$index_2)
    expect_error(rLink(a, b, block_1 = list(surname), block_key = NA),
                 "block_key")
})

test_that("comparators are resolved and named", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given),
                                sn_jac = jaccard_index(surname, q = 3),
                                "levenshtein_similarity"(surname)))
    expect_true(all(c("given_jaro_winkler", "sn_jac",
                      "surname_levenshtein_similarity") %in% names(out)))
    expect_equal(out$given_jaro_winkler,
                 jaro_winkler(toupper(a$given)[out$index_1],
                              toupper(b$given)[out$index_2]))
    # Extra arguments reach the comparator.
    expect_false(isTRUE(all.equal(
        rLink(a, b, block_1 = list(soundex(surname)),
              compare = list(s = jaro_winkler(surname, p = 0)))$s,
        rLink(a, b, block_1 = list(soundex(surname)),
              compare = list(s = jaro_winkler(surname, p = 0.25)))$s)))
})

test_that("comparators can compare differently named columns", {
    a <- enrol()
    b <- claims()
    cl2 <- data.frame(family_name = b$surname, stringsAsFactors = FALSE)
    out <- rLink(a, cl2, block_1 = list(soundex(surname)),
                 block_2 = list(soundex(family_name)),
                 compare = list(sn = jaro_winkler(surname | family_name)))
    expect_equal(out$sn, jaro_winkler(toupper(a$surname)[out$index_1],
                                      toupper(cl2$family_name)[out$index_2]))
})

test_that("a custom comparator works and only gets nthreads if declared", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(eq = (function(x, y) x == y)(surname)))
    expect_type(out$eq, "logical")

    seen <- NA
    spy <- function(x, y, nthreads) { seen <<- nthreads; nchar(x) }
    rLink(a, b, block_1 = list(soundex(surname)),
          compare = list(s = spy(surname)), nthreads = 3)
    expect_identical(seen, 3)

    # A plain function of two arguments must not be handed nthreads.
    plain <- function(x, y) nchar(x)
    expect_silent(rLink(a, b, block_1 = list(soundex(surname)),
                        compare = list(p = plain(surname)), nthreads = 3))
})

test_that("scores are normalised so that 1 is an exact match", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(lev = levenshtein(surname),
                                jw  = jaro_winkler(surname),
                                fz  = fuzz_ratio(surname),
                                ed  = editex(surname)))
    exact <- out$surname_1 == out$surname_2
    for (col in c("lev", "jw", "fz", "ed")) {
        expect_true(all(out[[col]][exact] == 1), label = col)
        expect_true(all(out[[col]][!exact] < 1), label = col)
        expect_true(all(out[[col]] >= 0 & out[[col]] <= 1), label = col)
    }
    expect_identical(attr(out, "compare")$scale,
                     c("edit distance", "similarity", "similarity/100",
                       "edit distance"))
})

test_that("an edit distance is normalised pairwise, not by the whole vector", {
    # 1 - stringdist(m, n, "dl") / max(nchar(c(m, n))) divided every score by
    # the longest string anywhere in either vector. Here that is 5, so the
    # second pair would score 1 - 1/5 = 0.8 instead of its true 0.
    a <- data.frame(k = c("1", "2"), s = c("ABCDE", "A"),
                    stringsAsFactors = FALSE)
    b <- data.frame(k = c("1", "2"), s = c("ABCDE", "B"),
                    stringsAsFactors = FALSE)
    out <- rLink(a, b, block_1 = list(k),
                 compare = list(d = damerau_levenshtein(s)))
    expect_identical(out$d, c(1, 0))
})

test_that("a comparator that warns is still calibrated", {
    noisy <- function(a, b) {
        warning("noisy comparator")
        levenshtein(a, b)
    }
    a <- data.frame(k = c("1", "2"), s = c("ABCDE", "A"),
                    stringsAsFactors = FALSE)
    b <- data.frame(k = c("1", "2"), s = c("ABCDE", "B"),
                    stringsAsFactors = FALSE)
    # The real call's warning still reaches the caller; the probe's does not.
    expect_warning(
        out <- rLink(a, b, block_1 = list(k), compare = list(d = noisy(s))),
        "noisy comparator")
    expect_identical(out$d, c(1, 0))
    expect_identical(attr(out, "compare")$scale, "edit distance")
})

test_that("an edit distance is normalised in the unit it counts", {
    # One accented letter is two bytes: two edits out of seven bytes, or one
    # out of six characters, never two out of six.
    a <- data.frame(k = 1, s = enc2utf8("MÜLLER"))
    b <- data.frame(k = 1, s = "MULLER")
    out <- rLink(a, b, block_1 = list(k),
                 compare = list(byte = levenshtein(s),
                                chr = levenshtein(s, use_bytes = FALSE)))
    expect_equal(out$byte, 1 - 2 / 7)
    expect_equal(out$chr, 1 - 1 / 6)
})

test_that("normalise = FALSE leaves the comparator's own scale", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(lev = levenshtein(surname)),
                 normalise = FALSE)
    expect_identical(out$lev, levenshtein(toupper(a$surname)[out$index_1],
                                          toupper(b$surname)[out$index_2]))
    expect_identical(attr(out, "compare")$scale, "as is")
})

test_that("the output column order keeps each variable's pair together", {
    out <- rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given), sn = jaro_winkler(surname)))
    expect_identical(names(out), c(
        "pair_id", "index_1", "index_2", "pass", "block_key",
        "id_1", "surname_1", "surname_2", "sn",
        "given_1", "given_2", "given_jaro_winkler",
        "dob_1", "dob_2", "ref_2"))
    end <- rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given), sn = jaro_winkler(surname)),
                 score_position = "end")
    expect_identical(tail(names(end), 2L), c("given_jaro_winkler", "sn"))
    sfx <- rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                 suffixes = c(".x", ".y"))
    expect_true(all(c("index.x", "surname.x", "surname.y") %in% names(sfx)))
})

test_that("carried columns keep their values and classes", {
    a <- enrol()
    b <- claims()
    a$grp <- factor(c("u", "v", "u", "v"))
    out <- rLink(a, b, block_1 = list(soundex(surname)))
    expect_identical(out$dob_1, a$dob[out$index_1])
    expect_s3_class(out$dob_1, "Date")
    expect_identical(out$grp_1, a$grp[out$index_1])
    expect_s3_class(out$grp_1, "factor")
    # force_upper_case affects the scores, never the columns returned.
    low <- data.frame(s = c("smith", "smith"), stringsAsFactors = FALSE)
    expect_identical(unique(rLink(low, low, block_1 = list(s))$s_1), "smith")
})

test_that("keep selects the carried columns", {
    expect_identical(
        names(rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                    keep = "surname")),
        c("pair_id", "index_1", "index_2", "pass", "block_key",
          "surname_1", "surname_2"))
    expect_identical(
        names(rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                    keep = character())),
        c("pair_id", "index_1", "index_2", "pass", "block_key"))
    expect_error(rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                       keep = "nope"), "not there")
})

test_that("the inputs are never modified", {
    a <- enrol()
    b <- claims()
    snap_a <- a
    snap_b <- b
    rLink(a, b, block_1 = list(soundex(surname)),
          compare = list(jaro_winkler(given)))
    expect_identical(a, snap_a)
    expect_identical(b, snap_b)

    # A data.table input used to be impossible: class(data1) == "character"
    # has length 2, which is an error since R 4.2.
    da <- data.table::as.data.table(a)
    db <- data.table::as.data.table(b)
    expect_identical(nrow(rLink(da, db, block_1 = list(soundex(surname)))), 4L)
    expect_true(data.table::is.data.table(da))
})

test_that("bad arguments are signalled, not printed", {
    a <- enrol()
    b <- claims()
    expect_error(rLink(a, b, block_1 = list(soundex(surnme))), "surnme")
    expect_error(rLink(a, b, block_1 = list(soundex(surnme))),
                 "Columns in `data_1`")
    expect_error(rLink(a, b, block_1 = list(soundex(surname)),
                       compare = list(jaro_winkler(givn))), "givn")
    expect_error(rLink(a, b), "`block_1` is required")
    expect_error(rLink(a, b, block_1 = list(surname),
                       pairs = data.frame(x_index = 1L, y_index = 1L)),
                 "not both")
    expect_error(rLink(a, b, block_1 = list(double_metaphone(surname))),
                 "\\$primary")
    expect_error(rLink(a, b, block_1 = list(soundex(surname)),
                       compare = list(jaro_winkler(surname),
                                      jaro_winkler(surname))),
                 "Two comparators")
    expect_error(rLink(a, b, block_1 = list(soundex(surname)),
                       compare = list(id_1 = jaro_winkler(surname))),
                 "already on the output")
    expect_error(rLink(a, b, block_1 = list(soundex(surname)),
                       compare = list(eq = function(x, y) x == y)),
                 "does not say which column")
    expect_error(rLink(a, b, block_1 = list(soundex(surname)),
                       compare = list((function(x, y) x == y)(surname))),
                 "needs a name")
    expect_error(rLink(a, b, block_1 = list(surname),
                       suffixes = c("_1", "_1")), "two different strings")
    expect_error(rLink("nope", b, block_1 = list(surname)), "must be a data frame")
    expect_error(rLink(a, b, block_1 = list(surname), nthreads = 0), "nthreads")
})

test_that("a comparator returning the wrong shape is reported by name", {
    expect_error(
        rLink(enrol(), claims(), block_1 = list(soundex(surname)),
              compare = list(bad = (function(x, y) "nope")(surname))),
        "one number or logical per pair")
})

test_that("zero pairs gives an empty table with the full schema", {
    a <- data.frame(k = "A", v = 1, stringsAsFactors = FALSE)
    b <- data.frame(k = "B", v = 2, stringsAsFactors = FALSE)
    out <- rLink(a, b, block_1 = list(k), compare = list(jaro_winkler(v)))
    expect_identical(nrow(out), 0L)
    expect_identical(names(out), c(
        "pair_id", "index_1", "index_2", "pass", "block_key",
        "k_1", "k_2", "v_1", "v_2", "v_jaro_winkler"))
})

test_that("pairs = scores a candidate set from elsewhere", {
    a <- enrol()
    b <- claims()
    cand <- block_pairs(soundex(a$surname), soundex(b$surname))
    out <- rLink(a, b, pairs = cand, compare = list(jaro_winkler(surname)))
    expect_identical(out$index_1, cand$x_index)
    expect_false("block_key" %in% names(out))
    expect_error(rLink(a, b, pairs = data.frame(x_index = 99L, y_index = 1L)),
                 "outside")
    expect_error(rLink(a, b, pairs = data.frame(nope = 1L)), "`x_index`")
})

test_that("block_method = 'sorted_neighbourhood' widens the candidate set", {
    a <- enrol()
    b <- claims()
    exact <- rLink(a, b, block_1 = list(surname))
    wide <- rLink(a, b, block_1 = list(surname),
                  block_method = "sorted_neighbourhood", window = 3)
    expect_true(nrow(wide) >= nrow(exact))
    one <- rLink(a, b, block_1 = list(surname),
                 block_method = "sorted_neighbourhood", window = 1)
    expect_identical(one$index_1, exact$index_1)
})

test_that("a lone numeric key sorts as a number, not as text", {
    a <- data.frame(age = c(9, 50))
    b <- data.frame(age = c(10, 100, 8))
    out <- rLink(a, b, block_1 = list(age),
                 block_method = "sorted_neighbourhood", window = 3)
    ref <- sorted_neighbourhood(a$age, b$age, window = 3)
    expect_identical(out$index_1, ref$x_index)
    expect_identical(out$index_2, ref$y_index)
    expect_identical(out$block_key, c("9", "9", "50", "50"))
})

test_that("a Date key on one side and text on the other still match", {
    a <- enrol()
    b <- claims()
    b$dob <- format(b$dob)
    expect_identical(nrow(rLink(a, b, block_1 = list(dob))),
                     nrow(rLink(a, claims(), block_1 = list(dob))))
})

test_that("max_block_size warns and skips", {
    a <- data.frame(k = c("a", "a", "a", "b", "b"), stringsAsFactors = FALSE)
    expect_warning(rLink(a, block_1 = list(k), max_block_size = 2),
                   "Skipped 1 block")
})

test_that("results do not depend on the number of threads", {
    set.seed(1)
    n <- 2000
    a <- data.frame(
        s = sample(c("SMITH", "SMYTH", "JONES", "BROWN"), n, TRUE),
        g = sample(c("JOHN", "JON", "MARY"), n, TRUE),
        stringsAsFactors = FALSE)
    one <- rLink(a, block_1 = list(soundex(s)),
                 compare = list(jaro_winkler(g)), nthreads = 1)
    many <- rLink(a, block_1 = list(soundex(s)),
                  compare = list(jaro_winkler(g)))
    # The columns, not the attributes: `timings` differs by design.
    strip <- function(x) {
        x <- as.data.frame(x)
        attr(x, "timings") <- NULL
        attr(x, "compare") <- NULL
        x
    }
    expect_identical(strip(one), strip(many))
})

test_that("timings are returned and verbose prints them", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    tm <- attr(out, "timings")
    expect_s3_class(tm, "data.frame")
    expect_true(all(c("validate", "keys", "pairs", "attach",
                      "given_jaro_winkler", "assemble", "total") %in% tm$stage))
    expect_true(all(tm$seconds >= 0))
    expect_silent(rLink(a, b, block_1 = list(soundex(surname))))
    expect_message(rLink(a, b, block_1 = list(sn = list(soundex(surname), dob)),
                         compare = list(jaro_winkler(given)), verbose = TRUE),
                   "expected pairs")
})

test_that("rLink() agrees with the block_pairs() pipeline it wraps", {
    a <- enrol()
    b <- claims()
    p <- block_pairs(soundex(a$surname), soundex(b$surname))
    want <- jaro_winkler(toupper(a$given)[p$x_index],
                         toupper(b$given)[p$y_index])
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    expect_identical(out$index_1, p$x_index)
    expect_equal(out$given_jaro_winkler, want)
})


# --- block_1 and the `block` it replaced -----------------------------------

test_that("`block` is still accepted as a synonym for `block_1`", {
    a <- enrol()
    b <- claims()
    new <- rLink(a, b, block_1 = list(soundex(surname)))
    old <- rLink(a, b, block = list(soundex(surname)))
    expect_identical(new$index_1, old$index_1)
    expect_identical(new$index_2, old$index_2)
    expect_identical(new$pass, old$pass)
})

test_that("giving both blocking names at once is an error", {
    expect_error(
        rLink(enrol(), claims(), block_1 = list(surname),
              block = list(surname)),
        "not both")
})

test_that("blocking errors name whichever argument the caller wrote", {
    a <- enrol()
    b <- claims()
    # The term's position is reported as "<arg>, pass 1, term 1".
    expect_error(rLink(a, b, block_1 = list(soundex(nope))),
                 "^block_1, pass 1")
    expect_error(rLink(a, b, block = list(soundex(nope))),
                 "^block, pass 1")
    # And so is the pass/term mismatch against block_2.
    cl2 <- data.frame(ref = b$ref, family_name = b$surname, dob = b$dob,
                      stringsAsFactors = FALSE)
    expect_error(
        rLink(a, cl2, block = list(list(soundex(surname), dob)),
              block_2 = list(soundex(family_name))),
        "in `block` but")
    expect_error(
        rLink(a, cl2, block_1 = list(list(soundex(surname), dob)),
              block_2 = list(soundex(family_name))),
        "in `block_1` but")
})


# --- pair_id ----------------------------------------------------------------

id_enrol <- function() {
    d <- enrol()
    d$frame_id <- c("E01", "E02", "E03", "E04")
    d
}

id_claims <- function() {
    d <- claims()
    d$frame_id <- c("C01", "C02", "C03")
    d$person_id <- c("P1", "P2", "P3")
    d
}

test_that("pair_id joins the two record identifiers", {
    a <- id_enrol()
    b <- id_claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)))
    expect_type(out$pair_id, "character")
    expect_identical(out$pair_id,
                     paste0(a$frame_id[out$index_1], "|",
                            b$frame_id[out$index_2]))
    # The identifier is stable: the same two records keep their key under a
    # different blocking pass, which is what a saved review relies on.
    other <- rLink(a, b, block_1 = list(dob))
    shared <- intersect(out$pair_id, other$pair_id)
    expect_true(length(shared) > 0L)
    for (key in shared) {
        i <- match(key, out$pair_id)
        j <- match(key, other$pair_id)
        expect_identical(out$index_1[i], other$index_1[j])
        expect_identical(out$index_2[i], other$index_2[j])
    }
})

test_that("pair_id takes one name per input", {
    a <- id_enrol()
    b <- id_claims()
    out <- rLink(a, b, block_1 = list(soundex(surname)),
                 pair_id = c("frame_id", "person_id"))
    expect_identical(out$pair_id,
                     paste0(a$frame_id[out$index_1], "|",
                            b$person_id[out$index_2]))
})

test_that("pair_id = NULL numbers the pairs", {
    out <- rLink(id_enrol(), id_claims(), block_1 = list(soundex(surname)),
                 pair_id = NULL)
    expect_identical(out$pair_id, seq_len(nrow(out)))
})

test_that("the default pair_id column falls back silently when absent", {
    a <- enrol()
    b <- claims()
    expect_false("frame_id" %in% names(a))
    out <- expect_silent(rLink(a, b, block_1 = list(soundex(surname))))
    expect_identical(out$pair_id, seq_len(nrow(out)))
})

test_that("a pair_id column named explicitly must be there", {
    expect_error(
        rLink(enrol(), claims(), block_1 = list(soundex(surname)),
              pair_id = "frame_id"),
        "not a column of `data_1`", fixed = TRUE)
    expect_error(
        rLink(id_enrol(), claims(), block_1 = list(soundex(surname)),
              pair_id = "frame_id"),
        "not a column of `data_2`", fixed = TRUE)
})

test_that("pair_id rejects shapes it cannot use", {
    a <- id_enrol()
    b <- id_claims()
    expect_error(rLink(a, b, block_1 = list(surname), pair_id = 1),
                 "must be NULL")
    expect_error(rLink(a, b, block_1 = list(surname),
                       pair_id = c("a", "b", "c")), "must be NULL")
    expect_error(rLink(a, b, block_1 = list(surname), pair_id = NA_character_),
                 "must be NULL")
    expect_error(rLink(a, b, block_1 = list(surname), pair_id = ""),
                 "must be NULL")
    # Two names describe two inputs, and deduplicating has only one.
    expect_error(rLink(a, block_1 = list(surname),
                       pair_id = c("frame_id", "frame_id")),
                 "only one", fixed = TRUE)
})

test_that("an identifier holding the separator is an error, not a collision", {
    a <- id_enrol()
    a$frame_id[1L] <- "E|01"
    expect_error(rLink(a, id_claims(), block_1 = list(soundex(surname))),
                 "would be ambiguous", fixed = TRUE)
})

test_that("identifiers that do not identify records raise a warning", {
    a <- id_enrol()
    a$frame_id <- c("E01", "E01", "E03", "E01")
    expect_warning(rLink(a, id_claims(), block_1 = list(fsubstr(surname, 1, 1))),
                   "duplicated")

    na_id <- id_enrol()
    na_id$frame_id[1L] <- NA
    # Assigned inside, because expect_warning() returns the condition.
    expect_warning(
        out <- rLink(na_id, id_claims(), block_1 = list(soundex(surname))),
        "missing")
    expect_true(anyNA(out$pair_id))
    # A missing identifier makes that one key NA; the rest are unaffected.
    expect_false(all(is.na(out$pair_id)))
})

test_that("a Date identifier is formatted, not deparsed", {
    a <- id_enrol()
    a$frame_id <- a$dob
    out <- suppressWarnings(
        rLink(a, id_claims(), block_1 = list(soundex(surname)),
              keep = character()))
    expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", out$pair_id)))
    expect_true(all(grepl("|", out$pair_id, fixed = TRUE)))
})


# --- the fs_pairs class -----------------------------------------------------

test_that("the result is an fs_pairs data.table", {
    out <- rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    expect_identical(class(out),
                     c("fs_pairs", "data.table", "data.frame"))
    expect_true(data.table::is.data.table(out))
})

test_that("the class and the compare attribute survive a filter", {
    out <- rLink(enrol(), claims(), block_1 = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    kept <- out[out$given_jaro_winkler > 0.5, ]
    expect_s3_class(kept, "fs_pairs")
    expect_false(is.null(attr(kept, "compare")))
    # data.table's own subsetting too, and a column selection, and an order.
    expect_s3_class(out[given_jaro_winkler > 0.5], "fs_pairs")
    expect_s3_class(out[order(-given_jaro_winkler)], "fs_pairs")
    expect_false(is.null(attr(out[given_jaro_winkler > 0.5], "compare")))
})
