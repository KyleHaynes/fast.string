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
    out <- rLink(a, b, block = list(list(soundex(surname), dob),
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
    out <- rLink(a, block = list(soundex(surname)))
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
        rLink(a, b, block = list(soundex(surname, nthreads = my_threads)))$index_1,
        rLink(a, b, block = list(soundex(surname)))$index_1)
    v <- "surname"
    expect_identical(
        rLink(a, b, block = list(soundex(!!rlang::sym(v))))$index_1,
        rLink(a, b, block = list(soundex(surname)))$index_1)
    # A length-1 term is recycled, which pairs everything with everything.
    expect_identical(nrow(rLink(a, b, block = list(1))), 12L)
})

test_that("the pass grammar reads list() as passes and c() as terms", {
    a <- enrol()
    b <- claims()
    # One pass of two terms, written three ways.
    n <- nrow(rLink(a, b, block = list(list(surname, dob))))
    expect_identical(nrow(rLink(a, b, block = c("surname", "dob"))), n)
    expect_identical(nrow(rLink(a, b, block = list(c(surname, dob)))), n)
    # Two passes, each one term.
    expect_identical(max(rLink(a, b, block = list(surname, dob))$pass), 2L)
    expect_error(rLink(a, b, block = list(list())), "no terms")
})

test_that("a missing key term pairs with nothing unless match_missing", {
    a <- data.frame(k = c("X", NA, ""), stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(a, a, block = list(k))), 1L)
    # "" is in na_strings, so both missing rows share one key.
    expect_identical(nrow(rLink(a, a, block = list(k),
                                match_missing = TRUE)), 5L)
    expect_identical(nrow(rLink(a, a, block = list(k),
                                na_strings = character())), 2L)

    # A missing value in any one term makes the whole pass key missing.
    m <- data.frame(s = c("SMITH", "JONES"), t = c(NA, NA),
                    stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(m, m, block = list(list(s, t)))), 0L)
    # With match_missing the terms stay distinct, so SMITH does not reach
    # JONES through their shared blank.
    expect_identical(nrow(rLink(m, m, block = list(list(s, t)),
                                match_missing = TRUE)), 2L)
})

test_that("a value that looks like a sentinel survives untouched", {
    # The function this replaces wrote paste0("1", colname, seq_len(n)) into
    # the data and stripped it afterwards with grepl("^1colname"), which
    # destroyed any real value of that shape.
    a <- data.frame(x = c("1x1", "1x2"), stringsAsFactors = FALSE)
    out <- rLink(a, a, block = list(x), match_missing = TRUE)
    expect_identical(sort(unique(c(out$x_1, out$x_2))), c("1x1", "1x2"))
})

test_that("key terms are joined without colliding", {
    # Joined with "~", c("A~B", "C") and c("A", "B~C") shared a key.
    tl <- data.frame(p = c("A~B", "A"), q = c("C", "B~C"),
                     stringsAsFactors = FALSE)
    expect_identical(nrow(rLink(tl, tl, block = list(list(p, q)))), 2L)
    # The separator actually used is rejected in the data rather than hoped
    # against.
    sep <- data.frame(p = c("A\u001fB", "A"), q = c("C", "B"),
                      stringsAsFactors = FALSE)
    expect_error(rLink(sep, sep, block = list(list(p, q))),
                 "control character")
})

test_that("block_2 maps columns named differently", {
    a <- enrol()
    b <- claims()
    cl2 <- data.frame(ref = b$ref, family_name = b$surname,
                      firstname = b$given, dob = b$dob,
                      stringsAsFactors = FALSE)
    expect_identical(
        rLink(a, cl2, block = list(soundex(surname)),
              block_2 = list(soundex(family_name)))$index_2,
        rLink(a, b, block = list(soundex(surname)))$index_2)
    # A NULL pass reuses block's own expression.
    expect_identical(
        nrow(rLink(a, b, block = list(soundex(surname)), block_2 = list(NULL))),
        4L)
    expect_error(
        rLink(a, cl2, block = list(soundex(surname)),
              block_2 = list(soundex(family_name), dob)),
        "matched up")
    expect_error(
        rLink(a, cl2, block = list(list(soundex(surname), dob)),
              block_2 = list(soundex(family_name))),
        "can never agree")
})

test_that("comparators are resolved and named", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block = list(soundex(surname)),
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
        rLink(a, b, block = list(soundex(surname)),
              compare = list(s = jaro_winkler(surname, p = 0)))$s,
        rLink(a, b, block = list(soundex(surname)),
              compare = list(s = jaro_winkler(surname, p = 0.25)))$s)))
})

test_that("comparators can compare differently named columns", {
    a <- enrol()
    b <- claims()
    cl2 <- data.frame(family_name = b$surname, stringsAsFactors = FALSE)
    out <- rLink(a, cl2, block = list(soundex(surname)),
                 block_2 = list(soundex(family_name)),
                 compare = list(sn = jaro_winkler(surname | family_name)))
    expect_equal(out$sn, jaro_winkler(toupper(a$surname)[out$index_1],
                                      toupper(cl2$family_name)[out$index_2]))
})

test_that("a custom comparator works and only gets nthreads if declared", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block = list(soundex(surname)),
                 compare = list(eq = (function(x, y) x == y)(surname)))
    expect_type(out$eq, "logical")

    seen <- NA
    spy <- function(x, y, nthreads) { seen <<- nthreads; nchar(x) }
    rLink(a, b, block = list(soundex(surname)),
          compare = list(s = spy(surname)), nthreads = 3)
    expect_identical(seen, 3)

    # A plain function of two arguments must not be handed nthreads.
    plain <- function(x, y) nchar(x)
    expect_silent(rLink(a, b, block = list(soundex(surname)),
                        compare = list(p = plain(surname)), nthreads = 3))
})

test_that("scores are normalised so that 1 is an exact match", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block = list(soundex(surname)),
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
    out <- rLink(a, b, block = list(k),
                 compare = list(d = damerau_levenshtein(s)))
    expect_identical(out$d, c(1, 0))
})

test_that("normalise = FALSE leaves the comparator's own scale", {
    a <- enrol()
    b <- claims()
    out <- rLink(a, b, block = list(soundex(surname)),
                 compare = list(lev = levenshtein(surname)),
                 normalise = FALSE)
    expect_identical(out$lev, levenshtein(toupper(a$surname)[out$index_1],
                                          toupper(b$surname)[out$index_2]))
    expect_identical(attr(out, "compare")$scale, "as is")
})

test_that("the output column order keeps each variable's pair together", {
    out <- rLink(enrol(), claims(), block = list(soundex(surname)),
                 compare = list(jaro_winkler(given), sn = jaro_winkler(surname)))
    expect_identical(names(out), c(
        "pair_id", "index_1", "index_2", "pass", "block_key",
        "id_1", "surname_1", "surname_2", "sn",
        "given_1", "given_2", "given_jaro_winkler",
        "dob_1", "dob_2", "ref_2"))
    end <- rLink(enrol(), claims(), block = list(soundex(surname)),
                 compare = list(jaro_winkler(given), sn = jaro_winkler(surname)),
                 score_position = "end")
    expect_identical(tail(names(end), 2L), c("given_jaro_winkler", "sn"))
    sfx <- rLink(enrol(), claims(), block = list(soundex(surname)),
                 suffixes = c(".x", ".y"))
    expect_true(all(c("index.x", "surname.x", "surname.y") %in% names(sfx)))
})

test_that("carried columns keep their values and classes", {
    a <- enrol()
    b <- claims()
    a$grp <- factor(c("u", "v", "u", "v"))
    out <- rLink(a, b, block = list(soundex(surname)))
    expect_identical(out$dob_1, a$dob[out$index_1])
    expect_s3_class(out$dob_1, "Date")
    expect_identical(out$grp_1, a$grp[out$index_1])
    expect_s3_class(out$grp_1, "factor")
    # force_upper_case affects the scores, never the columns returned.
    low <- data.frame(s = c("smith", "smith"), stringsAsFactors = FALSE)
    expect_identical(unique(rLink(low, low, block = list(s))$s_1), "smith")
})

test_that("keep selects the carried columns", {
    expect_identical(
        names(rLink(enrol(), claims(), block = list(soundex(surname)),
                    keep = "surname")),
        c("pair_id", "index_1", "index_2", "pass", "block_key",
          "surname_1", "surname_2"))
    expect_identical(
        names(rLink(enrol(), claims(), block = list(soundex(surname)),
                    keep = character())),
        c("pair_id", "index_1", "index_2", "pass", "block_key"))
    expect_error(rLink(enrol(), claims(), block = list(soundex(surname)),
                       keep = "nope"), "not there")
})

test_that("the inputs are never modified", {
    a <- enrol()
    b <- claims()
    snap_a <- a
    snap_b <- b
    rLink(a, b, block = list(soundex(surname)),
          compare = list(jaro_winkler(given)))
    expect_identical(a, snap_a)
    expect_identical(b, snap_b)

    # A data.table input used to be impossible: class(data1) == "character"
    # has length 2, which is an error since R 4.2.
    da <- data.table::as.data.table(a)
    db <- data.table::as.data.table(b)
    expect_identical(nrow(rLink(da, db, block = list(soundex(surname)))), 4L)
    expect_true(data.table::is.data.table(da))
})

test_that("bad arguments are signalled, not printed", {
    a <- enrol()
    b <- claims()
    expect_error(rLink(a, b, block = list(soundex(surnme))), "surnme")
    expect_error(rLink(a, b, block = list(soundex(surnme))),
                 "Columns in `data_1`")
    expect_error(rLink(a, b, block = list(soundex(surname)),
                       compare = list(jaro_winkler(givn))), "givn")
    expect_error(rLink(a, b), "`block` is required")
    expect_error(rLink(a, b, block = list(surname),
                       pairs = data.frame(x_index = 1L, y_index = 1L)),
                 "not both")
    expect_error(rLink(a, b, block = list(double_metaphone(surname))),
                 "\\$primary")
    expect_error(rLink(a, b, block = list(soundex(surname)),
                       compare = list(jaro_winkler(surname),
                                      jaro_winkler(surname))),
                 "Two comparators")
    expect_error(rLink(a, b, block = list(soundex(surname)),
                       compare = list(id_1 = jaro_winkler(surname))),
                 "already on the output")
    expect_error(rLink(a, b, block = list(soundex(surname)),
                       compare = list(eq = function(x, y) x == y)),
                 "does not say which column")
    expect_error(rLink(a, b, block = list(soundex(surname)),
                       compare = list((function(x, y) x == y)(surname))),
                 "needs a name")
    expect_error(rLink(a, b, block = list(surname),
                       suffixes = c("_1", "_1")), "two different strings")
    expect_error(rLink("nope", b, block = list(surname)), "must be a data frame")
    expect_error(rLink(a, b, block = list(surname), nthreads = 0), "nthreads")
})

test_that("a comparator returning the wrong shape is reported by name", {
    expect_error(
        rLink(enrol(), claims(), block = list(soundex(surname)),
              compare = list(bad = (function(x, y) "nope")(surname))),
        "one number or logical per pair")
})

test_that("zero pairs gives an empty table with the full schema", {
    a <- data.frame(k = "A", v = 1, stringsAsFactors = FALSE)
    b <- data.frame(k = "B", v = 2, stringsAsFactors = FALSE)
    out <- rLink(a, b, block = list(k), compare = list(jaro_winkler(v)))
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
    exact <- rLink(a, b, block = list(surname))
    wide <- rLink(a, b, block = list(surname),
                  block_method = "sorted_neighbourhood", window = 3)
    expect_true(nrow(wide) >= nrow(exact))
    one <- rLink(a, b, block = list(surname),
                 block_method = "sorted_neighbourhood", window = 1)
    expect_identical(one$index_1, exact$index_1)
})

test_that("max_block_size warns and skips", {
    a <- data.frame(k = c("a", "a", "a", "b", "b"), stringsAsFactors = FALSE)
    expect_warning(rLink(a, block = list(k), max_block_size = 2),
                   "Skipped 1 block")
})

test_that("results do not depend on the number of threads", {
    set.seed(1)
    n <- 2000
    a <- data.frame(
        s = sample(c("SMITH", "SMYTH", "JONES", "BROWN"), n, TRUE),
        g = sample(c("JOHN", "JON", "MARY"), n, TRUE),
        stringsAsFactors = FALSE)
    one <- rLink(a, block = list(soundex(s)),
                 compare = list(jaro_winkler(g)), nthreads = 1)
    many <- rLink(a, block = list(soundex(s)),
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
    out <- rLink(a, b, block = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    tm <- attr(out, "timings")
    expect_s3_class(tm, "data.frame")
    expect_true(all(c("validate", "keys", "pairs", "attach",
                      "given_jaro_winkler", "assemble", "total") %in% tm$stage))
    expect_true(all(tm$seconds >= 0))
    expect_silent(rLink(a, b, block = list(soundex(surname))))
    expect_message(rLink(a, b, block = list(sn = list(soundex(surname), dob)),
                         compare = list(jaro_winkler(given)), verbose = TRUE),
                   "expected pairs")
})

test_that("rLink() agrees with the block_pairs() pipeline it wraps", {
    a <- enrol()
    b <- claims()
    p <- block_pairs(soundex(a$surname), soundex(b$surname))
    want <- jaro_winkler(toupper(a$given)[p$x_index],
                         toupper(b$given)[p$y_index])
    out <- rLink(a, b, block = list(soundex(surname)),
                 compare = list(jaro_winkler(given)))
    expect_identical(out$index_1, p$x_index)
    expect_equal(out$given_jaro_winkler, want)
})
