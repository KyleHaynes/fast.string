# Independent, deliberately slow reference implementations of the comparators
# this file tests, so that the C++ kernels are checked against the definition
# rather than against themselves.

ref_gestalt_matches <- function(a, b) {
    if (!nchar(a) || !nchar(b)) return(0L)
    ca <- strsplit(a, "")[[1]]
    cb <- strsplit(b, "")[[1]]
    best <- c(i = 0L, j = 0L, len = 0L)
    for (i in seq_along(ca)) for (j in seq_along(cb)) {
        len <- 0L
        while (i + len <= length(ca) && j + len <= length(cb) &&
               ca[i + len] == cb[j + len]) len <- len + 1L
        if (len > best[["len"]]) best <- c(i = i, j = j, len = len)
    }
    if (best[["len"]] == 0L) return(0L)
    i <- best[["i"]]; j <- best[["j"]]; len <- best[["len"]]
    take <- function(v, from, to)
        if (to < from) "" else paste(v[from:to], collapse = "")
    best[["len"]] +
        ref_gestalt_matches(take(ca, 1L, i - 1L), take(cb, 1L, j - 1L)) +
        ref_gestalt_matches(take(ca, i + len, length(ca)),
                            take(cb, j + len, length(cb)))
}

ref_gestalt <- function(a, b) {
    total <- nchar(a) + nchar(b)
    if (total == 0) return(1)
    2 * ref_gestalt_matches(a, b) / total
}

ref_bag <- function(a, b) {
    ca <- table(strsplit(a, "")[[1]])
    cb <- table(strsplit(b, "")[[1]])
    keys <- union(names(ca), names(cb))
    get <- function(tab, k) if (k %in% names(tab)) as.integer(tab[[k]]) else 0L
    only_a <- sum(vapply(keys, function(k) max(0L, get(ca, k) - get(cb, k)), 0L))
    only_b <- sum(vapply(keys, function(k) max(0L, get(cb, k) - get(ca, k)), 0L))
    max(only_a, only_b)
}

ref_needleman <- function(a, b, match = 1, mismatch = -1, gap = -1) {
    ca <- if (nchar(a)) strsplit(a, "")[[1]] else character()
    cb <- if (nchar(b)) strsplit(b, "")[[1]] else character()
    d <- matrix(0, length(ca) + 1L, length(cb) + 1L)
    d[, 1L] <- seq_len(length(ca) + 1L) * gap - gap
    d[1L, ] <- seq_len(length(cb) + 1L) * gap - gap
    for (i in seq_along(ca)) for (j in seq_along(cb))
        d[i + 1L, j + 1L] <- max(
            d[i, j] + if (ca[i] == cb[j]) match else mismatch,
            d[i, j + 1L] + gap,
            d[i + 1L, j] + gap
        )
    d[length(ca) + 1L, length(cb) + 1L]
}

ref_qgrams <- function(x, q) {
    if (nchar(x) < q) return(character())
    unique(substring(x, seq_len(nchar(x) - q + 1L),
                     seq_len(nchar(x) - q + 1L) + q - 1L))
}

ref_haversine <- function(lat_1, lon_1, lat_2, lon_2, radius = 6371.0088) {
    rad <- pi / 180
    h <- sin((lat_2 - lat_1) * rad / 2)^2 +
        cos(lat_1 * rad) * cos(lat_2 * rad) * sin((lon_2 - lon_1) * rad / 2)^2
    2 * radius * asin(sqrt(h))
}

words <- c("SMITH", "SMYTH", "SMITHE", "JOHN", "JON", "JOHNSON", "JONSON",
           "MACDONALD", "MCDONALD", "BROWN", "BROWNLEE", "NGUYEN", "NGUEYN",
           "kitten", "sitting", "GEORGE SMITH", "SMITH GEORGE", "a", "ab",
           "aab", "abracadabra", "", "x")

test_that("ratcliff_obershelp() reproduces gestalt matching", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    expect_equal(
        ratcliff_obershelp(grid$a, grid$b),
        mapply(ref_gestalt, grid$a, grid$b, USE.NAMES = FALSE)
    )
    expect_equal(
        ratcliff_obershelp_distance(grid$a, grid$b),
        1 - ratcliff_obershelp(grid$a, grid$b)
    )
})

test_that("ratcliff_obershelp() agrees with difflib's documented values", {
    # Python: SequenceMatcher(None, "kitten", "sitting").ratio()
    expect_equal(ratcliff_obershelp("kitten", "sitting"), 2 * 4 / 13)
    # "GEORGE" matches, " SMITH" is then on the wrong side of it
    expect_equal(ratcliff_obershelp("GEORGE SMITH", "SMITH GEORGE"), 0.5)
    expect_equal(ratcliff_obershelp("", ""), 1)
    expect_equal(ratcliff_obershelp("abc", ""), 0)
    expect_equal(ratcliff_obershelp("abc", "abc"), 1)
})

test_that("bag_distance() counts the multiset difference", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    keep <- nzchar(grid$a) & nzchar(grid$b)
    expect_equal(
        bag_distance(grid$a[keep], grid$b[keep]),
        as.double(mapply(ref_bag, grid$a[keep], grid$b[keep], USE.NAMES = FALSE))
    )
    expect_equal(bag_distance("", ""), 0)
    expect_equal(bag_distance("abc", ""), 3)
    expect_equal(bag_distance("SMITH", "HTIMS"), 0)
})

test_that("bag_distance() never exceeds the Levenshtein distance", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    expect_true(all(bag_distance(grid$a, grid$b) <=
                        levenshtein(grid$a, grid$b)))
})

test_that("bag_similarity() normalises by the longer string", {
    expect_equal(bag_similarity("kitten", "sitting"), 1 - 3 / 7)
    expect_equal(bag_similarity("", ""), 1)
    expect_equal(bag_similarity("SMITH", "HTIMS"), 1)
})

test_that("needleman_wunsch() reproduces the alignment score", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    expect_equal(
        needleman_wunsch(grid$a, grid$b),
        mapply(ref_needleman, grid$a, grid$b, USE.NAMES = FALSE)
    )
    expect_equal(
        needleman_wunsch(grid$a, grid$b, match = 2, mismatch = -3, gap = -2),
        mapply(ref_needleman, grid$a, grid$b, MoreArgs =
                   list(match = 2, mismatch = -3, gap = -2), USE.NAMES = FALSE)
    )
    # The textbook example, which scores 0 with unit match, mismatch and gap
    expect_equal(needleman_wunsch("GATTACA", "GCATGCU"), 0)
})

test_that("needleman_wunsch_similarity() is 1 only for identical strings", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    sim <- needleman_wunsch_similarity(grid$a, grid$b)
    expect_true(all(sim >= 0 & sim <= 1))
    expect_equal(sim == 1, grid$a == grid$b)
    expect_equal(needleman_wunsch_similarity("", ""), 1)
    expect_equal(needleman_wunsch_similarity("", "XYZ"), 0)
    # 8 matches and one gap, against 9 matches at best and -17 at worst
    expect_equal(needleman_wunsch_similarity("MACDONALD", "MCDONALD"),
                 (7 - -17) / (9 - -17))
})

test_that("the new comparators handle NA and vector recycling errors", {
    for (f in list(ratcliff_obershelp, ratcliff_obershelp_distance,
                   bag_distance, bag_similarity, needleman_wunsch,
                   needleman_wunsch_similarity)) {
        expect_equal(f(c("a", NA, "b"), c(NA, "a", "b"))[1:2],
                     c(NA_real_, NA_real_))
        expect_error(f(c("a", "b"), "a"), "same length")
        expect_error(f(1, "a"), "character")
        expect_length(f(character(), character()), 0)
    }
    expect_error(needleman_wunsch("a", "b", gap = NA), "finite")
    expect_error(needleman_wunsch("a", "b", match = c(1, 2)), "finite")
})

test_that("the new comparators compare bytes or code points as asked", {
    # Two code points, four bytes
    a <- "éé"
    expect_equal(bag_distance(a, "", use_bytes = FALSE), 2)
    expect_equal(bag_distance(a, "", use_bytes = TRUE), 4)
    expect_equal(ratcliff_obershelp(a, "é", use_bytes = FALSE), 2 / 3)
})

test_that("eudex() hashes to 16 hex digits and ignores case and punctuation", {
    expect_match(eudex("SMITH"), "^[0-9a-f]{16}$")
    expect_equal(eudex("SMITH"), eudex("smith"))
    expect_equal(eudex("SMITH"), eudex("S.M-I'T H"))
    expect_equal(eudex(c("SMITH", NA, "")),
                 c(eudex("SMITH"), NA, "0000000000000000"))
    expect_length(eudex(character()), 0)
    expect_error(eudex(1), "character")
})

test_that("eudex() gives sound-alike names the same hash", {
    expect_equal(eudex("SMITH"), eudex("SMYTH"))
    expect_equal(eudex("JOHN"), eudex("JON"))
    expect_true(eudex("SMITH") != eudex("JONES"))
})

test_that("eudex_distance() reproduces the crate's documented behaviour", {
    # Values from the eudex crate's own documentation and tests
    expect_equal(eudex_distance("lulz", "lol"), 4)
    expect_equal(eudex_distance("hello", "hellou"), 1)
    expect_true(eudex_distance("hello", "hellou") < 15)   # Difference::similar()
    expect_equal(eudex_distance("SMITH", "SMYTH"), 0)
    expect_true(eudex_distance("JOHN", "MARGARET") > 15)
})

test_that("eudex_distance() is a distance on the hashes", {
    pairs <- expand.grid(a = words[nzchar(words)], b = words[nzchar(words)],
                         stringsAsFactors = FALSE)
    for (method in c("graduated", "hamming")) {
        d <- eudex_distance(pairs$a, pairs$b, method = method)
        expect_equal(d == 0, eudex(pairs$a) == eudex(pairs$b))
        expect_equal(d, eudex_distance(pairs$b, pairs$a, method = method))
        expect_true(all(d >= 0))
    }
    # Flat Hamming is never larger than the weighted form
    expect_true(all(eudex_distance(pairs$a, pairs$b, method = "hamming") <=
                        eudex_distance(pairs$a, pairs$b)))
    expect_equal(eudex_distance(c("a", NA), c(NA, "a")), c(NA_real_, NA_real_))
    expect_error(eudex_distance("a", c("a", "b")), "same length")
    expect_error(eudex_distance("a", "b", method = "nope"), "'arg'")
})

test_that("geo_distance() reproduces the haversine formula", {
    set.seed(11)
    n <- 500
    lat_1 <- runif(n, -85, 85); lon_1 <- runif(n, -180, 180)
    lat_2 <- runif(n, -85, 85); lon_2 <- runif(n, -180, 180)
    expect_equal(geo_distance(lat_1, lon_1, lat_2, lon_2),
                 ref_haversine(lat_1, lon_1, lat_2, lon_2))
    expect_equal(geo_distance(lat_1, lon_1, lat_1, lon_1), rep(0, n))
})

test_that("geo_distance() handles units, recycling and bad input", {
    km <- geo_distance(-27.4698, 153.0251, -33.8688, 151.2093)
    expect_equal(geo_distance(-27.4698, 153.0251, -33.8688, 151.2093,
                              unit = "m"), km * 1000, tolerance = 1e-6)
    expect_equal(round(km), 732)                  # Brisbane to Sydney
    expect_equal(geo_distance(0, 0, 0, 180), pi * 6371.0088)
    expect_length(geo_distance(c(1, 2, 3), 0, 0, 0), 3)
    expect_equal(geo_distance(c(1, NA), 0, 0, 0)[2], NA_real_)
    expect_error(geo_distance(c(1, 2), c(1, 2, 3), 0, 0), "same length")
    expect_error(geo_distance("a", 0, 0, 0), "numeric")
    expect_error(geo_distance(0, 0, 0, 0, radius = -1), "positive")
    expect_equal(geo_distance(0, 0, 0, 180, radius = 1), pi)
})

test_that("overlap_coefficient() divides by the smaller q-gram set", {
    grid <- expand.grid(a = words, b = words, stringsAsFactors = FALSE)
    for (q in 2:3) {
        expected <- mapply(function(a, b) {
            if (identical(a, b)) return(1)
            qa <- ref_qgrams(a, q); qb <- ref_qgrams(b, q)
            smaller <- min(length(qa), length(qb))
            if (smaller == 0) return(0)
            length(intersect(qa, qb)) / smaller
        }, grid$a, grid$b, USE.NAMES = FALSE)
        expect_equal(overlap_coefficient(grid$a, grid$b, q = q), expected)
    }
    expect_equal(overlap_coefficient("BROWN", "BROWNLEE"), 1)
    expect_true(overlap_coefficient("BROWN", "BROWNLEE") >
                    jaccard_index("BROWN", "BROWNLEE"))
    expect_equal(overlap_coefficient(c("a", NA), c(NA, "a")),
                 c(NA_real_, NA_real_))
})

test_that("overlap_matrix() matches overlap_coefficient() cell by cell", {
    a <- words[1:6]
    b <- words[5:12]
    m <- overlap_matrix(a, b)
    expect_equal(dim(m), c(length(a), length(b)))
    grid <- expand.grid(i = seq_along(a), j = seq_along(b))
    expect_equal(as.vector(m), overlap_coefficient(a[grid$i], b[grid$j]))
    named <- overlap_matrix(c(x = "BROWN"), c(y = "BROWNLEE"))
    expect_equal(dimnames(named), list("x", "y"))
    # Code points and bytes both reach the kernel
    expect_equal(overlap_matrix(a, b, use_bytes = FALSE), m)
})
