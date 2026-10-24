# All pairs within `h` places of each other in the sorted distinct keys of
# each pass, deduplicated across passes, in the functions' output order.
brute_pairs <- function(kx, ky, h) {
    dedupe <- is.null(ky)
    nx <- length(kx[[1]])
    ny <- if (dedupe) nx else length(ky[[1]])
    out <- data.frame(x_index = integer(), y_index = integer(), pass = integer())
    for (p in seq_along(kx)) {
        a <- kx[[p]]
        b <- if (dedupe) a else ky[[p]]
        keys <- c(a, b)
        u <- sort(unique(keys[!is.na(keys)]), method = "radix")
        ra <- match(a, u)
        rb <- match(b, u)
        g <- expand.grid(i = seq_len(nx), j = seq_len(ny))
        keep <- !is.na(ra[g$i]) & !is.na(rb[g$j]) & abs(ra[g$i] - rb[g$j]) <= h
        if (dedupe) keep <- keep & g$i < g$j
        out <- rbind(out, data.frame(x_index = g$i[keep], y_index = g$j[keep],
                                     pass = rep(p, sum(keep))))
    }
    out <- out[order(out$x_index, out$y_index, out$pass), ]
    out <- out[!duplicated(out[c("x_index", "y_index")]), ]
    rownames(out) <- NULL
    out
}

test_that("block_pairs() pairs records sharing any key", {
    people <- data.frame(
        surname = c("SMITH", "SMYTH", "JONES", "SMITH", "JONES"),
        dob = as.Date(c("1980-01-02", "1980-01-02", "1975-06-30",
                        "1990-12-01", "1975-06-30"))
    )
    p <- block_pairs(people)
    # 1-2 share a date of birth, 1-4 a surname, 3-5 both
    expect_identical(p, data.frame(x_index = c(1L, 1L, 3L),
                                   y_index = c(2L, 4L, 5L),
                                   pass = c(2L, 1L, 1L)))
    expect_identical(block_pairs(c("4000", "4101", NA), c("4101", "4000", "4000")),
                     data.frame(x_index = c(1L, 1L, 2L), y_index = c(2L, 3L, 1L),
                                pass = 1L))
    expect_identical(nrow(block_pairs(c(NA, NA, "a"))), 0L)
    expect_identical(block_pairs(factor(c("a", "b", "a"))),
                     data.frame(x_index = 1L, y_index = 3L, pass = 1L))
})

test_that("block_pairs() and sorted_neighbourhood() match brute force", {
    set.seed(8)
    for (trial in 1:120) {
        nx <- sample(0:25, 1)
        ny <- sample(1:25, 1)
        np <- sample(1:3, 1)
        mk <- function(m) lapply(seq_len(np), function(p) {
            if (p == 2) sample(c(1:5, NA), m, TRUE)
            else sample(c(letters[1:7], NA), m, TRUE)
        })
        kx <- mk(nx)
        ky <- if (trial %% 2) mk(ny) else NULL
        w <- sample(c(1L, 3L, 5L), 1)
        as_df <- function(k) {
            if (is.null(k)) return(NULL)
            stats::setNames(as.data.frame(k), paste0("k", seq_along(k)))
        }
        want <- brute_pairs(kx, ky, (w - 1) / 2)
        expect_identical(sorted_neighbourhood(as_df(kx), as_df(ky), window = w),
                         want)
        if (w == 1)
            expect_identical(block_pairs(as_df(kx), as_df(ky)), want)
    }
})

test_that("sorted_neighbourhood() moves over distinct sorted keys", {
    surname <- c("SMITH", "SMITHE", "SMYTH", "JONES", "JONSON", "SMITH")
    p <- sorted_neighbourhood(surname, window = 3)
    expect_identical(paste(p$x_index, p$y_index),
                     c("1 2", "1 5", "1 6", "2 3", "2 6", "4 5", "5 6"))
    d <- as.Date("2020-01-01") + c(0, 1, 5, 6, 30)
    expect_identical(nrow(sorted_neighbourhood(d, window = 1)), 0L)
    expect_identical(nrow(sorted_neighbourhood(d, window = 3)), 4L)
    expect_error(sorted_neighbourhood(surname, window = 2), "odd")
    expect_error(sorted_neighbourhood(surname, window = 0), "odd")
})

test_that("block_pairs() skips oversized blocks with a warning", {
    expect_warning(p <- block_pairs(c("a", "a", "a", "b", "b"), max_block_size = 2),
                   "Skipped 1 block")
    expect_identical(p, data.frame(x_index = 4L, y_index = 5L, pass = 1L))
    expect_error(block_pairs("a", max_block_size = 0), "max_block_size")
    expect_error(block_pairs(list(1:3, 1:2)), "same length")
    expect_error(block_pairs(1:3, list(1:3, 1:3)), "same number")
})

test_that("pairs do not depend on the number of threads", {
    set.seed(1)
    k <- sample(sprintf("K%03d", 1:500), 20000, TRUE)
    expect_identical(block_pairs(k, nthreads = 1), block_pairs(k))
    expect_identical(sorted_neighbourhood(k, nthreads = 1), sorted_neighbourhood(k))
})

test_that("cluster_pairs() finds connected components", {
    expect_identical(cluster_pairs(c(1, 2, 5), c(2, 4, 6)), c(1L, 1L, 2L, 1L, 3L, 3L))
    expect_identical(cluster_pairs(c(1, 2, 5), c(2, 4, 6), n = 8),
                     c(1L, 1L, 2L, 1L, 3L, 3L, 4L, 5L))
    expect_identical(cluster_pairs(integer(), integer(), n = 3), 1:3)
    expect_identical(cluster_pairs(integer(), integer()), integer())
    p <- data.frame(x_index = c(3L, 1L), y_index = c(4L, 3L))
    expect_identical(cluster_pairs(p), c(1L, 2L, 1L, 1L))
    expect_error(cluster_pairs(1:2, 1:3), "same length")
    expect_error(cluster_pairs(0, 1), "positive")
    expect_error(cluster_pairs(1, 5, n = 3), "larger than")
    expect_error(cluster_pairs(p, 1), "only when")
})

test_that("cluster_pairs() agrees with igraph::components()", {
    skip_if_not_installed("igraph")
    set.seed(3)
    for (trial in 1:50) {
        n <- sample(1:200, 1)
        m <- sample(0:250, 1)
        i <- sample.int(n, m, TRUE)
        j <- sample.int(n, m, TRUE)
        g <- igraph::make_empty_graph(n, directed = FALSE)
        if (m) g <- igraph::add_edges(g, as.vector(rbind(i, j)))
        membership <- igraph::components(g)$membership
        expect_identical(cluster_pairs(i, j, n = n),
                         match(membership, unique(membership)))
    }
})
