u <- function(...) intToUtf8(c(...))

# Leftmost-first matches in bytes: list of c(start, length, pattern).
ref_matches <- function(s, patterns, ignore_case = FALSE) {
    fold <- function(r) {
        if (ignore_case) {
            up <- r >= as.raw(65) & r <= as.raw(90)
            r[up] <- as.raw(as.integer(r[up]) + 32L)
        }
        r
    }
    h <- fold(charToRaw(s))
    p <- lapply(patterns, function(q) fold(charToRaw(q)))
    find <- function(pat, from) {
        m <- length(pat)
        if (from + m - 1L > length(h)) return(0L)
        for (st in from:(length(h) - m + 1L))
            if (all(h[st:(st + m - 1L)] == pat)) return(st)
        0L
    }
    out <- list()
    pos <- 1L
    while (pos <= length(h)) {
        best <- NULL
        for (j in seq_along(p)) {
            st <- find(p[[j]], pos)
            if (st > 0 && (is.null(best) || st < best[1])) best <- c(st, length(p[[j]]), j)
        }
        if (is.null(best)) break
        out[[length(out) + 1L]] <- best
        pos <- best[1] + best[2]
    }
    out
}

slice <- function(s, a, b) {
    if (b < a) return("")
    v <- rawToChar(charToRaw(s)[a:b])
    Encoding(v) <- "UTF-8"
    v
}

set.seed(202)
make_case <- function(np, alpha) {
    patterns <- vapply(1:np, function(i) paste(sample(alpha[1:3], sample(1:4, 1), TRUE),
                                               collapse = ""), "")
    x <- c(vapply(1:150, function(i) paste(sample(c(alpha, "d"), sample(0:16, 1), TRUE),
                                            collapse = ""), ""), NA, "")
    list(patterns = patterns, x = x)
}

test_that("fgrepl_any(), fwhich_any(), fcount_any() and fextract_any() agree with a brute-force search", {
    for (np in c(1, 3, 9, 40)) {
        cs <- make_case(np, c("a", "b", "c", "A", u(0xe9)))
        p <- cs$patterns
        x <- cs$x
        m <- lapply(x, function(s) if (is.na(s)) NULL else ref_matches(s, p))
        expect_identical(fgrepl_any(p, x),
                         ifelse(is.na(x), NA, lengths(m) > 0), info = np)
        expect_identical(fcount_any(p, x),
                         ifelse(is.na(x), NA_integer_, lengths(m)), info = np)
        which_ref <- vapply(x, function(s) {
            if (is.na(s)) return(NA_integer_)
            hit <- which(vapply(p, grepl, NA, s, fixed = TRUE))
            if (length(hit)) hit[1] else NA_integer_
        }, 1L, USE.NAMES = FALSE)
        expect_identical(fwhich_any(p, x), which_ref, info = np)
        first <- vapply(seq_along(x), function(i) {
            if (is.na(x[i]) || !length(m[[i]])) return(NA_character_)
            b <- m[[i]][[1]]
            slice(x[i], b[1], b[1] + b[2] - 1L)
        }, "")
        expect_identical(fextract_any(p, x), first, info = np)
        every <- lapply(seq_along(x), function(i) {
            if (is.na(x[i])) return(NA_character_)
            vapply(m[[i]], function(b) slice(x[i], b[1], b[1] + b[2] - 1L), "")
        })
        expect_identical(fextract_any(p, x, all = TRUE), every, info = np)
    }
})

test_that("ignore.case folds ASCII letters only", {
    cs <- make_case(12, c("a", "b", "c", "A", "B"))
    ref <- vapply(cs$x, function(s) if (is.na(s)) NA
                  else length(ref_matches(s, cs$patterns, TRUE)) > 0, NA,
                  USE.NAMES = FALSE)
    expect_identical(fgrepl_any(cs$patterns, cs$x, ignore.case = TRUE), ref)
    expect_identical(fgrepl_any("ABC", "xabcx", ignore.case = TRUE), TRUE)
    expect_identical(fgrepl_any(u(0xc9), u(0xe9), ignore.case = TRUE), FALSE)
})

test_that("gsub_all(fixed, sequential = FALSE) is leftmost-first with the automaton", {
    for (np in c(3, 8, 30)) {
        cs <- make_case(np, c("a", "b", "c"))
        repl <- paste0("<", seq_len(np), ">")
        ref <- vapply(cs$x, function(s) {
            if (is.na(s)) return(NA_character_)
            m <- ref_matches(s, cs$patterns)
            out <- ""
            pos <- 1L
            for (b in m) {
                out <- paste0(out, slice(s, pos, b[1] - 1L), repl[b[3]])
                pos <- b[1] + b[2]
            }
            paste0(out, slice(s, pos, nchar(s, "bytes")))
        }, "", USE.NAMES = FALSE)
        expect_identical(
            gsub_all(cs$patterns, repl, cs$x, fixed = TRUE, sequential = FALSE),
            ref, info = np)
    }
    # an NA replacement still makes the row NA
    expect_identical(gsub_all(letters[1:9], c(NA, LETTERS[2:9]), c("bcd", "xyz", "abc"),
                              fixed = TRUE, sequential = FALSE),
                     c("BCD", "xyz", NA))
})

test_that("multi-pattern search validates patterns and keeps names", {
    expect_error(fgrepl_any(c("a", ""), "a"), "empty strings")
    expect_error(fgrepl_any(c("a", NA), "a"), "NA")
    expect_error(fgrepl_any(character(), "a"), "non-empty")
    expect_identical(fwhich_any("x", c(a = "x", b = "y")), c(a = 1L, b = NA))
    expect_identical(fextract_any(c("na", "ba"), c("banana", NA, "x")), c("ba", NA, NA))
    notes <- c("stolen bicycle", "break and enter, tools taken")
    keywords <- c("stolen", "taken", "break and enter")
    expect_identical(fwhich_any(keywords, notes), c(1L, 2L))
    expect_identical(fextract_any(keywords, notes), c("stolen", "break and enter"))
})

test_that("many patterns on many strings agree with a fgrepl() loop", {
    set.seed(9)
    kw <- unique(vapply(1:2000, function(i) paste(sample(letters, sample(3:6, 1), TRUE),
                                                   collapse = ""), ""))
    x <- vapply(1:3000, function(i) paste(sample(c(letters, " "), 30, TRUE), collapse = ""), "")
    loop <- Reduce(`|`, lapply(kw, function(k) grepl(k, x, fixed = TRUE)))
    expect_identical(fgrepl_any(kw, x), loop)
    expect_identical(fgrepl_any(kw, x, nthreads = 1), loop)
})
