# Stage timings for rLink() on synthetic data.
#
#   Rscript tools/bench_rlink.R [n] [lib] [out.rds] [scenarios]
#
# `n` is the number of records in each file (500,000 by default), `lib` an
# optional library to load fast.string from, so that two builds can be timed
# against each other, `out.rds` somewhere to keep the results, and
# `scenarios` a comma-separated choice of the ones below (all, by default).
#
# Each scenario runs in an R process of its own. That is not tidiness: how
# long R takes to make ten million new strings depends on how far its heap
# has already grown, so a scenario timed after a heavy one comes out up to
# three times faster than the same scenario timed first. One process each is
# what makes two builds comparable.
#
# The data are made to look like a linkage rather than like noise: surnames
# and given names follow a Zipf distribution, so a few values are very common
# and most are rare, which is what makes blocks uneven and what makes the same
# pair of values turn up again and again among the candidates. About 70% of
# the second file are records of the first, a share of them with a typing
# error in the surname.

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) >= 1L) as.integer(as.numeric(args[1L])) else 500000L
lib <- if (length(args) >= 2L && nzchar(args[2L])) args[2L] else NULL
out_file <- if (length(args) >= 3L && nzchar(args[3L])) args[3L] else NULL
only <- if (length(args) >= 4L) strsplit(args[4L], ",", fixed = TRUE)[[1L]]

scenarios <- c("exact1", "multi3", "dedupe", "filtered", "dates", "bare")
if (is.null(only) || length(only) > 1L) {
    script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE),
                                       value = TRUE)[1L])
    results <- list()
    for (name in if (is.null(only)) scenarios else only) {
        part <- tempfile(fileext = ".rds")
        system2(file.path(R.home("bin"), "Rscript"),
                c(shQuote(script), n, shQuote(if (is.null(lib)) "" else lib),
                  shQuote(part), name))
        if (file.exists(part)) results <- c(results, readRDS(part))
    }
    cat("\nSummary\n")
    print(data.frame(
        scenario = names(results),
        seconds = round(vapply(results, `[[`, 0, "elapsed"), 2),
        pairs = format(vapply(results, `[[`, 0L, "n_pairs"), big.mark = ","),
        peak_mb = round(vapply(results, `[[`, 0, "peak_mb"))),
        row.names = FALSE)
    if (!is.null(out_file)) saveRDS(results, out_file)
    quit(save = "no")
}

suppressPackageStartupMessages({
    options(fast.string.verbose = FALSE)
    if (is.null(lib)) library(fast.string)
    else library(fast.string, lib.loc = lib)
    library(data.table)
})

set.seed(20261111)

zipf_pool <- function(size, syllables) {
    parts <- c("an", "ber", "cha", "dor", "el", "fin", "gar", "ham", "is",
               "jo", "kel", "lin", "mac", "nor", "ow", "per", "qui", "ros",
               "son", "tho", "ul", "van", "wil", "xu", "yor", "zam", "smi",
               "th", "ng", "uy", "en", "bro", "wn", "es", "ley", "ton")
    out <- character()
    while (length(out) < size) {
        k <- sample(syllables, size, replace = TRUE)
        fresh <- vapply(k, function(m)
            paste(sample(parts, m, replace = TRUE), collapse = ""), "")
        out <- unique(c(out, toupper(fresh)))
    }
    out[seq_len(size)]
}

draw_zipf <- function(pool, n) {
    w <- 1 / seq_along(pool)
    pool[sample.int(length(pool), n, replace = TRUE, prob = w)]
}

surnames <- zipf_pool(40000L, 2:4)
givens <- zipf_pool(4000L, 1:3)

make_file <- function(n, prefix) data.frame(
    frame_id = sprintf("%s%08d", prefix, seq_len(n)),
    surname = draw_zipf(surnames, n),
    given = draw_zipf(givens, n),
    dob = as.Date("1930-01-01") + sample.int(29200L, n, replace = TRUE),
    postcode = sprintf("%04d", sample.int(2600L, n, replace = TRUE) + 799L),
    age = sample.int(90L, n, replace = TRUE),
    stringsAsFactors = FALSE)

file_1 <- make_file(n, "A")
file_2 <- make_file(n, "B")
# 70% of the second file are people in the first.
same <- sample.int(n, floor(0.7 * n))
for (nm in c("surname", "given", "dob", "postcode", "age"))
    file_2[[nm]][same] <- file_1[[nm]][same]
# A transposition in the surname of 15% of those.
typo <- same[sample.int(length(same), floor(0.15 * length(same)))]
s <- file_2$surname[typo]
at <- pmax(1L, nchar(s) %/% 2L)
file_2$surname[typo] <- paste0(substr(s, 1L, at - 1L), substr(s, at + 1L, at + 1L),
                               substr(s, at, at), substr(s, at + 2L, nchar(s)))

has_filter <- "filter" %in% names(formals(rLink))
exact <- function(a, b) as.numeric(a == b)

run <- function(name, label, expr) {
    if (!is.null(only) && !name %in% only) return(invisible(NULL))
    invisible(gc(reset = TRUE, full = TRUE))
    t <- system.time(out <- eval.parent(substitute(expr)))[["elapsed"]]
    mem <- gc()
    peak_mb <- sum(mem[, ncol(mem)])
    cat(sprintf("\n== %s: %.2fs, %s pairs, peak %.0f Mb ==\n", label, t,
                format(nrow(out), big.mark = ","), peak_mb))
    timings <- attr(out, "timings")
    if (!is.null(timings)) {
        timings$seconds <- round(timings$seconds, 3)
        print(timings, row.names = FALSE)
    }
    res <- list(label = label, elapsed = t, n_pairs = nrow(out),
                peak_mb = peak_mb, timings = timings)
    rm(out)
    results[[name]] <<- res
    invisible(res)
}

results <- list()

# 1. One exact pass with large blocks: every candidate carries every column.
run("exact1", "one exact pass (dob), all columns carried", rLink(
    file_1, file_2,
    block_1 = list(dob),
    compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                   pc = exact(postcode))))

# 2. Three passes of multi-term keys.
run("multi3", "three multi-term passes", rLink(
    file_1, file_2,
    block_1 = list(list(soundex(surname), dob), list(surname, postcode),
                   list(given, dob)),
    compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                   pc = exact(postcode))))

# 3. Deduplicating one file.
run("dedupe", "dedupe (surname + year of birth)", rLink(
    file_1,
    block_1 = list(list(surname, format_date(dob, "y"))),
    compare = list(gn = jaro_winkler(given), pc = exact(postcode))))

# 4. The same candidates as 1, keeping only the plausible ones. Before
#    `filter` existed this is what a caller had to do: score and carry
#    everything, then subset.
run("filtered", "one exact pass (dob), kept where sn or gn is close",
    if (has_filter) rLink(
        file_1, file_2,
        block_1 = list(dob),
        compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                       pc = exact(postcode)),
        filter = sn >= 0.85 | gn >= 0.9)
    else {
        out <- rLink(
            file_1, file_2,
            block_1 = list(dob),
            compare = list(sn = jaro_winkler(surname),
                           gn = jaro_winkler(given), pc = exact(postcode)))
        kept <- out[sn >= 0.85 | gn >= 0.9]
        data.table::setattr(kept, "timings", attr(out, "timings"))
        kept
    })

# 5. A date comparator on a Date column.
run("dates", "date_compare() on a Date column", rLink(
    file_1, file_2,
    block_1 = list(list(soundex(surname), postcode)),
    compare = list(db = date_compare(dob), sn = jaro_winkler(surname)),
    keep = character()))

# 6. No identifier and nothing carried: the pair generator and the
#    comparators on their own.
run("bare", "one exact pass (dob), nothing carried, no pair_id", rLink(
    file_1, file_2,
    block_1 = list(dob),
    compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given)),
    keep = character(), pair_id = NULL, block_key = FALSE))

if (!is.null(out_file)) saveRDS(results, out_file)
