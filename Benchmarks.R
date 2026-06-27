# ============================================================================
# fast.string — Extensive Benchmarks
# ============================================================================
# Compares every exported function against base R (and, where there is no
# base equivalent, reports absolute throughput). Uses large, messy vectors
# (2M strings, length 2-40, with a scattering of "" and NA) so the numbers
# reflect realistic data-linkage / ETL workloads rather than toy inputs.
#
# Run section-by-section (each is self-contained after the "Test data"
# block) — the full script takes several minutes on 2M rows.

suppressPackageStartupMessages(library(fast.string))
library(stringi)
library(microbenchmark)

# ============================================================================
# Test data
# ============================================================================

set.seed(1)
n <- 2e6

x <- stri_rand_strings(n, sample(2:40, n, replace = TRUE), pattern = "[A-Za-z0-9 ]")
na_idx    <- sample(n, 2000)
empty_idx <- sample(setdiff(seq_len(n), na_idx), 2000)
x[na_idx]    <- NA_character_
x[empty_idx] <- ""

cat(sprintf("n = %d strings (%d NA, %d empty, lengths 2-40)\n\n", n, length(na_idx), length(empty_idx)))

# ============================================================================
# 1. GREPL / GREP / SUB / GSUB
# ============================================================================

cat("=== fgrepl: regex vs fixed ===\n")
print(microbenchmark(
    base_regex = base::grepl("[0-9]{2}", x),
    fast_regex = fast.string::fgrepl("[0-9]{2}", x),
    base_perl = base::grepl("abc", x, perl = TRUE),
    base_fixed = base::grepl("abc", x, fixed = TRUE),
    fast_fixed = fast.string::fgrepl("abc", x, fixed = TRUE),
    times = 5
))

cat("\n=== fgrepl: case-insensitive ===\n")
print(microbenchmark(
    base = base::grepl("abc", x, ignore.case = TRUE),
    base_perl = base::grepl("abc", x, ignore.case = TRUE, perl = TRUE),
    fast = fast.string::fgrepl("abc", x, ignore.case = TRUE),
    times = 5
))

cat("\n=== fgrep: indices and value=TRUE ===\n")
print(microbenchmark(
    base_idx = base::grep("abc", x),
    fast_idx = fast.string::fgrep("abc", x),
    base_val = base::grep("abc", x, value = TRUE),
    fast_val = fast.string::fgrep("abc", x, value = TRUE),
    times = 5
))

cat("\n=== fsub: first-match substitution ===\n")
print(microbenchmark(
    base = base::sub("[0-9]+", "#", x, perl = TRUE),
    fast = fast.string::fsub("[0-9]+", "#", x),
    times = 5
))

cat("\n=== fgsub: regex vs fixed ===\n")
print(microbenchmark(
    base_regex = base::gsub("[0-9]+", "#", x, perl = TRUE),
    fast_regex = fast.string::fgsub("[0-9]+", "#", x),
    base_fixed = base::gsub("a", "@", x, fixed = TRUE),
    fast_fixed = fast.string::fgsub("a", "@", x, fixed = TRUE),
    times = 5
))

cat("\n=== gsub_all: 5 patterns, sequential vs single-scan ===\n")
patterns <- c("a", "e", "i", "o", "u")
repls    <- c("4", "3", "1", "0", "*")
print(microbenchmark(
    base_loop = {
        y <- x
        for (i in seq_along(patterns)) y <- base::gsub(patterns[i], repls[i], y, fixed = TRUE)
        y
    },
    fast_sequential = fast.string::gsub_all(patterns, repls, x, fixed = TRUE, sequential = TRUE),
    fast_single_scan = fast.string::gsub_all(patterns, repls, x, fixed = TRUE, sequential = FALSE),
    times = 5
))

# ============================================================================
# 2. STRING UTILITIES (trimws / substr / nchar / chartr)
# ============================================================================

x_ws <- ifelse(is.na(x), NA_character_, paste0("  ", x, "\t\n"))

cat("\n=== ftrimws ===\n")
print(microbenchmark(
    base = base::trimws(x_ws),
    fast = fast.string::ftrimws(x_ws),
    times = 5
))

cat("\n=== fsubstr ===\n")
print(microbenchmark(
    base = base::substr(x, 1, 10),
    fast = fast.string::fsubstr(x, 1, 10),
    times = 5
))

cat("\n=== fnchar: bytes vs chars ===\n")
print(microbenchmark(
    base_bytes = base::nchar(x, "bytes"),
    fast_bytes = fast.string::fnchar(x, "bytes"),
    base_chars = base::nchar(x, "chars"),
    fast_chars = fast.string::fnchar(x, "chars"),
    times = 5
))

cat("\n=== fchartr ===\n")
print(microbenchmark(
    base = base::chartr("aeiou", "AEIOU", x),
    fast = fast.string::fchartr("aeiou", "AEIOU", x),
    times = 5
))

# ============================================================================
# 3. DATES (format_date / format_date_parts / date_parts / fas.Date)
# ============================================================================

dates <- as.Date(sample(seq(as.Date("1950-01-01"), as.Date("2024-12-31"), by = "day"), n, replace = TRUE))
dates[na_idx] <- NA
date_strings_iso <- format_date(dates, "iso")

cat("\n=== format_date: iso vs base format.Date ===\n")
print(microbenchmark(
    base = format(dates, "%Y-%m-%d"),
    fast = fast.string::format_date(dates, "iso"),
    times = 5
))

cat("\n=== date_parts vs base year/month/day extraction ===\n")
print(microbenchmark(
    base = data.frame(
        year  = as.integer(format(dates, "%Y")),
        month = as.integer(format(dates, "%m")),
        day   = as.integer(format(dates, "%d"))
    ),
    fast = fast.string::date_parts(dates),
    times = 5
))

cat("\n=== format_date_parts: assembling year/month/day into a string ===\n")
parts <- fast.string::date_parts(dates)
print(microbenchmark(
    fast = fast.string::format_date_parts(parts$year, parts$month, parts$day, "iso"),
    times = 5
))

cat("\n=== fas.Date vs base::as.Date (fixed ISO format) ===\n")
print(microbenchmark(
    base = base::as.Date(date_strings_iso, format = "%Y-%m-%d"),
    fast = fast.string::fas.Date(date_strings_iso, "iso"),
    times = 5
))

# ============================================================================
# 4. PHONETIC CODES (soundex / nysiis) — no base equivalent
# ============================================================================

names_x <- stri_rand_strings(n, sample(3:15, n, replace = TRUE), pattern = "[A-Z]")
names_x[na_idx] <- NA_character_

cat("\n=== soundex (absolute throughput, no base equivalent) ===\n")
print(microbenchmark(fast = fast.string::soundex(names_x), times = 5))

cat("\n=== nysiis (absolute throughput, no base equivalent) ===\n")
print(microbenchmark(fast = fast.string::nysiis(names_x), times = 5))

# ============================================================================
# 5. STRING COMPARISON — Jaro-Winkler
# ============================================================================

cat("\n=== jaro_winkler: pairwise similarity over the full 2M vector ===\n")
print(microbenchmark(
    fast = fast.string::jaro_winkler(names_x, rev(names_x)),
    times = 5
))

# jaro_winkler_matrix is O(n*m); cap both sides at 2,000 for a realistic
# blocking-table-sized comparison rather than a 2M x 2M matrix.
m <- 2000
a_small <- names_x[seq_len(m)]
b_small <- names_x[(n - m + 1):n]

cat(sprintf("\n=== jaro_winkler_matrix: %d x %d all-pairs comparison ===\n", m, m))
print(microbenchmark(
    fast = fast.string::jaro_winkler_matrix(a_small, b_small),
    times = 5
))

cat("\n=== jaro_winkler_tokens: multi-token (3-word) names, absolute throughput, no base equivalent ===\n")
cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
make_multitoken_names <- function(n) {
    paste(
        cap1(stri_rand_strings(n, sample(3:9, n, replace = TRUE), pattern = "[a-z]")),
        cap1(stri_rand_strings(n, sample(3:9, n, replace = TRUE), pattern = "[a-z]")),
        cap1(stri_rand_strings(n, sample(3:9, n, replace = TRUE), pattern = "[a-z]"))
    )
}
tok_a <- make_multitoken_names(n)
tok_b <- make_multitoken_names(n)
print(microbenchmark(
    plain_jaro_winkler = fast.string::jaro_winkler(tok_a, tok_b),
    jaro_winkler_tokens = fast.string::jaro_winkler_tokens(tok_a, tok_b),
    times = 5
))

cat("\n=== Done ===\n")
