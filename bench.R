devtools::install()
library(fast.string)
cat("=== Correctness Tests ===\n")

library(stringi)

# 7M random 3-40 character strings
x_large <- stri_rand_strings(
  n = 7e6,
  length = 3:40,
  pattern = "[A-Z0-9 ]"
)


cat("=== Benchmark ===\n")

library(microbenchmark)

cat("--- Simple pattern ('pattern') ---\n")
mb1 <- microbenchmark(
    base_grepl       = base::grepl("pattern", x_large),
    fast_grepl_regex = fast.string::fgrepl("pattern", x_large),
    fast_grepl_fixed = fast.string::fgrepl("pattern", x_large, fixed=TRUE),
    times = 2
)
print(mb1)
med1 <- summary(mb1)$median
cat("Speedup vs base grepl: regex=", round(med1[1]/med1[2], 1), "x  fixed=",
    round(med1[1]/med1[3], 1), "x\n\n")

cat("--- Complex alternation ('(hello|world|foo|bar|test|HELLO|pattern)') ---\n")
complex_pat <- "(hello|world|foo|bar|test|HELLO|pattern)"
mb2 <- microbenchmark(
    base_grepl       = base::grepl(complex_pat, x_large, perl=TRUE),
    fast_grepl_regex = fast.string::fgrepl(complex_pat, x_large),
    times = 2
)
print(mb2)
med2 <- summary(mb2)$median
cat("Speedup vs base grepl:", round(med2[1]/med2[2], 1), "x\n\n")

cat("--- Case-insensitive ('hello', ignore.case=TRUE) ---\n")
mb3 <- microbenchmark(
    base_grepl       = base::grepl("hello", x_large, ignore.case=TRUE),
    fast_grepl_regex = fast.string::fgrepl("hello", x_large, ignore.case=TRUE),
    fast_grepl_fixed = fast.string::fgrepl("hello", x_large, ignore.case=TRUE, fixed=TRUE),
    times = 2
)
print(mb3)
med3 <- summary(mb3)$median
cat("Speedup vs base grepl: regex=", round(med3[1]/med3[2], 1), "x  fixed=",
    round(med3[1]/med3[3], 1), "x\n\n")

cat("Thread count:", RcppParallel::defaultNumThreads(), "\n")

cat("\n=== grep / sub / gsub Correctness ===\n")
y <- c("hello world", "foo bar", NA, "test123", "HELLO WORLD")
cat("grep match:   ", identical(base::grep("foo", y), fast.string::fgrep("foo", y)), "\n")
cat("grep value:   ", identical(base::grep("foo", y, value=TRUE), fast.string::fgrep("foo", y, value=TRUE)), "\n")
cat("sub match:    ", identical(base::sub("(\\w+)", "[\\1]", y), fast.string::fsub("(\\w+)", "[\\1]", y)), "\n")
cat("gsub match:   ", identical(base::gsub("(\\w+)", "[\\1]", y), fast.string::fgsub("(\\w+)", "[\\1]", y)), "\n")
cat("fixed gsub:   ", identical(base::gsub("o", "0", y, fixed=TRUE), fast.string::fgsub("o", "0", y, fixed=TRUE)), "\n")
cat("fixed sub:    ", identical(base::sub("o", "0", y, fixed=TRUE), fast.string::fsub("o", "0", y, fixed=TRUE)), "\n")
cat("gsub NA:      ", is.na(fast.string::fgsub("x", "y", NA_character_)), "\n")
cat("icase regex:  ", identical(base::gsub("hello","HI",y,ignore.case=TRUE), fast.string::fgsub("hello","HI",y,ignore.case=TRUE)), "\n")
cat("NOTE: fixed+ignore.case: fast.string supports it; base R ignores ignore.case (and warns)\n\n")

cat("--- gsub benchmark ---\n")
mb4 <- microbenchmark(
    base_gsub       = base::gsub("(\\w+)", "[\\1]", x_large),
    fast_gsub_regex = fast.string::fgsub("(\\w+)", "[\\1]", x_large),
    base_gsub_fixed = base::gsub("O", "0", x_large, fixed=TRUE),
    fast_gsub_fixed = fast.string::fgsub("O", "0", x_large, fixed=TRUE),
    times = 2
)
print(mb4)
med4 <- summary(mb4)$median
cat("Speedup regex gsub:", round(med4[1]/med4[2], 1), "x\n")
cat("Speedup fixed gsub:", round(med4[3]/med4[4], 1), "x\n")


# ---------------------------------------------------------------------------
# gsub_all benchmark — 3M strings, system.time
# ---------------------------------------------------------------------------

cat("\n=== gsub_all Benchmark (3M strings) ===\n")
x3m <- stri_rand_strings(3e6, 3:40, "[A-Z0-9 ]")

patterns <- c("THE", "AND", "FOR", "YOU", "ARE")
repls    <- c("the", "and", "for", "you", "are")

cat("\nBase R for-loop (fixed, sequential):\n")
print(system.time({
    x_base <- x3m
    for (i in seq_along(patterns))
        x_base <- base::gsub(patterns[i], repls[i], x_base, fixed = TRUE)
}))

cat("gsub_all fixed sequential (default):\n")
print(system.time(
    x_seq <- fast.string::gsub_all(patterns, repls, x3m, fixed = TRUE, sequential = TRUE)
))

cat("gsub_all fixed single-scan (sequential=FALSE):\n")
print(system.time(
    x_par <- fast.string::gsub_all(patterns, repls, x3m, fixed = TRUE, sequential = FALSE)
))

cat("\nCorrectness (sequential matches base R loop):", identical(x_seq, x_base), "\n")

cat("\n--- Regex gsub_all vs lapply + gsub ---\n")
pat_rx  <- c("\\bTHE\\b", "\\bAND\\b", "\\bFOR\\b")
repl_rx <- c("the", "and", "for")

cat("Base R lapply+gsub (regex):\n")
print(system.time({
    x_base_rx <- x3m
    for (i in seq_along(pat_rx))
        x_base_rx <- base::gsub(pat_rx[i], repl_rx[i], x_base_rx, perl = TRUE)
}))

cat("gsub_all regex sequential:\n")
print(system.time(
    fast.string::gsub_all(pat_rx, repl_rx, x3m, sequential = TRUE)
))


## Soundex
library(fast.string)
library(phonics)
library(stringi)
x_large <- stri_rand_strings(
  n = 7e6,
  length = 3:40,
  pattern = "[A-Z]"
)


system.time({xx <- phonics::soundex(x_large)})
system.time({yy <- fast.string::soundex(x_large)})
system.time({zz <- stringdist::phonetic(x_large)})


