library(fgrepl)
cat("=== Correctness Tests ===\n")

x <- c("hello world", "foo bar", NA, "test pattern here", "HELLO WORLD")

# Basic regex (use identical() carefully — NA != NA with ==, so use isTRUE(all.equal()))
cat("regex match:\n")
cat("  grepl:      ", paste(grepl("pattern", x), collapse=", "), "\n")
cat("  fast_grepl: ", paste(fast_grepl("pattern", x), collapse=", "), "\n")
cat("  match:", isTRUE(all.equal(grepl("pattern", x), fast_grepl("pattern", x))), "\n\n")

# ignore.case
cat("ignore.case:\n")
cat("  grepl:      ", paste(grepl("hello", x, ignore.case=TRUE), collapse=", "), "\n")
cat("  fast_grepl: ", paste(fast_grepl("hello", x, ignore.case=TRUE), collapse=", "), "\n")
cat("  match:", isTRUE(all.equal(grepl("hello", x, ignore.case=TRUE), fast_grepl("hello", x, ignore.case=TRUE))), "\n\n")

# fixed
cat("fixed:\n")
cat("  grepl:      ", paste(grepl("foo", x, fixed=TRUE), collapse=", "), "\n")
cat("  fast_grepl: ", paste(fast_grepl("foo", x, fixed=TRUE), collapse=", "), "\n")
cat("  match:", isTRUE(all.equal(grepl("foo", x, fixed=TRUE), fast_grepl("foo", x, fixed=TRUE))), "\n\n")

# NA propagation
cat("NA handling:\n")
cat("  grepl NA:      ", grepl("x", NA_character_), "\n")
cat("  fast_grepl NA: ", fast_grepl("x", NA_character_), "\n")
cat("  both NA:", is.na(fast_grepl("x", NA_character_)), "\n\n")

# Anchors
cat("anchors test (^foo):", identical(grepl("^foo", x), fast_grepl("^foo", x)), "\n")
cat("char class (\\\\d+):", identical(grepl("\\d+", c("abc123", "xyz"), perl=TRUE),
                                       fast_grepl("\\d+", c("abc123", "xyz"))), "\n\n")

cat("=== Benchmark ===\n")
x_large <- rep(c("hello world", "foo bar", "test pattern here", "HELLO WORLD"), 250000)
cat("Vector length:", length(x_large), "\n\n")

library(microbenchmark)

cat("--- Simple pattern ('pattern') ---\n")
mb1 <- microbenchmark(
    base_grepl       = grepl("pattern", x_large),
    fast_grepl_regex = fast_grepl("pattern", x_large),
    fast_grepl_fixed = fast_grepl("pattern", x_large, fixed=TRUE),
    times = 5
)
print(mb1)
med1 <- summary(mb1)$median
cat("Speedup vs base grepl: regex=", round(med1[1]/med1[2], 1), "x  fixed=",
    round(med1[1]/med1[3], 1), "x\n\n")

cat("--- Complex alternation ('(hello|world|foo|bar|test|HELLO|pattern)') ---\n")
complex_pat <- "(hello|world|foo|bar|test|HELLO|pattern)"
mb2 <- microbenchmark(
    base_grepl       = grepl(complex_pat, x_large, perl=TRUE),
    fast_grepl_regex = fast_grepl(complex_pat, x_large),
    times = 5
)
print(mb2)
med2 <- summary(mb2)$median
cat("Speedup vs base grepl:", round(med2[1]/med2[2], 1), "x\n\n")

cat("--- Case-insensitive ('hello', ignore.case=TRUE) ---\n")
mb3 <- microbenchmark(
    base_grepl       = grepl("hello", x_large, ignore.case=TRUE),
    fast_grepl_regex = fast_grepl("hello", x_large, ignore.case=TRUE),
    fast_grepl_fixed = fast_grepl("hello", x_large, ignore.case=TRUE, fixed=TRUE),
    times = 5
)
print(mb3)
med3 <- summary(mb3)$median
cat("Speedup vs base grepl: regex=", round(med3[1]/med3[2], 1), "x  fixed=",
    round(med3[1]/med3[3], 1), "x\n\n")

cat("Thread count:", RcppParallel::defaultNumThreads(), "\n")
