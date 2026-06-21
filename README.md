# fast.string

Parallel string, date, and phonetic-matching functions for R, built on
PCRE2 (regex), RE2 `StringPiece` (fixed strings), and RcppParallel (Intel
TBB). Typically 5–40x faster than the equivalent base R functions on large
character vectors — useful for record linkage, ETL, and other workloads
that grep/sub/clean millions of strings at once.

## Installation

```r
remotes::install_github("KyleHaynes/anthropic_fast")
```

## Functions

- **Matching / substitution**: `fgrepl()`, `fgrep()`, `fsub()`, `fgsub()`, `gsub_all()`
- **String utilities**: `ftrimws()`, `fsubstr()`, `fnchar()`, `fchartr()`
- **Dates**: `format_date()`, `format_date_parts()`, `date_parts()`, `fas.Date()`
- **Phonetic blocking keys**: `soundex()`, `nysiis()`
- **Fuzzy string similarity**: `jaro_winkler()`, `jaro_winkler_matrix()`

Loading the package (`library(fast.string)`) prints a one-time startup
banner listing all of these with a short description; suppress it with
`options(fast.string.verbose = FALSE)` (set before `library()`) or
`suppressPackageStartupMessages()`.

## Quick benchmark

```r
library(fast.string)
set.seed(1)
n <- 1e6
x <- stringi::stri_rand_strings(n, sample(3:40, n, replace = TRUE))
x_ws <- paste0("  ", x, "  ")

system.time(base::grepl("[0-9]{2}", x))
#    user  system elapsed
#    0.24    0.00    0.25
system.time(fast.string::fgrepl("[0-9]{2}", x))
#    user  system elapsed
#    0.14    0.11    0.03    # ~8x

system.time(base::trimws(x_ws))
#    user  system elapsed
#    1.25    0.00    1.25
system.time(fast.string::ftrimws(x_ws))
#    user  system elapsed
#    0.35    0.16    0.30    # ~4x

system.time(fast.string::jaro_winkler(x, rev(x)))
#    user  system elapsed
#    1.86    0.22    0.18    # 1M pairwise comparisons, no base equivalent
```

(1M strings, 8-core machine — your numbers will vary with core count and
pattern complexity.)

For an extensive, per-function breakdown across all five function groups
at 2M-row scale (including `NA`/`""` edge cases), see
[Benchmarks.R](Benchmarks.R).
