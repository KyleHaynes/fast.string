# fast.string

[![R-CMD-check](https://github.com/KyleHaynes/graphfast/workflows/R-CMD-check/badge.svg)](https://github.com/KyleHaynes/graphfast/actions)
[![Status](https://img.shields.io/badge/status-development-orange)](https://github.com/KyleHaynes/graphfast)


Parallel string, date, fuzzy-matching, and phonetic-coding functions for R,
built on PCRE2 (regex), RE2 `StringPiece` (fixed strings), and RcppParallel
(Intel TBB). Typically 2–40x faster than the equivalent base R or
`stringdist` functions on large character vectors — useful for record
linkage, ETL, and other workloads that grep/sub/clean/compare millions of
strings at once.

## Installation

```r
remotes::install_github("KyleHaynes/fast.string")
```

## Functions

- **Matching / substitution**: `fgrepl()`, `fgrep()`, `fsub()`, `fgsub()`, `gsub_all()`
- **String utilities**: `ftrimws()`, `fsubstr()`, `fnchar()`, `fchartr()`
- **Dates**: `format_date()`, `format_date_parts()`, `date_parts()`, `fas.Date()`
- **Phonetic blocking keys**: `soundex()`, `nysiis()`, `double_metaphone()`, `caverphone()`
- **Fuzzy string similarity**: `jaro_winkler()`, `jaro_winkler_matrix()`, `jaro_winkler_tokens()`
- **Edit distance** (faster than `stringdist`): `levenshtein()`, `damerau_levenshtein()`, `hamming()` (+ `_matrix()` variants)
- **Q-gram set overlap** (faster than `stringdist`): `jaccard_index()`, `dice_coefficient()`, `tversky_index()` (+ `_matrix()` variants)
- **fuzzywuzzy-style ratios** (R port of Python's `fuzzywuzzy`): `fuzz_ratio()`, `fuzz_partial_ratio()`, `fuzz_token_sort_ratio()`, `fuzz_token_set_ratio()`

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

system.time(stringdist::stringdist(x, rev(x), method = "lv"))
#    user  system elapsed
#    36.6    0.0   36.6
system.time(fast.string::levenshtein(x, rev(x)))
#    user  system elapsed
#     7.3    0.4    7.7      # ~5x vs stringdist
```

(1M strings, 8-core machine — your numbers will vary with core count and
pattern complexity.)

For an extensive, per-function breakdown across every function group at
2M-row scale (including `NA`/`""` edge cases, and head-to-head comparisons
against `stringdist`/`phonics` for the edit-distance, q-gram, and
phonetic-coding functions), see [Benchmarks.R](Benchmarks.R).

For a narrative walkthrough with worked examples for every function, see
the Quarto vignette [https://kylehaynes.github.io/fast.string/](https://kylehaynes.github.io/fast.string/)
