# fast.string

[![R-CMD-check](https://github.com/KyleHaynes/graphfast/workflows/R-CMD-check/badge.svg)](https://github.com/KyleHaynes/graphfast/actions)
[![Status](https://img.shields.io/badge/status-development-orange)](https://github.com/KyleHaynes/graphfast)


Parallel string, date, fuzzy-matching, and phonetic-coding functions for R,
built on PCRE2 (regex), prepared byte-oriented literal search, and
RcppParallel (Intel TBB). It is designed for record linkage, ETL, and other
workloads that grep, substitute, clean, or compare large collections of
strings. Actual speedups depend on string length, match density, input reuse,
matrix shape, and available cores; `bench.R` records those factors against
the exact checkout being measured.

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
- **Fuzzy lookup**: `fuzzy_match()`, `fuzzy_top_n()` (streaming best/top-N matches without a full matrix)
- **Edit distance**: `levenshtein()`, `osa_distance()`, `damerau_levenshtein()`, `hamming()` (+ matrix, normalized-similarity, and bounded variants)
- **Q-gram set overlap**: `jaccard_index()`, `dice_coefficient()`, `tversky_index()` (+ `_matrix()` variants)
- **fuzzywuzzy-style ratios** (R port of Python's `fuzzywuzzy`): `fuzz_ratio()`, `fuzz_partial_ratio()`, `fuzz_token_sort_ratio()`, `fuzz_token_set_ratio()`

Loading the package (`library(fast.string)`) prints a one-time startup
banner listing all of these with a short description; suppress it with
`options(fast.string.verbose = FALSE)` (set before `library()`) or
`suppressPackageStartupMessages()`.

The established similarity APIs compare encoded bytes by default for
compatibility and speed; pass `use_bytes = FALSE` for UTF-8 code-point
comparison. The new fuzzy lookup APIs use code points by default and retain
only the requested matches, so memory grows with `length(x) * top_n` rather
than `length(x) * length(table)`.

```r
fuzzy_match(c("SMITH", "JONES"), c("SMYTH", "JONAS", "JONES"))
fuzzy_top_n("kitten", c("sitting", "mitten", "cat"),
            method = "levenshtein", top_n = 2)
```

## Quick benchmark

```r
library(fast.string)
set.seed(1)
n <- 1e6
x <- stringi::stri_rand_strings(n, sample(3:40, n, replace = TRUE))
x_ws <- paste0("  ", x, "  ")

system.time(base::grepl("[0-9]{2}", x))
system.time(fast.string::fgrepl("[0-9]{2}", x))

system.time(base::trimws(x_ws))
system.time(fast.string::ftrimws(x_ws))

system.time(fast.string::jaro_winkler(x, rev(x)))

system.time(stringdist::stringdist(x, rev(x), method = "lv"))
system.time(fast.string::levenshtein(x, rev(x)))
```

For reproducible results, run:

```sh
Rscript bench.R --full --output=benchmark-results/core
```

The harness installs the current checkout into an isolated library and
records the exact commit, compiler, backend, operating system, corpus,
requested thread count, warmed timings, and peak memory.

For an extensive, per-function breakdown across every function group at
2M-row scale (including `NA`/`""` edge cases, and head-to-head comparisons
against `stringdist`/`phonics` for the edit-distance, q-gram, and
phonetic-coding functions), see [Benchmarks.R](Benchmarks.R).

For a narrative walkthrough with worked examples for every function, see
the Quarto vignette [https://kylehaynes.github.io/fast.string/](https://kylehaynes.github.io/fast.string/)
