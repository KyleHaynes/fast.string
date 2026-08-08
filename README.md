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

- **Matching / substitution**: `fgrepl()`, `fgrep()`, `fcount()`, `fsub()`, `fgsub()`, `gsub_all()`
- **String utilities**: `ftrimws()`, `fsubstr()`, `fnchar()`, `fchartr()`
- **Dates**: `format_date()`, `format_date_parts()`, `date_parts()`, `fas.Date()`, `format_datetime()`, `fas.POSIXct()`
- **Phonetic blocking keys**: `soundex()`, `nysiis()`, `refined_soundex()`, `cologne()`, `double_metaphone()`, `caverphone()`
- **Fuzzy string similarity**: `jaro_winkler()`, `jaro_winkler_matrix()`, `jaro_winkler_tokens()`
- **Fuzzy lookup**: `fuzzy_match()`, `fuzzy_top_n()` (streaming best/top-N matches without a full matrix)
- **Edit distance**: `levenshtein()`, `osa_distance()`, `damerau_levenshtein()`, `hamming()` (+ matrix, normalized-similarity, and bounded variants)
- **Q-gram similarity**: `jaccard_index()`, `dice_coefficient()`, `tversky_index()`, `cosine_similarity()` (+ matrix variants)
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

## Data linkage in practice

The functions are designed to compose into a linkage pipeline: profile the
input, parse it, block it, score what survives.

```r
library(fast.string)

incoming <- data.frame(
    name     = c("  JOHN O'BRIEN  ", "Hans Müller", "UNKNOWN 00000000"),
    dob      = c("15/03/1985", "09/09/1971", "01/01/1900"),
    received = c("2024-06-18 09:15:00", "2024-06-18 12:00:00",
                 "2024-06-18 12:30:00")
)

# 1. Profile: a name field containing digits is a placeholder, not a name.
fcount("[0-9]", incoming$name)
#> [1] 0 0 8

# 2. Parse. fas.Date() validates field ranges; fas.POSIXct() validates the
#    full calendar (leap years included) and returns NA for impossible values.
incoming$dob_date <- fas.Date(incoming$dob, "dmy")
incoming$loaded   <- fas.POSIXct(incoming$received)

# 3. Block on more than one phonetic key and union the candidates —
#    refined_soundex() is precise, cologne() folds umlauts.
refined_soundex(c("Müller", "Mueller"))   #> "M8709"  "M80709"  -- split
cologne(c("Müller", "Mueller"))           #> "657"    "657"     -- merged

# 4. Score survivors on two independent signals: token comparison
#    (order-insensitive) and q-gram cosine (order- and frequency-sensitive).
jaro_winkler_tokens("Kyle John Haynes", "Haynes John Kyle")  #> 1
cosine_similarity("KYLE JOHN HAYNES", "HAYNES JOHN KYLE")    #> 0.867

# 5. Stamp the batch without a timezone-database lookup.
format_datetime(max(incoming$loaded, na.rm = TRUE), "rfc3339")
```

The [worked pipeline chapter](https://kylehaynes.github.io/fast.string/04-record-linkage-workflow.html)
runs this end to end — profiling, deduplication by load timestamp,
multi-key blocking, two-feature scoring, and routing thin-margin pairs to
review instead of silently picking one.

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
