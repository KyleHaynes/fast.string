# fgrepl — String & Phonetic Benchmarks

`trimws()`, `substr()`, `nchar()`, and `chartr()` are drop-in replacements for
their `base` equivalents, parallelised across all CPU cores via Intel TBB.
All four preserve `names(x)` and `NA` handling exactly like `base`.

`soundex()` and `nysiis()` are phonetic-matching functions with no `base`
equivalent, intended as blocking keys alongside `jaro_winkler_matrix()`.

```r
library(fgrepl)
library(stringi)
library(microbenchmark)
options(fgrepl.verbose = FALSE)

set.seed(1)
n    <- 1e6
x    <- stri_rand_strings(n, sample(3:40, n, replace = TRUE), pattern = "[A-Z]")
x_ws <- paste0("  ", x, "\t\n")          # padded, for trimws()
```

## trimws()

```r
identical(base::trimws(x_ws), fgrepl::trimws(x_ws))

microbenchmark(
    base   = base::trimws(x_ws),
    fgrepl = fgrepl::trimws(x_ws),
    times = 5
)
```

## substr()

```r
identical(base::substr(x, 1, 10), fgrepl::substr(x, 1, 10))

microbenchmark(
    base   = base::substr(x, 1, 10),
    fgrepl = fgrepl::substr(x, 1, 10),
    times = 5
)
```

## nchar()

```r
identical(base::nchar(x), fgrepl::nchar(x))

microbenchmark(
    base   = base::nchar(x),
    fgrepl = fgrepl::nchar(x),
    times = 5
)
```

## chartr()

```r
identical(base::chartr("AEIOU", "aeiou", x), fgrepl::chartr("AEIOU", "aeiou", x))

microbenchmark(
    base   = base::chartr("AEIOU", "aeiou", x),
    fgrepl = fgrepl::chartr("AEIOU", "aeiou", x),
    times = 5
)
```

## soundex() / nysiis()

No `base` equivalent to compare against — these are phonetic blocking keys
for fuzzy name matching, not string transforms. Sanity-checked against known
reference codes (`soundex("Robert") == "R163"`,
`soundex("Ashcraft") == "A261"`) elsewhere; here we just confirm output shape
and measure absolute throughput on 1M rows.

```r
sx <- soundex(x)
ny <- nysiis(x)
all(nchar(sx) == 4)   # soundex is always exactly 4 chars (or NA)
all(nchar(ny) <= 6)   # nysiis is truncated to at most 6 chars (or NA)

microbenchmark(
    soundex = soundex(x),
    nysiis  = nysiis(x),
    times = 5
)
```

## Observed (1M strings, 1 run, 8-core Windows machine — your numbers will vary)

| Function  | base (median) | fgrepl (median) | Speedup |
|-----------|---------------:|-----------------:|--------:|
| `trimws`  | 1049 ms        | 335 ms           | ~3x     |
| `substr`  | 180 ms         | 195 ms           | ~1x (no win — base's substr is already cheap) |
| `nchar`   | 186 ms         | 6.8 ms           | ~27x    |
| `chartr`  | 1133 ms        | 255 ms           | ~4x     |
| `soundex` | —              | 66 ms            | n/a (no base equivalent) |
| `nysiis`  | —              | 206 ms           | n/a (no base equivalent) |

`substr` doesn't benefit from parallelism here because base R's substr is
already close to memcpy speed; the win mainly shows up on patterns with
heavier per-element cost (`trimws`, `chartr`) or branchy scalar loops (`nchar`).
`nysiis` is slower than `soundex` because it has a much heavier per-element
rule table (leading/trailing transforms plus a stateful per-letter pass)
versus soundex's single fixed lookup table.
