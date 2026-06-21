# fast.string — String & Phonetic Benchmarks

`ftrimws()`, `fsubstr()`, `fnchar()`, and `fchartr()` are fast equivalents of
`base::trimws()`, `base::substr()`, `base::nchar()`, and `base::chartr()`,
parallelised across all CPU cores via Intel TBB. All four preserve
`names(x)` and `NA` handling exactly like `base`.

`soundex()` and `nysiis()` are phonetic-matching functions with no `base`
equivalent, intended as blocking keys alongside `jaro_winkler_matrix()`.

```r
suppressPackageStartupMessages(library(fast.string))
library(stringi)
library(microbenchmark)

set.seed(1)
n    <- 1e6
x    <- stri_rand_strings(n, sample(3:40, n, replace = TRUE), pattern = "[A-Z]")
x_ws <- paste0("  ", x, "\t\n")          # padded, for trimws()


system.time(xx <- jaro_winkler(x, rev(x)))
system.time(yy <- RecordLinkage::jarowinkler(x, rev(x)))
system.time(zz <- 1 - stringdist::stringdist(x, rev(x), "jw"))
table(xx == yy)
table(zz == xx)
head

```

## ftrimws()

```r
identical(base::trimws(x_ws), fast.string::ftrimws(x_ws))

microbenchmark(
    base   = base::trimws(x_ws),
    fast.string = fast.string::ftrimws(x_ws),
    times = 5
)
```

## fsubstr()

```r
identical(base::substr(x, 1, 10), fast.string::fsubstr(x, 1, 10))

microbenchmark(
    base   = base::substr(x, 1, 10),
    fast.string = fast.string::fsubstr(x, 1, 10),
    times = 5
)
```

## fnchar()

```r
identical(base::nchar(x), fast.string::fnchar(x))

microbenchmark(
    base   = base::nchar(x),
    fast.string = fast.string::fnchar(x),
    times = 5
)
```

## fchartr()

```r
identical(base::chartr("AEIOU", "aeiou", x), fast.string::fchartr("AEIOU", "aeiou", x))

microbenchmark(
    base   = base::chartr("AEIOU", "aeiou", x),
    fast.string = fast.string::fchartr("AEIOU", "aeiou", x),
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

| Function    | base (median) | fast.string (median) | Speedup |
|-------------|---------------:|-----------------:|--------:|
| `ftrimws`   | 1049 ms        | 335 ms           | ~3x     |
| `fsubstr`   | 180 ms         | 195 ms           | ~1x (no win — base's substr is already cheap) |
| `fnchar`    | 186 ms         | 6.8 ms           | ~27x    |
| `fchartr`   | 1133 ms        | 255 ms           | ~4x     |
| `soundex`   | —              | 66 ms            | n/a (no base equivalent) |
| `nysiis`    | —              | 206 ms           | n/a (no base equivalent) |

`fsubstr` doesn't benefit from parallelism here because base R's substr is
already close to memcpy speed; the win mainly shows up on patterns with
heavier per-element cost (`ftrimws`, `fchartr`) or branchy scalar loops (`fnchar`).
`nysiis` is slower than `soundex` because it has a much heavier per-element
rule table (leading/trailing transforms plus a stateful per-letter pass)
versus soundex's single fixed lookup table.
