# fast.string

[![R-CMD-check](https://github.com/KyleHaynes/fast.string/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KyleHaynes/fast.string/actions/workflows/R-CMD-check.yaml)
[![Status](https://img.shields.io/badge/status-development-orange)](https://github.com/KyleHaynes/fast.string)


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

## What it's for

| I need to... | Functions | Speed-up over the usual R approach |
|---|---|---|
| Decode HTML entities, squish odd whitespace, fold accents | `html_unescape()`, `fsquish()`, `ascii_fold()` | ~1,400x vs `xml2`; ~70x vs `stringi` |
| Expand abbreviations or map variants to one spelling | `freplace_tokens()` | ~40x vs `strsplit()` + `match()` + `paste()` |
| Restore leading zeros, pad to a width | `fpad()` | ~6x vs `stringi::stri_pad_left()` |
| Decode or encode URLs and query strings | `url_decode()`, `url_encode()` | ~290x vs `URLdecode()`, ~480x vs `URLencode()` |
| Flag text that mentions any of thousands of terms | `fgrepl_any()`, `fwhich_any()`, `fcount_any()`, `fextract_any()` | ~4,900x vs one `grepl()` alternation of 1,000 terms |
| Find misspelled mentions | `fagrepl()`, `fagrep()` | ~150x vs `agrepl()` |
| Pull numbers, fields and matches out of text | `fparse_number()`, `fextract()`, `fextract_all()`, `fsplit_field()` | ~34x vs `readr::parse_number()` |
| See which formats a column holds | `string_shape()` | ~26x vs `chartr()` + `gsub()` |
| Check ABNs, ACNs, TFNs, Medicare, card numbers, ISBNs, EANs, IBANs | `valid_id()` | ~200x vs vectorised base R (Luhn) |
| Pseudonymise identifiers with a secret key | `fhash(x, key = ...)` | ~17x vs `openssl::sha256(key =)` |
| Group spelling variants (OpenRefine key collision) | `fingerprint()`, `ngram_fingerprint()` | ~500x vs an R implementation |
| Candidate pairs for deduplication or linkage | `block_pairs()`, `sorted_neighbourhood()`, `fuzzy_within()`, `tfidf_match()`, `minhash_pairs()` | 10-20x vs `data.table` joins |
| Turn matched pairs into one ID per entity | `cluster_pairs()` | ~4x vs `igraph::components()` |
| Score names and addresses | `jaro_winkler()`, `editex()`, `monge_elkan()`, `smith_waterman()`, `fuzz_*()` | see the benchmarks chapter |

Speed-ups are `bench::mark()` medians at 200,000 strings on a 16-thread
Ryzen 7 3700X (Windows, R 4.5), with the slowest baselines timed on a sample
and scaled; they vary with the data. Against fast C baselines the
margin is smaller: `url_decode()` is about 1.5x `curl::curl_unescape()`,
because building R strings dominates both. The
[benchmarks chapter](https://kylehaynes.github.io/fast.string/06-benchmarks.html)
has the code and every baseline. Each task has a worked chapter in the
documentation:
[cleaning](https://kylehaynes.github.io/fast.string/uc-clean.html),
[finding and extracting](https://kylehaynes.github.io/fast.string/uc-extract.html),
[profiling and validating](https://kylehaynes.github.io/fast.string/uc-profile.html),
[matching and linking](https://kylehaynes.github.io/fast.string/uc-link.html)
and [pseudonymising](https://kylehaynes.github.io/fast.string/uc-privacy.html).

```r
library(fast.string)

# Clean
html_unescape("Fish &amp; Chips &#8211; open")      #> "Fish & Chips – open"
freplace_tokens("12 SMITH ST", c("ST", "RD"), c("STREET", "ROAD"))
#> "12 SMITH STREET"
fpad(c("800", "4000"), 4, pad = "0")                #> "0800" "4000"

# Extract, profile and validate
fparse_number(c("$1,234.50", "-$45", "approx. 3,000 kg"))   #> 1234.5  -45  3000
string_shape(c("0412 345 678", "QLD 4000"))         #> "9999 999 999" "AAA 9999"
valid_id(c("51 824 753 556", "51 824 753 557"), "abn")      #> TRUE FALSE

# Pseudonymise: the same ID always gives the same token, but only key holders can make it
fhash("123456782", key = "a long random secret")

# Deduplicate: candidate pairs from two blocking passes, score, then one ID per person
people <- data.frame(
    name = c("JOHN SMITH", "JON SMITH", "MARY JONES", "JOHN SMYTH"),
    dob  = as.Date(c("1980-01-02", "1980-01-02", "1975-06-30", "1980-01-02"))
)
p <- block_pairs(data.frame(soundex(sub(".* ", "", people$name)), people$dob))
p <- p[jaro_winkler(people$name[p$x_index], people$name[p$y_index]) > 0.85, ]
cluster_pairs(p, n = nrow(people))                  #> 1 1 2 1
```

## Functions

- **Matching & substitution**: `fgrepl()`, `fgrep()`, `fcount()`, `fsub()`, `fgsub()`, `gsub_all()`, `fextract()`, `fextract_all()`
  - *Many literal patterns at once* (Aho-Corasick): `fgrepl_any()`, `fcount_any()`, `fwhich_any()`, `fextract_any()`
  - *Approximate matching*: `fagrepl()`, `fagrep()` (faster `agrepl()`/`agrep()`)
- **String utilities**: `ftrimws()`, `fsubstr()`, `fnchar()`, `fchartr()`, `fsquish()`, `fsplit_field()`, `ascii_fold()`, `fpad()`, `freplace_tokens()` (whole-word dictionary replacement)
  - *Web text*: `url_encode()`, `url_decode()` (faster `URLencode()`/`URLdecode()`), `html_unescape()` (HTML entities, as Python's `html.unescape()`)
- **Parsing, validation & hashing**: `fparse_number()` (numbers from text such as `"$1,234.50"`), `valid_id()` (check digits: Luhn, ABN, ACN, TFN, Medicare, ISBN, EAN, IBAN), `string_shape()` (format profiles such as `"AAA 9999"`), `fhash()` (SHA-256, SHA-512, SHA-1, MD5 and XXH64, keyed HMAC for pseudonyms)
- **Clustering keys** (OpenRefine): `fingerprint()`, `ngram_fingerprint()`
- **Dates & timestamps**: `fas.Date()` (fixed-format strings, or day counts from the Excel, SAS/Stata, SPSS, MATLAB and Julian-day epochs), `fas.POSIXct()`, `format_date()`, `format_datetime()`, `date_parts()`, `format_date_parts()`
- **Phonetic codes** (blocking keys): `soundex()`, `refined_soundex()`, `nysiis()`, `cologne()`, `metaphone()`, `double_metaphone()`, `caverphone()`, `mra_encode()`/`mra_compare()`, `statcan()`, `lein()`, `phonex()`, `roger_root()`, `onca()`
- **String similarity**
  - *Jaro-Winkler*: `jaro_winkler()`, `jaro_winkler_matrix()`, `jaro_winkler_tokens()`
  - *Edit distance*: `levenshtein()` (with optional insert/delete/substitute `weights`), `osa_distance()`, `damerau_levenshtein()`, `hamming()` (+ `_similarity()`, `_matrix()`, and `levenshtein_within()` variants), `edit_ops()`, `editex()`, `editex_similarity()`
  - *Subsequence & affix* (RapidFuzz's distance module): `lcs_seq_distance()`, `lcs_seq_similarity()`, `indel_distance()`, `indel_similarity()`, `prefix_distance()`, `prefix_similarity()`, `postfix_distance()`, `postfix_similarity()`
  - *Alignment & tokens*: `lcs_str()`, `lcs_str_similarity()`, `smith_waterman()`, `monge_elkan()`
  - *Q-gram overlap*: `jaccard_index()`, `dice_coefficient()`, `tversky_index()`, `cosine_similarity()` (+ `_matrix()` variants)
  - *RapidFuzz scorers* (ports of Python's [RapidFuzz](https://github.com/rapidfuzz/RapidFuzz), the successor to fuzzywuzzy; scores match `rapidfuzz.fuzz`): `fuzz_ratio()`, `fuzz_partial_ratio()`, `fuzz_partial_ratio_alignment()`, `fuzz_token_sort_ratio()`, `fuzz_token_set_ratio()`, `fuzz_token_ratio()`, `fuzz_partial_token_sort_ratio()`, `fuzz_partial_token_set_ratio()`, `fuzz_partial_token_ratio()`, `fuzz_wratio()`, `fuzz_qratio()`, `fuzz_default_process()`, `fuzz_cdist()`, `fuzz_cpdist()`
- **Fuzzy lookup**: `fuzzy_match()`, `fuzzy_top_n()` (streaming best/top-N matches without a full matrix), RapidFuzz's `process.extract()`/`extractOne()` as `fuzz_extract()`/`fuzz_extract_one()`, and candidate search that never scores every pair: `fuzzy_within()` (everything within k edits, SymSpell's symmetric-delete index), `tfidf_top_n()`/`tfidf_match()` (TF-IDF character n-gram cosine) and `minhash_pairs()` (MinHash LSH blocking)
- **Blocking & clustering**: `block_pairs()` (multi-pass exact blocking), `sorted_neighbourhood()` (the sorted neighbourhood method), `cluster_pairs()` (entity IDs from matched pairs)

Loading the package (`library(fast.string)`) prints every exported function
as a colour-coded tree grouped by these categories, each with a one-line
description. The same index is the "Function index" section of
`?fast.string`. Suppress the banner with `options(fast.string.verbose =
FALSE)` (set before `library()`) or `suppressPackageStartupMessages()`.
Colour follows your console's support (it is off when output is piped or
`NO_COLOR` is set), and the tree falls back to ASCII on non-UTF-8 consoles.

In interactive sessions, calling `grepl()`, `grep()`, `sub()`, `gsub()`,
`trimws()`, `substr()`, `nchar()`, or `chartr()` prints a one-time-per-function
reminder pointing at the fast.string equivalent — the base function still
runs and returns the same result either way. Turn it off with
`options(fast.string.reminders = FALSE)`.

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

# 3. Block on more than one phonetic key and union the candidates. Accented
#    letters are folded first, so "Müller" and "Mueller" share a key, and
#    cologne() also groups German spellings that soundex() keeps apart.
soundex(c("Müller", "Mueller"))           #> "M460"   "M460"
soundex(c("Philipp", "Filip"))            #> "P410"   "F410"   -- split
cologne(c("Philipp", "Filip"))            #> "351"    "351"    -- merged

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
