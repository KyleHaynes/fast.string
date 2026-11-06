# fast.string (development version)

## New features

### A probabilistic linkage model

The package could find candidate pairs and score them, but it left the
question those scores exist to answer --- *is this pair the same person?* ---
to the caller and a hand-picked threshold. These functions close that gap with
the Fellegi-Sunter model that `fastLink`, `RecordLinkage` and `Splink` are
each built on, fitted by EM so that no training data is needed:

* `compare_levels()` cuts a comparator score into ordered agreement levels,
  which is what the model reads. Two levels is the classic agree/disagree;
  three or four let a near match count for something without counting as much
  as an exact one.
* `agreement_patterns()` collapses scored pairs into the distinct patterns of
  agreement they show, with a count of each. Millions of pairs compared on
  half a dozen fields reduce to a few thousand patterns, and every calculation
  after this runs over the patterns.
* `em_linkage()` fits the model: for every comparison and level, the chance a
  true match reaches it (`m`, how reliably the field is recorded) and the
  chance a non-match does (`u`, how discriminating the field is). A comparison
  a pair has no value for is left out of that pair's weight rather than read
  as a disagreement.
* `match_weights()` applies a fitted model, giving each pair its match weight
  in bits (`log2(m / u)` summed over its comparisons) and its match
  probability.
* `value_frequency()` and `tf_adjust()` make agreement on a rare value count
  for more than agreement on a common one, which is the single largest
  accuracy gain available once the model is fitted. `Splink` exposes the same
  idea as its term frequency adjustments.
* `select_greedy()` resolves scored pairs into links, taking the best weight
  first and keeping each record's allowance of links --- one each way, by
  default, for a one-to-one linkage.
* `linkage_eval()` reports precision, recall and F-measure against a known
  truth, and `blocking_eval()` the reduction ratio, pairs completeness and
  pairs quality of the blocking that produced the candidates. Both read the
  `index_1`/`index_2` columns of an `rLink()` result, or the
  `x_index`/`y_index` of a `block_pairs()` one, so a result can be passed
  straight in.

The whole pipeline now runs without leaving the package:

``` r
pairs <- rLink(enrol, claims,
               block   = list(list(soundex(surname), dob)),
               compare = list(sn = jaro_winkler(surname),
                              gn = jaro_winkler(given_name),
                              db = date_compare(dob)))

levels <- data.frame(
    sn = compare_levels(pairs$sn, c(0.85, 0.94, 1)),
    gn = compare_levels(pairs$gn, c(0.85, 0.94, 1)),
    db = compare_levels(pairs$db, c(0.5, 1))
)

fit    <- em_linkage(levels)      # m and u per field and level
scored <- match_weights(fit, levels)
keep   <- scored$weight > 8
links  <- pairs[keep, ][select_greedy(pairs[keep, ],
                                      weight = scored$weight[keep]), ]
linkage_eval(links, truth)
```

### Comparators for the fields that are not text

A linkage is rarely decided on names alone, and until now the package could
only compare strings.

* `numeric_compare()` turns a difference into an agreement score through any
  of five curves --- step, linear, squared, exponential and Gaussian --- the
  five of the `Numeric` comparator in Python's `recordlinkage`.
* `date_compare()` scores dates on the difference in days, but first
  recognises the two errors that dates in administrative data actually suffer
  from and that a difference in days reads as a wild disagreement: the day and
  month exchanged, and a mistyped year (one digit wrong, or two transposed).
* `geo_distance()` gives the haversine great-circle distance between two
  WGS84 positions, so that geocoded addresses can be compared as a distance
  rather than as text.

### Four more string measures, and a phonetic hash

Drawn from the parts of the wider string-matching ecosystem the package had
not yet covered:

* `ratcliff_obershelp()` and `ratcliff_obershelp_distance()`: gestalt pattern
  matching, which repeatedly takes the longest common substring and does the
  same again either side of it. It rewards long runs of agreement wherever
  they have moved to, which suits names and addresses whose parts have been
  reordered. This is the measure behind Python's
  `difflib.SequenceMatcher.ratio()`.
* `bag_distance()` and `bag_similarity()`: the multiset difference of two
  strings' characters, a lower bound on the Levenshtein distance that costs a
  sort rather than a dynamic-programming matrix (Bartolini, Ciaccia and
  Patella, 2002).
* `needleman_wunsch()` and `needleman_wunsch_similarity()`: global alignment,
  which must consume both strings in full --- the whole-field counterpart to
  the local `smith_waterman()`.
* `overlap_coefficient()` and `overlap_matrix()`: the Szymkiewicz-Simpson
  q-gram overlap, the shared q-grams over the smaller of the two sets, so a
  string wholly contained in another scores 1 where `jaccard_index()` would
  penalise it for the extra q-grams. The measure to reach for when one field
  is an abbreviation or a truncation of the other.
* `eudex()` and `eudex_distance()`, ported from the Rust crate `eudex`: a
  locality-sensitive phonetic hash, whose *bits* encode how each sound is
  articulated, so that names which sound alike differ in few bits rather than
  either matching a code or not. `eudex()` is a blocking key; `eudex_distance()`
  is a phonetic comparator that weights a difference in the first letter 34
  times as heavily as one in the last sound recorded.

## Bug fixes

* A stray `// [[Rcpp::export]]` in `src/text_tools.cpp` was attached to the
  internal `already_cased_ascii()` helper, which made `Rcpp::compileAttributes()`
  emit an entry point that did not compile. The tag is gone; nothing about
  the package's behaviour changes.


## Bug fixes

* `roger_root()` now reproduces `phonics::rogerroot()` on every input, as
  its documentation says it does. It previously applied its rule table as a
  sequence of whole-string replacements, which is not the same thing as the
  single left-to-right pass the algorithm specifies: at each position the
  longest rule starting there wins, so `"SCHMIDT"` is read as `SCH` rather
  than as the `CH` inside it. Two further differences went with it --- the
  two-letter rule is `DC`, not `DG`, and a code of all zeros is a real code
  (`"00000"`) rather than `""`, which is the `phonex()` rule, not this one.
  Across 59,511 generated names the two implementations disagreed on 28% of
  inputs; they now agree on all of them, for every `max_length`. **Codes
  produced by earlier versions will differ, so blocking keys stored from a
  previous release should be rebuilt.**

## Performance

Measured on 16 cores. Ratios are against the previous release on the same
machine and data.

* `roger_root()` is about 6x faster in parallel and 100x faster
  single-threaded. The rewrite above replaced 29 allocating passes over
  every name with one pass and no allocation.
* `caverphone()` is about 4x faster in parallel and 15x faster
  single-threaded. Its roughly fifty rule steps now test whether they have
  anything to do before touching the string, rewrite in place when the
  replacement is the same width, and share two buffers across the whole
  vector instead of allocating per step.
* `editex()` and `editex_similarity()` are about 1.5x faster in parallel and
  15x faster single-threaded. The per-column insertion cost and letter group
  no longer get recomputed in every cell of the dynamic-programming matrix.
* `fupper()` and `flower()` are up to 4x faster on text that is already in
  the target case --- the normal state of a cleaned record-linkage field,
  and of every phonetic code and fixed-format date this package produces.
  Such a string is now recognised with one scan instead of being rebuilt and
  compared.
* `rLink()` is about 2.5x faster end to end on a realistic linkage (two
  200,000-record files, three blocking passes, four comparators):
  * a `Date` blocking key or comparison column is rendered with
    `format_date()` rather than `as.character()`, which is 14x faster per
    column;
  * a blocking term written into more than one pass --- `soundex(surname)`
    in two of them --- is now evaluated once per side rather than once per
    pass;
  * `na_strings` of length one is applied with a comparison rather than a
    hash lookup, and a date key skips the case fold and the separator scan
    it cannot need.
* `block_pairs()` is about 1.5x faster. Exact blocking no longer sorts the
  distinct keys, since only equality matters to it; both sides are coded
  against one hash table instead of one each; and a single exact pass now
  writes its pairs straight out, as they are already in order and cannot
  repeat.

## Documentation

* The linkage chapters are rebuilt around `rLink()` and around two
  realistic files of people whose names, dates of birth, addresses, sex and
  postcodes are each recorded differently, with a known truth so that
  blocking recall, precision and recall are measured rather than asserted.
  `docs/_people.R` generates them.
* A new *Performance* section in the troubleshooting chapter says which
  functions are **not** faster than base R (`fsubstr()` on positive
  positions, `fstrsplit()`, fixed-string matching on short values) and why,
  and how to read `attr(result, "timings")` when `rLink()` is slow.
