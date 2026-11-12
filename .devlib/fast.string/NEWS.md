# fast.string (development version)

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
