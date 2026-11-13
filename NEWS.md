# fast.string 0.3.0

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

### Choosing a threshold, and recording the judgement

A linkage ends in a decision, and the decision has never been the part this
package helped with. It could score a pair to four decimal places and then
leave the two questions that matter --- *where does the cut-off go?* and *was
this particular pair really the same person?* --- to a hand-picked number in a
script, with no record of who chose it or why. These functions close that gap.

* `threshold_sweep()` reports, for every candidate cut-off, what keeping the
  pairs above it would actually produce: the pairs kept, the records linked on
  each side, how many of those links are a clean one-to-one, and how many
  records are left holding two links or more. Reading it downwards, the point
  where the conflict counts start climbing is where the cut has begun admitting
  pairs that contradict pairs already accepted --- and it needs no known truth.
  Given the posteriors from `match_weights()` it also estimates precision and
  recall by summing probabilities rather than counting, as `Splink` does, and
  given a truth it reports the observed figures beside the estimates so the
  estimates themselves can be checked.
* `review_order()` orders pairs by how undecided they are, so that an hour of
  manual review is spent on the pairs where a judgement changes the outcome
  rather than on the obvious ones at either end.
* `summary()` and `plot()` methods now describe an `rLink()` result: the pairs,
  the records reached, the contribution of each blocking pass, and the
  distribution of every score. A linkage that is going to work shows two humps
  in `plot()`, and the dip between them is where the threshold belongs; a
  single hump means the comparator is not separating the two populations, which
  is worth knowing before tuning a cut-off that cannot exist. `plot()` on a
  sweep draws the curve.

The sweep is exact at every distinct score and costs one pass over the pairs,
not one per threshold: a quarter of a second for a million pairs against all
999,859 of their distinct scores. It is therefore cheap enough to recompute
while a slider moves, which is what the next part needs.

* `rLink_inspect()` opens the candidate pairs in a browser --- a reactable
  table with the score columns filled by a colour gradient, one slider per
  score, and three buttons and a comment box on every row. Pairs failing the
  current cut are dimmed rather than hidden, because a false negative you
  cannot see is a false negative you will not find. Every count it reports is
  computed over *all* the pairs, not only the ones on screen.
* The JSON file it writes is the point of the exercise: it records who
  reviewed what and when, the thresholds they accepted and the reason they
  typed for each, every per-pair verdict with its comment, and fingerprints of
  the data as it stood. Its decision log is **append-only**, so a second
  reviewer's session is added to it rather than written over it --- which is
  what makes disagreements between two reviewers visible, and what stops a QA
  pass quietly erasing the work it is meant to be checking.

`shiny`, `reactable`, `jsonlite`, `bslib` and `sortable` are suggested, not
required; only `rLink_inspect()` needs them, and it names all the missing ones
in a single message.

```r
pairs <- rLink(enrol, claims,
               block_1 = list(list(soundex(surname), dob)),
               compare = list(sn = jaro_winkler(surname),
                              gn = jaro_winkler(given_name)))

# Where should the cut go?
sweep <- threshold_sweep(pairs, "sn")
plot(sweep)

# Decide it, and record the decision
out <- rLink_inspect(pairs)

# What the next person runs
d <- read_decisions("rLink_decisions.json", x = pairs)
d$disagreements
pairs <- apply_decisions(pairs, d)
```

### The review, and getting the decision back out

A review is only worth the trouble if the next run reproduces it. These
functions make the file the app writes into something a second person can read,
replay and apply, and name themselves after what it holds:

* `read_decisions()` reads the file. Printing the result ends with the two
  lines to paste into a script, so a review stops being a note in a drawer.
* `apply_decisions()` adds `link` and `link_reason` columns to the pairs:
  `link` for whether each pair is in scope, `link_reason` for which rule
  decided it --- the accepted threshold, or the person who overrode it. Doing
  this by hand is where a review goes wrong, because the threshold has to be
  applied with the operator it was recorded with and the pairs that have since
  vanished have to be noticed rather than dropped in silence.
* `rLink_inspect()` now hands the whole review back when you close it, so
  `out <- rLink_inspect(pairs)` captures it and `out` prints the code.

```r
out <- rLink_inspect(pairs)      # choose a cut, mark the exceptions, finish
out                              # prints the R that reproduces and applies it
pairs <- apply_decisions(pairs, "rLink_decisions.json")
```

### A review app you can work in all afternoon

* The sidebar and every panel below the table collapse, so the table gets the
  width. The threshold sweep, the score distributions, the effect of the cut
  and the session log are all panels now rather than permanent furniture.
* **One table, grouped into in scope and out of scope**, with a pinned scope
  column, so the consequence of the cut is the first thing visible. The blocks
  follow the threshold and regroup when it moves; a pair judged by hand turns
  over in its scope cell immediately and says it was judged, because
  re-rendering the table on every click would cost the reviewer their sort,
  filter and page position.
* **Columns are dragged** between a Shown and a Hidden list to reorder and
  remove them, and are resizable in the table. reactable cannot drag a table
  header, so this is the honest version of that.
* **Ctrl-click marks rows and shift-click marks a range**, and the marked set
  can be judged in one action --- which is how a block of obvious links is
  dealt with without two hundred clicks. Marks are keyed on `pair_id`, not on a
  row position, so they survive sorting, filtering and paging; reactable's own
  selection indices do not, because they mean position in the data.
* **Choosing the cut got four ways in.** A numeric box beside every slider, so
  an exact 0.8832 can be typed rather than hunted for. **Suggest a cut**, which
  reads the sweep for the last cut-off that buys more clean one-to-one links
  than contested records, moves the sliders there, and says in a sentence why.
  Clicking a row of the sweep table moves the cut to it. And beyond `all` and
  `any`, a rule such as `sn >= 0.9 | (sn >= 0.8 & gn >= 0.95)` can be written
  out; it is checked as you type, stored with the threshold, and re-applied by
  `apply_decisions()`.
* **Finish and close** stops the app and hands back the decisions.

A rule expression is checked against a whitelist of comparison and logical
operators before it runs, and evaluated against the score columns in an
environment holding nothing else --- a decisions file may arrive from someone
else, and `eval()` on text from a file is otherwise a way to run code.

### A judgement in `rLink_inspect()` now beats the threshold

The app let you mark a pair a link or not a link, and `apply_decisions()`
resolved those verdicts over the recorded cut --- but the app's own numbers
described the cut alone. Marking an out-of-scope pair a link turned its scope
cell over and changed nothing else: the header still counted it out, the
in scope / out of scope blocks still had it in the wrong one, "What this cut
does" still ignored it, and with out-of-scope rows hidden it vanished the
moment it was judged. The scope the tool reported was not the scope the tool
produced.

It is now the same resolution everywhere, and the same one `apply_decisions()`
performs: the cut decides the pairs nobody looked at, and a per-pair verdict
overrides it in either direction. The count in the header is the count the
next run gets.

Because an override is a decision somebody has to be able to defend, it is
reported as one rather than absorbed into the total:

* the scope cell says **forced in** or **forced out** where a judgement
  contradicts the cut, and **confirmed** where it agrees --- the old label
  said "judged" for both, which buried the overrides among the
  confirmations;
* those rows carry a marker down the left edge, so they are findable by eye
  on a page of several hundred;
* the header breaks the total into what the cut gave and what was forced
  each way;
* **What this cut does** shows the cut's counts beside the reviewed ones;
* **Show** gains *overriding the cut*, which lists exactly the pairs where
  the reviewer and the cut disagree --- the page to re-read before signing
  off --- along with *in scope* and *out of scope*.

### `rLink_inspect()`: the pair, the keyboard, and any score's sweep

* **Clicking a row shows the pair.** The sidebar now puts the clicked pair
  beside itself field by field: the value each input held on each side, and
  the score it earned. This is what a clerical decision is actually made on
  --- `SMITH / JOHN / 1980-01-02` against `SMYTHE / JON / 1980-01-02` --- and
  a row of scores is a summary of it rather than a substitute. A missing
  value and an empty string are drawn differently, because one was never
  collected and the other was collected blank. The same three verdict
  buttons sit under it, so a pair can be judged where it is read.

  (The browser had been sending the click since the app was written and
  nothing listened for it, so the panel's own helper was unreachable.)
* **`y`, `n` and `u` judge from the keyboard** --- the marked rows, or, with
  none marked, the row last clicked. Clerical review is thousands of small
  verdicts, and the hand leaving the keyboard for the mouse on each one is
  most of the cost. Ignored while a text box has focus, so typing `n` in a
  comment stays typing. The events are routed through the same recorder as a
  click, so the audit trail cannot tell them apart.
* **The threshold sweep is no longer hardwired to the first score.** A
  multi-score linkage can now sweep, suggest a cut for, and re-rank by any
  of its scores, chosen in the sweep panel. The first score is usually the
  one whose cut is obvious; it is the second and third where the sweep was
  worth having and could not be reached.

### `rLink()` names its arguments in pairs

* `block` is now `block_1`, which pairs it with the `block_2` that was already
  there. `block` keeps working, and every error message names whichever of the
  two the caller actually wrote.
* `pair_id` builds the `pair_id` column from a record identifier on each side
  --- `"frame_id"` by default, or `c("frame_id", "person_id")` where the two
  inputs name it differently --- instead of numbering the pairs. A row number
  describes one particular run: re-block the same two files and pair 412 is a
  different pair, so a judgement filed against it is silently about something
  else. An identifier built from the records does not move, which is what lets
  a saved review survive a re-run. An identifier containing `"|"` is an error
  rather than an ambiguous key, and identifiers that are missing or repeated
  raise a warning saying how many. A default `frame_id` that is not there
  falls back to numbering the pairs, so nothing that worked before changes.
* The result is now also classed `fs_pairs`, which is what `summary()` and
  `plot()` dispatch on. `data.table` carries the class and the `timings` and
  `compare` attributes through `[`, `order()`, `head()` and `:=`, so they all
  survive a filter on the scores.
* `attr(out, "compare")` gains a `direction` column recording which end of each
  score means "a match". Under the default `normalise = TRUE` that is always
  `"higher"`, because every comparator's output is converted so 1 is an exact
  match. Under `normalise = FALSE` it is the only record of the fact, and
  without it `threshold_sweep()` and `apply_decisions()` would cut an unnormalised
  edit distance upside down.

### `rLink()`: filter while scoring, and comparators of numbers and dates

* **`filter`** keeps only the scored pairs a rule says to keep, inside
  `rLink()`: `filter = sn >= 0.85 & (gn >= 0.8 | dob_1 == dob_2)`. The rule can
  name any score, `pass`, and any column of either input under its suffix. It
  returns the same table as scoring everything and subsetting it, and is much
  cheaper to get: candidates are generated, scored and thinned a slice of the
  records at a time, so the whole candidate set never exists at once, and
  `pair_id` and the carried columns are built only for the pairs that are
  left. It also lifts the limit of 2^31 candidate pairs --- only the pairs kept
  have to fit in one table. `attr(out, "filter")` records the rule, the
  candidates scored and the pairs kept.
* **`numeric_compare()` and `date_compare()` now work in `compare`.** They are
  handed the column as it is stored. A comparator of your own asks for the
  same by wrapping its column in `I()`, as in `my_cmp(I(age))`. An unbounded
  distance between numbers or dates is left on its own scale and recorded as
  lower-is-a-match, so `threshold_sweep()` and the review app cut it the right
  way round.
* `attr(out, "timings")` reports `pair_id` as its own stage. On a large
  unfiltered result it is most of the run, and now it can be seen.
* `attr(out, "dedupe")` says whether the pairs are within one file.

### `rLink()`: see the blocking, mix the methods, stage the runs

Blocking decides what a linkage costs and what it can find, and `rLink()`
said almost nothing about it: an estimate printed under `verbose`, and a
warning when a block was skipped. These changes are about everything around
the scoring.

* **A blocking report on every result**, `attr(out, "blocking")`, which
  prints as a table. For each pass: the key as written, the records with no
  key on each side, the distinct keys, the largest block, the blocks
  `max_block_size` skipped, the pairs the pass was expected to give, the
  candidates it was the first to find and how many of those are in the
  result. Beside it, the five keys of each pass that make the most pairs ---
  where a placeholder such as `"UNKNOWN"` that belongs in `na_strings`
  shows up at once. It is read off the integer codes the keys have already
  become, so it costs a `tabulate()` per pass.
* **`dry_run = TRUE`** returns that report and stops. The keys are built and
  every argument checked, and not one pair is generated, so a call can be
  tried against the full files before paying for it.
* **`max_pairs`** stops a run expected to give more candidates than that,
  before it generates any, naming the pass and the block responsible.
* **Passes can be named**, `block_1 = list(name = list(soundex(surname),
  dob), born = dob)`, and the report and `summary()` then call them that.
* **A method per pass.** `block_method` and `window` each take one value or
  one per pass, so an exact pass and a sorted neighbourhood run in one call:
  `block_method = c("exact", "sorted_neighbourhood")`. The pair generator
  always took a window per pass; only the R side insisted on one.
* **`filter` no longer needs `compare`.** `filter = sex_1 == sex_2`, or
  `abs(yob_1 - yob_2) <= 1`, is a condition on the candidates that no
  blocking key could express.
* **The part of a rule that reads no score is applied before scoring.** A
  rule is taken apart at its top-level `&`s, and in
  `sn >= 0.85 & sex_1 == sex_2` the comparators only see the pairs whose sex
  agrees. The result is the same table; `attr(out, "filter")` gains
  `before_scoring` and `n_scored`.
* **`exclude`** leaves out pairs already settled, before they are scored.
  Given as `pair_id`s they are found again through the identifier columns,
  so they survive the files being re-ordered or changed between runs; given
  as a table they are read as row numbers, and a table made from files of a
  different size is refused rather than applied to the wrong rows.
* **`rLink_unmatched()`** returns the records no pair reaches --- the residue
  a second, looser run is for.
* `attr(out, "n_records")` records how many records each input had. With it
  `summary()` reports the records reached *of how many* and the reduction
  ratio, and `blocking_eval()` no longer needs `n_1` and `n_2` for an
  `rLink()` result.
* Under `verbose`, a run that a `filter` cuts into slices shows a progress
  bar.

Three things `rLink()` did silently, it now says:

* `max_block_size` was dropped without a word under the sorted
  neighbourhood. It applies to exact passes, including those of a mixed
  run, and warns when there are none for it to act on.
* The pairs each pass was expected to give were counted as if every pass
  were exact, so the estimate printed for a sorted neighbourhood was too
  low. It is now the number that pass gives when run alone.
* The same data given as both `data_1` and `data_2` pairs every record with
  itself, which is a warning unless `block_2` is there to say why --- one file
  blocked on two different fields, a surname against a maiden name.

### `rLink()`: faster scoring, and a `filter` that scores less

Measured on two files of 500,000 records (16 threads, an installed build);
every run returns exactly the table it did before.

* **Comparators read each record once.** A pair is two record numbers, and a
  comparator used to be handed `surname[index_1]` and `surname[index_2]` built
  out in full, then resolve every one of those strings on the calling thread
  before a worker could start. This package's comparators are now handed the
  per-record column and the indices as one vector that is never built, and
  resolve each record's string once. One exact pass of 8.9 million pairs
  with two `jaro_winkler()` scores and nothing carried goes from 1.06s to
  0.50s; a sorted neighbourhood of 64 million pairs with the same two from
  7.7s to 3.2s. A comparator of your own is handed ordinary character
  vectors, as before.
* **Each `&` condition of a `filter` is applied as soon as its scores exist.**
  In `filter = sn >= 0.85 & gn >= 0.8`, `gn` is scored only for the pairs
  `sn` kept, and a comparator the rule does not name is scored only for the
  pairs the whole rule kept. Comparators run in the order the rule asks for
  them. One exact pass of 8.9 million candidates with four comparators and
  that rule: 2.1s to 0.7s. `attr(out, "compare")` gains `n_scored`, the pairs
  each comparator was run on. A condition that drops less than a third of
  the pairs is held and settled with the next, since dropping pairs is
  itself a pass over everything held for them.
* An edit distance is put on the 0--1 scale in one parallel pass instead of
  six passes in R.
* A run that builds `pair_id` for millions of unfiltered pairs is no faster:
  it is still the strings, as *Pair identifiers* in `?rLink` says.
* `pairs` takes an earlier `rLink()` result, so a costly comparator can be
  run over only the pairs a cheap run kept.
* `max_block_size` takes one value per pass, as `block_method` and `window`
  do.

### `rLink_inspect()`: a long review stays quick, and a second look

* **Saving no longer slows down as the review grows.** The app saves two
  seconds after anything changes, and each save re-encoded every decision the
  file had ever held: one second at 1,000 decisions, five at 5,000 and over
  twenty at 20,000, with the app frozen meanwhile. A decision never changes
  once filed, so each is now encoded once and its text reused, and the
  events of the two sides of a merge are compared as vectors. A save at
  20,000 decisions takes about a fifth of a second. The file is the same,
  byte for byte.
* Reading a long review back is faster too: disagreements are found over the
  whole table at once, and a reviewer's own verdicts are restored as vectors.
* **A second look.** When the file holds another reviewer's verdicts,
  *Rebuild the queue* offers *the pairs another reviewer judged*, the header
  counts how many of them have been gone over and how many differ, and *Show*
  gains *judged by another reviewer* and *where I and another reviewer
  disagree*. Nothing of theirs is filled in.
* A `pair_id` holding a double quote was written unescaped into the table,
  and a verdict on that row was dropped without a word. Identifiers are now
  escaped.

### `rLink_weights()`: match weights in one call

`rLink_weights(pairs)` cuts each score into agreement levels, fits the
Fellegi-Sunter model by EM and adds `weight` and `probability` columns --- the
four steps of the pipeline above, with the bookkeeping between them done.
Levels are chosen automatically or given per score; `model =` applies an
earlier fit, so a later run is weighed the same way; `tf =` makes agreement on
a rare value worth more than agreement on a common one. The new columns are
registered as scores, so `rLink_inspect()` thresholds the probability alone
and shows the field scores as the evidence behind it.

Its help says plainly what the model needs: **block on something you are not
also scoring**. Blocked on surname and date of birth with both compared, the
two populations EM finds are the two blocking passes.

### `rLink_inspect()`: a review you can do from the keyboard

* **The pair sits beside the table**, not in a panel of the settings sidebar.
  It shows the compared fields with what differs between the two values
  marked, every other column that was carried, and the other candidates for
  the same two records, each a click away. With weights it also shows the bits
  each field contributed.
* **Keys.** `j`/`k` or the arrows move, `y`/`n` judge and move on to the next
  undecided pair, `u` un-judges, `x` marks, `c` comments, `1`--`9` file a stock
  reason (`reasons = c("married name", "twin")`), `Ctrl+Z` takes the last
  action back, `?` lists them. Undo is recorded as new events, so the log
  stays append-only.
* **A review band.** `band = c(0.75, 0.95)`, or two boxes in the app, names
  the scores a cut cannot be trusted on. The header counts the pairs in it
  and how many are left.
* **The queue follows the cut.** It used to be fixed when the review opened,
  around the cut it opened with. *Rebuild the queue* draws it again around
  the cut as it stands, as the review band, or as an audit sample.
* **An audit sample.** A fixed, repeatable sample of the pairs the cut lets
  through; judge it and the header estimates the false-link rate with a 95%
  interval. The band and the audit are filed with a recorded threshold and
  come back in `read_decisions()$thresholds`.
* Moving a cut no longer redraws the table, so the sort, the filters and the
  page are kept.

## Bug fixes

* **`numeric_compare()` inside `rLink(compare = )` was an error**, and
  `date_compare()` there was reported as "direction not detected". `rLink()`
  turned every compared column into text before handing it over, so
  `numeric_compare()` refused it, `date_compare()` parsed each string back
  into the date it had just been, and the calibration probe --- two strings
  of letters --- told `rLink()` nothing about either. See above.
* **`rLink()` with a `pair_id` column and no candidate pairs was an error.**
  `paste0()` of nothing and a separator is the separator, once, so an empty
  result was handed one identifier and `setDT()` refused the ragged table.
* **The verdict buttons in `rLink_inspect()` were off the right-hand edge.**
  reactable draws columns in the order the data has them, and the `link?`
  and comment columns were last --- behind every carried column, on any table
  wider than the window. They are now first.
* **A grouped table drew every row of a group, whatever the page size.** With
  the default grouping by scope, two groups were two rows to the pager and
  all 20,000 pairs were one page. Sub-rows are now paginated.
* **The threshold sweep sent one row per distinct score to the browser.** On
  a continuous score over millions of pairs that is millions of rows to draw
  a curve. It is now swept at no more than 200 cut-offs.
* **Marks survived being judged from the keyboard.** `y` on a marked set left
  it marked, so the next key meant for one row was applied to all of them
  again. They are cleared, as the buttons always cleared them.
* **A comment being typed lost the cursor** when the debounced save redrew
  the box it was being typed into.
* **The sidebar of `rLink_inspect()` was unstyled.** Its CSS was scoped to a
  wrapper the sidebar is not inside, so the font size, the compact spacing
  and the note boxes never applied to the controls they were written for.
* **A review session's `started` time was its last save.** The run header
  was restamped on every write.
* **Incremental builds no longer link stale object files.** `src/Makevars`
  and `src/Makevars.win` now generate header dependencies (`-MMD -MP`) and
  include them, so editing one of the headers in `src/` rebuilds the objects
  that include it. R's shlib rules only know that an object depends on its
  own `.cpp`, so before this an edit to `string_snapshot.h`,
  `sequence_input.h` or `pairwise_worker.h` --- all of which define structs
  shared across twenty-odd translation units --- reported "Nothing to be done
  for 'all'" and relinked the old objects. The result was a DLL in which some
  objects used the old struct layout and some the new: undefined behaviour,
  whose symptom is a corrupted heap and an R session that exits with no
  message, minutes or hours after the build. Nothing warned, and no clean
  build (`R CMD build`, `R CMD check`, installing from a tarball) was ever
  affected --- only repeated installs from a working directory, which is what
  a developer does all day. Switching `FAST_STRING_TINYTHREAD` still needs
  `src/*.o` deleted by hand, and `src/Makevars.win` now says so.
* **`em_linkage()` could write past the end of its parameter vectors.** Its
  C++ kernel indexes `m` and `u` as `offset[comparison] + level` and did not
  check the level against the number of levels declared for that comparison.
  `agreement_patterns()` keeps that number in an attribute, so any edit to
  the returned table that raised a level --- `p$surname[1] <- 3L` on a
  two-level comparison --- left the attribute describing data it no longer
  matched, and the M step then accumulated into heap memory belonging to
  something else. Levels are now validated on both sides: `em_linkage()`
  takes the declared count as a floor and re-reads the table, which is the
  rule `compare_levels()` input already went through, and the kernel refuses
  an out-of-range level rather than trusting it. `match_weights()` already
  checked this, which is what the inconsistency was.
* `rLink_inspect(threshold = ...)` now records **every** cut in force, not
  only the scores the caller named. A score left at its default cut was in
  force on screen but absent from the file, so `apply_decisions()` resolved
  the recorded threshold over a shorter list of cuts and brought pairs into
  scope that the review had excluded. The effect counts stored beside the
  cuts were already computed over all of them, so the two halves of one
  event disagreed.
* `rLink_inspect(order_by = "score")` now orders a score where low is the
  match ascending rather than descending. It previously put the least
  match-like pairs at the top of the review queue and pushed the best ones
  past `max_pairs`, for any comparator used with `normalise = FALSE`.
* Reopening a decisions file no longer fills the screen with another
  reviewer's verdicts. `read_decisions()$decisions` holds one row per
  reviewer *and* pair, and all of it was being seeded as the current
  reviewer's own state --- with whichever reviewer came last in the frame
  winning each pair. A QA pass, which is the main reason the file is shared,
  therefore opened pre-answered. Your own verdicts are restored; everyone
  else's are shown against the pair and attributed, so agreeing still takes
  a click and still records as your own judgement.
* `read_decisions(x = )$n_undecided` now counts the pairs in `x` that nobody
  has reached a verdict on, which is what it is documented to count. It
  previously counted the pairs that appear nowhere in the decision log, so a
  pair somebody had opened and left undecided -- or marked and then reverted
  -- counted as judged. That understated the work remaining in exactly the
  case where it is asked: a second pass over a queue somebody has already
  been through once.
* `cluster_pairs()` now reads a data frame's `index_1`/`index_2` columns, or
  its `x_index`/`y_index`, before falling back to the first two columns ---
  the rule `select_greedy()`, `linkage_eval()` and `blocking_eval()` already
  follow. Passing an `rLink()` result straight in previously clustered
  `pair_id` against `index_1`, because `pair_id` is the first column.
* A stray `// [[Rcpp::export]]` in `src/text_tools.cpp` was attached to the
  internal `already_cased_ascii()` helper, which made `Rcpp::compileAttributes()`
  emit an entry point that did not compile. The tag is gone; nothing about
  the package's behaviour changes.
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
* `rLink()` again, measured with `tools/bench_rlink.R` on two files of 500,000
  records, each scenario in an R process of its own:

  | scenario | pairs | before | after |
  |---|---:|---:|---:|
  | one exact pass, every column carried | 8.9M | 19.2s | 9.4s |
  | three passes of two-term keys | 1.8M | 8.4s | 3.6s |
  | deduplicating one file | 20.6M | 37.0s | 20.3s |
  | one exact pass, keeping close names (`filter`) | 0.66M of 8.9M | 19.3s | 2.0s |
  | `date_compare()` on a `Date` column | 1.6M | 23.2s | 2.1s |
  | nothing carried, `pair_id = NULL` | 8.9M | 1.2s | 1.1s |

  * `filter` scores before it builds anything, and builds `pair_id` and the
    carried columns for the survivors alone. Peak memory in that row fell
    from 2.7 Gb to 0.85 Gb.
  * A typed comparator is no longer handed text to parse back.
  * A key of several terms matched exactly is numbered rather than pasted:
    each term's distinct values are coded over both files and the codes
    combined, so no string is made per record. On two files of three million
    records, three such passes went from 12.1s of keys and 6.8s of coding them
    to 5.7s and 1.9s.
  * The carried columns are gathered before `pair_id` is built rather than
    after, which alone took that stage from 4.4s to 1.3s on 8.9M pairs: each
    gather can trigger a collection, and a collection is cheaper before ten
    million new strings exist than after.
  * A single exact pass counts its pairs and then writes them straight into
    the result in parallel, instead of growing a buffer per chunk and copying
    out of it.

  **What is left is `pair_id`.** Every pair gets a string of its own and R
  takes about a second to make a million new strings, whoever asks: 6.8s of
  the 9.4s above, and 52s of 81s on 53 million pairs. A hand-written C++
  builder was measured at within 15% of `paste0()`, so it was not kept.
  `filter` avoids the cost for the pairs it drops and `pair_id = NULL` avoids
  it altogether.
* `rLink_inspect()` on two million pairs:

  | | before | after |
  |---|---:|---:|
  | opening the review | 20.4s | 1.7s |
  | 500 marked rows judged at once | 12.3s | 0.1s |
  | twenty verdicts, one at a time | 2.9s | 1.2s |
  | moving a cut | 0.9s | 0.2s |

  Opening was the fingerprint of the pair set, which made an R string per
  pair and a factor to split them by; it is now computed in C++ over the same
  bytes and gives the same digest, so an existing decisions file still reads
  as unchanged. Verdicts are held as one code per pair rather than a named
  list copied on every click.

## Documentation

* `rLink()`'s `compare` has always been able to compare columns named
  differently in the two inputs, by writing
  `jaro_winkler(family_name | surname)`: the expression to the left of the
  `|` is evaluated against `data_1` and the one to its right against
  `data_2`, and either side may be any expression rather than only a bare
  column name. That is to `compare` what `block_2` is to `block_1`, and it was
  buried in a subordinate clause of a single sentence. It now has its own
  paragraph, a line in the worked example and a mention in `@param compare`.
* `rLink()` gains a *Pair identifiers* section, covering what `pair_id` is
  built from and why an identifier that comes from the records rather than
  from the row order is what lets a saved review outlive the run that made it.
* The linkage chapters are rebuilt around `rLink()` and around two
  realistic files of people whose names, dates of birth, addresses, sex and
  postcodes are each recorded differently, with a known truth so that
  blocking recall, precision and recall are measured rather than asserted.
  `docs/_people.R` generates them.
* A new *Performance* section in the troubleshooting chapter says which
  functions are **not** faster than base R (`fsubstr()` on positive
  positions, `fstrsplit()`, fixed-string matching on short values) and why,
  and how to read `attr(result, "timings")` when `rLink()` is slow.
