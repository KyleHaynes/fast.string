#' Link or deduplicate records
#'
#' Finds candidate record pairs by blocking, scores each pair with any number
#' of string comparators, and returns one table of pairs and their scores. It
#' is a single call over [block_pairs()] (or [sorted_neighbourhood()]) and the
#' comparison functions in this package.
#'
#' Blocking keys and comparison columns are written as ordinary R expressions
#' evaluated against the data, so a phonetic code, a substring or any other
#' transformation goes in the call itself rather than into a column prepared
#' beforehand:
#'
#' ```
#' block_1 = list(list(soundex(surname), dob), fsubstr(postcode, 1, 2))
#' ```
#'
#' The outer `list()` separates blocking *passes*: a pair is a candidate if it
#' agrees on the key of any one pass, and each pair is reported once, with
#' `pass` recording the first pass that found it. Within a pass, `list()` or
#' `c()` separates the *terms* joined into one key, so the pass above requires
#' the same Soundex of surname *and* the same date of birth. Passes may have
#' different numbers of terms. A single expression, or a `c()` of column
#' names, is one pass. A pass may be given a name, as in
#' `list(name = list(soundex(surname), dob), born = dob)`, and the blocking
#' report and [summary()] then call it that.
#'
#' Expressions see the data first and the calling environment behind it, so
#' `soundex(surname, nthreads = my_threads)` resolves `my_threads` from the
#' caller. `.data$x` and `.env$x` disambiguate a column that shares a name
#' with a local variable, and `!!` builds a specification programmatically.
#'
#' Every pass requires its keys to be equal unless `block_method` says
#' otherwise. `block_method` and `window` each take one value for every pass
#' or one value per pass, so a strict pass and a forgiving one run together:
#'
#' ```
#' block_1      = list(list(soundex(surname), dob), surname),
#' block_method = c("exact", "sorted_neighbourhood"),
#' window       = 5
#' ```
#'
#' @section Checking the blocking:
#' Blocking decides what the rest of the run costs and what it can find, and
#' it is where a linkage usually goes wrong: one key that half the file
#' shares, or a pass that most records have no key for. Every blocked result
#' therefore carries a report, `attr(out, "blocking")`, which prints as a
#' table and holds
#'
#' * `passes`, one row per pass: the key as written for each input, the
#'   records with no key (`n_missing_1`, `n_missing_2`), the distinct keys,
#'   the records in the largest block, the blocks `max_block_size` skipped,
#'   the pairs the pass was expected to give (`n_expected`), the candidates
#'   it was the first to find (`n_pairs`) and how many of those are in the
#'   result (`n_kept`, smaller after a `filter` or an `exclude`);
#' * `largest`, the five keys of each pass that make the most pairs, with
#'   the records each side holds. A placeholder such as `"UNKNOWN"` or
#'   `"0000"` at the top of this table belongs in `na_strings`.
#'
#' `dry_run = TRUE` returns that report and stops: the keys are built and
#' every argument is checked, but no pair is generated, so a call can be
#' tried on the full files in the time it takes to read them. `max_pairs`
#' is the same check made automatically --- a run expected to exceed it stops
#' before generating anything, naming the pass and the block responsible.
#' Before a run the passes can only be added up, and a pair that two passes
#' find is counted in both, so with several passes `n_expected` totals to an
#' upper bound.
#'
#' @section Missing values:
#' A record whose key is missing pairs with nothing, which is what makes
#' blocking on an incomplete variable safe. With the default
#' `match_missing = FALSE` a missing value in *any* term makes the whole key
#' for that pass missing. `match_missing = TRUE` instead treats "missing" as a
#' value of its own, so two records both missing a term can pair on it --- the
#' behaviour of the `missingPos = FALSE` default in the function this
#' replaces. `na_strings` lists the text that counts as missing, `""` by
#' default; set it to `character()` to take blanks literally.
#'
#' @section Comparators:
#' `compare` is a list of comparator calls whose first argument is the column
#' to compare. Any function of two character vectors works, so switching
#' metric is a one-word edit:
#'
#' ```
#' compare = list(
#'     jaro_winkler(given_name),
#'     sn_jac = jaccard_index(surname, q = 3),
#'     name   = jaro_winkler_tokens(full_name, contractions = TRUE),
#'     cross  = jaro_winkler(family_name | surname),
#'     exact  = (function(a, b) a == b)(postcode)
#' )
#' ```
#'
#' A comparator of your own is applied to its column the same way, so it is
#' either named first (`my_cmp(surname)`) or written inline and applied, as
#' `exact` is above. A function on its own names no column and is an error.
#'
#' A score column takes its name from the list element; unnamed elements fall
#' back to `<column>_<comparator>`, so `jaro_winkler(given_name)` becomes
#' `given_name_jaro_winkler`. `nthreads` reaches a comparator only when it
#' declares that argument, so plain functions of two arguments are safe to
#' supply. This package's own comparators are also the fastest to use here:
#' they are handed each record's string once, however many pairs the record
#' is in, where a function of your own is handed one string per pair.
#'
#' **Columns named differently in the two inputs** are compared by writing
#' `left | right`: the expression to the left of the `|` is evaluated against
#' `data_1` and the one to the right against `data_2`, as `cross` is above.
#' Either side may be any expression, not only a bare column name, so
#' `soundex(family_name) | surname_code` works too --- this is to `compare`
#' what `block_2` is to `block_1`. Such an element compares no single column on
#' both sides, so it has no name to fall back on and must be given one.
#'
#' **Numbers and dates.** A string comparator is handed text whatever the
#' column holds, which is what lets `jaro_winkler(dob)` or
#' `levenshtein(postcode)` work on a `Date` or a number. [numeric_compare()]
#' and [date_compare()] are handed the column as it is stored instead, so
#' `numeric_compare(age, scale = 2)` and `date_compare(dob)` compare numbers
#' and dates rather than their spelling. A comparator of your own asks for the
#' same by wrapping its column in `I()`:
#'
#' ```
#' compare = list(
#'     age   = numeric_compare(age, method = "step", offset = 1),
#'     dob   = date_compare(dob),
#'     years = (function(a, b) abs(as.numeric(a - b)) / 365.25)(I(dob))
#' )
#' ```
#'
#' `na_strings` and `force_upper_case` are about text and do not touch a
#' column passed this way. A distance between numbers or dates that has no
#' upper limit, like `years` above, cannot be put on a 0--1 scale and is
#' returned as it is, recorded as lower-is-a-match.
#'
#' Comparators disagree about which end of their scale means "identical":
#' [jaro_winkler()] and the `*_similarity()` family return 1, edit distances
#' such as [levenshtein()] and [hamming()] return 0, and the RapidFuzz
#' `fuzz_*` scorers return 100. Each comparator is therefore probed once on a
#' calibration pair and, where needed, converted so that **1 always means an
#' exact match** --- an unbounded edit distance by
#' `1 - distance / max(nchar(a), nchar(b))`, pairwise, with lengths counted
#' in bytes when the comparator counts edits in bytes. `normalise = FALSE`
#' leaves every comparator's own scale untouched. What was detected is
#' recorded in `attr(out, "compare")` and printed when `verbose = TRUE`.
#'
#' @section Filtering while scoring:
#' Blocking is deliberately generous, so most candidate pairs are not matches
#' and are thrown away as soon as they have been scored. `filter` does the
#' throwing away inside `rLink()`:
#'
#' ```
#' rLink(enrol, claims,
#'       block_1 = list(dob, soundex(surname)),
#'       compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given)),
#'       filter  = sn >= 0.85 & gn >= 0.8)
#' ```
#'
#' The rule is an ordinary R expression giving `TRUE` for the pairs to keep.
#' It can name any score, `pass`, and any column of either input under its
#' suffix, so `sex_1 == sex_2` is as good a rule as one about a score. A pair
#' for which it gives `NA` --- a score that could not be computed --- is
#' dropped.
#'
#' A rule need not mention a score, and then needs no `compare` either:
#' `filter = sex_1 == sex_2` on its own is a condition on the candidates that
#' no blocking key could express, such as `abs(yob_1 - yob_2) <= 1`.
#'
#' Where a rule joins several conditions with `&`, each is applied as soon as
#' the scores it reads exist, and a comparator only ever sees the pairs still
#' in the running when its turn comes. In
#'
#' ```
#' filter = sex_1 == sex_2 & sn >= 0.85 & gn >= 0.8
#' ```
#'
#' the first condition reads no score, so it is applied before anything is
#' scored; `sn` is then scored for the pairs whose sex agrees; `gn` is scored
#' only for the pairs that passed both; and a comparator the rule does not
#' name is scored last, for the pairs that were kept. The comparators run in
#' the order the rule asks for them, whatever order `compare` lists them in,
#' so the condition written first is the one that should drop the most for
#' the least: a cheap comparator with a firm cut-off ahead of a costly one.
#' `attr(out, "compare")$n_scored` says how many pairs each comparator was
#' run on. Nothing is taken out of an `|`, which needs all of its scores.
#'
#' The result is the same table as scoring everything and subsetting it, and
#' is much cheaper to get. Candidates are generated, scored and thinned a
#' slice of the records at a time, so the full candidate set never exists at
#' once, and the expensive columns --- `pair_id` above all, then the carried
#' ones --- are built only for the pairs that are left. It also lifts the
#' limit of 2^31 candidate pairs: only the pairs kept have to fit in one
#' table. A slice holds about ten million candidates; set
#' `options(fast.string.rlink_slice_pairs = )` to trade memory for fewer
#' slices.
#'
#' All of this rests on the rule being about one pair at a time. A condition
#' that sets a pair against the others, such as `sn >= median(sn)`, is
#' answered over whichever pairs happen to be in hand when it is asked, which
#' is not every candidate; subset the result for that instead.
#'
#' What was removed is recorded in `attr(out, "filter")`: the rule, the part
#' of it applied before scoring (`before_scoring`), and the number of
#' candidates found (`n_candidates`), scored by any comparator (`n_scored`)
#' and kept (`n_kept`). A filtered table is no longer the candidate set, so
#' [blocking_eval()] on it measures the blocking and the filter together.
#'
#' @section Staged linkage:
#' A large linkage is rarely one run. The usual shape is a strict pass whose
#' links can be accepted without review, then a looser one over what is left,
#' and two arguments keep the second from repeating the first.
#'
#' `exclude` names pairs to leave out. They are dropped as soon as they are
#' found, before scoring, so pairs already settled cost nothing the second
#' time. Given as a character vector they are read as `pair_id`s and found
#' again through the identifier columns, which is what makes them safe to
#' carry between runs: the files may have been re-ordered, added to or cut
#' down since, and an identifier that is no longer there is simply not
#' matched. Given as a data frame --- an earlier result, say --- they are read
#' as row numbers, which mean the same records only against the same data, so
#' a table made from files of a different size is an error.
#' `attr(out, "exclude")` records how many were given, how many named records
#' that are not there, and how many candidates were left out.
#'
#' [rLink_unmatched()] is the other half: the records no accepted link
#' reaches, which are the ones worth a second run.
#'
#' ```
#' first <- rLink(enrol, claims, block_1 = list(list(surname, dob)),
#'                compare = list(gn = jaro_winkler(given)))
#' links <- first[gn >= 0.95]
#' again <- rLink(enrol[rLink_unmatched(links), ],
#'                claims[rLink_unmatched(links, side = 2), ],
#'                block_1 = list(soundex(surname), dob),
#'                compare = list(sn = jaro_winkler(surname),
#'                               gn = jaro_winkler(given)))
#' ```
#'
#' @section Pair identifiers:
#' `pair_id` identifies each pair. By default it is built from the `frame_id`
#' column of each input, as `"<id_1>|<id_2>"`, and falls back to numbering the
#' pairs when either input has no such column. Two different names are given
#' as `pair_id = c("frame_id", "person_id")`.
#'
#' An identifier built from the records is *stable*: the same two records give
#' the same `pair_id` whatever blocking found them, so a judgement recorded
#' against a pair is still about that pair after a re-run with another pass. A
#' row number is not stable, which is why [rLink_inspect()] keys its review
#' artifact on this column. The identifiers are carried onto the output as
#' ordinary columns under both suffixes too, so nothing is lost by joining
#' them into one key.
#'
#' An identifier containing `"|"` is an error rather than an ambiguous key, and
#' identifiers that are missing or repeated raise a warning saying how many,
#' because a review cannot then tell those pairs apart.
#'
#' Building it is the slowest thing `rLink()` does on a large result: every
#' pair gets a string of its own, and R takes about a second to make a million
#' new strings whoever asks it to. On tens of millions of unfiltered pairs
#' that is most of the run, and `attr(out, "timings")` reports it as its own
#' stage so that it can be seen. `filter` avoids it for the pairs it drops;
#' `pair_id = NULL` avoids it altogether, for a run whose pairs will be
#' thresholded and clustered but never reviewed.
#'
#' @param data_1 A data frame (or `data.table`). Never modified.
#' @param data_2 An optional second data frame to link `data_1` to. When
#'   `NULL` (the default) `rLink()` deduplicates within `data_1`, returning
#'   pairs with `index_1 < index_2` and no record paired with itself.
#' @param block_1 Blocking passes, as described above. Required unless
#'   `pairs` is given. This argument was called `block` before it gained the
#'   `_1` that pairs it with `block_2`; both names still work.
#' @param block_2 Optional blocking expressions for `data_2`, for when the two
#'   inputs name or store the same information differently (`dob` as a `Date`
#'   in one and `"DDMMYYYY"` text in the other). Must have as many passes as
#'   `block_1`, each with the same number of terms; a pass given as `NULL`
#'   reuses `block_1`'s. By default `block_1`'s own expressions are evaluated
#'   against `data_2`. An error when deduplicating, as there is no `data_2`:
#'   to block one file on two different fields, a surname against a maiden
#'   name, give the file as both inputs and drop the self-pairs with
#'   `filter = index_1 != index_2`.
#' @param compare A list of comparator calls, as described above. `NULL`
#'   returns the candidate pairs unscored.
#' @param pairs Optional candidate pairs to score instead of blocking: any
#'   data frame with `x_index` and `y_index` columns, such as the result of
#'   [minhash_pairs()], [fuzzy_within()] or [tfidf_top_n()], or an earlier
#'   `rLink()` result, whose pairs are then scored again --- the way to run a
#'   costly comparator over only the pairs a cheap run kept. Only the pairs
#'   are read, and a result made from files of another size is an error.
#'   Supply this or `block_1`, not both.
#' @param block_method `"exact"` (the default) requires keys to be equal, via
#'   [block_pairs()]. `"sorted_neighbourhood"` also pairs records whose keys
#'   are near each other in sorted order, via [sorted_neighbourhood()]. One
#'   value for every pass, or one per pass.
#' @param window Window width of a sorted-neighbourhood pass; an odd positive
#'   integer, `3` by default. One value for every such pass, or one per pass,
#'   in which case the entries of exact passes are not read. Not read at all
#'   when every pass is exact.
#' @param max_block_size Blocks holding more records than this (from both
#'   inputs together) are skipped, with a warning saying how many and a count
#'   per pass in the blocking report. `Inf` (the default) keeps every block.
#'   One value for every pass, or one per pass, as `c(Inf, 500)` to cap the
#'   second pass and leave the first whole. Applies to exact passes only: a
#'   sorted neighbourhood has no blocks to skip, and a warning says so when
#'   there is nothing for it to act on.
#' @param match_missing Whether records missing a key term can pair with each
#'   other. See *Missing values*.
#' @param na_strings Character vector of values to read as missing in keys and
#'   comparison columns, `""` by default.
#' @param force_upper_case Whether to upper-case keys and comparison values
#'   before use. Affects the scores, never the columns returned.
#' @param normalise Whether to convert each comparator's score so that 1 means
#'   an exact match. See *Comparators*.
#' @param keep Columns to carry onto the output, or `NULL` (the default) for
#'   all of them. `character()` carries none.
#' @param pair_id Record identifier column from which to build the `pair_id`
#'   column: one name used on both sides, or two when the inputs name it
#'   differently, as `c("frame_id", "person_id")`. `"frame_id"` by default,
#'   and a default that is not there falls back to numbering the pairs, so
#'   naming no identifier costs nothing; a name given explicitly must exist.
#'   `NULL` always numbers the pairs. See *Pair identifiers*.
#' @param block_key Whether to add the `block_key` column, a readable copy of
#'   the key that paired each record. It is there for diagnosis only, and
#'   `FALSE` saves one string per pair. Ignored when `pairs` is given.
#' @param suffixes Length-2 character vector distinguishing the two sides of
#'   each pair, `c("_1", "_2")` by default.
#' @param score_position `"grouped"` (the default) puts each score beside the
#'   pair of columns it scores; `"end"` puts all scores after all carried
#'   columns.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param verbose Whether to print the pairs each pass is expected to give,
#'   what each comparator was found to return, the pairs found and the time
#'   taken, with a progress bar when a `filter` cuts the run into slices.
#'   Timings are returned either way, as `attr(out, "timings")`.
#' @param filter Optional rule saying which pairs to keep, such as
#'   `sn >= 0.85 & gn >= 0.8` or `sex_1 == sex_2`. See *Filtering while
#'   scoring*.
#' @param exclude Optional pairs to leave out: a character vector of
#'   `pair_id`s, or a data frame of pairs by row number, such as an earlier
#'   result. See *Staged linkage*.
#' @param dry_run `TRUE` returns the blocking report without generating any
#'   pairs. See *Checking the blocking*.
#' @param max_pairs Stop, before generating any, if the blocking is expected
#'   to give more candidate pairs than this. `Inf` (the default) never stops.
#' @param block Former name of `block_1`, still accepted. Give one or the
#'   other, not both.
#' @return A `data.table`, also classed `fs_pairs` so that [summary()] and
#'   [plot()] describe it, with one row per candidate pair: `pair_id`
#'   (character when built from identifier columns, otherwise the pair's
#'   number), `index_1` and `index_2` (row numbers into `data_1` and `data_2`,
#'   suffixed as `suffixes` says), `pass`, `block_key` (when blocking and
#'   `block_key = TRUE`), the carried columns under both suffixes, and one
#'   column per comparator.
#'
#'   Its attributes describe the run, and survive `[`, [order()] and
#'   [head()]: `timings` (seconds per stage), `compare` (what each score is,
#'   which way it runs and how many pairs it was scored for), `blocking` (what each pass did; absent when
#'   `pairs` was given), `n_records` (the records in each input), `dedupe`
#'   (whether they were one file), and `filter` and `exclude` (what each
#'   removed, when given).
#'
#'   With `dry_run = TRUE`, the blocking report alone: a list of class
#'   `fs_blocking` holding `passes`, `largest`, `n_records` and `dedupe`.
#' @seealso [block_pairs()] and [sorted_neighbourhood()] for the candidate
#'   pairs alone, [cluster_pairs()] to turn matched pairs into entity
#'   identifiers, [rLink_unmatched()] for the records no pair reaches, and
#'   [minhash_pairs()], [fuzzy_within()] and [tfidf_top_n()] for
#'   similarity-based candidates to pass as `pairs`.
#' @family record linkage pair functions
#' @examples
#' enrol <- data.frame(
#'     id      = 1:4,
#'     surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
#'     given   = c("JOHN", "MARY", "ROB", "JON"),
#'     dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01",
#'                         "1980-01-02"))
#' )
#' claims <- data.frame(
#'     ref     = c("A", "B", "C"),
#'     surname = c("SMITH", "JONSE", "BROWNE"),
#'     given   = c("JON", "MARY", "ROBERT"),
#'     dob     = as.Date(c("1980-01-02", "1975-06-30", "1990-12-01"))
#' )
#'
#' # Block on the Soundex of surname together with date of birth, then score
#' out <- rLink(
#'     enrol, claims,
#'     block_1 = list(list(soundex(surname), dob)),
#'     compare = list(jaro_winkler(given), sn = jaro_winkler(surname))
#' )
#' out$given_jaro_winkler
#'
#' # An edit distance is converted so that 1 is still an exact match
#' rLink(enrol, claims,
#'       block_1 = list(soundex(surname)),
#'       compare = list(lev = levenshtein(surname)))$lev
#'
#' # `normalise = FALSE` keeps the comparator's own scale, here edits
#' rLink(enrol, claims,
#'       block_1   = list(soundex(surname)),
#'       compare   = list(lev = levenshtein(surname)),
#'       normalise = FALSE)$lev
#'
#' # Two passes: same Soundex of surname, or same date of birth
#' rLink(enrol, claims, block_1 = list(soundex(surname), dob))$pass
#'
#' # --- block_method = "sorted_neighbourhood" ----------------------------
#' # Blocking on surname exactly only ever finds SMITH = SMITH: the typos
#' # put every other true match in a block of its own.
#' rLink(enrol, claims, block_1 = list(surname))[, c("index_1", "index_2")]
#'
#' # The sorted neighbourhood slides a window over the keys in sorted order
#' # instead, so neighbours pair without agreeing: BROWN with BROWNE,
#' # JONES with JONSE, SMITH with SMYTH.
#' sn <- rLink(
#'     enrol, claims,
#'     block_1      = list(surname),
#'     block_method = "sorted_neighbourhood",
#'     window       = 3,
#'     compare      = list(jaro_winkler(surname)),
#'     keep         = "surname"
#' )
#' # The window also pairs unrelated neighbours, such as SMITH with JONSE,
#' # which is what the scores are for.
#' sn[, c("surname_1", "surname_2", "surname_jaro_winkler")]
#'
#' # A wider window reaches further either side, for more pairs to score
#' nrow(rLink(enrol, claims, block_1 = list(surname),
#'            block_method = "sorted_neighbourhood", window = 5))
#'
#' # --- the rest of the arguments ----------------------------------------
#' # `keep` carries only the columns asked for, `suffixes` renames the two
#' # sides, `score_position = "end"` puts the scores after them, and
#' # `block_key = FALSE` drops the diagnostic key column.
#' rLink(enrol, claims,
#'       block_1        = list(dob),
#'       compare        = list(jaro_winkler(surname), jaro_winkler(given)),
#'       keep           = c("surname", "given"),
#'       suffixes       = c(".x", ".y"),
#'       score_position = "end",
#'       block_key      = FALSE)
#'
#' # `column_1 | column_2` compares columns named differently in each input
#' enrol2 <- enrol
#' names(enrol2)[2] <- "family_name"
#' rLink(enrol2, claims,
#'       block_1 = list(dob),
#'       compare = list(sn = jaro_winkler(family_name | surname)))$sn
#'
#' # `block_2` builds the same key from a column stored differently
#' claims2 <- claims
#' claims2$dob <- format(claims2$dob, "%d%m%Y")
#' rLink(enrol, claims2,
#'       block_1 = list(format(dob, "%d%m%Y")),
#'       block_2 = list(dob),
#'       compare = list(jaro_winkler(surname)))$block_key
#'
#' # `match_missing` decides whether two records missing a key can pair
#' enrol3 <- enrol
#' enrol3$surname[4] <- NA
#' claims3 <- claims
#' claims3$surname[1] <- NA
#' nrow(rLink(enrol3, claims3, block_1 = list(surname)))
#' nrow(rLink(enrol3, claims3, block_1 = list(surname), match_missing = TRUE))
#'
#' # `force_upper_case = FALSE` makes keys and scores case-sensitive
#' enrol4 <- enrol
#' enrol4$surname <- tolower(enrol4$surname)
#' nrow(rLink(enrol4, claims, block_1 = list(surname)))
#' nrow(rLink(enrol4, claims, block_1 = list(surname),
#'            force_upper_case = FALSE))
#'
#' # `max_block_size` skips blocks too big to be worth comparing
#' nrow(rLink(enrol, claims, block_1 = list(fsubstr(surname, 1, 1)),
#'            max_block_size = 2))
#' # One limit per pass caps a loose pass and leaves a strict one whole
#' nrow(rLink(enrol, claims,
#'            block_1 = list(dob, fsubstr(surname, 1, 1)),
#'            max_block_size = c(Inf, 2)))
#'
#' # `pairs` scores candidates found some other way instead of blocking
#' rLink(enrol, claims,
#'       pairs   = data.frame(x_index = c(1L, 4L), y_index = c(1L, 1L)),
#'       compare = list(jaro_winkler(surname)),
#'       keep    = "surname")
#'
#' # `filter` keeps only the pairs worth having, and builds identifiers and
#' # carried columns for those alone. The rule may name scores, `pass` and
#' # any column of either input under its suffix.
#' # Here every record is a candidate for every other (a key of one value
#' # blocks nothing), and four of the twelve pairs are worth keeping.
#' kept <- rLink(enrol, claims,
#'               block_1 = list(1),
#'               compare = list(sn = jaro_winkler(surname),
#'                              gn = jaro_winkler(given)),
#'               filter  = sn >= 0.85 & dob_1 == dob_2,
#'               keep    = c("surname", "given"))
#' kept[, c("surname_1", "surname_2", "sn", "gn")]
#' # The half of the rule about `dob` reads no score, so it was applied
#' # first and only the four pairs it left were ever scored.
#' unlist(attr(kept, "filter")[c("n_candidates", "n_scored", "n_kept")])
#' # `sn` is what the rest of the rule asks for, so it was scored for those
#' # four; `gn`, which the rule does not read, only for the pairs kept.
#' attr(kept, "compare")[, c("score", "n_scored")]
#'
#' # `pairs` takes an earlier result, to score its pairs again: here a
#' # second comparator, for the pairs the first run kept.
#' rLink(enrol, claims, pairs = kept,
#'       compare = list(lev = levenshtein(given)),
#'       keep = "given")
#'
#' # A rule that reads no score needs no `compare`: it is a condition on the
#' # candidates that no blocking key could express.
#' rLink(enrol, claims,
#'       block_1 = list(fsubstr(surname, 1, 1)),
#'       filter  = abs(as.numeric(dob_1 - dob_2)) <= 31,
#'       keep    = "dob")
#'
#' # --- checking the blocking --------------------------------------------
#' # `dry_run = TRUE` builds the keys and stops: what each pass would do,
#' # and not one pair. The passes are named here, and each has its own
#' # method -- surname and date of birth exactly, or surnames that sort
#' # next to each other.
#' plan <- rLink(enrol, claims,
#'               block_1      = list(strict = list(soundex(surname), dob),
#'                                   near   = surname),
#'               block_method = c("exact", "sorted_neighbourhood"),
#'               dry_run      = TRUE)
#' plan
#' plan$passes[, c("label", "method", "n_expected")]
#'
#' # The same report rides on every result, with what was found filled in.
#' # `near` expected six pairs and was the first to find two: the rest had
#' # already been found by `strict`.
#' both <- rLink(enrol, claims,
#'               block_1      = list(strict = list(soundex(surname), dob),
#'                                   near   = surname),
#'               block_method = c("exact", "sorted_neighbourhood"))
#' attr(both, "blocking")$passes[, c("label", "n_expected", "n_pairs")]
#' summary(both)
#'
#' # `max_pairs` stops a run that would be too large, before it starts
#' try(rLink(enrol, claims, block_1 = list(1), max_pairs = 10))
#'
#' # Numbers and dates are compared as numbers and dates: numeric_compare()
#' # and date_compare() get the column as it is stored, and a comparator of
#' # your own asks for the same with I().
#' enrol5 <- cbind(enrol, age = c(44, 49, 34, 44))
#' claims5 <- cbind(claims, age = c(45, 49, 30))
#' rLink(enrol5, claims5,
#'       block_1 = list(soundex(surname)),
#'       compare = list(age   = numeric_compare(age, offset = 1, scale = 2),
#'                      dob   = date_compare(dob),
#'                      years = (function(a, b) abs(a - b))(I(age))),
#'       keep = character())[, c("age", "dob", "years")]
#'
#' # `verbose = TRUE` reports the pairs expected, the timings and what each
#' # comparator was found to return
#' invisible(rLink(enrol, claims, block_1 = list(soundex(surname)),
#'                 compare = list(jaro_winkler(given)), verbose = TRUE))
#'
#' # `pair_id` names the record identifier on each side, which is what
#' # makes a pair identifiable rather than merely numbered. These two files
#' # call it different things, so both names are given.
#' ided <- rLink(enrol, claims,
#'               block_1 = list(dob),
#'               compare = list(sn = jaro_winkler(surname)),
#'               pair_id = c("id", "ref"),
#'               keep    = character())
#' ided[, c("pair_id", "sn")]
#'
#' # Why it matters: re-run with different blocking and the same two records
#' # give the same `pair_id`, so a judgement recorded against a pair is
#' # still about that pair. A row number would not survive this.
#' again <- rLink(enrol, claims,
#'                block_1 = list(soundex(surname)),
#'                compare = list(sn = jaro_winkler(surname)),
#'                pair_id = c("id", "ref"),
#'                keep    = character())
#' intersect(ided$pair_id, again$pair_id)
#'
#' # --- staged linkage ---------------------------------------------------
#' # Accept the close pairs of the first run as links...
#' links <- ided[sn >= 0.95]
#'
#' # ...and `exclude` leaves them out of the next, however it is blocked.
#' # They are named by `pair_id`, so they are found through the identifiers
#' # rather than by where the records happen to sit.
#' rLink(enrol, claims,
#'       block_1 = list(soundex(surname)),
#'       compare = list(sn = jaro_winkler(surname)),
#'       pair_id = c("id", "ref"),
#'       exclude = links$pair_id,
#'       keep    = character())
#'
#' # rLink_unmatched() is the records no link reaches: the residue that a
#' # looser second run is for.
#' enrol[rLink_unmatched(links), ]
#'
#' # Deduplicating one file, then turning the matches into entity IDs
#' dup <- rLink(enrol, block_1 = list(soundex(surname)),
#'              compare = list(jaro_winkler(given)))
#' cluster_pairs(dup$index_1[dup$given_jaro_winkler > 0.8],
#'               dup$index_2[dup$given_jaro_winkler > 0.8], n = nrow(enrol))
#' @export
rLink <- function(data_1,
                  data_2 = NULL,
                  block_1 = NULL,
                  block_2 = NULL,
                  compare = NULL,
                  pairs = NULL,
                  block_method = c("exact", "sorted_neighbourhood"),
                  window = 3L,
                  max_block_size = Inf,
                  match_missing = FALSE,
                  na_strings = "",
                  force_upper_case = TRUE,
                  normalise = TRUE,
                  keep = NULL,
                  pair_id = "frame_id",
                  block_key = TRUE,
                  suffixes = c("_1", "_2"),
                  score_position = c("grouped", "end"),
                  nthreads = NULL,
                  verbose = FALSE,
                  filter = NULL,
                  exclude = NULL,
                  dry_run = FALSE,
                  max_pairs = Inf,
                  block = NULL) {
    t_all <- .bench_now()
    # Read before anything touches `pair_id`: a default that is not in the
    # data falls back to the pair number, while a name the caller chose must
    # be there.
    pair_id_default <- missing(pair_id)
    timings <- list()
    t0 <- t_all

    # Captured before anything touches them, so a term naming a column is
    # never evaluated as a variable in the caller first.
    quo_block <- rlang::enquo(block_1)
    quo_block_old <- rlang::enquo(block)
    quo_block_2 <- rlang::enquo(block_2)
    quo_compare <- rlang::enquo(compare)
    quo_filter <- rlang::enquo(filter)

    # `block` was this argument's name before it gained the `_1` that pairs it
    # with `block_2`. Both are accepted, and every message below names
    # whichever was written, so it points at the caller's own code.
    block_arg <- "block_1"
    if (!rlang::quo_is_null(quo_block_old)) {
        if (!rlang::quo_is_null(quo_block))
            stop(paste0("Give `block_1` or `block`, not both: they are the ",
                        "same argument, and `block_1` is the current name."),
                 call. = FALSE)
        quo_block <- quo_block_old
        block_arg <- "block"
    }

    # One method for every pass, or one each. match.arg() would not do:
    # c("exact", "sorted_neighbourhood") written out for two passes is the
    # default itself, which it reads as "nothing was chosen", and with
    # `several.ok` it drops a method it does not recognise without a word.
    block_method <- if (missing(block_method)) "exact" else
        .rlink_methods(block_method)
    score_position <- match.arg(score_position)
    .check_flag(match_missing, "match_missing")
    .check_flag(force_upper_case, "force_upper_case")
    .check_flag(normalise, "normalise")
    .check_flag(block_key, "block_key")
    .check_flag(verbose, "verbose")
    .check_flag(dry_run, "dry_run")
    if (!is.numeric(max_pairs) || length(max_pairs) != 1L ||
        is.na(max_pairs) || max_pairs < 0)
        stop("`max_pairs` must be a single number, or Inf for no limit.",
             call. = FALSE)
    # Validated here but deliberately not converted: every function called
    # below runs .as_nthreads() itself, and the -1L that marks "the
    # RcppParallel default" would fail that check if it were passed on.
    .as_nthreads(nthreads)

    if (!is.data.frame(data_1)) stop("`data_1` must be a data frame.")
    dedupe <- is.null(data_2)
    if (!dedupe && !is.data.frame(data_2))
        stop("`data_2` must be a data frame or NULL.")
    .rlink_check_names(data_1, "data_1")
    if (!dedupe) .rlink_check_names(data_2, "data_2")
    # A binding, not a copy: columns are only ever read.
    d2 <- if (dedupe) data_1 else data_2

    if (!is.character(suffixes) || length(suffixes) != 2L ||
        anyNA(suffixes) || suffixes[1L] == suffixes[2L])
        stop("`suffixes` must be two different strings.")
    if (is.null(na_strings)) na_strings <- character()
    if (!is.character(na_strings) || anyNA(na_strings))
        stop("`na_strings` must be a character vector without NA.")
    if (!is.null(pair_id) &&
        (!is.character(pair_id) || length(pair_id) < 1L ||
         length(pair_id) > 2L || anyNA(pair_id) || !all(nzchar(pair_id))))
        stop(paste0("`pair_id` must be NULL, the name of one record ",
                    "identifier column, or two names, one per input."),
             call. = FALSE)
    if (dedupe && !is.null(pair_id) && length(pair_id) == 2L)
        stop(paste0("`pair_id` names a column in each input, but there is ",
                    "only one: deduplicating takes a single name."),
             call. = FALSE)

    use_block <- !rlang::quo_is_null(quo_block)
    if (use_block && !is.null(pairs))
        stop(sprintf("Give `%s` or `pairs`, not both.", block_arg))
    if (!use_block && is.null(pairs))
        stop(sprintf("`%s` is required (or supply `pairs`).", block_arg))
    # It would be evaluated against data_1 and then never used.
    if (dedupe && !rlang::quo_is_null(quo_block_2))
        stop(sprintf(paste0(
            "`block_2` describes `data_2`, but there is none: deduplicating ",
            "uses `%s` alone."), block_arg))
    show_key <- use_block && block_key
    if (dry_run && !use_block)
        stop(paste0("`dry_run` reports what the blocking would do, and with ",
                    "`pairs` there is none."), call. = FALSE)
    # The same file twice links every record to itself, which is almost
    # never what was meant. With `block_2` it can be: that is how one file is
    # blocked on two different fields, a surname against a maiden name.
    if (use_block && !dedupe && rlang::quo_is_null(quo_block_2) &&
        nrow(data_1) && identical(data_1, data_2))
        warning(paste0(
            "`data_1` and `data_2` are the same data, so every record is ",
            "paired with itself. Leave `data_2` out to deduplicate one file."),
            call. = FALSE)

    use_filter <- !rlang::quo_is_null(quo_filter)
    slice_pairs <- .rlink_slice_pairs()

    mask_1 <- as.list(data_1)
    mask_2 <- as.list(d2)
    timings$validate <- .bench_now() - t0

    # --- blocking keys -----------------------------------------------------
    labels <- NULL
    keys_1 <- NULL
    key_parts <- NULL
    codes <- NULL
    given <- NULL
    blocking <- NULL
    n_pass <- 0L
    if (use_block) {
        t0 <- .bench_now()
        spec <- .rlink_passes(quo_block, block_arg)
        labels <- spec$labels
        n_pass <- length(spec$passes)
        spec_2 <- if (rlang::quo_is_null(quo_block_2)) NULL else
            .rlink_passes(quo_block_2, "block_2")
        if (!is.null(spec_2))
            .rlink_check_block_2(spec, spec_2, n_pass, block_arg)

        # How far either side of its own key each pass reaches: nowhere for
        # an exact pass, `(window - 1) / 2` distinct keys for a sorted
        # neighbourhood. A window is only read for the passes that use one.
        method <- .rlink_per_pass(block_method, n_pass, "block_method")
        half <- integer(n_pass)
        if (any(method == "sorted_neighbourhood")) {
            width <- .rlink_per_pass(window, n_pass, "window")
            for (p in which(method == "sorted_neighbourhood"))
                half[p] <- .half_window(width[[p]])
        }
        # One limit for every pass, or one each. It used to be dropped
        # without a word under the sorted neighbourhood, and a safety valve
        # that is not connected should say so.
        if (length(max_block_size) == 1L) {
            .check_max_block_size(max_block_size)
            if (is.finite(max_block_size) && all(half > 0L))
                warning(paste0(
                    "`max_block_size` applies to exact passes, and every ",
                    "pass here is a sorted neighbourhood, so no block is ",
                    "skipped."), call. = FALSE)
        }
        block_cap <- .rlink_block_caps(max_block_size, n_pass)
        idle <- which(is.finite(block_cap) & half > 0L)
        if (length(max_block_size) > 1L && length(idle))
            warning(sprintf(paste0(
                "`max_block_size` applies to exact passes, and pass %s is a ",
                "sorted neighbourhood, so its limit skips nothing."),
                paste(idle, collapse = ", ")), call. = FALSE)

        probs <- .rlink_problems()
        keys_1 <- vector("list", n_pass)
        keys_2 <- vector("list", n_pass)
        key_parts <- vector("list", n_pass)
        # A term written into more than one pass -- soundex(surname) in both
        # `list(list(soundex(surname), dob), list(soundex(surname),
        # postcode))` -- is evaluated once per side. The comparators already
        # share a column this way; the passes did not, and a phonetic code
        # or a date is the most expensive thing in a pass.
        cache_1 <- .rlink_term_cache()
        cache_2 <- .rlink_term_cache()
        for (p in seq_len(n_pass)) {
            terms_1 <- spec$passes[[p]]
            inherit <- is.null(spec_2) || is.null(spec_2$passes[[p]])
            terms_2 <- if (inherit) terms_1 else spec_2$passes[[p]]
            env_2 <- if (is.null(spec_2)) spec$env else spec_2$env
            arg_2 <- if (inherit) block_arg else "block_2"
            parts_1 <- lapply(seq_along(terms_1), function(k)
                .rlink_cached_term(cache_1, terms_1[[k]], mask_1, spec$env,
                                   nrow(data_1), block_arg, p, k, probs))
            # Deduplicating, the second side would be the same key built
            # from the same data a second time.
            parts_2 <- if (!dedupe) lapply(seq_along(terms_2), function(k)
                .rlink_cached_term(cache_2, terms_2[[k]], mask_2, env_2,
                                   nrow(d2), arg_2, p, k, probs))
            if (length(probs$msgs)) next
            # Several terms, matched exactly: the key never needs to exist
            # as a string, only as "which records share one".
            coded <- if (half[p] == 0L && length(parts_1) > 1L)
                .rlink_key_codes(parts_1, parts_2, p, force_upper_case,
                                 na_strings, match_missing, nthreads)
            if (!is.null(coded)) {
                keys_1[[p]] <- coded$x
                keys_2[[p]] <- if (dedupe) coded$x else coded$y
                key_parts[[p]] <- coded$parts
                next
            }
            keys_1[[p]] <- .rlink_key(parts_1, p, force_upper_case,
                                      na_strings, match_missing, nthreads)
            keys_2[[p]] <- if (dedupe) keys_1[[p]] else
                .rlink_key(parts_2, p, force_upper_case, na_strings,
                           match_missing, nthreads)
            # A lone term keeps its type, so a Date on one side and text on
            # the other would never be equal; compare them as text instead.
            if (!.rlink_same_type(keys_1[[p]], keys_2[[p]])) {
                keys_1[[p]] <- as.character(keys_1[[p]])
                keys_2[[p]] <- as.character(keys_2[[p]])
            }
        }
        .rlink_stop(probs, data_1, if (!dedupe) data_2)
        timings$keys <- .bench_now() - t0

        # The keys become integer codes once, here; the pairs are then drawn
        # from the codes a slice of the records at a time (one slice, unless
        # there is a `filter` to thin each one before the next is made).
        t0 <- .bench_now()
        codes <- .candidate_codes(keys_1, if (dedupe) NULL else keys_2, half)
        timings$pairs <- .bench_now() - t0

        # What each pass is about to do, read off the codes before a single
        # pair exists: it is the cheapest moment to find the pass that is
        # about to explode, and the only one at which stopping costs nothing.
        t0 <- .bench_now()
        written <- function(terms) paste(vapply(terms, function(e)
            if (is.character(e) && length(e) == 1L) e
            else paste(deparse(e), collapse = " "), ""), collapse = ", ")
        blocking <- .rlink_blocking_report(
            codes, block_cap,
            labels = vapply(seq_len(n_pass), function(p)
                .rlink_label(labels, p, bare = TRUE), ""),
            key_1 = vapply(spec$passes, written, ""),
            key_2 = if (dedupe) rep(NA_character_, n_pass) else
                vapply(seq_len(n_pass), function(p) written(
                    if (is.null(spec_2) || is.null(spec_2$passes[[p]]))
                        spec$passes[[p]] else spec_2$passes[[p]]), ""),
            keys_1 = keys_1, key_parts = key_parts,
            n_records = c(nrow(data_1), nrow(d2)), nthreads = nthreads)
        timings$report <- .bench_now() - t0

        if (verbose) {
            cli::cli_h2("rLink()")
            cli::cli_alert_info(sprintf(
                "%s x %s records, %d blocking pass%s.",
                format(nrow(data_1), big.mark = ","),
                format(nrow(d2), big.mark = ","),
                n_pass, if (n_pass == 1L) "" else "es"))
            for (p in seq_len(n_pass))
                cli::cli_alert_info(sprintf(
                    "Pass %d%s: %s expected pairs.", p,
                    .rlink_label(labels, p),
                    format(blocking$passes$n_expected[p], big.mark = ",",
                           scientific = FALSE)))
        }
        if (sum(blocking$passes$n_expected) > max_pairs)
            stop(.rlink_too_many(blocking, max_pairs), call. = FALSE)

        t0 <- .bench_now()
        slices <- .candidate_slices(
            if (use_filter) .candidate_bound(codes) else numeric(codes$n_x),
            if (use_filter) slice_pairs else Inf)
        timings$pairs <- timings$pairs + (.bench_now() - t0)
    } else {
        t0 <- .bench_now()
        given <- .rlink_given_pairs(pairs, nrow(data_1), nrow(d2), suffixes)
        slices <- .candidate_slices(rep(1, nrow(given)),
                                    if (use_filter) slice_pairs else Inf)
        timings$pairs <- .bench_now() - t0
    }

    # --- which columns are carried ----------------------------------------
    # Decided now, though nothing is carried until the pairs have been scored
    # and thinned: a pair the filter drops never has its columns gathered.
    avail <- union(names(data_1), names(d2))
    if (is.null(keep)) {
        keep_cols <- avail
    } else {
        if (!is.character(keep) || anyNA(keep))
            stop("`keep` must be a character vector without NA.")
        bad <- setdiff(keep, avail)
        if (length(bad))
            stop(sprintf("`keep` names columns that are not there: %s.",
                         paste0("`", bad, "`", collapse = ", ")))
        keep_cols <- intersect(avail, keep)
    }
    keep_1 <- intersect(names(data_1), keep_cols)
    keep_2 <- intersect(names(d2), keep_cols)
    lead <- c("pair_id", paste0("index", suffixes), "pass",
              if (show_key) "block_key")
    taken <- c(lead, if (length(keep_1)) paste0(keep_1, suffixes[1L]),
               if (length(keep_2)) paste0(keep_2, suffixes[2L]))

    # --- comparators: once per record --------------------------------------
    cmp <- .rlink_compare(quo_compare, taken)
    score_names <- vapply(cmp, `[[`, "", "name")
    # Everything the caller wrote has now been read and checked, and nothing
    # has been generated or scored.
    if (dry_run) return(blocking)

    # What is scored when. Without a `filter` that is every comparator, once.
    # With one, the rule is taken apart at its `&`s and each part is applied
    # as soon as the scores it reads exist, so a comparator only ever sees
    # the pairs that are still in the running when its turn comes.
    stages <- .rlink_stages(if (use_filter) quo_filter, score_names)
    t0 <- .bench_now()
    excluded <- if (!is.null(exclude))
        .rlink_exclude(exclude, data_1, d2, pair_id, pair_id_default, dedupe,
                       suffixes, nthreads)
    if (!is.null(exclude)) timings$exclude <- .bench_now() - t0

    # Comparators often share a column, as jaro_winkler(surname) and
    # levenshtein(surname) do, and when deduplicating both sides of one read
    # the same data. Each column is therefore prepared once per record and
    # spread to the pairs once, and the pair-level copy is dropped after the
    # last comparator that reads it. A column read both as text and in its
    # own type is two different things, hence the mark in the key.
    #
    # A comparator of this package is not handed a pair-level copy at all:
    # it gets the per-record column and the pairs' indices into it, as one
    # vector that is never built (see src/indexed_strings.h), and resolves
    # each record's string once however many pairs the record is in.
    src <- lapply(cmp, function(s) vapply(list(s$expr_1, s$expr_2),
        function(e) paste(deparse(e), collapse = " "), ""))
    kind <- vapply(cmp, function(s) if (s$typed) "typed" else "text", "")
    native <- vapply(cmp, function(s) !s$typed && .rlink_native(s$fn), NA)
    rec_key <- lapply(seq_along(cmp), function(i)
        paste0(if (dedupe) "" else 1:2, ":", kind[i], ":", src[[i]]))
    pair_key <- lapply(seq_along(cmp), function(i)
        paste0(1:2, ":", kind[i], if (native[i]) ":indexed:" else ":",
               src[[i]]))
    rec_cache <- list()
    orient <- vector("list", length(cmp))
    score_time <- numeric(length(cmp))
    for (i in seq_along(cmp)) {
        t0 <- .bench_now()
        s <- cmp[[i]]
        probs <- .rlink_problems()
        for (k in 1:2) {
            if (!is.null(rec_cache[[rec_key[[i]][k]]])) next
            v <- .rlink_term(
                if (k == 1L) s$expr_1 else s$expr_2,
                if (k == 1L) mask_1 else mask_2, s$env,
                if (k == 1L) nrow(data_1) else nrow(d2),
                probs = probs,
                where = sprintf("compare, element %d (`%s`), %s", i, s$name,
                                if (k == 1L || dedupe) "data_1" else "data_2"))
            if (!is.null(v))
                rec_cache[[rec_key[[i]][k]]] <-
                    .rlink_value(v, s$typed, force_upper_case, na_strings,
                                 nthreads)
        }
        .rlink_stop(probs, data_1, if (!dedupe) data_2)
        # Which end of its scale means "identical" is found once per
        # comparator, not once per slice.
        orient[[i]] <- .rlink_orient(
            s$fn, s$args, normalise, nthreads, typed = s$typed,
            sample = rec_cache[[rec_key[[i]][1L]]])
        score_time[i] <- .bench_now() - t0
    }

    threads <- .as_nthreads(nthreads)
    cmp_scored <- numeric(length(cmp))
    filter_time <- 0
    # A part of the rule that turned out to need a score it did not name,
    # and so is held until every score exists. See below.
    held_back <- logical(length(stages))

    # One slice of candidates through every stage: the comparators of the
    # stage, then the part of the rule they make readable.
    #
    # Dropping pairs is itself a pass over every vector held for the slice,
    # so it is only done when it pays: once a part has left at most two
    # thirds of the pairs standing. A part that drops fewer is remembered in
    # `alive` and settled with the next, or at the end. which() is what
    # drops the pairs a part could not decide: a missing score is not a
    # pass.
    score_slice <- function(xi, yi, ps) {
        scores <- vector("list", length(cmp))
        n_done <- 0L
        seen <- NULL
        alive <- NULL
        drop_dead <- function() {
            hit <- which(alive)
            alive <<- NULL
            if (length(hit) == length(xi)) return(invisible())
            xi <<- xi[hit]
            yi <<- yi[hit]
            ps <<- ps[hit]
            for (i in which(!vapply(scores, is.null, NA)))
                scores[[i]] <<- scores[[i]][hit]
        }
        apply_rule <- function(quo, sure) {
            t0 <- .bench_now()
            have <- which(!vapply(scores, is.null, NA))
            run <- function() .rlink_filter(
                quo, scores[have], score_names[have], ps, xi, yi, data_1, d2,
                suffixes, known = score_names)
            ok <- if (sure) run() else tryCatch(run(), error = function(cnd)
                NULL)
            if (!is.null(ok)) {
                alive <<- if (is.null(alive)) ok else alive & ok
                if (sum(alive, na.rm = TRUE) <= length(xi) * 2 / 3)
                    drop_dead()
            }
            filter_time <<- filter_time + (.bench_now() - t0)
            !is.null(ok)
        }
        for (s in seq_along(stages)) {
            stage <- stages[[s]]
            if (length(stage$score) && is.null(seen)) seen <- length(xi)
            pair_cache <- list()
            for (j in seq_along(stage$score)) {
                i <- stage$score[j]
                t0 <- .bench_now()
                spec <- cmp[[i]]
                a0 <- rec_cache[[rec_key[[i]][1L]]]
                b0 <- rec_cache[[rec_key[[i]][2L]]]
                ka <- pair_key[[i]][1L]
                kb <- pair_key[[i]][2L]
                if (is.null(pair_cache[[ka]]))
                    pair_cache[[ka]] <- .rlink_spread(a0, xi, native[i],
                                                      threads)
                if (is.null(pair_cache[[kb]]))
                    pair_cache[[kb]] <- .rlink_spread(b0, yi, native[i],
                                                      threads)
                a <- pair_cache[[ka]]
                b <- pair_cache[[kb]]
                later <- unlist(pair_key[stage$score[-seq_len(j)]])
                for (id in c(ka, kb))
                    if (!id %in% later) pair_cache[[id]] <- NULL
                score <- .rlink_call(spec$fn, a, b, spec$args, nthreads)
                if (!(is.numeric(score) || is.logical(score)) ||
                    length(score) != length(xi))
                    stop(sprintf(paste0(
                        "The comparator for `%s` returned %s of length %s; ",
                        "it must return one number or logical per pair (%s)."),
                        spec$name, class(score)[1L],
                        format(length(score), big.mark = ","),
                        format(length(xi), big.mark = ",")), call. = FALSE)
                rm(a, b)
                scores[[i]] <- orient[[i]]$convert(score, a0, b0, xi, yi)
                cmp_scored[i] <<- cmp_scored[i] + length(xi)
                score_time[i] <<- score_time[i] + (.bench_now() - t0)
            }
            n_done <- n_done + length(stage$score)
            if (is.null(stage$rule) || held_back[s]) next
            # A part is read as soon as the scores it names exist. It may
            # still need one it did not name -- `.data[[nm]] >= 0.9`, with
            # the name in a variable -- and then cannot be read yet. That is
            # not an error until every score exists and it still cannot be,
            # so the part is held back and read last, for this slice and the
            # rest.
            if (!apply_rule(stage$rule, sure = n_done == length(cmp)))
                held_back[s] <<- TRUE
        }
        if (any(held_back))
            apply_rule(.rlink_rule(unlist(
                lapply(stages[held_back], `[[`, "exprs"), recursive = FALSE),
                rlang::quo_get_env(quo_filter)), sure = TRUE)
        if (!is.null(alive)) {
            t0 <- .bench_now()
            drop_dead()
            filter_time <<- filter_time + (.bench_now() - t0)
        }
        list(xi = xi, yi = yi, pass = ps, scores = scores,
             seen = if (is.null(seen)) length(xi) else seen)
    }

    # --- candidate pairs, scored a slice at a time -------------------------
    n_slices <- length(slices$from)
    parts <- vector("list", n_slices)
    n_candidates <- 0
    n_scored <- 0
    n_excluded <- 0
    pass_found <- numeric(n_pass)
    n_2 <- as.numeric(nrow(d2))
    # Only a run cut into slices is long enough to be worth watching.
    bar <- if (verbose && n_slices > 1L)
        cli::cli_progress_bar("Scoring candidate pairs", total = n_slices)
    for (k in seq_len(n_slices)) {
        if (!is.null(bar)) cli::cli_progress_update(id = bar)
        t0 <- .bench_now()
        if (use_block) {
            found <- .candidate_run(codes, block_cap, nthreads,
                                    slices$from[k], slices$to[k],
                                    warn = k == 1L)
            xi <- found$x_index
            yi <- found$y_index
            ps <- found$pass
            rm(found)
        } else if (n_slices == 1L) {
            xi <- given$x_index
            yi <- given$y_index
            ps <- given$pass
        } else {
            at <- seq.int(slices$from[k] + 1, slices$to[k])
            xi <- given$x_index[at]
            yi <- given$y_index[at]
            ps <- given$pass[at]
        }
        timings$pairs <- timings$pairs + (.bench_now() - t0)
        n_candidates <- n_candidates + length(xi)
        if (use_block) pass_found <- pass_found + tabulate(ps, n_pass)

        # Pairs the caller has already dealt with come off before anything
        # is scored. Nearly every candidate has a record that is in no
        # excluded pair at all, which a lookup per record settles; only the
        # few left are looked up as pairs.
        if (!is.null(excluded) && length(excluded$code)) {
            t0 <- .bench_now()
            maybe <- which(excluded$in_1[xi] & excluded$in_2[yi])
            gone <- maybe[((xi[maybe] - 1) * n_2 + yi[maybe]) %in%
                              excluded$code]
            if (length(gone)) {
                n_excluded <- n_excluded + length(gone)
                xi <- xi[-gone]
                yi <- yi[-gone]
                ps <- ps[-gone]
            }
            timings$exclude <- timings$exclude + (.bench_now() - t0)
        }

        done <- score_slice(xi, yi, ps)
        n_scored <- n_scored + done$seen
        done$seen <- NULL
        parts[[k]] <- done
    }
    if (!is.null(bar)) cli::cli_progress_done(id = bar)
    # One slice is handed on as it is; several are laid end to end. The
    # slices ascend through the records, so the order is the one a single
    # run would have given.
    bind <- function(get) if (n_slices == 1L) get(parts[[1L]]) else
        unlist(lapply(parts, get), use.names = FALSE)
    xi <- bind(function(p) p$xi)
    yi <- bind(function(p) p$yi)
    ps <- bind(function(p) p$pass)
    scores <- lapply(seq_along(cmp), function(i)
        bind(function(p) p$scores[[i]]))
    rm(parts)
    n_pairs <- length(xi)
    for (i in seq_along(cmp)) timings[[score_names[i]]] <- score_time[i]
    if (use_filter) timings$filter <- filter_time

    # --- identifiers and carried columns, for the pairs that are left ------
    # Timed apart from the carried columns, because on a large unfiltered run
    # it is most of the wall clock and the caller can do something about it.
    t0 <- .bench_now()
    out <- list(pair_id = integer())
    out[[paste0("index", suffixes[1L])]] <- xi
    out[[paste0("index", suffixes[2L])]] <- yi
    out[["pass"]] <- ps
    if (show_key)
        out[["block_key"]] <- .rlink_block_key(keys_1, key_parts, ps, xi,
                                               n_pairs, nthreads)
    for (nm in keep_1) out[[paste0(nm, suffixes[1L])]] <- data_1[[nm]][xi]
    for (nm in keep_2) out[[paste0(nm, suffixes[2L])]] <- d2[[nm]][yi]
    for (i in seq_along(cmp)) out[[score_names[i]]] <- scores[[i]]
    rm(scores)
    timings$attach <- .bench_now() - t0
    t0 <- .bench_now()
    out[["pair_id"]] <- .rlink_pair_id(data_1, d2, pair_id, pair_id_default,
                                       dedupe, xi, yi, n_pairs, nthreads,
                                       unique_pairs = use_block)
    timings$pair_id <- .bench_now() - t0
    data.table::setDT(out)

    info <- lapply(seq_along(cmp), function(i) {
        s <- cmp[[i]]
        data.frame(score = s$name, comparator = s$label,
                   column_1 = s$col_1, column_2 = s$col_2,
                   scale = orient[[i]]$kind,
                   direction = orient[[i]]$direction,
                   # How many pairs the comparator was run on: all of them,
                   # unless a `filter` had already dropped some.
                   n_scored = cmp_scored[i])
    })

    # --- order and return --------------------------------------------------
    t0 <- .bench_now()
    score_on <- vapply(cmp, `[[`, "", "group_on")
    placed <- unlist(lapply(avail[avail %in% keep_cols], function(nm) c(
        if (nm %in% keep_1) paste0(nm, suffixes[1L]),
        if (nm %in% keep_2) paste0(nm, suffixes[2L]),
        if (score_position == "grouped") score_names[score_on == nm]
    )), use.names = FALSE)
    data.table::setcolorder(out, c(lead, placed,
                                   setdiff(names(out), c(lead, placed))))
    timings$assemble <- .bench_now() - t0

    total <- .bench_now() - t_all
    data.table::setattr(out, "timings", data.frame(
        stage = c(names(timings), "total"),
        seconds = unlist(c(timings, list(total)), use.names = FALSE)))
    data.table::setattr(out, "compare",
                        if (length(info)) do.call(rbind, info) else NULL)
    # Whether the two indices are into one file. Nothing in the columns says
    # so, and what reads the pairs afterwards has to know: when deduplicating
    # a record can be on either side of a pair, and when linking the same row
    # number on the two sides is two different records.
    data.table::setattr(out, "dedupe", dedupe)
    # How many records each side had. The pairs cannot say -- a record that
    # was never paired is in none of them -- and it is what the reduction
    # ratio, the records left unmatched and blocking_eval() all need.
    data.table::setattr(out, "n_records", c(nrow(data_1), nrow(d2)))
    # What each pass did: the report made before the pairs were, with what
    # was then found filled in. `n_kept` is what is in the table, which a
    # `filter` or an `exclude` leaves smaller than what the pass found.
    if (use_block) {
        blocking$passes$n_pairs <- pass_found
        blocking$passes$n_kept <- as.numeric(tabulate(ps, n_pass))
        data.table::setattr(out, "blocking", blocking)
    }
    # What the rule was and how much it removed, because a table of 40,000
    # pairs reads very differently once it is known to be what is left of
    # 30 million -- and because measures such as blocking_eval() are about
    # the candidates, which this no longer is.
    if (use_filter)
        data.table::setattr(out, "filter", list(
            rule = paste(deparse(rlang::quo_get_expr(quo_filter)),
                         collapse = " "),
            before_scoring = if (!length(stages[[1L]]$score) &&
                                 !held_back[1L]) stages[[1L]]$text
                             else NA_character_,
            n_candidates = n_candidates, n_scored = n_scored,
            n_kept = n_pairs, n_slices = n_slices))
    if (!is.null(excluded))
        data.table::setattr(out, "exclude", list(
            n_given = excluded$n_given, n_unknown = excluded$n_unknown,
            n_excluded = n_excluded))
    # `fs_pairs` ahead of the data.table classes, so that summary() and plot()
    # describe the result. data.table carries both this class and the
    # attributes above through `[`, `order()`, `head()` and `:=`, so they
    # survive a filter on the scores.
    data.table::setattr(out, "class",
                        c("fs_pairs", "data.table", "data.frame"))
    if (verbose) {
        for (x in info)
            cli::cli_alert_info(sprintf(
                "%s: %s (%s)%s.", x$score, x$comparator, x$scale,
                if (use_filter) sprintf(
                    ", scored on %s pairs",
                    format(x$n_scored, big.mark = ",", scientific = FALSE))
                else ""))
        if (!is.null(excluded))
            cli::cli_alert_info(sprintf(
                "`exclude` left out %s candidate pair%s.",
                format(n_excluded, big.mark = ",", scientific = FALSE),
                if (n_excluded == 1) "" else "s"))
        if (use_filter)
            cli::cli_alert_info(sprintf(
                paste0("`filter` kept %s of %s candidate pairs (%s scored), ",
                       "in %d slice%s."),
                format(n_pairs, big.mark = ","),
                format(n_candidates, big.mark = ",", scientific = FALSE),
                format(n_scored, big.mark = ",", scientific = FALSE),
                n_slices, if (n_slices == 1L) "" else "s"))
        cli::cli_alert_success(sprintf(
            "%s pairs, %d score column%s, in %.3fs.",
            format(n_pairs, big.mark = ","), length(cmp),
            if (length(cmp) == 1L) "" else "s", total))
    }
    out
}

# How many candidate pairs to generate, score and thin at a time when there is
# a `filter`. Large enough that the per-slice overheads vanish, small enough
# that a slice and its scores sit comfortably in memory: ten million pairs
# with half a dozen scores is about half a gigabyte.
.rlink_slice_pairs <- function() {
    value <- getOption("fast.string.rlink_slice_pairs", 1e7)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value < 1)
        stop(paste0("The option `fast.string.rlink_slice_pairs` must be a ",
                    "single number of at least 1."), call. = FALSE)
    value
}

# Whether the caller's rule keeps each pair of one slice: TRUE, FALSE, or NA
# where it could not decide, which the caller reads as "no".
#
# The rule sees the scores and `pass` as they stand, and every column of
# either input under its suffixed name -- `sex_1 == sex_2` is as natural a
# thing to rule on as a score. Those columns are promises: one is gathered
# for the slice only if the rule actually names it, so a rule about two
# scores never pays for carrying a dozen columns it does not read.
#
# `known` is every score the run produces, for the message: the part of a
# rule applied before scoring is given no scores to read, but the caller who
# mistyped one still needs to be told what they are called.
.rlink_filter <- function(quo, scores, score_names, pass, xi, yi, data_1,
                          data_2, suffixes, known = score_names) {
    n <- length(xi)
    env <- new.env(parent = emptyenv())
    promise <- function(name, data, column, index) {
        # Forced, or `column` would itself be a promise for the loop
        # variable and every name would resolve to the last column.
        force(column)
        delayedAssign(name, data[[column]][index], assign.env = env)
    }
    for (nm in names(data_1)) promise(paste0(nm, suffixes[1L]), data_1, nm, xi)
    for (nm in names(data_2)) promise(paste0(nm, suffixes[2L]), data_2, nm, yi)
    assign(paste0("index", suffixes[1L]), xi, envir = env)
    assign(paste0("index", suffixes[2L]), yi, envir = env)
    assign("pass", pass, envir = env)
    # Last, so that a score wins a name it shares with a column that is not
    # being carried.
    for (i in seq_along(scores)) assign(score_names[i], scores[[i]], envir = env)
    mask <- rlang::new_data_mask(env)
    mask$.data <- rlang::as_data_pronoun(mask)
    hit <- tryCatch(rlang::eval_tidy(quo, mask), error = function(cnd)
        stop(sprintf(paste0(
            "`filter` could not be evaluated: %s\nIt can name %s`pass`, and ",
            "any column of the data under its suffix, such as `%s`."),
            conditionMessage(cnd),
            if (length(known)) sprintf("the scores (%s), ",
                paste0("`", known, "`", collapse = ", "))
            else "",
            paste0(names(data_1)[1L], suffixes[1L])), call. = FALSE))
    if (!is.logical(hit) || !(length(hit) == n || length(hit) == 1L))
        stop(sprintf(paste0(
            "`filter` must give TRUE or FALSE for every pair; it gave %s of ",
            "length %s for %s pairs."), class(hit)[1L],
            format(length(hit), big.mark = ","), format(n, big.mark = ",")),
            call. = FALSE)
    if (length(hit) == 1L) hit <- rep(hit, n)
    hit
}

# The order a run scores and filters in: a list of stages, each a set of
# comparators to score and the part of the rule that can be read once they
# have been.
#
# `sex_1 == sex_2 & sn >= 0.85 & gn >= 0.8` asks three things. The first
# needs no comparator, so it is asked before any is run; `sn` is then scored
# for the pairs that are left and the second asked; `gn` is scored only for
# the pairs that passed both. A comparator that no part of the rule names
# runs last, on the pairs the whole rule kept. Nothing about the answer
# changes: a pair is kept when the whole rule is TRUE, which is when every
# part is, and which() over the survivors of which() is that -- a part that
# gives NA drops the pair either way.
#
# The rule is taken apart at its top-level `&`s only. Each round takes the
# part that needs the fewest comparators not yet scored (the first written,
# among equals), scores those, and asks every part they make readable.
#
# A score counts as named if it appears anywhere in the part, as a symbol or
# as a string, so `.data$sn` and `.data[["sn"]]` are both seen. A part that
# reaches a score some other way is found out when it is read, and held back
# until every score exists.
.rlink_stages <- function(quo, score_names) {
    n <- length(score_names)
    stage <- function(score, exprs = list(), env = NULL) list(
        score = score, exprs = exprs,
        rule = if (length(exprs)) .rlink_rule(exprs, env),
        text = if (length(exprs)) paste(deparse(rlang::quo_get_expr(
            .rlink_rule(exprs, env))), collapse = " ") else NA_character_)
    if (is.null(quo)) return(list(stage(seq_len(n))))
    env <- rlang::quo_get_env(quo)
    parts <- .rlink_conjuncts(rlang::quo_get_expr(quo))
    needs <- lapply(parts, function(e) which(score_names %in%
        c(all.names(e), .rlink_strings(e))))
    stages <- list()
    scored <- integer()
    score <- integer()
    open <- seq_along(parts)
    repeat {
        ready <- open[vapply(needs[open], function(v) all(v %in% scored), NA)]
        if (length(score) || length(ready))
            stages[[length(stages) + 1L]] <- stage(score, parts[ready], env)
        open <- setdiff(open, ready)
        if (!length(open)) break
        extra <- lapply(needs[open], setdiff, scored)
        score <- sort(extra[[which.min(lengths(extra))]])
        scored <- c(scored, score)
    }
    rest <- setdiff(seq_len(n), scored)
    if (length(rest) || !length(stages))
        stages[[length(stages) + 1L]] <- stage(rest)
    stages
}

# Several conditions as one rule: all of them must hold.
.rlink_rule <- function(exprs, env)
    rlang::new_quosure(Reduce(function(a, b) call("&", a, b), exprs), env)

# Whether a comparator is one of this package's own, which read an indexed
# vector without building it. Anyone else's function is handed an ordinary
# character vector, as it always was.
.rlink_native <- function(fn)
    identical(environment(fn), environment(.rlink_native))

# A compared column laid out by pair: `v[index]`, or for a comparator that
# can read it that way, the same thing left unbuilt.
.rlink_spread <- function(v, index, indexed, threads) {
    if (indexed && is.character(v) && is.integer(index) &&
        is.null(attributes(v)))
        fast_indexed_strings_impl(v, index, threads)
    else v[index]
}

.rlink_conjuncts <- function(e) {
    if (is.call(e) && length(e) == 2L && identical(e[[1L]], quote(`(`)))
        return(.rlink_conjuncts(e[[2L]]))
    if (is.call(e) && length(e) == 3L && identical(e[[1L]], quote(`&`)))
        return(c(.rlink_conjuncts(e[[2L]]), .rlink_conjuncts(e[[3L]])))
    list(e)
}

# Every string constant in an expression. A skipped argument, as in `x[, 1]`,
# is the empty symbol, which cannot be passed on.
.rlink_strings <- function(e) {
    if (is.character(e)) return(e)
    if (!is.call(e)) return(character())
    unlist(lapply(seq_along(e), function(i) {
        if (identical(e[[i]], quote(expr = ))) character()
        else .rlink_strings(e[[i]])
    }), use.names = FALSE)
}

.rlink_methods <- function(value) {
    choices <- c("exact", "sorted_neighbourhood")
    at <- if (is.character(value) && length(value))
        pmatch(value, choices, duplicates.ok = TRUE) else NA_integer_
    if (anyNA(at))
        stop(paste0("`block_method` must be \"exact\" or ",
                    "\"sorted_neighbourhood\": one for every pass, or one ",
                    "per pass."), call. = FALSE)
    choices[at]
}

# The largest block each pass keeps: one limit for every pass, or one each.
.rlink_block_caps <- function(value, n_pass) {
    if (!is.numeric(value) || !length(value) || anyNA(value) ||
        any(value < 1))
        stop(paste0("`max_block_size` must be a number of at least 1, or ",
                    "one per blocking pass."), call. = FALSE)
    .rlink_per_pass(value, n_pass, "max_block_size")
}

# One setting for every pass, or one each.
.rlink_per_pass <- function(value, n_pass, arg) {
    if (length(value) == 1L) return(rep(value, n_pass))
    if (length(value) != n_pass)
        stop(sprintf(paste0(
            "`%s` must be one value, or one per blocking pass: there %s %d ",
            "pass%s and it has %d value%s."), arg,
            if (n_pass == 1L) "is" else "are", n_pass,
            if (n_pass == 1L) "" else "es", length(value),
            if (length(value) == 1L) "" else "s"), call. = FALSE)
    value
}


# --- blocking specification -----------------------------------------------

# Control characters that cannot occur in the data: one joins the terms of a
# pass into a single key, the other stands for a missing term under
# match_missing. Both are checked against the data in .rlink_key() rather
# than assumed, because a silently collided key is a wrong answer. The
# function this replaces joined terms with "~", which does occur in real
# names and addresses, so c("A~B", "C") and c("A", "B~C") shared a key.
.rlink_sep <- "\u001f"
.rlink_na_key <- "\u0001"

.rlink_is_call_to <- function(e, fns)
    is.call(e) && is.name(e[[1L]]) && as.character(e[[1L]]) %in% fns

# The grammar: a top-level list() separates passes, and list()/c() inside a
# pass separates the terms joined into one key. Anything else is one pass. A
# c() at the top level is one pass of several terms, which is how the
# blockStrategy1 = c("a", "b") of the earlier function carries over.
.rlink_passes <- function(quo, arg) {
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    if (.rlink_is_call_to(expr, "list")) {
        elems <- as.list(expr)[-1L]
        labels <- names(elems)
        passes <- lapply(elems, function(e) {
            if (is.null(e)) return(NULL)
            if (.rlink_is_call_to(e, c("list", "c"))) as.list(e)[-1L]
            else list(e)
        })
    } else if (.rlink_is_call_to(expr, "c")) {
        passes <- list(as.list(expr)[-1L])
        labels <- NULL
    } else {
        passes <- list(list(expr))
        labels <- NULL
    }
    if (!length(passes))
        stop(sprintf("`%s` must name at least one blocking pass.", arg),
             call. = FALSE)
    for (p in seq_along(passes))
        if (!is.null(passes[[p]]) && !length(passes[[p]]))
            stop(sprintf("Pass %d of `%s` has no terms.", p, arg),
                 call. = FALSE)
    list(passes = passes, labels = labels, env = env)
}

.rlink_check_block_2 <- function(spec, spec_2, n_pass, arg = "block_1") {
    n2 <- length(spec_2$passes)
    if (n2 != n_pass)
        stop(sprintf(paste0(
            "`block_2` has %d pass%s but `%s` has %d; they are matched up ",
            "pass by pass."), n2, if (n2 == 1L) "" else "es", arg, n_pass),
            call. = FALSE)
    for (p in seq_len(n_pass)) {
        if (is.null(spec_2$passes[[p]])) next
        k1 <- length(spec$passes[[p]])
        k2 <- length(spec_2$passes[[p]])
        if (k1 != k2)
            stop(sprintf(paste0(
                "Pass %d has %d term%s in `%s` but %d in `block_2`; keys ",
                "built from different numbers of terms can never agree."),
                p, k1, if (k1 == 1L) "" else "s", arg, k2), call. = FALSE)
    }
    invisible(NULL)
}

# Errors are collected rather than raised one at a time. A typo in a blocking
# variable used to be reported with print() and then carried on into an NA
# column; reporting every bad name at once is what made that check worth
# having, so it is kept, as a stop().
.rlink_problems <- function() {
    e <- new.env(parent = emptyenv())
    e$msgs <- character()
    e
}

.rlink_note <- function(probs, ...) {
    probs$msgs <- c(probs$msgs, sprintf(...))
    NULL
}

# Both inputs' columns are listed when there are two, because the name that
# was not found may have been looked for in either: `block_1`'s expressions
# are evaluated against `data_2` as well unless `block_2` says otherwise.
.rlink_stop <- function(probs, data_1, data_2 = NULL) {
    if (!length(probs$msgs)) return(invisible(NULL))
    columns <- sprintf("Columns in `data_1`: %s.",
                       paste(names(data_1), collapse = ", "))
    if (!is.null(data_2) && !identical(names(data_2), names(data_1)))
        columns <- c(columns, sprintf("Columns in `data_2`: %s.",
                                      paste(names(data_2), collapse = ", ")))
    stop(paste(c(probs$msgs, columns), collapse = "\n"), call. = FALSE)
}

# One term of one pass. A length-1 string is a column name; anything else is
# evaluated with the data in front of the calling environment. `where` says
# where in the caller's code the term is, for the message if it fails.
.rlink_term <- function(e, mask, env, n, arg = NULL, pass = NULL,
                        slot = NULL, probs, where = NULL) {
    if (is.null(where))
        where <- sprintf("%s, pass %d, term %d", arg, pass, slot)
    if (is.character(e) && length(e) == 1L && !is.na(e)) {
        if (!e %in% names(mask))
            return(.rlink_note(probs, "%s: no column named `%s`", where, e))
        return(mask[[e]])
    }
    shown <- paste(deparse(e), collapse = " ")
    v <- tryCatch(rlang::eval_tidy(rlang::new_quosure(e, env), data = mask),
                  error = function(cnd)
                      .rlink_note(probs, "%s: %s - %s", where, shown,
                                  conditionMessage(cnd)))
    if (is.null(v)) return(NULL)
    if (is.data.frame(v) || !is.null(dim(v)))
        return(.rlink_note(probs, paste0(
            "%s: %s gave %s, but a term must be a vector. Take one column, ",
            "e.g. `double_metaphone(surname)$primary`."), where, shown,
            if (is.data.frame(v)) "a data frame" else "a matrix"))
    if (!is.atomic(v))
        return(.rlink_note(probs,
            "%s: %s gave a %s, but a term must be an atomic vector.",
            where, shown, class(v)[1L]))
    if (length(v) == 1L) v <- rep(v, n)
    if (length(v) != n)
        return(.rlink_note(probs,
            "%s: %s gave %d values, but the data has %d rows.",
            where, shown, length(v), n))
    v
}

# Terms are shared across passes by their written form, so an expression
# that is not a pure function of the data (a sample(), say) is evaluated
# once rather than once per pass -- the same contract the comparators have
# always had.
.rlink_term_cache <- function() new.env(parent = emptyenv())

.rlink_cached_term <- function(cache, e, mask, env, n, arg, pass, slot,
                               probs) {
    id <- paste(deparse(e), collapse = "
")
    held <- cache[[id]]
    # Wrapped, so that a term that failed (and left its message with
    # `probs`) is remembered as NULL rather than reported once per pass.
    if (!is.null(held)) return(held$value)
    value <- .rlink_term(e, mask, env, n, arg, pass, slot, probs)
    cache[[id]] <- list(value = value)
    value
}

# One blocking key per pass. A lone term keeps its own type, so numbers and
# dates still sort as numbers and dates under the sorted neighbourhood.
.rlink_key <- function(parts, pass, upper, na_strings, match_missing,
                       nthreads) {
    multi <- length(parts) > 1L
    # Only the characters actually used to build this key are rejected: the
    # separator when there is more than one term to join, the placeholder
    # when missings are being matched.
    guard <- c(if (multi) .rlink_sep, if (match_missing) .rlink_na_key)
    parts <- lapply(parts, .rlink_key_part, force_chr = multi, upper = upper,
                    na_strings = na_strings, match_missing = match_missing,
                    pass = pass, guard = guard, nthreads = nthreads)
    if (!multi) return(parts[[1L]])
    key <- do.call(paste, c(parts, list(sep = .rlink_sep)))
    if (!match_missing)
        key[Reduce(`|`, lapply(parts, is.na))] <- NA_character_
    key
}

.rlink_check_sep <- function(v, pass, guard, nthreads) {
    for (ch in guard)
        if (any(fgrepl(ch, v, fixed = TRUE, nthreads = nthreads),
                na.rm = TRUE))
            stop(sprintf(paste0(
                "Blocking values in pass %d contain a control character ",
                "(U+%04X) that is used to build the key for a pass."),
                pass, utf8ToInt(ch)), call. = FALSE)
    invisible(NULL)
}

.rlink_key_part <- function(v, force_chr, upper, na_strings, match_missing,
                            pass, guard, nthreads) {
    if (is.factor(v)) v <- as.character(v)
    # Upper-casing and na_strings are about text, so they never turn a lone
    # number or date into text: that would sort 10 before 9 under the sorted
    # neighbourhood.
    dated <- FALSE
    if (force_chr || match_missing) {
        text <- .rlink_date_chr(v, nthreads)
        dated <- !is.null(text)
        v <- if (dated) text else as.character(v)
    }
    if (is.character(v)) {
        v <- .rlink_na_strings(v, na_strings)
        # A date that came through format_date() is digits and dashes: there
        # is no case to fold and no control character to find, so it skips
        # two whole passes over the column.
        if (!dated) {
            if (upper) v <- fupper(v)
            # Before the placeholder goes in, or it would flag itself.
            if (length(guard)) .rlink_check_sep(v, pass, guard, nthreads)
        }
    }
    if (match_missing) v[is.na(v)] <- .rlink_na_key
    v
}

# A readable rendering of the key that paired each record, which was the most
# used diagnostic of the earlier function. Display only: the control
# characters are swapped for text.
#
# Each pass is rendered once per record, and the passes laid end to end, so
# one index picks every pair's key: as.character() of a Date and gsub() over
# every pair cost many times what they do over the records.
#
# A pass whose key was only ever a code (see .rlink_key_codes()) has no string
# to render, so its terms are joined here instead -- for the records that
# ended up in a pair and no others, which after a `filter` can be a small
# share of the file.
.rlink_block_key <- function(keys_1, key_parts, pass, xi, n_pairs,
                             nthreads = NULL) {
    render <- function(k) {
        if (!is.character(k)) return(.rlink_as_character(k, nthreads))
        k <- gsub(.rlink_na_key, "<NA>", k, fixed = TRUE)
        gsub(.rlink_sep, "|", k, fixed = TRUE)
    }
    coded <- !vapply(key_parts, is.null, NA)
    if (!any(coded)) {
        shown <- lapply(keys_1, render)
        if (length(shown) == 1L) return(shown[[1L]][xi])
        offset <- c(0L, cumsum(lengths(shown)))
        return(unlist(shown, use.names = FALSE)[xi + offset[pass]])
    }
    one <- length(keys_1) == 1L
    out <- rep(NA_character_, n_pairs)
    for (p in seq_along(keys_1)) {
        rows <- if (one) seq_len(n_pairs) else which(pass == p)
        if (!length(rows)) next
        x <- xi[rows]
        if (!coded[p]) {
            out[rows] <- render(keys_1[[p]])[x]
            next
        }
        records <- unique(x)
        terms <- lapply(key_parts[[p]], function(v) {
            v <- v[records]
            v[v == .rlink_na_key] <- "<NA>"
            v
        })
        out[rows] <- do.call(paste, c(terms, list(sep = "|")))[
            match(x, records)]
    }
    out
}

# The key of a pass of several terms, as an integer per record rather than as
# the terms pasted together.
#
# Pasting was the obvious way to join them and it was most of the cost of
# building a key: each record's key is a new string, R files every string in
# one global table, and a new entry costs about a microsecond -- so the keys
# of three passes over two files of three million records were twelve seconds
# of strings whose only use was to be compared for equality. Each term is
# instead numbered by its distinct values, over both files at once so that
# equal values get equal numbers, and the terms' numbers are combined: two
# records share a key exactly when they share every term's number.
#
# NULL when the combination could not be held exactly in a double, which
# takes more distinct values than a real file has; the caller then pastes.
# The sorted neighbourhood always pastes, because it needs the keys' order
# and not only their equality.
.rlink_key_codes <- function(parts_1, parts_2, pass, upper, na_strings,
                             match_missing, nthreads) {
    guard <- c(.rlink_sep, if (match_missing) .rlink_na_key)
    prepare <- function(parts) lapply(
        parts, .rlink_key_part, force_chr = TRUE, upper = upper,
        na_strings = na_strings, match_missing = match_missing, pass = pass,
        guard = guard, nthreads = nthreads)
    text_1 <- prepare(parts_1)
    text_2 <- if (!is.null(parts_2)) prepare(parts_2)
    n_1 <- length(text_1[[1L]])
    code <- NULL
    top <- 1
    for (k in seq_along(text_1)) {
        all <- if (is.null(text_2)) text_1[[k]]
               else c(text_1[[k]], text_2[[k]])
        values <- unique(all)
        values <- values[!is.na(values)]
        # NA where the term is missing, which makes the whole key missing
        # below: that is `match_missing = FALSE`. Under `match_missing` the
        # term already holds a placeholder instead and is numbered like any
        # other value.
        term <- data.table::chmatch(all, values)
        if (is.null(code)) {
            code <- term
            top <- length(values)
            next
        }
        if (top * length(values) >= 2^52) return(NULL)
        joint <- (code - 1) * length(values) + term
        seen <- unique(joint)
        seen <- seen[!is.na(seen)]
        code <- match(joint, seen)
        top <- length(seen)
    }
    list(x = code[seq_len(n_1)],
         y = if (!is.null(text_2)) code[n_1 + seq_along(text_2[[1L]])],
         parts = text_1)
}

# as.character() on a Date runs format.Date(), which reaches strftime() per
# element: for a date of birth it is by far the most expensive step in
# building a blocking key or a comparison value --- 147 ms per 200,000 dates
# against 11 ms for format_date(), which writes the same "YYYY-MM-DD" from
# integer calendar arithmetic, in parallel.
#
# The two agree on every four-digit year. Outside that, format_date() pads
# year 1 to "0001" where base R writes "1", and gives NA for a year past
# 9999 or a non-finite day where base R writes the number. .rlink_date_chr()
# therefore returns NULL for anything out of range, deciding it with a range
# check over the day numbers rather than per element, and the caller falls
# back to as.character().
.rlink_date_min <- -354285   # 1000-01-01
.rlink_date_max <- 2932896   # 9999-12-31

.rlink_date_chr <- function(v, nthreads = NULL) {
    if (!inherits(v, "Date")) return(NULL)
    days <- unclass(v)
    if (!is.numeric(days) || !is.null(attributes(days))) return(NULL)
    if (anyNA(days)) {
        # is.na() is TRUE of NaN too, which base R renders as "NaN"; only
        # a column that has missings at all pays for telling them apart.
        if (any(is.nan(days))) return(NULL)
        days <- days[!is.na(days)]
    }
    if (length(days) &&
        !(is.finite(min(days)) && min(days) >= .rlink_date_min &&
          max(days) <= .rlink_date_max))
        return(NULL)
    format_date(unclass(v), nthreads = nthreads)
}

# A stable identifier per pair, built from a record identifier column on each
# side rather than from the row number. Row numbers describe a particular run:
# re-block the same two files and pair 412 is a different pair, so a judgement
# filed against it is silently about something else. Record identifiers do not
# move, which is what lets rLink_inspect() keep a review across re-runs.
.rlink_pair_sep <- "|"

# This is the one part of rLink() that cannot be made cheap. Every pair gets
# a string of its own, R keeps every string in one global table, and adding a
# new one to it costs about a microsecond -- so ten million pairs are ten
# seconds here, whoever builds the string (a hand-written C++ builder was
# measured against paste0() and came out within 15% of it). It is why
# `pair_id` is built after `filter` has thinned the pairs rather than before,
# and why `pair_id = NULL` is worth knowing about for a run whose pairs will
# never be reviewed.
.rlink_pair_id <- function(data_1, d2, pair_id, default, dedupe, xi, yi,
                           n_pairs, nthreads, unique_pairs = FALSE) {
    cols <- .rlink_id_columns(data_1, d2, pair_id, default, dedupe)
    if (is.null(cols)) return(seq_len(n_pairs))
    nm_1 <- cols[1L]
    nm_2 <- cols[2L]
    # Rendered once per record and then indexed, never once per pair, which is
    # the discipline .rlink_block_key() follows and for the same reason.
    a <- .rlink_as_character(data_1[[nm_1]], nthreads)
    shared <- dedupe && identical(nm_1, nm_2)
    b <- if (shared) a else .rlink_as_character(d2[[nm_2]], nthreads)
    .rlink_check_pair_sep(a, nm_1, "data_1", nthreads)
    if (!shared)
        .rlink_check_pair_sep(b, nm_2, if (dedupe) "data_1" else "data_2",
                              nthreads)
    # paste0() of nothing with a separator is the separator, once: a table of
    # no pairs would be handed one identifier.
    if (!n_pairs) return(character())
    left <- a[xi]
    right <- b[yi]
    key <- paste0(left, .rlink_pair_sep, right)
    key[is.na(left) | is.na(right)] <- NA_character_
    # Blocking reports each pair of records once, so two pairs can only
    # share an identifier if two records do. Where no identifier repeats
    # among the records there is therefore nothing to look for among the
    # pairs, and a pass over every pair's string is saved. Pairs supplied by
    # the caller carry no such guarantee -- the same pair may be listed
    # twice -- so those are always checked.
    settled <- unique_pairs && !anyDuplicated(a) &&
        (shared || !anyDuplicated(b))
    .rlink_warn_pair_id(key, nm_1, nm_2, check_duplicates = !settled)
    key
}

# The separator is rejected in the data rather than assumed absent, because a
# key that collides is a wrong answer rather than an untidy one -- the same
# stance .rlink_check_sep() takes for the blocking keys.
.rlink_check_pair_sep <- function(v, column, side, nthreads) {
    if (any(fgrepl(.rlink_pair_sep, v, fixed = TRUE, nthreads = nthreads),
            na.rm = TRUE))
        stop(sprintf(paste0(
            "Values of `%s` in `%s` contain `%s`, which joins the two ",
            "identifiers into `pair_id`, so the key would be ambiguous. Name ",
            "another `pair_id` column, or take the character out first."),
            column, side, .rlink_pair_sep), call. = FALSE)
    invisible(NULL)
}

.rlink_warn_pair_id <- function(key, nm_1, nm_2, check_duplicates = TRUE) {
    n_na <- sum(is.na(key))
    n_dup <- if (check_duplicates) sum(duplicated(key[!is.na(key)])) else 0L
    if (!n_na && !n_dup) return(invisible(NULL))
    parts <- c(
        if (n_na) sprintf("%s missing", format(n_na, big.mark = ",")),
        if (n_dup) sprintf("%s duplicated", format(n_dup, big.mark = ",")))
    warning(sprintf(paste0(
        "`pair_id` has %s value%s, because `%s` does not identify records ",
        "uniquely. A review keyed on `pair_id` cannot tell those pairs ",
        "apart."),
        paste(parts, collapse = " and "),
        if (n_na + n_dup == 1L) "" else "s",
        if (identical(nm_1, nm_2)) nm_1 else paste0(nm_1, "`/`", nm_2)),
        call. = FALSE)
    invisible(NULL)
}

.rlink_as_character <- function(v, nthreads = NULL) {
    text <- .rlink_date_chr(v, nthreads)
    if (is.null(text)) as.character(v) else text
}

# v[v %in% na_strings] <- NA, but %in% builds a hash table of na_strings and
# probes it once per record. The default is the single string "", for which
# a direct comparison is three times faster. which() is what drops the NAs
# that == leaves behind.
.rlink_na_strings <- function(v, na_strings) {
    if (!length(na_strings)) return(v)
    hit <- if (length(na_strings) == 1L) which(v == na_strings)
           else which(v %in% na_strings)
    if (length(hit)) v[hit] <- NA_character_
    v
}

.rlink_same_type <- function(a, b)
    identical(class(a), class(b)) ||
        (is.numeric(a) && is.numeric(b) &&
         is.null(oldClass(a)) && is.null(oldClass(b)))

.rlink_label <- function(labels, p, bare = FALSE) {
    if (is.null(labels) || is.na(labels[p]) || !nzchar(labels[p])) ""
    else if (bare) labels[p]
    else sprintf(" (%s)", labels[p])
}

# What each blocking pass will do, worked out from the codes alone.
#
# The earlier function printed an estimate per block, which is how you find
# the pass that is about to explode; this is that, returned rather than
# printed, and with the rest of what goes wrong in blocking beside it: the
# records that have no key and so can pair with nothing, and the keys that
# hold the most records -- which are, more often than a common surname, a
# placeholder such as "UNKNOWN" that should have been in `na_strings`.
#
# Everything is counted per distinct key, so the cost is a tabulate() of the
# records per pass however many pairs the pass goes on to make. The counts
# are doubles throughout: the function this replaces multiplied integer
# frequencies and overflowed to NA on exactly the blocks it existed to warn
# about.
#
# A block is the records sharing one key. Under the sorted neighbourhood the
# pairs credited to a key are those between its records and the records in
# its window, which for a window of one key is the same thing.
.rlink_blocking_report <- function(codes, max_block_size, labels, key_1,
                                   key_2, keys_1, key_parts, n_records,
                                   nthreads = NULL, top = 5L) {
    dedupe <- codes$dedupe
    n_pass <- length(codes$x)
    cap <- rep_len(max_block_size, n_pass)
    rows <- vector("list", n_pass)
    large <- vector("list", n_pass)
    for (p in seq_len(n_pass)) {
        x <- codes$x[[p]]
        n <- codes$n[p]
        half <- codes$half[p]
        in_1 <- as.numeric(tabulate(x, n))
        in_2 <- if (dedupe) in_1 else as.numeric(tabulate(codes$y[[p]], n))
        # The size `max_block_size` is measured against, and the rule the
        # pair generator applies: exact passes only, both files together.
        size <- if (dedupe) in_1 else in_1 + in_2
        skipped <- if (half == 0L && is.finite(cap[p]))
            which(size > cap[p]) else integer()
        # Linking, a key's pairs are its records in the first file against
        # those in its window in the second. Deduplicating, every pair would
        # be met from both of its records, so a key is credited with the
        # pairs among its own records and those reaching up the window, and
        # each pair is counted once.
        if (dedupe) {
            pairs <- in_1 * (in_1 - 1) / 2
            if (half > 0L) {
                cum <- c(0, cumsum(in_1))
                at <- seq_len(n)
                pairs <- pairs +
                    in_1 * (cum[pmin(n, at + half) + 1L] - cum[at + 1L])
            }
        } else {
            pairs <- in_1 * (if (half > 0L) .candidate_window(codes, p)
                             else in_2)
        }
        pairs[skipped] <- 0

        rows[[p]] <- data.frame(
            pass = p, label = labels[[p]], key_1 = key_1[[p]],
            key_2 = key_2[[p]],
            method = if (half > 0L) "sorted_neighbourhood" else "exact",
            window = if (half > 0L) 2L * half + 1L else NA_integer_,
            # A record with no key is one that was not counted under any.
            n_missing_1 = as.integer(length(x) - sum(in_1)),
            n_missing_2 = if (dedupe) NA_integer_ else
                as.integer(length(codes$y[[p]]) - sum(in_2)),
            n_keys = n,
            largest_block = if (n) max(size) else 0,
            n_skipped = length(skipped),
            n_expected = sum(pairs),
            n_pairs = NA_real_, n_kept = NA_real_,
            row.names = NULL, stringsAsFactors = FALSE)

        # The few keys that make the most pairs, without sorting what can be
        # millions of them: the largest is taken `top` times over.
        pick <- integer()
        rest <- pairs
        for (i in seq_len(min(top, n))) {
            at <- which.max(rest)
            if (rest[at] <= 0) break
            pick <- c(pick, at)
            rest[at] <- 0
        }
        # One record holding each of those keys, to render the key from.
        # `%in%` hashes the five codes rather than every record's.
        held <- which(x %in% pick)
        large[[p]] <- data.frame(
            pass = rep(p, length(pick)),
            key = .rlink_render_key(keys_1, key_parts, p,
                                    held[match(pick, x[held])], nthreads),
            n_1 = in_1[pick],
            n_2 = if (dedupe) rep(NA_real_, length(pick)) else in_2[pick],
            pairs = pairs[pick],
            stringsAsFactors = FALSE)
    }
    structure(list(
        passes = do.call(rbind, rows),
        largest = do.call(rbind, large),
        n_records = n_records, dedupe = dedupe,
        max_block_size = max_block_size
    ), class = "fs_blocking")
}

# The key of pass `p` for a handful of records, as `block_key` would show it.
# .rlink_block_key() renders a pass for every record so that one index can
# pick every pair's key; here there are five records, so they are picked
# first and rendered after.
.rlink_render_key <- function(keys_1, key_parts, p, records, nthreads = NULL) {
    if (!length(records)) return(character())
    if (!is.null(key_parts[[p]])) {
        terms <- lapply(key_parts[[p]], function(v) {
            v <- v[records]
            v[which(v == .rlink_na_key)] <- "<NA>"
            v
        })
        return(do.call(paste, c(terms, list(sep = "|"))))
    }
    k <- keys_1[[p]][records]
    if (!is.character(k)) return(.rlink_as_character(k, nthreads))
    k <- gsub(.rlink_na_key, "<NA>", k, fixed = TRUE)
    gsub(.rlink_sep, "|", k, fixed = TRUE)
}

# The message for a run stopped by `max_pairs`: which pass is to blame, and
# the block inside it that is, because that is what has to change.
.rlink_too_many <- function(blocking, max_pairs) {
    big <- function(v) format(v, big.mark = ",", scientific = FALSE)
    p <- blocking$passes
    lines <- vapply(seq_len(nrow(p)), function(i) {
        worst <- blocking$largest[blocking$largest$pass == p$pass[i], ]
        sprintf("  Pass %d%s: %s expected%s", p$pass[i],
                if (nzchar(p$label[i])) sprintf(" (%s)", p$label[i]) else "",
                big(p$n_expected[i]),
                if (nrow(worst)) sprintf(
                    "; its largest block, `%s`, makes %s of them.",
                    worst$key[1L], big(worst$pairs[1L])) else ".")
    }, "")
    paste(c(
        sprintf(paste0("The blocking is expected to give up to %s candidate ",
                       "pairs, more than `max_pairs` (%s)."),
                big(sum(p$n_expected)), big(max_pairs)),
        lines,
        paste0("Block on more specific keys, add a placeholder to ",
               "`na_strings`, set `max_block_size`, or raise `max_pairs`. ",
               "`dry_run = TRUE` returns this as a table.")),
        collapse = "\n")
}

# Which column of each input identifies its records, or NULL when the pairs
# are to be numbered. The default name is a convention, not a promise: a file
# that does not use it still links, numbered as it always was. A name the
# caller chose must be there.
.rlink_id_columns <- function(data_1, d2, pair_id, default, dedupe) {
    if (is.null(pair_id)) return(NULL)
    nm_1 <- pair_id[[1L]]
    nm_2 <- if (length(pair_id) == 2L) pair_id[[2L]] else pair_id[[1L]]
    gone_1 <- !nm_1 %in% names(data_1)
    gone_2 <- !nm_2 %in% names(d2)
    if (gone_1 || gone_2) {
        if (default) return(NULL)
        side <- if (gone_1 || dedupe) "data_1" else "data_2"
        held <- if (gone_1) data_1 else d2
        stop(sprintf(paste0(
            "`pair_id` names `%s`, which is not a column of `%s`.\n",
            "Columns in `%s`: %s."),
            if (gone_1) nm_1 else nm_2, side, side,
            paste(names(held), collapse = ", ")), call. = FALSE)
    }
    c(nm_1, nm_2)
}

# The two index columns of a pair table: rLink()'s own under whatever
# suffixes it was given, else the names every pair function here uses.
.rlink_index_columns <- function(nms, suffixes = NULL) {
    own <- paste0("index", suffixes)
    if (length(own) == 2L && all(own %in% nms)) return(own)
    at <- .pair_columns(nms)
    if (is.character(at)) return(at)
    found <- grep("^index", nms, value = TRUE)
    if (length(found) == 2L) return(found)
    NULL
}

# The pairs to leave out, as one number each.
#
# Given as `pair_id`s they are found again through the identifier columns,
# so they survive the files being re-ordered, added to or cut down between
# runs -- which is the case `exclude` exists for: a second, looser run that
# should not hand back what the first already settled. Given as a table of
# row numbers they are taken at their word, which is only safe against the
# same data, so a table that remembers a different number of records is
# refused rather than applied to the wrong rows.
#
# (x - 1) * n_2 + y is exact in a double for any two files that could be
# linked at all, and one number per pair is what makes the test for each
# candidate a single hash lookup.
.rlink_exclude <- function(exclude, data_1, d2, pair_id, default, dedupe,
                           suffixes, nthreads = NULL) {
    n_1 <- as.numeric(nrow(data_1))
    n_2 <- as.numeric(nrow(d2))
    if (is.factor(exclude)) exclude <- as.character(exclude)
    if (is.character(exclude)) {
        cols <- .rlink_id_columns(data_1, d2, pair_id, default, dedupe)
        if (is.null(cols))
            stop(paste0(
                "`exclude` was given as `pair_id`s, but this run numbers its ",
                "pairs, and a number says nothing about which records they ",
                "were. Name the identifier column in `pair_id`, or give ",
                "`exclude` as a table of row numbers."), call. = FALSE)
        a <- .rlink_as_character(data_1[[cols[1L]]], nthreads)
        shared <- dedupe && identical(cols[1L], cols[2L])
        b <- if (shared) a else .rlink_as_character(d2[[cols[2L]]], nthreads)
        if (anyDuplicated(a[!is.na(a)]) ||
            (!shared && anyDuplicated(b[!is.na(b)])))
            stop(sprintf(paste0(
                "`exclude` was given as `pair_id`s, but `%s` does not ",
                "identify records uniquely, so a `pair_id` does not say ",
                "which pair to leave out."),
                if (identical(cols[1L], cols[2L])) cols[1L]
                else paste(cols, collapse = "`/`")), call. = FALSE)
        at <- regexpr(.rlink_pair_sep, exclude, fixed = TRUE)
        ok <- !is.na(exclude) & at > 0L
        i <- j <- rep(NA_integer_, length(exclude))
        i[ok] <- match(substr(exclude[ok], 1L, at[ok] - 1L), a)
        j[ok] <- match(substr(exclude[ok], at[ok] + 1L, nchar(exclude[ok])),
                       b)
    } else if (is.data.frame(exclude)) {
        at <- .rlink_index_columns(names(exclude), suffixes)
        if (is.null(at))
            stop(sprintf(paste0(
                "`exclude` must hold the pairs' two index columns: `%s` and ",
                "`%s`, or `x_index` and `y_index`."),
                paste0("index", suffixes[1L]), paste0("index", suffixes[2L])),
                call. = FALSE)
        i <- exclude[[at[1L]]]
        j <- exclude[[at[2L]]]
        if (!is.numeric(i) || !is.numeric(j) || anyNA(i) || anyNA(j) ||
            any(i != floor(i)) || any(j != floor(j)))
            stop("The index columns of `exclude` must be whole numbers.",
                 call. = FALSE)
        was <- attr(exclude, "n_records")
        moved <- !is.null(was) &&
            !isTRUE(all.equal(as.numeric(was), c(n_1, n_2)))
        if (moved || (length(i) && (min(i) < 1 || max(i) > n_1 ||
                                    min(j) < 1 || max(j) > n_2)))
            stop(paste0(
                "`exclude` holds row numbers that do not belong to this ",
                "data: ", if (moved) sprintf(
                    "it was made from %s and %s records, and these are %s and %s",
                    format(was[1L], big.mark = ","),
                    format(was[2L], big.mark = ","),
                    format(n_1, big.mark = ","), format(n_2, big.mark = ","))
                else "some are outside it",
                ". Row numbers only mean the same records against the same ",
                "data; give `exclude` as `pair_id`s instead, which do not ",
                "move."), call. = FALSE)
    } else {
        stop(paste0(
            "`exclude` must be a character vector of `pair_id`s, or a data ",
            "frame of pairs such as an earlier `rLink()` result."),
            call. = FALSE)
    }
    n_given <- length(i)
    known <- which(!is.na(i) & !is.na(j))
    i <- as.numeric(i[known])
    j <- as.numeric(j[known])
    # One file: a pair is the same pair whichever way round it was written.
    if (dedupe) {
        low <- pmin(i, j)
        j <- pmax(i, j)
        i <- low
    }
    if (n_1 * n_2 >= 2^53)
        stop("`exclude` cannot be used on files this large.", call. = FALSE)
    # Which records are in any excluded pair. A candidate whose records are
    # not both flagged cannot be one, and that is nearly all of them.
    in_1 <- logical(n_1)
    in_1[i] <- TRUE
    in_2 <- logical(n_2)
    in_2[j] <- TRUE
    list(code = unique((i - 1) * n_2 + j), in_1 = in_1, in_2 = in_2,
         n_given = n_given, n_unknown = n_given - length(known))
}

# Pairs found some other way: the `x_index` and `y_index` every pair
# function here returns, or the index columns of an earlier rLink() result,
# which is how a run is scored again with other comparators. Only the pairs
# are read, and a result made from files of another size is refused for the
# reason `exclude` refuses one: its row numbers are about other records.
.rlink_given_pairs <- function(pairs, n_1, n_2, suffixes = NULL) {
    at <- if (is.data.frame(pairs))
        .rlink_index_columns(names(pairs), suffixes)
    if (is.null(at))
        stop(paste0("`pairs` must be a data frame with `x_index` and ",
                    "`y_index`, or an earlier rLink() result."),
             call. = FALSE)
    was <- attr(pairs, "n_records")
    if (!is.null(was) && !isTRUE(all.equal(as.numeric(was),
                                           as.numeric(c(n_1, n_2)))))
        stop(sprintf(paste0(
            "`pairs` was made from %s and %s records, and these are %s and ",
            "%s: its row numbers do not belong to this data."),
            format(was[1L], big.mark = ","), format(was[2L], big.mark = ","),
            format(n_1, big.mark = ","), format(n_2, big.mark = ",")),
            call. = FALSE)
    out <- data.frame(
        x_index = as.integer(pairs[[at[1L]]]),
        y_index = as.integer(pairs[[at[2L]]]),
        pass = if ("pass" %in% names(pairs)) as.integer(pairs$pass)
               else rep(NA_integer_, nrow(pairs)))
    if (nrow(out) &&
        (anyNA(out$x_index) || anyNA(out$y_index) ||
         min(out$x_index) < 1L || min(out$y_index) < 1L ||
         max(out$x_index) > n_1 || max(out$y_index) > n_2))
        stop("`pairs` holds indices outside `data_1` or `data_2`.",
             call. = FALSE)
    out
}


# --- comparators -----------------------------------------------------------

# compare = list(jaro_winkler(surname), sn = jaccard_index(surname, q = 3))
# The call's first argument is the column expression; `a | b` takes the left
# from data_1 and the right from data_2. The rest are the comparator's own
# options. This one spec replaces the winkler1/winkler2/damLev1/damLev2 and
# winklerSplitVec1/2/damLevSplitVec1/2 arguments it was built from.
.rlink_compare <- function(quo, taken) {
    if (rlang::quo_is_null(quo)) return(list())
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    elems <- if (.rlink_is_call_to(expr, "list")) as.list(expr)[-1L]
             else list(expr)
    nms <- names(elems)
    out <- vector("list", length(elems))
    for (i in seq_along(elems)) {
        e <- elems[[i]]
        if (!is.call(e) || length(e) < 2L)
            stop(sprintf(paste0(
                "Element %d of `compare` must be a comparator call such as ",
                "`jaro_winkler(surname)`."), i), call. = FALSE)
        # A bare `function(a, b) ...` names no column to compare, and its
        # second element is the formals rather than an expression, so it
        # would otherwise be read as one.
        if (.rlink_is_call_to(e, "function"))
            stop(sprintf(paste0(
                "Element %d of `compare` is a function on its own, so it ",
                "does not say which column to compare. Apply it to one, ",
                "e.g. `list(my_score = (function(a, b) a == b)(postcode))`."),
                i), call. = FALSE)
        fn_expr <- e[[1L]]
        fn <- tryCatch(eval(fn_expr, env), error = function(cnd) NULL)
        if (!is.function(fn))
            stop(sprintf("Element %d of `compare` does not name a function.",
                         i), call. = FALSE)
        label <- if (is.name(fn_expr)) as.character(fn_expr)
                 else if (.rlink_is_call_to(fn_expr, c("::", ":::")))
                     as.character(fn_expr[[3L]])
                 else NA_character_

        # A comparator of numbers or dates has to be handed numbers or
        # dates. The two this package provides are known; one of the caller's
        # own says so by wrapping its column in I(), as in `my_cmp(I(age))`,
        # which is otherwise a column like any other.
        typed <- identical(fn, numeric_compare) || identical(fn, date_compare)
        unwrap <- function(x) {
            if (.rlink_is_call_to(x, "I") && length(x) == 2L) {
                typed <<- TRUE
                x[[2L]]
            } else x
        }
        col <- unwrap(e[[2L]])
        if (.rlink_is_call_to(col, "|")) {
            expr_1 <- unwrap(col[[2L]])
            expr_2 <- unwrap(col[[3L]])
        } else {
            expr_1 <- expr_2 <- col
        }
        rest <- as.list(e)[-(1:2)]
        args <- lapply(rest, function(a) eval(a, env))
        names(args) <- names(rest)

        col_1 <- .rlink_col_name(expr_1)
        col_2 <- .rlink_col_name(expr_2)
        plain <- !is.na(col_1) && identical(expr_1, expr_2)
        named <- !is.null(nms) && !is.na(nms[i]) && nzchar(nms[i])
        nm <- if (named) nms[i]
              else if (plain && !is.na(label)) paste0(col_1, "_", label)
              else NA_character_
        if (is.na(nm))
            stop(sprintf(paste0(
                "Element %d of `compare` needs a name for its score column, ",
                "because %s. Give it one, e.g. `list(my_score = %s)`."), i,
                if (is.na(label)) "its comparator is anonymous"
                else "it does not compare one plain column on both sides",
                paste(deparse(e), collapse = " ")), call. = FALSE)

        out[[i]] <- list(
            name = nm,
            label = if (is.na(label)) "<anonymous>" else label,
            fn = fn, args = args, env = env, typed = typed,
            expr_1 = expr_1, expr_2 = expr_2,
            col_1 = if (is.na(col_1))
                paste(deparse(expr_1), collapse = " ") else col_1,
            col_2 = if (is.na(col_2))
                paste(deparse(expr_2), collapse = " ") else col_2,
            # Only a plain column compared on both sides has a home beside
            # its own pair of output columns.
            group_on = if (plain) col_1 else "")
    }
    names(out) <- NULL
    used <- vapply(out, `[[`, "", "name")
    dup <- used[duplicated(used)]
    if (length(dup))
        stop(sprintf(paste0(
            "Two comparators would both produce the column `%s`. Name them, ",
            "e.g. `list(%s_a = ..., %s_b = ...)`."),
            dup[1L], dup[1L], dup[1L]), call. = FALSE)
    clash <- intersect(used, taken)
    if (length(clash))
        stop(sprintf(paste0(
            "The score column `%s` would overwrite a column already on the ",
            "output. Give that comparator another name."), clash[1L]),
            call. = FALSE)
    out
}

.rlink_col_name <- function(e) {
    if (is.name(e)) return(as.character(e))
    if (is.character(e) && length(e) == 1L && !is.na(e)) return(e)
    NA_character_
}

# A compared column as its comparator should receive it, once per record.
#
# A string comparator gets text, whatever the column was: that is what lets
# jaro_winkler() be pointed at a date or a postcode held as a number. A typed
# comparator -- numeric_compare(), date_compare(), or one whose column the
# caller wrapped in I() -- gets a number, a date or a logical exactly as it
# is stored. Turning those into text first made numeric_compare() an error
# and made date_compare() parse a string back into the date it had just been,
# once for every pair; on 1.6 million pairs that was 19 seconds of a 20
# second run.
.rlink_value <- function(v, typed, upper, na_strings, nthreads = NULL) {
    if (is.factor(v)) v <- as.character(v)
    if (typed && !is.character(v)) return(v)
    dated <- FALSE
    if (!is.character(v)) {
        text <- .rlink_date_chr(v, nthreads)
        dated <- !is.null(text)
        v <- if (dated) text else as.character(v)
    }
    v <- .rlink_na_strings(v, na_strings)
    if (upper && !dated) v <- fupper(v)
    v
}

.rlink_call <- function(fn, a, b, args, nthreads) {
    call_args <- c(list(a, b), args)
    # Only where the comparator declares it: a plain function(a, b) supplied
    # by the caller would otherwise be handed an argument it cannot take.
    if ("nthreads" %in% names(formals(fn)) &&
        !"nthreads" %in% names(call_args))
        call_args$nthreads <- nthreads
    do.call(fn, call_args)
}

# Comparators disagree about which end of their scale is an exact match, so
# each is probed once on a calibration pair: one identical, one sharing no
# characters. Reading the identical score against the disjoint one gives both
# the orientation and the scale.
#
#   identical > disjoint, identical == 1  -> similarity on 0-1    (as is)
#   identical > disjoint, identical >  1  -> similarity to a max  (/ max)
#   identical < disjoint, disjoint  <= 1  -> distance on 0-1      (1 - d)
#   identical < disjoint, disjoint  >  1  -> edit distance    (1 - d / len)
#
# An unbounded edit distance is normalised pairwise by the longer of the two
# strings. The earlier function did this with max(nchar(c(m, n))), which is
# the maximum over both whole vectors, so every score was divided by the
# longest string anywhere in the data rather than in the pair.
.rlink_probe_a <- c("ABCDEFGHIJKL", "ABCDEFGHIJKL")
.rlink_probe_b <- c("ABCDEFGHIJKL", "MNOPQRSTUVWX")

# The calibration pair for one comparator: an identical pair and a pair as
# unlike as the type allows. A typed comparator is probed in its own type --
# two strings of letters are an error to numeric_compare() and are not dates
# to date_compare(), which is why both used to come back "not detected" and
# the review app warned that it was guessing which way round they ran.
.rlink_probe <- function(fn, typed, sample) {
    if (typed) {
        if (identical(fn, date_compare) || inherits(sample, "Date")) {
            day <- structure(c(10957, 10957, -25402), class = "Date")
            return(list(a = day[c(1L, 2L)], b = day[c(1L, 3L)]))
        }
        if (inherits(sample, "POSIXct")) {
            at <- .POSIXct(c(946684800, 946684800, -2194732800), tz = "UTC")
            return(list(a = at[c(1L, 2L)], b = at[c(1L, 3L)]))
        }
        if (is.logical(sample))
            return(list(a = c(TRUE, TRUE), b = c(TRUE, FALSE)))
        if (is.integer(sample))
            return(list(a = c(1L, 1L), b = c(1L, 1000000L)))
        if (identical(fn, numeric_compare) || is.numeric(sample))
            return(list(a = c(1, 1), b = c(1, 1e6)))
    }
    list(a = .rlink_probe_a, b = .rlink_probe_b)
}

#
# convert() is given the compared values per record (a, b) and the pairs'
# indices into them, not the values per pair, so lengths are counted once
# per record.
.rlink_orient <- function(fn, args, normalise, nthreads, typed = FALSE,
                          sample = NULL) {
    probe <- .rlink_probe(fn, typed, sample)
    # The probe runs even when no conversion is wanted. `normalise = FALSE`
    # leaves a score on its own scale, and then which end of it means "match"
    # is the one thing a reader cannot recover from the column itself --- so
    # the probed direction is reported either way, and threshold_sweep() cuts
    # by that rather than guessing from the comparator's name.
    #
    # A warning is muffled rather than taken as failure: the probe's score
    # is still good, and the real call warns again if the data merit it.
    p <- tryCatch(
        withCallingHandlers(
            .rlink_call(fn, probe$a, probe$b, args, nthreads),
            warning = function(w) invokeRestart("muffleWarning")),
        error = function(cnd) NULL)
    usable <- !(is.null(p) || length(p) != 2L ||
                !(is.numeric(p) || is.logical(p)) || anyNA(p))
    hi <- if (usable) as.numeric(p[[1L]]) else NA_real_
    lo <- if (usable) as.numeric(p[[2L]]) else NA_real_
    if (usable && (!is.finite(hi) || !is.finite(lo) ||
                   isTRUE(all.equal(hi, lo))))
        usable <- FALSE
    probed <- if (!usable) NA_character_ else if (hi > lo) "higher" else "lower"

    keep <- function(kind, direction = probed)
        list(kind = kind, direction = direction,
             convert = function(s, a, b, xi, yi) s)
    if (!normalise) return(keep("as is"))
    if (!usable) return(keep("not detected"))
    # Past here the score is converted so that 1 is an exact match, whatever
    # the comparator returned, so the column that comes out is always
    # higher-is-better even when `scale` records that the input was a distance.
    if (hi > lo) {
        if (isTRUE(all.equal(hi, 1))) return(keep("similarity", "higher"))
        return(list(kind = sprintf("similarity/%g", hi), direction = "higher",
                    convert = function(s, a, b, xi, yi) s / hi))
    }
    if (lo <= 1.5)
        return(list(kind = "distance", direction = "higher",
                    convert = function(s, a, b, xi, yi) 1 - s))
    # An unbounded distance between two numbers or two dates -- a difference
    # in years, say -- has no string length to be divided by, and no other
    # natural ceiling. It is left as it is and reported as lower-is-a-match,
    # which is what the sweep and the review app then cut it by.
    if (typed) return(keep("distance, not normalised", "lower"))
    # Lengths in the unit the comparator counts edits in: a byte-wise
    # distance over a character length would charge an accented letter two
    # edits out of one.
    ub <- if ("use_bytes" %in% names(args)) args$use_bytes
          else formals(fn)$use_bytes
    unit <- if (isTRUE(ub)) "bytes" else "chars"
    # 1 - distance / the longer string, 1 where both are empty, kept within
    # 0-1. The lengths are counted once per record; the rest is one pass
    # over the pairs.
    threads <- .as_nthreads(nthreads)
    list(kind = "edit distance", direction = "higher",
         convert = function(s, a, b, xi, yi)
             fast_edit_scale_impl(as.numeric(s), nchar(a, unit),
                                  nchar(b, unit), xi, yi, threads))
}

.rlink_check_names <- function(d, arg) {
    nm <- names(d)
    if (is.null(nm) || anyNA(nm) || !all(nzchar(nm)))
        stop(sprintf("Every column of `%s` must have a name.", arg))
    if (anyDuplicated(nm))
        stop(sprintf("`%s` has duplicate column names: %s.", arg,
                     paste0("`", unique(nm[duplicated(nm)]), "`",
                            collapse = ", ")))
    invisible(NULL)
}
