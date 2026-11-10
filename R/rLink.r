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
#' names, is one pass.
#'
#' Expressions see the data first and the calling environment behind it, so
#' `soundex(surname, nthreads = my_threads)` resolves `my_threads` from the
#' caller. `.data$x` and `.env$x` disambiguate a column that shares a name
#' with a local variable, and `!!` builds a specification programmatically.
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
#' supply.
#'
#' **Columns named differently in the two inputs** are compared by writing
#' `left | right`: the expression to the left of the `|` is evaluated against
#' `data_1` and the one to the right against `data_2`, as `cross` is above.
#' Either side may be any expression, not only a bare column name, so
#' `soundex(family_name) | surname_code` works too --- this is to `compare`
#' what `block_2` is to `block_1`. Such an element compares no single column on
#' both sides, so it has no name to fall back on and must be given one.
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
#'   against `data_2`. An error when deduplicating, as there is no `data_2`.
#' @param compare A list of comparator calls, as described above. `NULL`
#'   returns the candidate pairs unscored.
#' @param pairs Optional candidate pairs to score instead of blocking: any
#'   data frame with `x_index` and `y_index` columns, such as the result of
#'   [minhash_pairs()], [fuzzy_within()] or [tfidf_top_n()]. Supply this or
#'   `block_1`, not both.
#' @param block_method `"exact"` (the default) requires keys to be equal, via
#'   [block_pairs()]. `"sorted_neighbourhood"` also pairs records whose keys
#'   are near each other in sorted order, via [sorted_neighbourhood()].
#' @param window Window width for `block_method = "sorted_neighbourhood"`; an
#'   odd positive integer, `3` by default.
#' @param max_block_size Blocks holding more records than this are skipped,
#'   with a warning saying how many. `Inf` (the default) keeps every block.
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
#' @param verbose Whether to print per-stage timings and the pairs found.
#'   Timings are returned either way, as `attr(out, "timings")`.
#' @param block Former name of `block_1`, still accepted. Give one or the
#'   other, not both.
#' @return A `data.table`, also classed `fs_pairs` so that [summary()] and
#'   [plot()] describe it, with one row per candidate pair: `pair_id`
#'   (character when built from identifier columns, otherwise the pair's
#'   number), `index_1` and `index_2` (row numbers into `data_1` and `data_2`,
#'   suffixed as `suffixes` says), `pass`, `block_key` (when blocking and
#'   `block_key = TRUE`), the carried columns under both suffixes, and one
#'   column per comparator. Attributes `timings` and `compare` describe the
#'   run.
#' @seealso [block_pairs()] and [sorted_neighbourhood()] for the candidate
#'   pairs alone, [cluster_pairs()] to turn matched pairs into entity
#'   identifiers, and [minhash_pairs()], [fuzzy_within()] and [tfidf_top_n()]
#'   for similarity-based candidates to pass as `pairs`.
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
#'
#' # `pairs` scores candidates found some other way instead of blocking
#' rLink(enrol, claims,
#'       pairs   = data.frame(x_index = c(1L, 4L), y_index = c(1L, 1L)),
#'       compare = list(jaro_winkler(surname)),
#'       keep    = "surname")
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

    block_method <- match.arg(block_method)
    score_position <- match.arg(score_position)
    .check_flag(match_missing, "match_missing")
    .check_flag(force_upper_case, "force_upper_case")
    .check_flag(normalise, "normalise")
    .check_flag(block_key, "block_key")
    .check_flag(verbose, "verbose")
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

    mask_1 <- as.list(data_1)
    mask_2 <- as.list(d2)
    timings$validate <- .bench_now() - t0

    # --- candidate pairs ---------------------------------------------------
    labels <- NULL
    keys_1 <- NULL
    if (use_block) {
        t0 <- .bench_now()
        spec <- .rlink_passes(quo_block, block_arg)
        labels <- spec$labels
        n_pass <- length(spec$passes)
        spec_2 <- if (rlang::quo_is_null(quo_block_2)) NULL else
            .rlink_passes(quo_block_2, "block_2")
        if (!is.null(spec_2))
            .rlink_check_block_2(spec, spec_2, n_pass, block_arg)

        probs <- .rlink_problems()
        keys_1 <- vector("list", n_pass)
        keys_2 <- vector("list", n_pass)
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
        .rlink_stop(probs, data_1)
        timings$keys <- .bench_now() - t0

        if (verbose) {
            t0 <- .bench_now()
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
                    format(.rlink_expected(keys_1[[p]], keys_2[[p]], dedupe),
                           big.mark = ",", scientific = FALSE)))
            timings$estimate <- .bench_now() - t0
        }

        t0 <- .bench_now()
        y <- if (dedupe) NULL else keys_2
        pairs <- if (block_method == "exact")
            block_pairs(keys_1, y, max_block_size = max_block_size,
                        nthreads = nthreads)
        else
            sorted_neighbourhood(keys_1, y, window = window,
                                 nthreads = nthreads)
        timings$pairs <- .bench_now() - t0
    } else {
        t0 <- .bench_now()
        pairs <- .rlink_given_pairs(pairs, nrow(data_1), nrow(d2))
        timings$pairs <- .bench_now() - t0
    }

    xi <- pairs$x_index
    yi <- pairs$y_index
    n_pairs <- length(xi)

    # --- carried columns ---------------------------------------------------
    t0 <- .bench_now()
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

    out <- list()
    out[["pair_id"]] <- .rlink_pair_id(data_1, d2, pair_id, pair_id_default,
                                       dedupe, xi, yi, n_pairs, nthreads)
    out[[paste0("index", suffixes[1L])]] <- xi
    out[[paste0("index", suffixes[2L])]] <- yi
    out[["pass"]] <- pairs$pass
    if (show_key)
        out[["block_key"]] <- .rlink_block_key(keys_1, pairs, xi, n_pairs,
                                               nthreads)
    for (nm in keep_1) out[[paste0(nm, suffixes[1L])]] <- data_1[[nm]][xi]
    for (nm in keep_2) out[[paste0(nm, suffixes[2L])]] <- d2[[nm]][yi]
    data.table::setDT(out)
    timings$attach <- .bench_now() - t0

    # --- comparators -------------------------------------------------------
    cmp <- .rlink_compare(quo_compare, names(out))
    info <- vector("list", length(cmp))
    # Comparators often share a column, as jaro_winkler(surname) and
    # levenshtein(surname) do, and when deduplicating both sides of one read
    # the same data. Each column is therefore prepared once per record and
    # spread to the pairs once, and the pair-level copy is dropped after the
    # last comparator that reads it.
    src <- lapply(cmp, function(s) vapply(list(s$expr_1, s$expr_2),
        function(e) paste(deparse(e), collapse = " "), ""))
    rec_id <- lapply(src, function(e)
        paste0(if (dedupe) "" else 1:2, ":", e))
    pair_id <- lapply(src, function(e) paste0(1:2, ":", e))
    last_use <- vapply(split(rep(seq_along(cmp), each = 2L),
                             unlist(pair_id)), max, 0L)
    rec_cache <- list()
    pair_cache <- list()
    for (i in seq_along(cmp)) {
        t0 <- .bench_now()
        s <- cmp[[i]]
        probs <- .rlink_problems()
        for (k in 1:2) {
            if (!is.null(rec_cache[[rec_id[[i]][k]]])) next
            v <- if (k == 1L)
                .rlink_term(s$expr_1, mask_1, s$env, nrow(data_1),
                            "compare", i, 1L, probs)
            else
                .rlink_term(s$expr_2, mask_2, s$env, nrow(d2),
                            "compare", i, 2L, probs)
            if (!is.null(v))
                rec_cache[[rec_id[[i]][k]]] <-
                    .rlink_chr(v, force_upper_case, na_strings, nthreads)
        }
        .rlink_stop(probs, data_1)
        a0 <- rec_cache[[rec_id[[i]][1L]]]
        b0 <- rec_cache[[rec_id[[i]][2L]]]
        if (is.null(pair_cache[[pair_id[[i]][1L]]]))
            pair_cache[[pair_id[[i]][1L]]] <- a0[xi]
        if (is.null(pair_cache[[pair_id[[i]][2L]]]))
            pair_cache[[pair_id[[i]][2L]]] <- b0[yi]
        a <- pair_cache[[pair_id[[i]][1L]]]
        b <- pair_cache[[pair_id[[i]][2L]]]
        for (id in pair_id[[i]])
            if (last_use[[id]] == i) pair_cache[[id]] <- NULL
        orient <- .rlink_orient(s$fn, s$args, normalise, nthreads)
        score <- .rlink_call(s$fn, a, b, s$args, nthreads)
        if (!(is.numeric(score) || is.logical(score)) ||
            length(score) != n_pairs)
            stop(sprintf(paste0(
                "The comparator for `%s` returned %s of length %s; it must ",
                "return one number or logical per pair (%s)."),
                s$name, class(score)[1L],
                format(length(score), big.mark = ","),
                format(n_pairs, big.mark = ",")), call. = FALSE)
        rm(a, b)
        data.table::set(out, j = s$name,
                        value = orient$convert(score, a0, b0, xi, yi))
        info[[i]] <- data.frame(score = s$name, comparator = s$label,
                                column_1 = s$col_1, column_2 = s$col_2,
                                scale = orient$kind,
                                direction = orient$direction)
        timings[[s$name]] <- .bench_now() - t0
    }

    # --- order and return --------------------------------------------------
    t0 <- .bench_now()
    lead <- c("pair_id", paste0("index", suffixes), "pass",
              if (show_key) "block_key")
    score_names <- vapply(cmp, `[[`, "", "name")
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
    # `fs_pairs` ahead of the data.table classes, so that summary() and plot()
    # describe the result. data.table carries both this class and the two
    # attributes above through `[`, `order()`, `head()` and `:=`, so they
    # survive a filter on the scores.
    data.table::setattr(out, "class",
                        c("fs_pairs", "data.table", "data.frame"))
    if (verbose) {
        for (x in info)
            cli::cli_alert_info(sprintf("%s: %s (%s).", x$score,
                                        x$comparator, x$scale))
        cli::cli_alert_success(sprintf(
            "%s pairs, %d score column%s, in %.3fs.",
            format(n_pairs, big.mark = ","), length(cmp),
            if (length(cmp) == 1L) "" else "s", total))
    }
    out
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

.rlink_stop <- function(probs, data_1) {
    if (!length(probs$msgs)) return(invisible(NULL))
    stop(sprintf("%s\nColumns in `data_1`: %s.",
                 paste(probs$msgs, collapse = "\n"),
                 paste(names(data_1), collapse = ", ")), call. = FALSE)
}

# One term of one pass. A length-1 string is a column name; anything else is
# evaluated with the data in front of the calling environment.
.rlink_term <- function(e, mask, env, n, arg, pass, slot, probs) {
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
.rlink_block_key <- function(keys_1, pairs, xi, n_pairs, nthreads = NULL) {
    shown <- lapply(keys_1, function(k) {
        if (!is.character(k)) return(.rlink_as_character(k, nthreads))
        k <- gsub(.rlink_na_key, "<NA>", k, fixed = TRUE)
        gsub(.rlink_sep, "|", k, fixed = TRUE)
    })
    if (length(shown) == 1L) return(shown[[1L]][xi])
    offset <- c(0L, cumsum(lengths(shown)))
    unlist(shown, use.names = FALSE)[xi + offset[pairs$pass]]
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

.rlink_pair_id <- function(data_1, d2, pair_id, default, dedupe, xi, yi,
                           n_pairs, nthreads) {
    if (is.null(pair_id)) return(seq_len(n_pairs))
    nm_1 <- pair_id[[1L]]
    nm_2 <- if (length(pair_id) == 2L) pair_id[[2L]] else pair_id[[1L]]
    gone_1 <- !nm_1 %in% names(data_1)
    gone_2 <- !nm_2 %in% names(d2)
    if (gone_1 || gone_2) {
        # The default name is a convention, not a promise: a file that does not
        # use it still links, numbered as it always was.
        if (default) return(seq_len(n_pairs))
        side <- if (gone_1 || dedupe) "data_1" else "data_2"
        held <- if (gone_1) data_1 else d2
        stop(sprintf(paste0(
            "`pair_id` names `%s`, which is not a column of `%s`.\n",
            "Columns in `%s`: %s."),
            if (gone_1) nm_1 else nm_2, side, side,
            paste(names(held), collapse = ", ")), call. = FALSE)
    }
    # Rendered once per record and then indexed, never once per pair, which is
    # the discipline .rlink_block_key() follows and for the same reason.
    a <- .rlink_as_character(data_1[[nm_1]], nthreads)
    shared <- dedupe && identical(nm_1, nm_2)
    b <- if (shared) a else .rlink_as_character(d2[[nm_2]], nthreads)
    .rlink_check_pair_sep(a, nm_1, "data_1", nthreads)
    if (!shared)
        .rlink_check_pair_sep(b, nm_2, if (dedupe) "data_1" else "data_2",
                              nthreads)
    left <- a[xi]
    right <- b[yi]
    key <- paste0(left, .rlink_pair_sep, right)
    key[is.na(left) | is.na(right)] <- NA_character_
    .rlink_warn_pair_id(key, nm_1, nm_2)
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

.rlink_warn_pair_id <- function(key, nm_1, nm_2) {
    n_na <- sum(is.na(key))
    n_dup <- sum(duplicated(key[!is.na(key)]))
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

.rlink_label <- function(labels, p) {
    if (is.null(labels) || is.na(labels[p]) || !nzchar(labels[p])) ""
    else sprintf(" (%s)", labels[p])
}

# The pre-flight estimate the earlier function printed per block, which is how
# you find the pass that is about to explode. as.numeric() matters: that
# function multiplied integer frequencies and overflowed to NA on exactly the
# large blocks the warning exists for.
.rlink_expected <- function(k1, k2, dedupe) {
    u <- unique(if (dedupe) k1 else c(k1, k2))
    u <- u[!is.na(u)]
    n1 <- as.numeric(tabulate(match(k1, u), length(u)))
    if (dedupe) return(sum(n1 * (n1 - 1) / 2))
    sum(n1 * tabulate(match(k2, u), length(u)))
}

.rlink_given_pairs <- function(pairs, n_1, n_2) {
    if (!is.data.frame(pairs) ||
        !all(c("x_index", "y_index") %in% names(pairs)))
        stop("`pairs` must be a data frame with `x_index` and `y_index`.",
             call. = FALSE)
    out <- data.frame(
        x_index = as.integer(pairs$x_index),
        y_index = as.integer(pairs$y_index),
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

        col <- e[[2L]]
        if (.rlink_is_call_to(col, "|")) {
            expr_1 <- col[[2L]]
            expr_2 <- col[[3L]]
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
            fn = fn, args = args, env = env,
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

.rlink_chr <- function(v, upper, na_strings, nthreads = NULL) {
    if (is.factor(v)) v <- as.character(v)
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

#
# convert() is given the compared values per record (a, b) and the pairs'
# indices into them, not the values per pair, so lengths are counted once
# per record.
.rlink_orient <- function(fn, args, normalise, nthreads) {
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
            .rlink_call(fn, .rlink_probe_a, .rlink_probe_b, args, nthreads),
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
    # Lengths in the unit the comparator counts edits in: a byte-wise
    # distance over a character length would charge an accented letter two
    # edits out of one.
    ub <- if ("use_bytes" %in% names(args)) args$use_bytes
          else formals(fn)$use_bytes
    unit <- if (isTRUE(ub)) "bytes" else "chars"
    list(kind = "edit distance", direction = "higher",
         convert = function(s, a, b, xi, yi) {
        n <- pmax(nchar(a, unit)[xi], nchar(b, unit)[yi])
        v <- 1 - s / n
        v[which(n == 0L)] <- 1
        v[which(v < 0)] <- 0
        v[which(v > 1)] <- 1
        v
    })
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
