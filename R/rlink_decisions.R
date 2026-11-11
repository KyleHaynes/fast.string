# The review artifact: a durable, human-readable record of the judgements an
# analyst made over a set of candidate pairs, written by rLink_inspect() and
# read back by read_decisions().
#
# The decision log is append-only. It is an array of decision *events*, not a
# map from pair to verdict, and that is the whole design. A map could not show
# that a reviewer changed their mind, could not show that two reviewers
# disagree, and would let a second reviewer's pass silently overwrite the first
# one's -- which is precisely the thing the file exists to make visible. Events
# merge by union, so two reviewers' work can never destroy each other's.
#
# Nothing in this file touches shiny or reactable. Everything here is a plain
# function of plain data, so the audit trail is testable without a browser and
# usable from the console by someone who never opens the app.

.review_schema <- "fast.string/decisions"
.review_format <- 1L
.review_sep <- "\u001f"

# Millisecond resolution, because a reviewer triages several pairs a second and
# "the latest event wins" has to mean something. The per-event `seq` is the
# real tiebreak; this is for people reading the file.
.review_now <- function()
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

# A run identifier unique across machines and sessions. Deliberately hashed
# from the clock, the process and a temp name rather than drawn with sample():
# a package has no business perturbing the caller's random stream.
.review_run_id <- function()
    paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
           substr(fhash(paste0(Sys.getpid(), tempfile(), Sys.time()),
                        algo = "xxh64"), 1L, 6L))

# One digest of a whole column, folded in chunks so that a million pairs never
# build a single enormous string. fhash() is vectorised -- it returns one hash
# per element -- so the fold is the point: hash the elements, hash the hashes,
# and repeat until one is left.
#
# That is the definition, and .review_fingerprint_r() below is it written
# out. What runs is fast_fold_digest_impl(), which hashes the same bytes in
# the same order without making an R string per row: on two million pairs the
# written-out form was twelve of the thirteen seconds it took to open a
# review. The two give the same digest, so a file written by one reads as
# unchanged under the other, and a test holds them together.
#
# `columns` may be several character vectors, which are hashed as if pasted
# with U+001F between them, and `order` the order to take the rows in.
.review_fingerprint <- function(keys, chunk = 100000L, order = integer(),
                                pasted = FALSE) {
    columns <- if (is.list(keys)) lapply(keys, as.character)
               else list(as.character(keys))
    if (!length(columns) || !length(columns[[1L]])) return("xxh64:none")
    digest <- fast_fold_digest_impl(columns, as.integer(order), pasted,
                                    chunk, -1L)
    if (!is.na(digest)) return(paste0("xxh64:", digest))
    # A missing key that was not pasted has no digest of its own; the
    # written-out definition says what happens to it.
    keys <- columns[[1L]]
    .review_fingerprint_r(if (length(order)) keys[order] else keys, chunk)
}

.review_fingerprint_r <- function(keys, chunk = 100000L) {
    keys <- as.character(keys)
    if (!length(keys)) return("xxh64:none")
    h <- fhash(keys, algo = "xxh64")
    while (length(h) > 1L) {
        group <- ceiling(seq_along(h) / chunk)
        h <- vapply(split(h, group), function(part)
            fhash(paste0(part, collapse = ""), algo = "xxh64"), "",
            USE.NAMES = FALSE)
    }
    paste0("xxh64:", h)
}

# Two digests answering two different questions: did the set of pairs change,
# and did the scores change for the pairs still there. The pair set is hashed
# in sorted order, so re-running with different blocking -- which reorders the
# pairs but need not change them -- is not reported as a change.
.review_fingerprints <- function(x, scores, pair_id) {
    ids <- as.character(x[[pair_id]])
    # Radix, for two reasons. It is ~50x faster than the default on a few
    # hundred thousand ids, and it sorts by bytes rather than by the session's
    # collation -- so the same pairs fingerprint the same on every machine. A
    # locale-sensitive sort would have told a reviewer on a differently
    # configured machine that the data had changed when it had not.
    o <- order(ids, method = "radix")
    # as.character() renders a double to 15 significant digits and is some
    # three orders of magnitude faster here than formatC() or sprintf(); two
    # scores differing only beyond that are not a change any linkage decision
    # could turn on.
    #
    # A score column holds far fewer distinct values than it has rows -- a
    # comparator has only so many answers -- so each distinct value is
    # rendered once and looked up, rather than every row being rendered.
    #
    # paste0() and not as.character(), though they write a number the same
    # way. as.character() of a numeric vector is deferred: R hands back a
    # promise to render each element when it is read, and indexing that
    # promise gives another one -- so every row was still rendered, one at a
    # time, the moment the digest read it. paste0() renders the distinct
    # values now. It writes a missing value as "NA", which is what paste()
    # made of it when the rows were joined anyway.
    as_text <- function(v) {
        v <- as.double(v)
        u <- unique(v)
        paste0(u)[match(v, u)]
    }
    list(pair_set = .review_fingerprint(ids, order = o),
         scores = if (!length(scores)) .review_fingerprint(character()) else
             .review_fingerprint(lapply(scores, function(s) as_text(x[[s]])),
                                 order = o, pasted = TRUE),
         columns = .review_fingerprint(paste0(names(x), collapse = .review_sep)))
}

.review_new <- function() {
    now <- .review_now()
    list(schema = .review_schema, format_version = .review_format,
         created = now, updated = now,
         runs = list(), thresholds = list(), decisions = list())
}

.review_encode <- function(obj)
    as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                                  digits = NA, null = "null", na = "null"))

# simplifyVector = FALSE is not optional. With simplification a one-element
# array parses to a different R shape than a two-element one, so code written
# against a hand-made fixture breaks on a real file.
.review_decode <- function(txt)
    jsonlite::fromJSON(txt, simplifyVector = FALSE)

# Write through a temporary file in the same directory and rename, so an
# interrupted write cannot leave a half-written audit record where a whole one
# used to be. Windows refuses the rename when the file is open elsewhere, hence
# the copy fallback, and if both fail the message names the temporary file so
# the work is recoverable.
.review_write <- function(obj, path) {
    obj$updated <- .review_now()
    json <- .review_encode(obj)
    folder <- dirname(path)
    if (!dir.exists(folder))
        stop(sprintf("Cannot write the review: there is no directory `%s`.",
                     folder), call. = FALSE)
    tmp <- tempfile(tmpdir = folder, fileext = ".json")
    con <- file(tmp, open = "wb")
    on.exit(if (!is.null(con)) try(close(con), silent = TRUE), add = TRUE)
    writeBin(charToRaw(enc2utf8(json)), con)
    close(con)
    con <- NULL
    if (file.exists(path))
        file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
    moved <- suppressWarnings(file.rename(tmp, path))
    if (!moved)
        moved <- suppressWarnings(file.copy(tmp, path, overwrite = TRUE))
    if (!moved)
        stop(sprintf(paste0(
            "Could not write the review to `%s`. Nothing is lost: it is in ",
            "`%s`."), path, tmp), call. = FALSE)
    if (file.exists(tmp)) unlink(tmp)
    invisible(path)
}

.review_read_file <- function(path) {
    if (!file.exists(path))
        stop(sprintf("There is no decisions file at `%s`.", path),
             call. = FALSE)
    size <- file.info(path)$size
    txt <- rawToChar(readBin(path, "raw", n = size))
    Encoding(txt) <- "UTF-8"
    obj <- tryCatch(.review_decode(txt), error = function(cnd)
        stop(sprintf("`%s` is not readable as JSON: %s", path,
                     conditionMessage(cnd)), call. = FALSE))
    if (!identical(obj$schema, .review_schema))
        stop(sprintf(paste0(
            "`%s` is not a fast.string decisions file (its schema is %s)."),
            path, if (is.null(obj$schema)) "absent" else
                paste0("`", obj$schema, "`")), call. = FALSE)
    version <- obj$format_version
    if (!is.numeric(version) || length(version) != 1L)
        stop(sprintf("`%s` has no usable format version.", path),
             call. = FALSE)
    if (version > .review_format)
        stop(sprintf(paste0(
            "`%s` was written in review format %s, and this version of ",
            "fast.string reads up to %d. Upgrade the package."),
            path, format(version), .review_format), call. = FALSE)
    for (nm in c("runs", "thresholds", "decisions"))
        if (is.null(obj[[nm]])) obj[[nm]] <- list()
    obj
}

# Events are immutable and identified, so they merge by union on `event_id`:
# two reviewers working from a shared file add to each other's work and neither
# can delete the other's.
#
# Runs are not events. A run's `ended` legitimately advances every time the
# session saves again, so a run with an id already in the file replaces it
# rather than being discarded as a duplicate -- which is what `key` is for.
.review_merge <- function(old, new) {
    union_by <- function(a, b, key) {
        if (!length(a)) return(b)
        if (!length(b)) return(a)
        seen <- vapply(a, function(e) .review_one(e, key, ""), "")
        c(a, Filter(function(e) !(.review_one(e, key, "") %in% seen), b))
    }
    replace_by <- function(a, b, key) {
        if (!length(a)) return(b)
        fresh <- vapply(b, function(e) .review_one(e, key, ""), "")
        kept <- Filter(function(e) !(.review_one(e, key, "") %in% fresh), a)
        c(kept, b)
    }
    out <- old
    out$runs <- replace_by(old$runs, new$runs, "run_id")
    out$thresholds <- union_by(old$thresholds, new$thresholds, "event_id")
    out$decisions <- union_by(old$decisions, new$decisions, "event_id")
    out$created <- min(c(.review_one(old, "created", .review_now()),
                         .review_one(new, "created", .review_now())))
    out
}

# One scalar field out of a decoded JSON object, of the type of `default`.
.review_one <- function(obj, name, default) {
    v <- obj[[name]]
    if (is.null(v) || !length(v)) return(default)
    v <- v[[1L]]
    if (is.null(v)) return(default)
    if (is.character(default)) as.character(v)
    else if (is.integer(default)) as.integer(v)
    else if (is.numeric(default)) as.double(v)
    else if (is.logical(default)) as.logical(v)
    else v
}

.review_column <- function(events, name, default) {
    if (!length(events)) return(default[0L])
    vapply(events, .review_one, default, name = name, default = default,
           USE.NAMES = FALSE)
}

.review_events_df <- function(decisions) {
    data.frame(
        event_id = .review_column(decisions, "event_id", NA_character_),
        run_id = .review_column(decisions, "run_id", NA_character_),
        reviewer = .review_column(decisions, "reviewer", NA_character_),
        at = .review_column(decisions, "at", NA_character_),
        seq = .review_column(decisions, "seq", NA_integer_),
        pair_id = .review_column(decisions, "pair_id", NA_character_),
        index_1 = .review_column(decisions, "index_1", NA_integer_),
        index_2 = .review_column(decisions, "index_2", NA_integer_),
        decision = .review_column(decisions, "decision", NA_character_),
        comment = .review_column(decisions, "comment", NA_character_),
        stringsAsFactors = FALSE)
}

# The current verdict per reviewer and pair: the latest event each of them
# filed about it, with a count of how many times they revised it.
.review_effective <- function(events) {
    if (!nrow(events)) {
        out <- events
        out$n_changes <- integer()
        return(out)
    }
    key <- paste0(events$reviewer, .review_sep, events$pair_id)
    # `seq` counts within one run, so it only breaks ties inside one. `run_id`
    # is prefixed with its start time, so sorting it lexicographically sorts
    # the runs chronologically and the whole ordering is principled.
    o <- order(key, events$at, events$run_id, events$seq)
    events <- events[o, , drop = FALSE]
    key <- key[o]
    revisions <- data.table::rowid(key)
    last <- !duplicated(key, fromLast = TRUE)
    out <- events[last, , drop = FALSE]
    out$n_changes <- revisions[last]
    rownames(out) <- NULL
    out[order(out$pair_id, out$reviewer), , drop = FALSE]
}

.review_disagreements <- function(effective) {
    empty <- data.frame(pair_id = character(), n_reviewers = integer(),
                        reviewers = character(), decisions = character(),
                        stringsAsFactors = FALSE)
    if (!nrow(effective)) return(empty)
    # "undecided" is the absence of a verdict, not a competing one.
    held <- effective[!is.na(effective$decision) &
                      effective$decision != "undecided", , drop = FALSE]
    if (!nrow(held)) return(empty)
    split_by <- split(seq_len(nrow(held)), held$pair_id)
    rows <- lapply(names(split_by), function(id) {
        at <- split_by[[id]]
        verdicts <- held$decision[at]
        if (length(unique(verdicts)) < 2L) return(NULL)
        o <- order(held$reviewer[at])
        data.frame(pair_id = id, n_reviewers = length(at),
                   reviewers = paste(held$reviewer[at][o], collapse = ", "),
                   decisions = paste(verdicts[o], collapse = ", "),
                   stringsAsFactors = FALSE)
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(empty)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}


# The R the reviewer should paste into their script so that the next run --
# theirs or a QA reviewer's -- reproduces what they just decided by hand. A
# review that cannot be replayed is a note in a drawer.
.decisions_code <- function(file, expression = NULL) {
    # Only a bare variable name is safe to put back into generated code; an
    # expression like `pairs[review_order(pairs)]` described the subset that
    # was on screen, not the object to apply the decisions to.
    object <- if (!is.null(expression) && length(expression) == 1L &&
                  !is.na(expression) &&
                  grepl("^[.A-Za-z][._A-Za-z0-9]*$", expression))
        expression else "pairs"
    shown <- gsub("\\\\", "/", file)
    c(sprintf("# Re-open this review, decisions and all:"),
      sprintf('rLink_inspect(%s, decisions = "%s")', object, shown),
      "",
      "# Apply it, adding `link` and `link_reason` columns:",
      sprintf('%s <- apply_decisions(%s, "%s")', object, object, shown))
}


#' Read saved pair decisions
#'
#' Reads the file that [rLink_inspect()] writes, so the judgements an analyst
#' made can be checked, reported on, or applied to a fresh set of pairs. This
#' is the function a second reviewer runs: it shows which thresholds were
#' accepted and why, every per-pair decision and its comment, and --- where two
#' people have reviewed the same pairs --- exactly where they disagree.
#'
#' The decision log is append-only, so nothing a previous reviewer recorded is
#' ever overwritten. `$decisions` collapses that log to the current verdict per
#' reviewer and pair, with `n_changes` counting the revisions behind it;
#' `$events` is the log itself, which is the actual audit record.
#'
#' Supplying `x` additionally answers the question a reviewer most needs
#' answered --- *has the data moved since this review was done?* --- by
#' comparing fingerprints of the pair set, the scores and the columns, and
#' reporting how many reviewed pairs have since disappeared and how many new
#' ones have arrived undecided.
#'
#' @param file Path to the file written by [rLink_inspect()].
#' @param x Optional candidate pairs, normally a fresh [rLink()] result, to
#'   check the decisions against. Adds `$status`.
#' @param reviewer Optional reviewer to restrict `$decisions` and `$events` to.
#'   `$disagreements` is always computed across everyone.
#' @return A list of class `fs_decisions`, with a `print()` method that ends
#'   with the R needed to reproduce and apply it:
#'
#'   * `file`, `format_version`, `created`, `updated`.
#'   * `runs`: one row per [rLink_inspect()] session, with who ran it, when,
#'     how many pairs they saw and the fingerprints at the time.
#'   * `scores`: the score columns and their cut directions, from the latest
#'     run.
#'   * `thresholds` and `cuts`: the thresholds accepted, one row per event and
#'     one row per score within it.
#'   * `decisions`: the current verdict per pair and reviewer.
#'   * `events`: the full append-only log.
#'   * `disagreements`: pairs two reviewers judged differently.
#'   * `agreement`: one row counting the pairs decided, agreed and disputed.
#'   * `code`: the lines `print()` shows, as a character vector.
#'   * `status`: present only when `x` was given. `n_orphaned` counts decided
#'     pairs that are no longer in `x`, and `n_undecided` the pairs in `x` that
#'     nobody has judged --- which is as much as the file can say, because it
#'     keeps the decisions and a fingerprint rather than the whole pair set it
#'     was made from.
#' @seealso [rLink_inspect()] to make the decisions, and [apply_decisions()] to
#'   apply them.
#' @family record linkage review functions
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' # A review of four pairs by two people, who disagreed about one of them.
#' path <- system.file("extdata", "example_decisions.json",
#'                     package = "fast.string")
#' d <- read_decisions(path)
#' d
#'
#' # The threshold that was accepted, with the operator it was cut with
#' d$cuts
#'
#' # Where the two reviewers parted company, and what each of them wrote
#' d$disagreements
#' d$events[, c("reviewer", "pair_id", "decision", "comment")]
#'
#' # Checked against the pairs it was made on
#' pairs <- data.frame(pair_id = c("A|X", "B|Y", "C|Z", "D|W"),
#'                     index_1 = 1:4, index_2 = 1:4,
#'                     score = c(0.97, 0.74, 0.44, 0.31))
#' read_decisions(path, x = pairs)$status
#' @export
read_decisions <- function(file, x = NULL, reviewer = NULL) {
    .review_require("jsonlite")
    if (!is.character(file) || length(file) != 1L || is.na(file))
        stop("`file` must be a single path.")
    obj <- .review_read_file(file)
    if (!is.null(reviewer) &&
        (!is.character(reviewer) || length(reviewer) != 1L || is.na(reviewer)))
        stop("`reviewer` must be a single name, or NULL.")

    runs <- obj$runs
    runs_df <- data.frame(
        run_id = .review_column(runs, "run_id", NA_character_),
        reviewer = .review_column(runs, "reviewer", NA_character_),
        started = .review_column(runs, "started", NA_character_),
        ended = .review_column(runs, "ended", NA_character_),
        n_pairs = .review_column(runs, "n_pairs", NA_integer_),
        n_shown = .review_column(runs, "n_shown", NA_integer_),
        truncated = .review_column(runs, "truncated", NA),
        pair_id_source = .review_column(runs, "pair_id_source", NA_character_),
        expression = .review_column(runs, "expression", NA_character_),
        package_version = .review_column(runs, "package_version",
                                         NA_character_),
        host = .review_column(runs, "host", NA_character_),
        fp_pair_set = .review_column(runs, "fp_pair_set", NA_character_),
        fp_scores = .review_column(runs, "fp_scores", NA_character_),
        fp_columns = .review_column(runs, "fp_columns", NA_character_),
        stringsAsFactors = FALSE)

    latest <- if (length(runs)) runs[[length(runs)]] else NULL
    score_rows <- if (is.null(latest)) list() else latest$scores
    scores_df <- data.frame(
        score = .review_column(score_rows, "score", NA_character_),
        comparator = .review_column(score_rows, "comparator", NA_character_),
        column_1 = .review_column(score_rows, "column_1", NA_character_),
        column_2 = .review_column(score_rows, "column_2", NA_character_),
        scale = .review_column(score_rows, "scale", NA_character_),
        operator = .review_column(score_rows, "operator", NA_character_),
        observed_min = .review_column(score_rows, "observed_min", NA_real_),
        observed_max = .review_column(score_rows, "observed_max", NA_real_),
        n_missing = .review_column(score_rows, "n_missing", NA_integer_),
        stringsAsFactors = FALSE)

    th <- obj$thresholds
    thresholds_df <- data.frame(
        event_id = .review_column(th, "event_id", NA_character_),
        run_id = .review_column(th, "run_id", NA_character_),
        reviewer = .review_column(th, "reviewer", NA_character_),
        at = .review_column(th, "at", NA_character_),
        status = .review_column(th, "status", NA_character_),
        rule = .review_column(th, "rule", NA_character_),
        expr = .review_column(th, "expr", NA_character_),
        n_pairs = .review_column(th, "n_pairs", NA_integer_),
        n_one_to_one = .review_column(th, "n_one_to_one", NA_integer_),
        n_conflict_1 = .review_column(th, "n_conflict_1", NA_integer_),
        n_conflict_2 = .review_column(th, "n_conflict_2", NA_integer_),
        # What the threshold was accepted on, where the reviewer set a band
        # or drew an audit sample. Missing from a file written before these
        # existed, and from a threshold accepted without them.
        band_score = .review_column(th, "band_score", NA_character_),
        band_lower = .review_column(th, "band_lower", NA_real_),
        band_upper = .review_column(th, "band_upper", NA_real_),
        audit_sample = .review_column(th, "audit_sample", NA_integer_),
        audit_judged = .review_column(th, "audit_judged", NA_integer_),
        audit_not_link = .review_column(th, "audit_not_link", NA_integer_),
        note = .review_column(th, "note", NA_character_),
        stringsAsFactors = FALSE)
    cut_rows <- lapply(th, function(e) {
        cuts <- e$cuts
        if (!length(cuts)) return(NULL)
        data.frame(event_id = .review_one(e, "event_id", NA_character_),
                   score = .review_column(cuts, "score", NA_character_),
                   operator = .review_column(cuts, "operator", NA_character_),
                   value = .review_column(cuts, "value", NA_real_),
                   stringsAsFactors = FALSE)
    })
    cut_rows <- Filter(Negate(is.null), cut_rows)
    cuts_df <- if (length(cut_rows)) do.call(rbind, cut_rows) else
        data.frame(event_id = character(), score = character(),
                   operator = character(), value = numeric(),
                   stringsAsFactors = FALSE)

    events <- .review_events_df(obj$decisions)
    effective <- .review_effective(events)
    disagreements <- .review_disagreements(effective)
    decided <- unique(effective$pair_id[!is.na(effective$decision) &
                                        effective$decision != "undecided"])
    agreement <- data.frame(
        n_pairs_decided = length(decided),
        n_reviewers = length(unique(effective$reviewer)),
        n_disagreed = nrow(disagreements),
        n_agreed = max(0L, length(decided) - nrow(disagreements)))

    shown_events <- events
    shown_effective <- effective
    if (!is.null(reviewer)) {
        shown_events <- events[events$reviewer %in% reviewer, , drop = FALSE]
        shown_effective <- effective[effective$reviewer %in% reviewer, ,
                                     drop = FALSE]
        rownames(shown_events) <- NULL
        rownames(shown_effective) <- NULL
    }

    status <- NULL
    if (!is.null(x)) {
        if (!is.data.frame(x)) stop("`x` must be a data frame, or NULL.")
        if (!"pair_id" %in% names(x))
            stop("`x` must have a `pair_id` column to check decisions against.")
        present <- as.character(x[["pair_id"]])
        fp <- .review_fingerprints(
            x, intersect(scores_df$score, names(x)), "pair_id")
        status <- data.frame(
            pair_set_match = identical(fp$pair_set,
                                       runs_df$fp_pair_set[nrow(runs_df)]),
            scores_match = identical(fp$scores,
                                     runs_df$fp_scores[nrow(runs_df)]),
            columns_match = identical(fp$columns,
                                      runs_df$fp_columns[nrow(runs_df)]),
            n_decided_present = sum(decided %in% present),
            n_orphaned = sum(!decided %in% present),
            # Not "new": the file keeps the decisions and a fingerprint, not
            # the whole pair set it was made from, so a pair nobody judged
            # cannot be told from one that arrived afterwards.
            #
            # Counted against `decided` -- the pairs somebody reached a
            # verdict on -- and not against every pair that appears in the
            # log. A reviewer who opens a pair and leaves it undecided, or
            # who marks one and then reverts it, files an event for it; that
            # pair has still not been judged, and counting it as judged
            # understated the work left in exactly the case where somebody
            # had been through the queue once already.
            n_undecided = sum(!present %in% decided))
    }

    path <- normalizePath(file, winslash = "/", mustWork = FALSE)
    structure(list(
        file = path,
        format_version = as.integer(obj$format_version),
        created = .review_one(obj, "created", NA_character_),
        updated = .review_one(obj, "updated", NA_character_),
        runs = runs_df, scores = scores_df,
        thresholds = thresholds_df, cuts = cuts_df,
        decisions = shown_effective, events = shown_events,
        disagreements = disagreements, agreement = agreement,
        code = .decisions_code(path, if (nrow(runs_df))
            runs_df$expression[nrow(runs_df)] else NULL),
        status = status
    ), class = "fs_decisions")
}

#' @export
print.fs_decisions <- function(x, ...) {
    # cat_rule()/cat_line() go to stdout. A print method is output, not a
    # message, and cli_h3() would send it to stderr where capture.output()
    # and a pipe cannot see it.
    cli::cat_rule("Pair decisions")
    cat(sprintf("File: %s\n", x$file))
    cat(sprintf("%d session%s, %d decision event%s\n",
                nrow(x$runs), if (nrow(x$runs) == 1L) "" else "s",
                nrow(x$events), if (nrow(x$events) == 1L) "" else "s"))
    if (nrow(x$runs)) {
        cat("\nSessions\n")
        print(x$runs[, c("reviewer", "started", "n_pairs", "n_shown",
                         "truncated")], row.names = FALSE)
    }
    accepted <- x$thresholds[!is.na(x$thresholds$status) &
                             x$thresholds$status == "accepted", , drop = FALSE]
    if (nrow(accepted)) {
        cat("\nThresholds accepted\n")
        for (k in seq_len(nrow(accepted))) {
            at <- x$cuts[x$cuts$event_id == accepted$event_id[k], ,
                         drop = FALSE]
            spelled <- if (!is.na(accepted$expr[k]) &&
                           nzchar(accepted$expr[k])) accepted$expr[k]
                       else paste(sprintf("%s %s %g", at$score, at$operator,
                                          at$value),
                                  collapse = if (identical(accepted$rule[k],
                                                           "any")) " | "
                                             else " & ")
            cat(sprintf("  %s by %s: %s\n", accepted$at[k],
                        accepted$reviewer[k], spelled))
            if (!is.na(accepted$note[k]) && nzchar(accepted$note[k]))
                cat(sprintf("      %s\n", accepted$note[k]))
        }
    }
    if (nrow(x$decisions)) {
        cat("\nDecisions\n")
        print(table(reviewer = x$decisions$reviewer,
                    decision = x$decisions$decision))
        cat(sprintf("With a comment: %d\n",
                    sum(!is.na(x$decisions$comment) &
                        nzchar(x$decisions$comment))))
    }
    if (nrow(x$disagreements)) {
        cat(sprintf("\n! %d pair%s judged differently by reviewers\n",
                    nrow(x$disagreements),
                    if (nrow(x$disagreements) == 1L) "" else "s"))
        print(utils::head(x$disagreements, 10L), row.names = FALSE)
    } else if (x$agreement$n_reviewers > 1L) {
        cat("\nReviewers agree on every pair they both saw.\n")
    }
    if (!is.null(x$status)) {
        cat("\n")
        if (x$status$pair_set_match && x$status$scores_match)
            cat("The pairs match the ones that were reviewed.\n")
        else
            cat(sprintf(paste0(
                "! The data has changed since the review: %s reviewed pair%s ",
                "no longer present, %s pair%s undecided.\n"),
                format(x$status$n_orphaned, big.mark = ","),
                if (x$status$n_orphaned == 1L) "" else "s",
                format(x$status$n_undecided, big.mark = ","),
                if (x$status$n_undecided == 1L) "" else "s"))
    }
    cat("\n")
    cli::cat_rule("Put this in your script")
    cat(paste0(x$code, collapse = "\n"), "\n", sep = "")
    invisible(x)
}


#' Apply saved decisions to candidate pairs
#'
#' Adds the columns a review was made to produce: `link`, saying whether each
#' pair is in scope, and `link_reason`, saying which rule decided it. The
#' accepted threshold decides the pairs nobody looked at individually, and the
#' per-pair verdicts override it, which is the order a clerical review is meant
#' to work in.
#'
#' Doing this by hand is where a review goes wrong. The threshold has to be
#' applied with the operator it was recorded with --- a score left on its own
#' scale by `normalise = FALSE` is cut with `<=`, not `>=` --- and pairs that
#' were reviewed but have since vanished have to be noticed rather than
#' silently dropped. This does both, reports what it did, and records in
#' `link_reason` which judgement produced every row, so a surprising link can
#' be traced back to the person who made it.
#'
#' @param x Candidate pairs with a `pair_id` column, normally an [rLink()]
#'   result. Never modified; a copy is returned.
#' @param decisions A [read_decisions()] result, or a path to a decisions file.
#' @param reviewer Optional reviewer whose verdicts to use. `NULL` uses
#'   everyone's, resolved by `rule`.
#' @param rule How to resolve two reviewers judging one pair differently:
#'   `"last"` takes the most recent verdict, `"unanimous"` takes a verdict only
#'   where every reviewer agrees and otherwise leaves the threshold to decide.
#' @param thresholds Whether to apply the most recently accepted threshold.
#'   `FALSE` starts every pair out of scope, so only the pairs explicitly
#'   included come back `TRUE`.
#' @param column Name for the logical column. Its reason column is that name
#'   with `_reason` appended.
#' @param quiet Whether to skip the summary of what was applied.
#' @return A copy of `x` with two columns added: `link` (logical) and
#'   `link_reason`, one of `"threshold"`, `"below threshold"`, `"included"`,
#'   `"excluded"`, `"disputed"` or `"no threshold"`.
#' @seealso [read_decisions()] to read the file, and [rLink_inspect()] to make
#'   the decisions.
#' @family record linkage review functions
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' path <- system.file("extdata", "example_decisions.json",
#'                     package = "fast.string")
#' pairs <- data.frame(pair_id = c("A|X", "B|Y", "C|Z", "D|W"),
#'                     index_1 = 1:4, index_2 = 1:4,
#'                     score = c(0.97, 0.74, 0.44, 0.31))
#'
#' # `B|Y` clears the recorded cut but was excluded by hand, and `link_reason`
#' # says so rather than leaving you to wonder why it is not a link.
#' out <- apply_decisions(pairs, path)
#' out[, c("pair_id", "score", "link", "link_reason")]
#'
#' # The two reviewers disagree about `C|Z`: the analyst included it and the
#' # QA reviewer later excluded it, so the default takes the later verdict.
#' apply_decisions(pairs, path, reviewer = "analyst", quiet = TRUE)$link[3]
#' apply_decisions(pairs, path, reviewer = "qa", quiet = TRUE)$link[3]
#'
#' # "unanimous" declines to pick a side, leaving the threshold to decide.
#' apply_decisions(pairs, path, rule = "unanimous", quiet = TRUE)$link_reason
#'
#' # Only what somebody explicitly included
#' apply_decisions(pairs, path, thresholds = FALSE, quiet = TRUE)$link
#' @export
apply_decisions <- function(x, decisions, reviewer = NULL,
                            rule = c("last", "unanimous"), thresholds = TRUE,
                            column = "link", quiet = FALSE) {
    rule <- match.arg(rule)
    .check_flag(thresholds, "thresholds")
    .check_flag(quiet, "quiet")
    if (!is.data.frame(x)) stop("`x` must be a data frame.")
    if (!"pair_id" %in% names(x))
        stop("`x` must have a `pair_id` column to apply decisions to it.")
    if (!is.character(column) || length(column) != 1L || is.na(column) ||
        !nzchar(column))
        stop("`column` must be a single column name.")
    if (is.character(decisions)) decisions <- read_decisions(decisions)
    if (!inherits(decisions, "fs_decisions"))
        stop(paste0("`decisions` must be a read_decisions() result or a path ",
                    "to a decisions file."), call. = FALSE)

    ids <- as.character(x[["pair_id"]])
    link <- rep(FALSE, length(ids))
    reason <- rep("no threshold", length(ids))

    accepted <- decisions$thresholds[
        !is.na(decisions$thresholds$status) &
        decisions$thresholds$status == "accepted", , drop = FALSE]
    combined <- NULL
    if (thresholds && nrow(accepted)) {
        last <- accepted[nrow(accepted), ]
        at <- decisions$cuts[decisions$cuts$event_id == last$event_id, ,
                             drop = FALSE]
        gone <- setdiff(at$score, names(x))
        if (length(gone))
            stop(sprintf(paste0(
                "The decisions were cut on %s, which `x` does not have. Score ",
                "the pairs with the same comparators first."),
                paste0("`", gone, "`", collapse = ", ")), call. = FALSE)
        passes <- lapply(seq_len(nrow(at)), function(k) {
            v <- as.double(x[[at$score[k]]])
            ok <- if (identical(at$operator[k], "<=")) v <= at$value[k]
                  else v >= at$value[k]
            # A pair with no score passes no cut.
            ok & !is.na(v)
        })
        # A recorded rule expression replaces the plain all/any combination
        # rather than refining it; a malformed one falls back to it.
        combined <- .decisions_rule_eval(last$expr, x)
        if (is.null(combined))
            combined <- if (identical(last$rule, "any")) Reduce(`|`, passes)
                        else Reduce(`&`, passes)
        link <- combined
        reason <- ifelse(combined, "threshold", "below threshold")
    }

    held <- decisions$decisions
    if (!is.null(reviewer))
        held <- held[held$reviewer %in% reviewer, , drop = FALSE]
    held <- held[!is.na(held$decision) & held$decision != "undecided", ,
                 drop = FALSE]
    n_override <- 0L
    if (nrow(held)) {
        verdict <- .review_verdict(held, rule)
        at <- match(ids, verdict$pair_id)
        hit <- !is.na(at)
        chosen <- verdict$decision[at[hit]]
        link[hit] <- chosen == "include"
        reason[hit] <- ifelse(chosen == "include", "included",
                              ifelse(chosen == "exclude", "excluded",
                                     "disputed"))
        disputed <- hit
        disputed[hit] <- chosen == "disputed"
        if (any(disputed))
            link[disputed] <- if (!is.null(combined)) combined[disputed]
                              else FALSE
        n_override <- sum(hit)
    }

    out <- if (data.table::is.data.table(x)) data.table::copy(x) else x
    out[[column]] <- link
    out[[paste0(column, "_reason")]] <- reason

    if (!quiet) {
        orphaned <- setdiff(held$pair_id, ids)
        cli::cli_alert_info(sprintf(
            "%s of %s pairs in scope (%s by threshold, %s judged by hand).",
            format(sum(link), big.mark = ","),
            format(length(ids), big.mark = ","),
            format(sum(reason == "threshold"), big.mark = ","),
            format(n_override, big.mark = ",")))
        if (length(orphaned))
            cli::cli_alert_warning(sprintf(paste0(
                "%s reviewed pair%s not in `x` at all, so %s judgement%s was ",
                "not applied."), format(length(orphaned), big.mark = ","),
                if (length(orphaned) == 1L) " is" else "s are",
                if (length(orphaned) == 1L) "that" else "those",
                if (length(orphaned) == 1L) "" else "s"))
        if (sum(reason == "disputed"))
            cli::cli_alert_warning(sprintf(
                "%s pair%s disputed between reviewers; the threshold decided.",
                format(sum(reason == "disputed"), big.mark = ","),
                if (sum(reason == "disputed") == 1L) " is" else "s are"))
    }
    out
}

# One verdict per pair out of possibly several reviewers'.
.review_verdict <- function(held, rule) {
    by_pair <- split(seq_len(nrow(held)), held$pair_id)
    decision <- vapply(by_pair, function(at) {
        verdicts <- held$decision[at]
        if (length(unique(verdicts)) == 1L) return(verdicts[1L])
        if (identical(rule, "unanimous")) return("disputed")
        # Latest wins, by when it was filed; `run_id` then `seq` keeps the
        # order deterministic when two reviewers land in the same millisecond.
        verdicts[order(held$at[at], held$run_id[at],
                       held$seq[at])][length(at)]
    }, "")
    data.frame(pair_id = names(by_pair), decision = unname(decision),
               stringsAsFactors = FALSE)
}

# Collects every missing package rather than stopping at the first, so one
# install line fixes the lot. The precedent is .bench_pkg().
.review_require <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L),
                            quietly = TRUE)]
    if (!length(missing)) return(invisible(TRUE))
    stop(sprintf(paste0(
        "This needs %s package%s that %s not installed: %s.\n",
        "Install %s with install.packages(%s)."),
        if (length(missing) == 1L) "a" else "some",
        if (length(missing) == 1L) "" else "s",
        if (length(missing) == 1L) "is" else "are",
        paste0("`", missing, "`", collapse = ", "),
        if (length(missing) == 1L) "it" else "them",
        if (length(missing) == 1L) paste0('"', missing, '"')
        else paste0('c("', paste(missing, collapse = '", "'), '")')),
        call. = FALSE)
}


# --- the rule expression ---------------------------------------------------
#
# `all` and `any` cover most linkages, but not the shape that actually turns up
# once a reviewer looks at the data: "a strong surname on its own, or a weaker
# one backed by an exact date of birth". That is an expression, so the app lets
# one be written and stores its text in the decisions file.
#
# A decisions file may arrive from someone else, and eval() on text from a file
# is a code-execution path. So the parsed expression is walked and checked
# against a whitelist before it runs, and it is then evaluated against the
# score columns alone, in an environment holding only the operators below --
# `system` and friends are not reachable even if the check were bypassed.

.decisions_rule_ops <- c("(", "{", "&", "|", "&&", "||", "!", "==", "!=",
                         "<", "<=", ">", ">=", "+", "-", "*", "/", "^",
                         "is.na", "!is.na", "xor", "abs", "round",
                         "pmin", "pmax", "ifelse")

# R syntax is canonical; the words are accepted because that is how the rule
# reads out loud and how a reviewer is likely to type it.
.decisions_rule_words <- function(text) {
    text <- gsub("\\bAND\\b", "&", text)
    text <- gsub("\\bOR\\b", "|", text)
    gsub("\\bNOT\\b", "!", text)
}

.decisions_rule_check <- function(e, allowed) {
    if (is.name(e)) {
        nm <- as.character(e)
        if (!nm %in% allowed)
            stop(sprintf(paste0(
                "`%s` is not a column a rule may use. Available: %s."),
                nm, paste0("`", allowed, "`", collapse = ", ")),
                call. = FALSE)
        return(invisible(TRUE))
    }
    if (is.call(e)) {
        fn <- e[[1L]]
        if (!is.name(fn) || !as.character(fn) %in% .decisions_rule_ops)
            stop(sprintf(paste0(
                "A rule may only compare and combine columns. `%s` is not ",
                "allowed; the permitted operators are %s."),
                paste(deparse(fn), collapse = " "),
                paste0("`", setdiff(.decisions_rule_ops, c("(", "{")), "`",
                       collapse = ", ")), call. = FALSE)
        for (k in seq_along(e)[-1L]) .decisions_rule_check(e[[k]], allowed)
        return(invisible(TRUE))
    }
    if (is.numeric(e) || is.logical(e) || is.character(e) || is.null(e))
        return(invisible(TRUE))
    stop("A rule may only hold columns, numbers and comparisons.",
         call. = FALSE)
}

# Columns a rule may name: the numeric and logical ones, which is what there is
# to compare. A carried surname is not something to threshold on.
.decisions_rule_columns <- function(x)
    names(x)[vapply(names(x), function(nm) {
        v <- x[[nm]]
        (is.numeric(v) || is.logical(v)) && is.null(dim(v))
    }, logical(1L))]

# Returns a logical per row, or NULL when the rule cannot be used -- the caller
# then falls back to the plain all/any combination rather than failing a whole
# linkage over one malformed rule.
.decisions_rule_eval <- function(text, x) {
    if (is.null(text) || !length(text) || is.na(text) || !nzchar(text))
        return(NULL)
    parsed <- tryCatch(parse(text = .decisions_rule_words(text))[[1L]],
                       error = function(cnd) NULL)
    if (is.null(parsed)) {
        warning(sprintf(paste0("The recorded rule `%s` does not parse; using ",
                               "the threshold instead."), text),
                call. = FALSE)
        return(NULL)
    }
    allowed <- .decisions_rule_columns(x)
    checked <- tryCatch({
        .decisions_rule_check(parsed, allowed)
        TRUE
    }, error = function(cnd) conditionMessage(cnd))
    if (!isTRUE(checked)) {
        warning(sprintf("The recorded rule `%s` was refused: %s", text,
                        checked), call. = FALSE)
        return(NULL)
    }
    # Only the whitelisted operators are reachable, and the mask holds only the
    # comparable columns. Built column by column with [[: `x[allowed]` on a
    # data.table is a join, not a column selection.
    env <- list2env(mget(setdiff(.decisions_rule_ops, "!is.na"),
                         envir = baseenv()), parent = emptyenv())
    mask <- lapply(allowed, function(nm) x[[nm]])
    names(mask) <- allowed
    out <- tryCatch(eval(parsed, mask, env), error = function(cnd) cnd)
    if (inherits(out, "condition")) {
        warning(sprintf("The recorded rule `%s` failed: %s", text,
                        conditionMessage(out)), call. = FALSE)
        return(NULL)
    }
    if (!is.logical(out) || length(out) != nrow(x)) {
        warning(sprintf(paste0(
            "The recorded rule `%s` gave %s of length %s, not one logical per ",
            "pair (%s); using the threshold instead."), text, class(out)[1L],
            format(length(out)), format(nrow(x))), call. = FALSE)
        return(NULL)
    }
    # A pair a rule cannot judge is not in scope.
    out & !is.na(out)
}
