enrol <- function() data.frame(
    frame_id = c("E1", "E2", "E3", "E4"),
    surname  = c("SMITH", "JONES", "BROWN", "SMYTH"),
    given    = c("JOHN", "MARY", "ROB", "JON"),
    stringsAsFactors = FALSE)

claims <- function() data.frame(
    frame_id = c("C1", "C2", "C3"),
    surname  = c("SMITH", "JONSE", "BROWNE"),
    given    = c("JON", "MARY", "ROBERT"),
    stringsAsFactors = FALSE)

scored <- function(...) rLink(
    enrol(), claims(), block_1 = list(fsubstr(surname, 1, 1)),
    compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given)),
    ...)

# A decision event the way the app writes one, but with an explicit timestamp:
# two reviewers really are minutes or days apart, and the test should say so
# rather than depend on which millisecond the loop happened to run in.
event <- function(reviewer, pair_id, decision, comment, seq, at,
                  run_id = reviewer) {
    list(event_id = paste0(run_id, "-d", seq), seq = seq, run_id = run_id,
         reviewer = reviewer, at = at, pair_id = pair_id, index_1 = 1L,
         index_2 = 1L, decision = decision, comment = comment)
}

write_events <- function(path, events) {
    obj <- fast.string:::.review_read_file(path)
    obj$decisions <- events
    fast.string:::.review_write(obj, path)
    path
}

spec_for <- function(x, ...) {
    args <- list(x = x, file = tempfile(), reviewer = "a", scores = NULL,
                 threshold = NULL, max_pairs = 100, page_size = 50,
                 font_size = 11,
                 order_by = c("review", "input", "score"), expression = "x")
    do.call(fast.string:::.inspect_spec, utils::modifyList(args, list(...)))
}

new_decisions <- function(x = scored(), ...) {
    path <- tempfile(fileext = ".json")
    rLink_inspect(x, decisions = path, reviewer = "analyst", launch = FALSE,
                  ...)
    path
}


# --- fingerprints ----------------------------------------------------------

test_that("a fingerprint is stable, and folds past the chunk boundary", {
    fp <- fast.string:::.review_fingerprint
    keys <- as.character(1:201)
    expect_identical(fp(keys), fp(keys))
    # Every size either side of a chunk edge, since that is where a chunked
    # fold breaks if the last partial chunk is mishandled.
    for (n in c(99L, 100L, 101L, 199L, 200L, 201L)) {
        small <- fp(keys[seq_len(n)], chunk = 100L)
        expect_identical(small, fp(keys[seq_len(n)], chunk = 100L))
        expect_true(startsWith(small, "xxh64:"))
    }
    expect_false(identical(fp(keys, chunk = 100L),
                           fp(keys[-1L], chunk = 100L)))
    expect_identical(fp(character()), "xxh64:none")
})

test_that("the fast fingerprint is the written-out one, digest for digest", {
    # A decisions file keeps the fingerprint of the pairs it was made on, so
    # a faster way of computing it has to give the same answer or every
    # existing review would open saying its data had changed.
    fast <- fast.string:::.review_fingerprint
    slow <- fast.string:::.review_fingerprint_r
    set.seed(3)
    keys <- c(sprintf("E%04d|C%04d", 1:450, sample(450)), "MÜLLER|x", "")
    for (n in c(1L, 2L, 99L, 100L, 101L, 452L)) for (chunk in c(2L, 7L, 100L))
        expect_identical(fast(keys[seq_len(n)], chunk = chunk),
                         slow(keys[seq_len(n)], chunk = chunk))
    # Taken in another order, it is the reordered keys' digest.
    o <- order(keys, method = "radix")
    expect_identical(fast(keys, chunk = 100L, order = o),
                     slow(keys[o], chunk = 100L))
    # Several columns are hashed as paste() would join them, missing values
    # and all.
    a <- as.character(c(0.5, NA, 1, 0.123456789012345, 1e-04, 1e+05))
    b <- as.character(c(1, 0.25, NA, NaN, Inf, -1))
    pasted <- paste(a, b, sep = "\u001f")
    expect_identical(fast(list(a, b), chunk = 4L, pasted = TRUE),
                     slow(pasted, chunk = 4L))
    expect_identical(fast(list(a), chunk = 4L, pasted = TRUE),
                     slow(paste(a), chunk = 4L))
    # A missing key that was not pasted falls back to the definition.
    expect_identical(fast(c("a", NA, "b")), slow(c("a", NA, "b")))

    # And the fingerprints of a table are what they were.
    out <- scored()
    got <- fast.string:::.review_fingerprints(out, c("sn", "gn"), "pair_id")
    ids <- as.character(out$pair_id)
    o <- order(ids, method = "radix")
    text <- paste(as.character(out$sn), as.character(out$gn), sep = "\u001f")
    expect_identical(got$pair_set, slow(ids[o]))
    expect_identical(got$scores, slow(text[o]))
})

test_that("the pair-set fingerprint ignores row order but not content", {
    out <- scored()
    fp <- function(x) fast.string:::.review_fingerprints(
        x, c("sn", "gn"), "pair_id")
    base <- fp(out)
    expect_identical(base$pair_set, fp(out[rev(seq_len(nrow(out))), ])$pair_set)
    moved <- out
    moved$sn[1L] <- moved$sn[1L] / 2
    expect_identical(base$pair_set, fp(moved)$pair_set)
    expect_false(identical(base$scores, fp(moved)$scores))
    expect_false(identical(base$pair_set, fp(out[-1L, ])$pair_set))
})


# --- the score specification ----------------------------------------------

test_that("the score columns and their cut directions come from rLink()", {
    spec <- fast.string:::.inspect_scores(scored())
    expect_identical(spec$score, c("sn", "gn"))
    expect_identical(spec$comparator, c("jaro_winkler", "jaro_winkler"))
    expect_identical(spec$direction, c("higher", "higher"))
    expect_identical(spec$operator, c(">=", ">="))
    expect_true(all(spec$direction_known))
    expect_identical(spec$column_1, c("surname", "given"))
})

test_that("an unnormalised distance is recognised as lower-is-a-match", {
    out <- rLink(enrol(), claims(), block_1 = list(fsubstr(surname, 1, 1)),
                 compare = list(sn = jaro_winkler(surname),
                                d = levenshtein(surname)),
                 normalise = FALSE)
    spec <- fast.string:::.inspect_scores(out)
    expect_identical(spec$direction, c("higher", "lower"))
    expect_identical(spec$operator, c(">=", "<="))
    expect_true(all(spec$direction_known))
})

test_that("score columns are found without the compare attribute", {
    bare <- as.data.frame(scored())
    attr(bare, "compare") <- NULL
    spec <- fast.string:::.inspect_scores(bare)
    expect_true(all(c("sn", "gn") %in% spec$score))
    # Unknown direction is read as higher-is-a-match, and flagged as a guess.
    expect_true(all(spec$direction == "higher"))
    expect_false(any(spec$direction_known))
})

test_that("`scores` given explicitly is validated", {
    out <- scored()
    expect_identical(fast.string:::.inspect_scores(out, "sn")$score, "sn")
    expect_error(fast.string:::.inspect_scores(out, "nope"), "not there")
    expect_error(fast.string:::.inspect_scores(out, "surname_1"),
                 "not numeric")
    expect_error(fast.string:::.inspect_scores(out, 1), "character vector")
})

test_that("a starting cut is 0.85 for a similarity and the midpoint else", {
    spec <- data.frame(
        score = c("sim", "wide"), direction = c("higher", "higher"),
        observed_min = c(0, 2), observed_max = c(1, 8),
        stringsAsFactors = FALSE)
    expect_equal(fast.string:::.inspect_default_cuts(spec), c(0.85, 5))
})


# --- the colour scale ------------------------------------------------------

test_that("the gradient covers its range, clamps and marks the missing", {
    colour <- fast.string:::.inspect_colour
    out <- colour(c(0, 0.5, 1, NA), 0, 1)
    expect_length(out, 4L)
    expect_true(all(grepl("^#[0-9a-fA-F]{6}$", out)))
    expect_identical(out[4L], fast.string:::.inspect_na_colour)
    # Out of range clamps rather than extrapolating off the ramp.
    expect_identical(colour(2, 0, 1), colour(1, 0, 1))
    expect_identical(colour(-1, 0, 1), colour(0, 0, 1))
    # A lower-is-a-match score is ramped the other way, so dark always means
    # "more like a match".
    expect_identical(colour(0, 0, 1, "lower"), colour(1, 0, 1, "higher"))
    # A degenerate range does not divide by zero.
    expect_true(grepl("^#", colour(5, 5, 5)))
})

test_that("the text colour flips on a dark fill", {
    expect_identical(fast.string:::.inspect_text_colour(c(0, 1), 0, 1),
                     c("#111111", "#ffffff"))
})


# --- which pairs pass ------------------------------------------------------

test_that("a pair passes on every cut, or on any, and never on no score", {
    values <- list(a = c(1, 0.5, 0.9, NA), b = c(1, 0.9, 0.4, 1))
    pass <- fast.string:::.inspect_pass
    expect_identical(pass(values, c(0.8, 0.8), c(">=", ">=")),
                     c(TRUE, FALSE, FALSE, FALSE))
    expect_identical(pass(values, c(0.8, 0.8), c(">=", ">="), "any"),
                     c(TRUE, TRUE, TRUE, TRUE))
    # A distance is cut the other way.
    expect_identical(pass(list(a = c(0, 3)), 1, "<="), c(TRUE, FALSE))
})


# --- the widget configuration ---------------------------------------------

test_that("the meta carries one domain per score, with its direction", {
    spec <- fast.string:::.inspect_scores(scored())
    meta <- fast.string:::.inspect_meta(spec, c(sn = 0.8, gn = 0.7), "all")
    expect_identical(names(meta$domains), c("sn", "gn"))
    expect_identical(meta$domains$sn$direction, "higher")
    expect_identical(meta$domains$sn$operator, ">=")
    expect_equal(meta$cuts$sn, 0.8)
    expect_identical(meta$rule, "all")
    expect_identical(meta$na_colour, fast.string:::.inspect_na_colour)
    expect_length(meta$ramp, length(fast.string:::.inspect_ramp))
})

test_that("the generated JavaScript reads the contract the meta provides", {
    skip_if_not_installed("reactable")
    # If a rename on the R side broke the names the renderers read, the table
    # would silently lose its colours; this pins the two sides together.
    expect_true(grepl("state.meta.domains",
                      as.character(fast.string:::.inspect_js_score()),
                      fixed = TRUE))
    expect_true(grepl("state.meta.decisions",
                      as.character(fast.string:::.inspect_js_decision()),
                      fixed = TRUE))
    expect_true(grepl("state.meta.comments",
                      as.character(fast.string:::.inspect_js_comment()),
                      fixed = TRUE))
    # Dimming reads the scope R computed, not a cut recomputed in the
    # browser: under a rule expression the cuts no longer decide scope, and a
    # browser-side recompute would dim the wrong rows.
    rowclass <- as.character(fast.string:::.inspect_js_rowclass())
    expect_true(grepl("_scope", rowclass, fixed = TRUE))
    expect_true(grepl("state.meta.decisions", rowclass, fixed = TRUE))
    expect_false(grepl("state.meta.cuts", rowclass, fixed = TRUE))
    js <- fast.string:::.inspect_js_bindings()
    expect_true(grepl("fs_decision", js, fixed = TRUE))
    expect_true(grepl("fs_comment", js, fixed = TRUE))
    # The nonce is what makes a repeated identical click fire at all.
    expect_true(grepl("nonce", js, fixed = TRUE))
})

test_that("every column gets a colDef, with the identity ones pinned", {
    skip_if_not_installed("reactable")
    spec <- spec_for(scored())
    cols <- fast.string:::.inspect_columns(spec, names(spec$view))
    expect_true(all(vapply(cols, inherits, logical(1L), "colDef")))
    expect_true(all(c("_scope", "_decision", "_comment") %in% names(cols)))
    # The identity and scope columns stay put when the table scrolls sideways.
    expect_identical(cols$pair_id$sticky, "left")
    expect_identical(cols[["_scope"]]$sticky, "left")
    expect_identical(cols[["_decision"]]$sticky, "left")
    expect_false(cols[["_decision"]]$sortable)
    expect_false(cols[["_comment"]]$sortable)
    # `_threshold` carries the cut's own verdict for the scope cell to
    # compare a judgement against, and is never drawn.
    expect_false(cols[["_threshold"]]$show)
    # Asking for fewer columns gets fewer, plus the control columns.
    few <- fast.string:::.inspect_columns(spec, c("pair_id", "sn"))
    expect_identical(sort(names(few)),
                     sort(c("pair_id", "sn", "_scope", "_threshold",
                            "_decision", "_comment")))
})

test_that("the CSS is scoped and carries the font size", {
    css <- fast.string:::.inspect_css(9L)
    expect_true(grepl("--fs-font: 9px", css, fixed = TRUE))
    # Every rule is under the wrapper id, so nothing leaks into a host page.
    rules <- grep("^#fs-inspect", strsplit(css, "\n")[[1L]], value = TRUE)
    expect_true(length(rules) > 5L)
    expect_false(any(grepl("^\\s*(body|table|input|button)\\s*\\{",
                           strsplit(css, "\n")[[1L]])))
})


# --- the prepared specification -------------------------------------------


test_that("the table is capped but the analysis is not", {
    out <- scored()
    spec <- spec_for(out, max_pairs = 2)
    expect_identical(spec$n_shown, 2L)
    expect_true(spec$truncated)
    expect_identical(nrow(spec$all), nrow(out))
    expect_length(spec$all_ids, nrow(out))
    # The rows kept are the ones review_order() ranked first.
    want <- review_order(out$sn, threshold = unname(spec$cuts[1L]))[1:2]
    expect_identical(spec$shown, want)
    expect_false(spec_for(out, max_pairs = 100)$truncated)
})

test_that("pair_id provenance is recorded", {
    expect_identical(spec_for(scored())$pair_id_source, "composite")
    # No frame_id, so rLink() numbered the pairs.
    plain <- rLink(enrol()[, -1L], claims()[, -1L],
                   block_1 = list(fsubstr(surname, 1, 1)),
                   compare = list(sn = jaro_winkler(surname)))
    expect_identical(spec_for(plain)$pair_id_source, "sequence")
    # No pair_id column at all.
    bare <- as.data.frame(plain)[, c("index_1", "index_2", "sn")]
    expect_identical(spec_for(bare)$pair_id_source, "index")
})

test_that("a pair_id that cannot identify a pair is refused", {
    out <- scored()
    dup <- out
    dup$pair_id[2L] <- dup$pair_id[1L]
    expect_error(spec_for(dup), "repeated")
    na_id <- out
    na_id$pair_id[1L] <- NA
    expect_error(spec_for(na_id), "missing")
})

test_that("the specification rejects what it cannot review", {
    out <- scored()
    expect_error(spec_for(out[0L, ]), "no pairs")
    expect_error(spec_for(as.data.frame(out)[, c("sn", "gn")]),
                 "which columns hold the pair indices")
    expect_error(spec_for(out, page_size = 5000), "between 5 and 1000")
    expect_error(spec_for(out, page_size = 1), "between 5 and 1000")
    expect_error(spec_for(out, font_size = 40), "between 6 and 24")
    expect_error(spec_for(out, max_pairs = 0), "at least 1")
    expect_error(spec_for(out, reviewer = ""), "non-empty")
    expect_error(spec_for(out, threshold = 0.8), "named numeric")
    expect_error(spec_for(out, threshold = c(nope = 0.8)), "not there")
    expect_error(spec_for(list(a = 1)), "must be a data frame")
})

test_that("a stated threshold overrides the default cut", {
    out <- scored()
    spec <- spec_for(out, threshold = c(sn = 0.42))
    expect_equal(unname(spec$cuts["sn"]), 0.42)
    # The other score keeps its default, which for a 0-1 similarity is 0.85
    # held inside the range the data actually covers, so that the slider opens
    # at a position it can reach.
    expect_equal(unname(spec$cuts["gn"]),
                 min(max(min(out$gn), 0.85), max(out$gn)))
})

test_that("the evidence pane puts the two sides of a pair together", {
    out <- scored()
    spec <- spec_for(out)
    ev <- fast.string:::.inspect_evidence(spec, out$pair_id[1L])
    expect_identical(nrow(ev), 2L)
    expect_identical(ev$score, c("sn", "gn"))
    expect_identical(ev$value_1[1L], out$surname_1[1L])
    expect_identical(ev$value_2[1L], out$surname_2[1L])
    expect_equal(ev$value[1L], out$sn[1L])
    expect_error(fast.string:::.inspect_evidence(spec, "nope"),
                 "not one of these pairs")
})

test_that("the effect of a cut is computed over every pair", {
    out <- scored()
    spec <- spec_for(out)
    e <- fast.string:::.inspect_effect(spec, c(sn = 0, gn = 0), "all")
    expect_identical(e$n_pairs, nrow(out))
    high <- fast.string:::.inspect_effect(spec, c(sn = 2, gn = 2), "all")
    expect_identical(high$n_pairs, 0L)
    expect_identical(high$n_one_to_one, 0L)
})


# --- the decisions file ----------------------------------------------------------

test_that("the decisions file is written as soon as the app is built", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    skip_if_not_installed("jsonlite")
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(scored(), decisions = path, reviewer = "analyst",
                         launch = FALSE)
    expect_s3_class(app, "shiny.appobj")
    expect_true(file.exists(path))
    review <- read_decisions(path)
    expect_s3_class(review, "fs_decisions")
    expect_identical(nrow(review$runs), 1L)
    expect_identical(review$runs$reviewer, "analyst")
    expect_identical(review$runs$n_pairs, nrow(scored()))
    expect_identical(review$scores$score, c("sn", "gn"))
    expect_identical(nrow(review$decisions), 0L)
})

test_that("an unwritable path fails before any review is done", {
    skip_if_not_installed("shiny")
    expect_error(
        rLink_inspect(scored(), decisions = file.path(tempdir(), "nope", "a.json"),
                      launch = FALSE),
        "no directory")
    expect_error(rLink_inspect(scored(), decisions = ""), "single path")
})

test_that("a stated threshold is recorded as an accepted one, with its operator", {
    skip_if_not_installed("shiny")
    path <- new_decisions(threshold = c(sn = 0.8))
    review <- read_decisions(path)
    expect_identical(nrow(review$thresholds), 1L)
    expect_identical(review$thresholds$status, "accepted")
    # Every cut in force is recorded, not only the one the caller named:
    # `gn` was left at its default, that default was what the reviewer saw,
    # and apply_decisions() has to cut on it too or it widens the scope.
    expect_identical(review$cuts$score, c("sn", "gn"))
    expect_identical(review$cuts$operator, c(">=", ">="))
    expect_equal(review$cuts$value[review$cuts$score == "sn"], 0.8)
    # The consequence of the cut is snapshotted, so a reader need not re-run it.
    expect_false(is.na(review$thresholds$n_pairs))
})

test_that("full precision survives the round trip", {
    skip_if_not_installed("shiny")
    # jsonlite rounds to 4 significant digits by default, which would silently
    # round the very numbers the file exists to record.
    path <- new_decisions(threshold = c(sn = 0.881234567890123))
    cuts <- read_decisions(path)$cuts
    expect_equal(cuts$value[cuts$score == "sn"], 0.881234567890123,
                 tolerance = 0)
})

test_that("a review reports the current verdict, the log, and the revisions", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "father and son", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("analyst", out$pair_id[1L], "include", "changed my mind", 2L,
              "2026-11-06T09:05:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "disagree", 1L,
              "2026-11-07T10:00:00.000Z"),
        event("qa", out$pair_id[2L], "include", "fine élève", 1L,
              "2026-11-07T10:01:00.000Z")))
    review <- read_decisions(path)
    expect_identical(nrow(review$events), 4L)
    expect_identical(nrow(review$decisions), 3L)
    mine <- review$decisions[review$decisions$reviewer == "analyst" &
                             review$decisions$pair_id == out$pair_id[1L], ]
    expect_identical(mine$decision, "include")
    expect_identical(mine$n_changes, 2L)
    expect_true(any(grepl("élève", review$events$comment)))
    expect_identical(nrow(read_decisions(path, reviewer = "qa")$decisions), 2L)
})

test_that("a review surfaces where two reviewers disagree", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "include", "same person", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "not convinced", 1L,
              "2026-11-07T10:00:00.000Z"),
        event("qa", out$pair_id[2L], "include", "agreed", 2L,
              "2026-11-07T10:01:00.000Z")))
    review <- read_decisions(path)
    expect_identical(nrow(review$disagreements), 1L)
    expect_identical(review$disagreements$pair_id, out$pair_id[1L])
    expect_identical(review$disagreements$reviewers, "analyst, qa")
    expect_identical(review$agreement$n_reviewers, 2L)
    expect_identical(review$agreement$n_disagreed, 1L)
    expect_output(print(review), "judged differently")
    expect_output(print(review), "Pair decisions")
})

test_that("an appended session never overwrites the one before it", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "mine", 1L,
              "2026-11-06T09:00:00.000Z")))
    # A second person opens the same file.
    rLink_inspect(out, decisions = path, reviewer = "qa", launch = FALSE)
    review <- read_decisions(path)
    expect_identical(nrow(review$runs), 2L)
    expect_identical(review$runs$reviewer, c("analyst", "qa"))
    # And the first person's decision is still there.
    expect_identical(nrow(review$events), 1L)
    expect_identical(review$events$reviewer, "analyst")
})

test_that("a changed input is reported rather than ignored", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    expect_true(read_decisions(path, x = out)$status$pair_set_match)
    fewer <- read_decisions(path, x = out[-1L, ])$status
    expect_false(fewer$pair_set_match)
    expect_output(print(read_decisions(path, x = out[-1L, ])), "has changed")
    expect_output(print(read_decisions(path, x = out)), "match the ones")
})

test_that("`n_undecided` counts the pairs nobody has reached a verdict on", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    ids <- out$pair_id
    # One pair judged, one opened and left undecided, the rest untouched.
    write_events(path, list(
        event("a", ids[1L], "include", "same person", 1L,
              "2024-01-01T00:00:00.000Z"),
        event("a", ids[2L], "undecided", "not sure", 2L,
              "2024-01-01T00:00:01.000Z")))
    status <- read_decisions(path, x = out)$status
    # Leaving a pair undecided files an event for it, but it has still not
    # been judged, so it is still work remaining.
    expect_identical(status$n_undecided, nrow(out) - 1L)
    expect_identical(status$n_decided_present, 1L)
})

test_that("a damaged or foreign file is refused by name", {
    skip_if_not_installed("jsonlite")
    path <- tempfile(fileext = ".json")
    writeLines("{ not json", path)
    expect_error(read_decisions(path), "not readable as JSON")
    writeLines('{"schema":"something/else","format_version":1}', path)
    expect_error(read_decisions(path), "not a fast.string decisions file")
    writeLines(sprintf('{"schema":"%s","format_version":99}',
                       fast.string:::.review_schema), path)
    expect_error(read_decisions(path), "reads up to")
    expect_error(read_decisions(tempfile()), "no decisions file")
    expect_error(read_decisions(1), "single path")
})

test_that("the previous version is kept beside a rewritten file", {
    skip_if_not_installed("shiny")
    path <- new_decisions()
    obj <- fast.string:::.review_read_file(path)
    fast.string:::.review_write(obj, path)
    expect_true(file.exists(paste0(path, ".bak")))
})

test_that("a path with spaces and unicode round-trips", {
    skip_if_not_installed("jsonlite")
    folder <- file.path(tempdir(), "a folder with spaces")
    dir.create(folder, showWarnings = FALSE)
    path <- file.path(folder, "réview.json")
    obj <- fast.string:::.review_new()
    obj$decisions <- list(event("a", "p1", "include", "café — yes",
                                1L, "2026-11-06T09:00:00.000Z"))
    fast.string:::.review_write(obj, path)
    expect_identical(read_decisions(path)$events$comment, "café — yes")
})


# --- applying decisions ----------------------------------------------------

applied <- function(...) suppressMessages(apply_decisions(...))

test_that("apply_decisions() adds the scope columns to a copy", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    got <- applied(out, path)
    expect_true(all(c("link", "link_reason") %in% names(got)))
    expect_identical(got$link, out$sn >= 0.9)
    expect_true(all(got$link_reason %in% c("threshold", "below threshold")))
    # The input is untouched.
    expect_false("link" %in% names(out))
    # The rest of the table comes through unchanged.
    expect_identical(got$pair_id, out$pair_id)
    expect_identical(nrow(got), nrow(out))
    # A different column name, with its reason beside it.
    named <- applied(out, path, column = "in_scope")
    expect_true(all(c("in_scope", "in_scope_reason") %in% names(named)))
    expect_identical(named$in_scope, got$link)
})

test_that("apply_decisions() cuts an unnormalised distance the right way", {
    skip_if_not_installed("shiny")
    # Applying it the wrong way round would invert the whole linkage.
    raw <- rLink(enrol(), claims(), block_1 = list(fsubstr(surname, 1, 1)),
                 compare = list(d = levenshtein(surname)), normalise = FALSE)
    path <- tempfile(fileext = ".json")
    rLink_inspect(raw, decisions = path, reviewer = "a", launch = FALSE,
                  threshold = c(d = 2))
    expect_identical(read_decisions(path)$cuts$operator, "<=")
    expect_identical(applied(raw, path)$link, raw$d <= 2)
})

test_that("apply_decisions() lets a per-pair verdict override the threshold", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    low <- which.min(out$sn)
    high <- which.max(out$sn)
    write_events(path, list(
        # Included although it fails the cut, and excluded although it passes.
        event("a", out$pair_id[low], "include", "checked", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("a", out$pair_id[high], "exclude", "twins", 2L,
              "2026-11-06T09:01:00.000Z")))
    got <- applied(out, path)
    expect_true(got$link[low])
    expect_identical(got$link_reason[low], "included")
    expect_false(got$link[high])
    expect_identical(got$link_reason[high], "excluded")
})

test_that("apply_decisions() resolves two reviewers by its rule", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    write_events(path, list(
        event("analyst", out$pair_id[1L], "include", "same", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "not same", 1L,
              "2026-11-07T10:00:00.000Z")))
    expect_identical(applied(out, path, rule = "last")$link_reason[1L],
                     "excluded")
    expect_identical(applied(out, path, rule = "unanimous")$link_reason[1L],
                     "disputed")
    expect_identical(applied(out, path, reviewer = "analyst")$link_reason[1L],
                     "included")
    # Under "unanimous" a disputed pair keeps whatever the threshold said.
    expect_identical(applied(out, path, rule = "unanimous")$link[1L],
                     out$sn[1L] >= 0.9)
})

test_that("apply_decisions() with no threshold keeps only what was included", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    got <- applied(out, path)
    expect_false(any(got$link))
    expect_true(all(got$link_reason == "no threshold"))
    # thresholds = FALSE does the same even when one was recorded.
    with_cut <- new_decisions(out, threshold = c(sn = 0))
    expect_true(all(applied(out, with_cut)$link))
    expect_false(any(applied(out, with_cut, thresholds = FALSE)$link))
})

test_that("apply_decisions() reports what it did", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    expect_message(apply_decisions(out, path), "in scope")
    expect_silent(apply_decisions(out, path, quiet = TRUE))
    # A decision about a pair that is not here is called out, not dropped
    # silently.
    write_events(path, list(
        event("a", "not-a-pair-at-all", "include", "stale", 1L,
              "2026-11-06T09:00:00.000Z")))
    expect_message(apply_decisions(out, path), "not in `x` at all")
})

test_that("apply_decisions() refuses pairs it cannot cut", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    expect_error(applied(as.data.frame(out)[, c("pair_id", "gn")], path),
                 "which `x` does not have")
    expect_error(applied(as.data.frame(out)[, c("sn", "gn")], path),
                 "`pair_id` column")
    expect_error(applied(out, 1), "must be a read_decisions")
    expect_error(applied(list(), path), "must be a data frame")
    expect_error(applied(out, path, column = ""), "single column name")
})

test_that("a decisions object prints the R that reproduces it", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    d <- read_decisions(path)
    expect_type(d$code, "character")
    expect_true(any(grepl("rLink_inspect(", d$code, fixed = TRUE)))
    expect_true(any(grepl("apply_decisions(", d$code, fixed = TRUE)))
    # The path is in the generated code, with forward slashes so it pastes
    # into a script on any platform.
    expect_true(any(grepl(gsub("\\\\", "/", path), d$code, fixed = TRUE)))
    expect_output(print(d), "Put this in your script")
    expect_output(print(d), "apply_decisions")
})


# --- rule expressions ------------------------------------------------------

test_that("a rule expression is evaluated over the score columns", {
    frame <- data.frame(sn = c(0.95, 0.85, 0.85, 0.40),
                        gn = c(0.10, 0.99, 0.10, 0.99))
    got <- fast.string:::.decisions_rule_eval("sn >= 0.9 | (sn >= 0.8 & gn >= 0.95)",
                                              frame)
    expect_identical(got, c(TRUE, TRUE, FALSE, FALSE))
    # The spoken words work too, because that is how the rule reads.
    expect_identical(
        fast.string:::.decisions_rule_eval("sn >= 0.9 OR (sn >= 0.8 AND gn >= 0.95)",
                                           frame),
        got)
})

test_that("a rule expression may only compare columns", {
    frame <- data.frame(sn = c(1, 0))
    refuse <- function(text)
        expect_warning(
            expect_null(fast.string:::.decisions_rule_eval(text, frame)))
    # Not a column of the data.
    refuse("nope >= 1")
    # Not an operator a rule may use: this is the path that would otherwise
    # make a decisions file from someone else an arbitrary-code channel.
    refuse("system('echo hi')")
    refuse("sn >= 1 & eval(parse(text = 'Sys.setenv(X = 1)'))")
    refuse("Recall(sn)")
    # Does not parse, and does not give one logical per row.
    refuse("sn >= ")
    refuse("sum(sn)")
})

test_that("a rule expression that cannot run falls back to the threshold", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out, threshold = c(sn = 0.9))
    # Plant a rule naming a column that is not there.
    obj <- fast.string:::.review_read_file(path)
    obj$thresholds[[1L]]$expr <- "not_a_column >= 1"
    fast.string:::.review_write(obj, path)
    # Assigned inside, because expect_warning() returns the condition.
    expect_warning(got <- applied(out, path), "refused")
    expect_identical(got$link, out$sn >= 0.9)
})


# --- the server, headlessly ------------------------------------------------

test_that("a decision is recorded, kept, and saved", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "analyst",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(fs_decision = list(
            pair_id = out$pair_id[1L], decision = "exclude", nonce = 1))
        expect_length(rv_events(), 1L)
        expect_identical(rv_decisions()[[out$pair_id[1L]]], "exclude")

        # A change of mind is another event, not an edit of the first.
        session$setInputs(fs_decision = list(
            pair_id = out$pair_id[1L], decision = "include", nonce = 2))
        expect_length(rv_events(), 2L)
        expect_identical(rv_decisions()[[out$pair_id[1L]]], "include")

        # A comment lands against the same pair.
        session$setInputs(fs_comment = list(
            pair_id = out$pair_id[1L], comment = "checked the address",
            nonce = 3))
        expect_identical(rv_comments()[[out$pair_id[1L]]],
                         "checked the address")

        # An unknown pair is ignored rather than recorded.
        session$setInputs(fs_decision = list(
            pair_id = "not-a-pair", decision = "include", nonce = 4))
        expect_length(rv_events(), 3L)

        # A decision that is not one of the three is ignored too.
        session$setInputs(fs_decision = list(
            pair_id = out$pair_id[2L], decision = "maybe", nonce = 5))
        expect_length(rv_events(), 3L)

        session$setInputs(save = 1)
    })
    review <- read_decisions(path)
    expect_identical(nrow(review$events), 3L)
    expect_identical(review$decisions$decision[
        review$decisions$pair_id == out$pair_id[1L]], "include")
    expect_identical(review$decisions$comment[
        review$decisions$pair_id == out$pair_id[1L]], "checked the address")
    expect_identical(review$decisions$n_changes[
        review$decisions$pair_id == out$pair_id[1L]], 3L)
})

test_that("moving a slider changes what passes, over every pair", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "all")
        session$elapse(300)
        expect_identical(sum(pass_all()), nrow(out))
        expect_identical(effect()$n_pairs, nrow(out))

        session$setInputs(cut_1 = 2, cut_2 = 2)
        session$elapse(300)
        expect_identical(sum(pass_all()), 0L)

        # "any" is a different question from "all".
        session$setInputs(cut_1 = 0, cut_2 = 2, rule = "any")
        session$elapse(300)
        expect_identical(sum(pass_all()), nrow(out))
        session$setInputs(rule = "all")
        session$elapse(300)
        expect_identical(sum(pass_all()), 0L)
    })
})

test_that("recording a threshold writes it with its effect", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a", launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "all",
                          note = "everything passes here")
        session$elapse(300)
        session$setInputs(accept = 1)
        expect_length(rv_thresholds(), 1L)
    })
    review <- read_decisions(path)
    expect_identical(nrow(review$thresholds), 1L)
    expect_identical(review$thresholds$note, "everything passes here")
    expect_identical(review$thresholds$n_pairs, nrow(out))
    expect_identical(nrow(review$cuts), 2L)
})

test_that("reopening a review shows the earlier decisions", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- new_decisions(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "earlier call", 1L,
              "2026-11-06T09:00:00.000Z")))
    app <- rLink_inspect(out, decisions = path, reviewer = "analyst",
                         launch = FALSE)
    shiny::testServer(app, {
        expect_identical(rv_decisions()[[out$pair_id[1L]]], "exclude")
        expect_identical(rv_comments()[[out$pair_id[1L]]], "earlier call")
    })
})

test_that("the table renders and carries the decision state", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.5, cut_2 = 0.5, rule = "all",
                          filter = "all", mode = "flag", page_size = 50,
                          font_size = 11, columns = c("pair_id", "sn", "gn"))
        session$elapse(300)
        data <- table_data()
        expect_true(all(c("pair_id", "sn", "_decision", "_comment") %in%
                        names(data)))
        expect_identical(unique(data[["_decision"]]), "undecided")
        expect_identical(nrow(data), nrow(out))

        # Filtering to the undecided keeps them all; to the included, none.
        session$setInputs(filter = "include")
        expect_identical(nrow(table_data()), 0L)
        session$setInputs(filter = "undecided")
        expect_identical(nrow(table_data()), nrow(out))

        # Hiding the failures really hides them.
        session$setInputs(filter = "all", mode = "filter", cut_1 = 2,
                          cut_2 = 2)
        session$elapse(300)
        expect_identical(nrow(table_data()), 0L)
    })
})




# --- the app shell ---------------------------------------------------------
#
# testServer() evaluates its expression inside the server function's own
# environment, which can see the named reactives but not `spec` -- that is the
# enclosing closure's argument. So these reach for `out` and the public
# surface, which is the better thing to assert against anyway.

test_that("the UI renders, with the sidebar, panels and controls in it", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    skip_if_not_installed("bslib")
    skip_if_not_installed("sortable")
    html <- as.character(htmltools::renderTags(
        fast.string:::.inspect_ui(spec_for(scored())))$html)
    expect_true(nchar(html) > 2000)
    # A collapsible sidebar and collapsible panels, so the table gets the width.
    expect_true(grepl("sidebar", html))
    expect_true(grepl("accordion", html))
    # Both ways of setting a cut, and the rule box.
    expect_true(grepl("cut_1", html, fixed = TRUE))
    expect_true(grepl("num_1", html, fixed = TRUE))
    expect_true(grepl("Suggest a cut", html, fixed = TRUE))
    # Bulk marking and the way out.
    expect_true(grepl("Ctrl-click", html, fixed = TRUE))
    expect_true(grepl("Finish and close", html, fixed = TRUE))
})

test_that("the column picker degrades rather than blanking the table", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    skip_if_not_installed("sortable")
    out <- scored()
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        # Nothing reported yet -- which is also what happens when sortable
        # cannot report at all -- and the table still shows every column.
        expect_true(all(c("pair_id", "sn", "gn") %in% col_order()))
        expect_true(length(col_order()) > 3L)
        # A reported order is honoured, and dragging a column to Hidden drops
        # it.
        session$setInputs(cols_shown = c("sn", "pair_id"))
        expect_identical(col_order(), c("sn", "pair_id"))
        expect_true(all(c("sn", "pair_id") %in% names(table_data())))
        expect_false("gn" %in% names(table_data()))
        # Nonsense from the widget does not empty the table.
        session$setInputs(cols_shown = "not_a_column")
        expect_true(length(col_order()) > 3L)
    })
})


# --- choosing the cut ------------------------------------------------------

test_that("the slider and the numeric box stay in step", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    app <- rLink_inspect(scored(), decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0.9, rule = "all")
        session$elapse(400)
        expect_equal(unname(cuts()[1L]), 0.9)
        # Typing an exact value into the box moves the cut too, which is the
        # point of having the box: 0.8832 is not findable with a slider.
        session$setInputs(num_1 = 0.8832)
        session$elapse(400)
        expect_equal(unname(cuts()[1L]), 0.8832)
    })
})

test_that("a suggested cut lands in range and explains itself", {
    out <- scored()
    sw <- threshold_sweep(
        data.frame(index_1 = out$index_1, index_2 = out$index_2), out$sn)
    got <- fast.string:::.inspect_suggest(sw)
    expect_false(is.null(got))
    expect_true(got$threshold >= min(sw$threshold))
    expect_true(got$threshold <= max(sw$threshold))
    expect_true(nzchar(got$why))
    # A number a reviewer cannot justify is barely better than a guess, so the
    # reasoning is part of the answer.
    expect_true(grepl("one-to-one", got$why))
    one <- threshold_sweep(data.frame(index_1 = 1L, index_2 = 1L), 0.5)
    expect_false(is.null(fast.string:::.inspect_suggest(one)))
    expect_null(fast.string:::.inspect_suggest(one[0L, ]))
})

test_that("a rule expression drives the cut, and a broken one does not", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "expr",
                          expr = "sn >= 0.99 | gn >= 0.99")
        session$elapse(600)
        expect_identical(pass_all(), out$sn >= 0.99 | out$gn >= 0.99)
        # Nonsense falls back to the sliders rather than failing the session.
        session$setInputs(expr = "this is not a rule")
        session$elapse(600)
        expect_true(all(pass_all()))
    })
})

test_that("a recorded threshold keeps the rule it was written with", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "expr",
                          expr = "sn >= 0.99", note = "strict surnames only")
        session$elapse(600)
        session$setInputs(accept = 1)
        expect_length(rv_thresholds(), 1L)
        expect_identical(rv_thresholds()[[1L]]$expr, "sn >= 0.99")
    })
    d <- read_decisions(path)
    expect_identical(d$thresholds$expr, "sn >= 0.99")
    expect_identical(d$thresholds$note, "strict surnames only")
    # The rule is what prints, not the slider positions behind it.
    expect_output(print(d), "sn >= 0.99")
    # And the rule is what gets applied, not the cuts stored beside it.
    expect_identical(suppressMessages(apply_decisions(out, path))$link,
                     out$sn >= 0.99)
})


# --- scope, marking and bulk judgement -------------------------------------

test_that("the table carries a scope column the threshold drives", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "all", filter = "all",
                          mode = "flag", group_scope = TRUE)
        session$elapse(400)
        d <- table_data()
        expect_true("_scope" %in% names(d))
        expect_true(all(d[["_scope"]] == "in scope"))
        # Moving the cut regroups, because the blocks are what it decided.
        session$setInputs(cut_1 = 2, cut_2 = 2)
        session$elapse(400)
        expect_true(all(table_data()[["_scope"]] == "out of scope"))
    })
})

test_that("a judgement reaches the scope cell without regrouping", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    low <- out$pair_id[which.min(out$sn)]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 2, cut_2 = 2, rule = "all", filter = "all",
                          mode = "flag", group_scope = TRUE)
        session$elapse(400)
        expect_true(all(table_data()[["_scope"]] == "out of scope"))
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        # The grouping blocks are untouched: re-rendering the table on every
        # click would cost the reviewer their sort, filter and page position.
        d <- table_data()
        expect_identical(d[["_scope"]][match(low, d$pair_id)], "out of scope")
        # The judgement is in the live state the scope cell reads instead.
        expect_identical(rv_touched()$decisions[[low]], "include")
    })
    # And the renderer really does read it from there, saying whether the
    # judgement confirmed the cut or overruled it.
    js <- as.character(fast.string:::.inspect_js_scope())
    expect_true(grepl("state.meta.decisions", js, fixed = TRUE))
    expect_true(grepl("forced in", js, fixed = TRUE))
    expect_true(grepl("forced out", js, fixed = TRUE))
    expect_true(grepl("confirmed", js, fixed = TRUE))
    expect_true(grepl("_threshold", js, fixed = TRUE))
})

# --- a judgement overrides the threshold -----------------------------------

test_that("a judgement overrides the cut in both directions", {
    scope <- fast.string:::.inspect_scope
    ids <- c("a", "b", "c", "d")
    pass <- c(TRUE, TRUE, FALSE, FALSE)
    # Nothing judged leaves the cut alone.
    expect_identical(scope(pass, ids, list()), pass)
    # Included pulls a failing pair in; excluded pushes a passing one out.
    expect_identical(
        scope(pass, ids, list(c = "include", a = "exclude")),
        c(FALSE, TRUE, TRUE, FALSE))
    # "undecided" is the absence of a verdict, not a verdict of its own.
    expect_identical(scope(pass, ids, list(a = "undecided")), pass)
    # A decision about a pair that is not here cannot move anything.
    expect_identical(scope(pass, ids, list(zz = "include")), pass)
})

test_that("the override counts separate the cut from the judgement", {
    counts <- fast.string:::.inspect_scope_counts(
        pass  = c(TRUE, TRUE, FALSE, FALSE),
        scope = c(FALSE, TRUE, TRUE, FALSE))
    expect_identical(counts$n_threshold, 2L)
    expect_identical(counts$n_scope, 2L)
    expect_identical(counts$n_forced_in, 1L)
    expect_identical(counts$n_forced_out, 1L)
})

test_that("an override moves the counts the app reports, not only the cell", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    low <- out$pair_id[which.min(out$sn)]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        # A cut nothing clears, so every pair starts out of scope.
        session$setInputs(cut_1 = 2, cut_2 = 2, rule = "all", filter = "all",
                          mode = "flag", group_scope = TRUE)
        session$elapse(400)
        expect_identical(effect()$n_pairs, 0L)
        expect_identical(overrides()$n_forced_in, 0L)
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        # The header, the effect panel and the override tally all move: the
        # scope the app reports is the scope apply_decisions() will produce.
        expect_identical(effect()$n_pairs, 1L)
        expect_identical(overrides()$n_threshold, 0L)
        expect_identical(overrides()$n_forced_in, 1L)
        expect_true(scope_all()[match(low, out$pair_id)])
        # And the other way: a cut everything clears, minus one excluded.
        session$setInputs(cut_1 = 0, cut_2 = 0)
        session$elapse(400)
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "exclude", nonce = 2))
        expect_identical(overrides()$n_forced_out, 1L)
        expect_identical(effect()$n_pairs, nrow(out) - 1L)
    })
})

test_that("the app's scope is the scope apply_decisions() produces", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    low <- out$pair_id[which.min(out$sn)]
    high <- out$pair_id[which.max(out$sn)]
    app <- rLink_inspect(out, decisions = path, reviewer = "a",
                         launch = FALSE)
    # testServer() does not hand back the value of its block, so the scope
    # the app reported is carried out in a variable.
    seen <- NULL
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0, rule = "all",
                          filter = "all", mode = "flag", group_scope = TRUE)
        session$elapse(400)
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        session$setInputs(fs_decision = list(pair_id = high,
                                             decision = "exclude", nonce = 2))
        session$setInputs(note = "cut recorded with the overrides in place")
        session$setInputs(accept = 1)
        seen <<- scope_all()
    })
    expect_length(seen, nrow(out))
    applied <- suppressMessages(apply_decisions(out, path))
    expect_identical(applied$link, seen)
    # And the reasons name which rule decided each one.
    expect_identical(applied$link_reason[match(low, out$pair_id)], "included")
    expect_identical(applied$link_reason[match(high, out$pair_id)], "excluded")
})

test_that("a recorded threshold snapshots the cut, not the review so far", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    low <- out$pair_id[which.min(out$sn)]
    app <- rLink_inspect(out, decisions = path, reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        # A cut nothing clears, then one pair forced in by hand.
        session$setInputs(cut_1 = 2, cut_2 = 2, rule = "all", filter = "all",
                          mode = "flag", group_scope = TRUE)
        session$elapse(400)
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        expect_identical(effect()$n_pairs, 1L)      # what is in scope
        expect_identical(cut_effect()$n_pairs, 0L)  # what the cut did
        session$setInputs(note = "nothing clears this")
        session$setInputs(accept = 1)
    })
    # The event is a record of the threshold, so its counts describe the
    # threshold. Storing the reviewed total would make the number depend on
    # how far through the review the button was pressed.
    th <- read_decisions(path)$thresholds
    accepted <- th[th$status == "accepted", , drop = FALSE]
    expect_identical(accepted$n_pairs[nrow(accepted)], 0L)
})

test_that("hiding out-of-scope rows keeps a pair included by hand", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    low <- out$pair_id[which.min(out$sn)]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 2, cut_2 = 2, rule = "all",
                          filter = "all", mode = "filter",
                          group_scope = TRUE)
        session$elapse(400)
        expect_identical(nrow(table_data()), 0L)
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        # Judging a pair a link and watching it vanish is how a reviewer
        # stops believing the tool recorded what they said.
        session$setInputs(mode = "flag")
        session$setInputs(mode = "filter")
        d <- table_data()
        expect_identical(d$pair_id, low)
    })
})

test_that("showing the overrides lists just the contested pairs", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    low <- out$pair_id[which.min(out$sn)]
    high <- out$pair_id[which.max(out$sn)]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0, rule = "all",
                          filter = "all", mode = "flag", group_scope = TRUE)
        session$elapse(400)
        # `low` fails the cut and is forced in; `high` passes it and is
        # confirmed, not overridden, so only `low` should be listed.
        session$setInputs(fs_decision = list(pair_id = low,
                                             decision = "include", nonce = 1))
        session$setInputs(fs_decision = list(pair_id = high,
                                             decision = "include", nonce = 2))
        session$setInputs(filter = "override")
        expect_identical(table_data()$pair_id, low)
        session$setInputs(filter = "in_scope")
        expect_true(low %in% table_data()$pair_id)
        session$setInputs(filter = "out_scope")
        expect_false(low %in% table_data()$pair_id)
    })
})


# --- the pair under review -------------------------------------------------

test_that("clicking a row puts that pair in the sidebar", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    id <- out$pair_id[1L]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0.9, rule = "all",
                          filter = "all", mode = "flag", group_scope = TRUE)
        session$elapse(400)
        expect_null(rv_focus())
        session$setInputs(fs_focus = list(pair_id = id, nonce = 1))
        expect_identical(rv_focus(), id)
        # An identifier that is not one of these pairs is ignored rather than
        # left to fail inside the renderer.
        session$setInputs(fs_focus = list(pair_id = "nope", nonce = 2))
        expect_identical(rv_focus(), id)
        # Judging from the sidebar is the same decision as judging from the
        # row, so the event log cannot tell them apart.
        session$setInputs(focus_exclude = 1)
        expect_identical(rv_decisions()[[id]], "exclude")
    })
})

test_that("a missing value and an empty one read differently", {
    shown <- fast.string:::.inspect_shown
    expect_identical(shown(NA_character_), fast.string:::.inspect_na_label)
    expect_identical(shown(""), "(empty)")
    expect_identical(shown("SMITH"), "SMITH")
})

test_that("y, n and u judge from the keyboard", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    ids <- out$pair_id[1:2]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0.9, rule = "all",
                          filter = "all", mode = "flag", group_scope = TRUE)
        session$elapse(400)
        session$setInputs(fs_keyjudge = list(pair_ids = as.list(ids),
                                             decision = "include",
                                             nonce = 1))
        expect_identical(rv_decisions()[[ids[1L]]], "include")
        expect_identical(rv_decisions()[[ids[2L]]], "include")
        # One event each, same as a click.
        expect_identical(length(rv_events()), 2L)
        # Anything that is not one of the three verdicts is ignored.
        session$setInputs(fs_keyjudge = list(pair_ids = as.list(ids),
                                             decision = "delete",
                                             nonce = 2))
        expect_identical(length(rv_events()), 2L)
    })
    js <- fast.string:::.inspect_js_bindings()
    expect_true(grepl("fs_keyjudge", js, fixed = TRUE))
    # Typing 'n' in a comment box must stay typing.
    expect_true(grepl("TEXTAREA", js, fixed = TRUE))
})


# --- a second reviewer's pass ----------------------------------------------

test_that("reopening does not pre-fill another reviewer's verdicts", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_decisions(out)
    id <- out$pair_id[1L]
    write_events(path, list(event(
        "analyst", id, "include", "same person", 1L,
        "2024-01-01T00:00:00.000Z")))
    # The analyst reopening their own file finds their own work waiting.
    mine <- fast.string:::.inspect_seed(spec_for(out, file = path,
                                                reviewer = "analyst"))
    expect_identical(mine$decisions[[id]], "include")
    expect_identical(mine$comments[[id]], "same person")
    expect_identical(nrow(mine$others), 0L)
    # A QA reviewer does not: a check that opens pre-answered is not a check.
    theirs <- fast.string:::.inspect_seed(spec_for(out, file = path,
                                                  reviewer = "qa"))
    expect_identical(length(theirs$decisions), 0L)
    # It is shown to them instead, attributed.
    expect_identical(theirs$others$reviewer, "analyst")
    expect_identical(theirs$others$pair_id, id)
    expect_identical(theirs$others$decision, "include")
})


test_that("ctrl and shift marking is keyed on the pair, not the row", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    # reactable's own `selected` indices mean position in the data, which stops
    # matching the screen as soon as anything is sorted, filtered or paged. The
    # bindings send pair ids instead, so this is the contract worth pinning.
    js <- fast.string:::.inspect_js_bindings()
    for (token in c("data-fs-id", "shiftKey", "ctrlKey", "metaKey",
                    "fs_marked", "mousedown"))
        expect_true(grepl(token, js, fixed = TRUE))

    out <- scored()
    ids <- out$pair_id[1:3]
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(fs_marked = list(pair_ids = ids, nonce = 1))
        expect_identical(sort(rv_marked()), sort(ids))
        # A mark for something that is not one of these pairs is dropped.
        session$setInputs(fs_marked = list(pair_ids = c(ids, "bogus"),
                                           nonce = 2))
        expect_identical(sort(rv_marked()), sort(ids))
    })
})

test_that("a bulk judgement writes one event per marked pair", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    ids <- out$pair_id[1:3]
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(fs_marked = list(pair_ids = ids, nonce = 1))
        session$setInputs(mark_include = 1)
        expect_length(rv_events(), 3L)
        expect_true(all(vapply(ids, function(i)
            identical(rv_decisions()[[i]], "include"), logical(1L))))
        # The marks are cleared, so the next bulk action cannot repeat them.
        expect_length(rv_marked(), 0L)

        session$setInputs(fs_marked = list(pair_ids = out$pair_id[4],
                                           nonce = 2))
        session$setInputs(mark_exclude = 1)
        expect_identical(rv_decisions()[[out$pair_id[4]]], "exclude")
        expect_length(rv_events(), 4L)

        # Clearing marks judges nothing.
        session$setInputs(fs_marked = list(pair_ids = ids, nonce = 3))
        session$setInputs(mark_clear = 1)
        expect_length(rv_marked(), 0L)
        expect_length(rv_events(), 4L)
        # Autosave is throttled, so ask for the write explicitly.
        session$setInputs(save = 1)
    })
    expect_identical(nrow(read_decisions(path)$events), 4L)
})


# --- finishing -------------------------------------------------------------

test_that("finishing saves, and the decisions carry the code to replay them", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(fs_decision = list(pair_id = out$pair_id[1L],
                                             decision = "exclude", nonce = 1))
        session$setInputs(exit = 1)
    })
    handed <- read_decisions(path, x = out)
    expect_s3_class(handed, "fs_decisions")
    expect_identical(nrow(handed$events), 1L)
    # The object knows how to be replayed, which is what makes a review
    # reproducible rather than a note in a drawer.
    code <- paste(handed$code, collapse = "\n")
    expect_true(grepl("rLink_inspect(", code, fixed = TRUE))
    expect_true(grepl("apply_decisions(", code, fixed = TRUE))
    expect_true(grepl(gsub("\\\\", "/", path), code, fixed = TRUE))
    # And it checks itself against the pairs it was made on.
    expect_true(handed$status$pair_set_match)
})


# --- the shipped example file ----------------------------------------------

test_that("the example decisions file matches the current format", {
    skip_if_not_installed("jsonlite")
    path <- system.file("extdata", "example_decisions.json",
                        package = "fast.string")
    expect_true(nzchar(path))
    pairs <- data.frame(pair_id = c("A|X", "B|Y", "C|Z", "D|W"),
                        index_1 = 1:4, index_2 = 1:4,
                        score = c(0.97, 0.74, 0.44, 0.31))
    d <- read_decisions(path, x = pairs)
    # The whole point of shipping it is that the examples on read_decisions()
    # and apply_decisions() run against it, so it has to stay in step with the
    # format and with the fingerprinting -- a change to either would otherwise
    # leave the documented output quietly wrong.
    expect_identical(d$format_version, fast.string:::.review_format)
    expect_true(d$status$pair_set_match)
    expect_true(d$status$scores_match)
    expect_true(d$status$columns_match)
    # And that it still shows every part of the format worth showing.
    expect_identical(nrow(d$runs), 2L)
    expect_identical(sort(unique(d$events$reviewer)), c("analyst", "qa"))
    expect_identical(nrow(d$events), 3L)
    expect_identical(nrow(d$disagreements), 1L)
    expect_identical(d$disagreements$pair_id, "C|Z")
    expect_identical(nrow(d$cuts), 1L)
    expect_identical(d$cuts$operator, ">=")
    expect_true(all(nzchar(d$events$comment)))
})


# --- the pair card ---------------------------------------------------------

# Pairs with a column on each side that was never compared, one that only the
# first input has and one that only the second has.
carded <- function() {
    a <- enrol()
    b <- claims()
    a$sex <- c("M", "F", "M", "M")
    b$sex <- c("M", "F", "F")
    a$street <- c("1 High St", "2 Park Rd", "3 Mill Ln", "4 Hill Ct")
    b$ward <- c("A", "B", "C")
    rLink(a, b, block_1 = list(fsubstr(surname, 1, 1)),
          compare = list(sn = jaro_winkler(surname), gn = jaro_winkler(given),
                         lev = levenshtein(surname)))
}

test_that("the card lists every field, compared ones first", {
    out <- carded()
    spec <- spec_for(out)
    fields <- spec$card_fields
    label <- vapply(fields, `[[`, "", "field")
    kind <- vapply(fields, `[[`, "", "kind")
    # Two scores of one column are one field, not two.
    expect_identical(label[kind == "compared"], c("surname", "given"))
    expect_identical(fields[[1L]]$scores, c("sn", "lev"))
    # Everything carried is there, however many inputs have it.
    expect_setequal(label[kind == "carried"],
                    c("frame_id", "sex", "street", "ward"))
    street <- fields[[match("street", label)]]
    expect_identical(street$col_1, "street_1")
    expect_true(is.na(street$col_2))
    ward <- fields[[match("ward", label)]]
    expect_true(is.na(ward$col_1))
    # The columns the table leads with are not fields of the records.
    expect_false(any(c("pair_id", "index_1", "pass", "block_key") %in% label))

    card <- fast.string:::.inspect_card(spec, 2L)
    expect_identical(card$pair_id, out$pair_id[2L])
    expect_identical(card$rows[[1L]]$value_1, out$surname_1[2L])
    expect_identical(card$rows[[1L]]$value_2, out$surname_2[2L])
    expect_equal(unname(unlist(card$rows[[1L]]$scores)),
                 c(out$sn[2L], out$lev[2L]))
    expect_identical(names(card$lead), c("pass", "block_key"))
    expect_null(card$bits)
})

test_that("what differs between two values is marked, and nothing else", {
    diff <- fast.string:::.inspect_diff
    d <- diff("MACDONALD", "MCDONALD")
    expect_identical(d$a, "M<mark class=\"fs-diff\">A</mark>CDONALD")
    expect_identical(d$b, "MCDONALD")
    # Rebuilt from the pieces, each side is still its own value.
    strip <- function(x) gsub("</?mark[^>]*>", "", x)
    for (pair in list(c("CHOWDHURY", "CHOUDHARI"), c("OBRIEN", "O BRIEN"),
                      c("SMITH", "SMYTHE"), c("A", "XYZ"))) {
        d <- diff(pair[1L], pair[2L])
        expect_identical(strip(d$a), pair[1L])
        expect_identical(strip(d$b), pair[2L])
        expect_true(grepl("fs-diff", paste(d$a, d$b)))
    }
    # Case is not a difference, as it is not one to rLink() either.
    expect_false(grepl("mark", diff("Smith", "SMITH")$a))
    expect_identical(diff("SMITH", "SMITH")$a, "SMITH")
    # Markup in the data is text, not markup.
    d <- diff("<b>A & B</b>", "<b>A & C</b>")
    expect_false(grepl("<b>", d$a, fixed = TRUE))
    expect_true(grepl("&lt;b&gt;A &amp; ", d$a, fixed = TRUE))
    # A missing or an empty value has nothing to align, and says which it is.
    expect_identical(diff(NA_character_, "X")$a, fast.string:::.inspect_na_label)
    expect_identical(diff("", "X")$a, "(empty)")
    # A long value is left alone.
    long <- strrep("A", 200)
    expect_false(grepl("mark", diff(long, paste0(long, "B"))$b))
})

test_that("the other candidates for the records of a pair are found", {
    rivals <- fast.string:::.inspect_rivals
    i <- c(1L, 1L, 2L, 3L, 3L)
    j <- c(1L, 2L, 2L, 1L, 4L)
    # Pair 1 is (1, 1): pair 2 shares its first record, pair 4 its second.
    got <- rivals(i, j, 1L)
    expect_identical(got$row, c(2L, 4L))
    expect_identical(got$shares, c(1L, 2L))
    expect_identical(nrow(rivals(i, j, 5L)), 1L)
    # Linking two files, row 1 of one is not row 1 of the other; deduplicating
    # one file, it is.
    expect_false(3L %in% rivals(i, j, 4L)$row)
    expect_true(all(c(1L, 2L, 5L) %in% rivals(i, j, 4L, dedupe = TRUE)$row))
    # rLink() says which it was.
    expect_true(attr(rLink(enrol(), block_1 = list(surname)), "dedupe"))
    expect_false(attr(scored(), "dedupe"))
    expect_true(spec_for(rLink(enrol(), block_1 = list(fsubstr(surname, 1, 1)),
                               compare = list(g = jaro_winkler(given))))$dedupe)
})


# --- the band, the audit sample and the sweep ------------------------------

test_that("the review band is the pairs scoring between its limits", {
    band <- fast.string:::.inspect_in_band
    v <- c(0.1, 0.5, 0.75, 0.9, NA)
    expect_identical(band(v, c(0.5, 0.9)), c(FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_identical(band(v, c(0.9, 0.5)), band(v, c(0.5, 0.9)))
    expect_false(any(band(v, NULL)))
    expect_false(any(band(v, c(NA, 0.9))))
    expect_error(spec_for(scored(), band = 0.5), "two different numbers")
    expect_error(spec_for(scored(), band = c(0.5, 0.5)), "two different")
    expect_identical(spec_for(scored(), band = c(0.9, 0.6))$band, c(0.6, 0.9))
})

test_that("the audit sample is fixed by the pairs, not by chance", {
    rows <- fast.string:::.inspect_audit_rows
    ids <- sprintf("E%03d|C%03d", 1:200, 200:1)
    order_of <- function(ids) order(fhash(ids, algo = "xxh64"),
                                    method = "radix")
    pass <- rep(c(TRUE, FALSE), 100)
    got <- rows(order_of(ids), pass, 20L)
    expect_length(got, 20L)
    expect_true(all(pass[got]))
    # The same pairs whatever order they arrive in.
    shuffle <- rev(seq_along(ids))
    again <- rows(order_of(ids[shuffle]), pass[shuffle], 20L)
    expect_setequal(ids[shuffle][again], ids[got])
    # A bigger sample contains the smaller one.
    expect_true(all(got %in% rows(order_of(ids), pass, 50L)))
    # Asked for more than there is, it is all of them.
    expect_length(rows(order_of(ids), pass, 500L), 100L)
    expect_length(rows(order_of(ids), pass, 0L), 0L)
})

test_that("the audit estimate is the share judged wrong, with its interval", {
    audit <- fast.string:::.inspect_audit
    none <- audit(integer(40))
    expect_identical(none$n_sample, 40L)
    expect_identical(none$n_judged, 0L)
    expect_true(is.na(none$rate))
    # 3 wrong of 50 judged; undecided ones are not counted either way.
    got <- audit(c(rep(1L, 47), rep(2L, 3), rep(3L, 5), rep(0L, 5)))
    expect_identical(got$n_judged, 50L)
    expect_identical(got$n_not_link, 3L)
    expect_equal(got$rate, 0.06)
    wilson <- stats::prop.test(3, 50, correct = FALSE)$conf.int
    expect_equal(c(got$lower, got$upper), as.numeric(wilson), tolerance = 1e-6)
    # None wrong is not proof of none: the interval does not collapse.
    clean <- audit(rep(1L, 40))
    expect_identical(clean$rate, 0)
    expect_gt(clean$upper, 0.05)
})

test_that("a sweep of many distinct scores is taken at a grid", {
    grid <- fast.string:::.inspect_sweep_grid
    # Few enough to draw: every distinct value, as before.
    expect_null(grid(c(0.1, 0.5, 0.5, 0.9, NA)))
    expect_null(grid(rep(seq(0, 1, length.out = 150), 20)))
    set.seed(5)
    v <- runif(50000)
    g <- grid(v)
    expect_lte(length(g), 200L)
    expect_gt(length(g), 150L)
    # At values the data actually holds, end to end.
    expect_true(all(g %in% v))
    expect_identical(range(g), range(v))
    # And the sweep at those cut-offs agrees with the full sweep there.
    pairs <- data.frame(index_1 = sample(5000, 50000, TRUE),
                        index_2 = sample(5000, 50000, TRUE))
    full <- threshold_sweep(pairs, v)
    some <- threshold_sweep(pairs, v, thresholds = sort(g, decreasing = TRUE))
    at <- match(some$threshold, full$threshold)
    expect_false(anyNA(at))
    expect_identical(some$n_one_to_one, full$n_one_to_one[at])
    expect_identical(some$n_pairs, full$n_pairs[at])
})

test_that("verdicts held as codes give the scope the named ones did", {
    pass <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
    ids <- letters[1:5]
    named <- list(b = "include", c = "exclude", e = "undecided")
    codes <- c(0L, 1L, 2L, 0L, 3L)
    want <- fast.string:::.inspect_scope(pass, ids, named)
    expect_identical(fast.string:::.inspect_scope_at(pass, codes), want)
    expect_identical(
        fast.string:::.inspect_scope_at(pass, codes, judged = c(2L, 3L, 5L)),
        want)
    expect_identical(fast.string:::.inspect_scope_at(pass, codes, integer()),
                     pass)
})


# --- the review flow -------------------------------------------------------

test_that("the JavaScript has the keys, and keeps a text box for typing", {
    js <- fast.string:::.inspect_js_bindings()
    for (token in c("fs_undo", "fs_reason", "fs_help", "fs_set_focus",
                    "fs_options", "MutationObserver", "arrowdown", "fs-focus",
                    "rt-next-button", "fs_card_comment", "live"))
        expect_true(grepl(token, js, fixed = TRUE), label = token)
    # No double quote, which would end the R string the script is held in.
    expect_false(grepl("\"", js, fixed = TRUE))
    css <- fast.string:::.inspect_css(11L)
    # The sidebar is outside the wrapper, so its rules are written for it too.
    expect_true(grepl(".fs-side .fs-note", css, fixed = TRUE))
    expect_true(grepl("#fs-inspect .fs-note", css, fixed = TRUE))
})

test_that("many pairs judged at once are one batch, and undo takes it back", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    ids <- out$pair_id
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a", launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(fs_decision = list(pair_id = ids[1L],
                                             decision = "include", nonce = 1))
        session$setInputs(fs_keyjudge = list(pair_ids = as.list(ids[1:3]),
                                             decision = "exclude", nonce = 2))
        expect_length(rv_events(), 4L)
        expect_identical(unlist(rv_decisions()[ids[1:3]], use.names = FALSE),
                         rep("exclude", 3L))

        # Undo restores what each pair held before, as new events: the first
        # was a link, the other two had never been judged.
        session$setInputs(fs_undo = list(nonce = 3))
        expect_identical(rv_decisions()[[ids[1L]]], "include")
        expect_identical(rv_decisions()[[ids[2L]]], "undecided")
        expect_identical(rv_decisions()[[ids[3L]]], "undecided")
        expect_length(rv_events(), 7L)
        # And again, back to nothing judged; then there is nothing to undo.
        session$setInputs(undo = 1)
        expect_identical(rv_decisions()[[ids[1L]]], "undecided")
        expect_length(rv_events(), 8L)
        session$setInputs(undo = 2)
        expect_length(rv_events(), 8L)
        session$setInputs(save = 1)
    })
    review <- read_decisions(path)
    # The log kept all of it; the verdicts are where undo left them.
    expect_identical(nrow(review$events), 8L)
    expect_true(all(review$decisions$decision == "undecided"))
    expect_false(any(suppressMessages(apply_decisions(out, path))$link_reason
                     %in% c("included", "excluded")))
})

test_that("a stock reason is filed as the comment", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    ids <- out$pair_id
    expect_error(spec_for(out, reasons = c("a", "a")), "nine different")
    expect_error(spec_for(out, reasons = as.character(1:10)), "nine different")
    app <- rLink_inspect(out, decisions = tempfile(), reviewer = "a",
                         launch = FALSE, reasons = c("married name", "twin"))
    shiny::testServer(app, {
        session$setInputs(fs_reason = list(pair_ids = as.list(ids[1:2]),
                                           index = 2, nonce = 1))
        expect_identical(rv_comments()[[ids[1L]]], "twin")
        expect_identical(rv_comments()[[ids[2L]]], "twin")
        # A reason is not a verdict.
        expect_length(rv_decisions(), 0L)
        # A key with no reason behind it does nothing.
        session$setInputs(fs_reason = list(pair_ids = as.list(ids[1L]),
                                           index = 7, nonce = 2))
        expect_identical(rv_comments()[[ids[1L]]], "twin")
        expect_length(rv_events(), 2L)
    })
})

test_that("a comment still being typed is filed without redrawing", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    id <- out$pair_id[1L]
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a", launch = FALSE)
    shiny::testServer(app, {
        tick <- rv_tick()
        session$setInputs(fs_comment = list(pair_id = id, comment = "che",
                                            live = TRUE, nonce = 1))
        expect_identical(rv_comments()[[id]], "che")
        expect_length(rv_events(), 1L)
        # Nothing the screen draws from was told.
        expect_identical(rv_tick(), tick)
        session$setInputs(fs_comment = list(pair_id = id, comment = "checked",
                                            live = FALSE, nonce = 2))
        expect_gt(rv_tick(), tick)
        session$setInputs(save = 1)
    })
    expect_identical(read_decisions(path)$decisions$comment, "checked")
})

test_that("the band counts the work left, and filters to it", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    inside <- out$sn >= 0.8 & out$sn <= 0.99
    expect_gt(sum(inside), 0L)
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a", launch = FALSE,
                         band = c(0.8, 0.99))
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.9, cut_2 = 0, rule = "all", mode = "flag",
                          filter = "band")
        session$elapse(400)
        expect_identical(in_band(), inside)
        expect_identical(band_counts()$n, sum(inside))
        expect_identical(band_counts()$n_left, sum(inside))
        expect_setequal(table_data()$pair_id, out$pair_id[inside])

        session$setInputs(fs_decision = list(
            pair_id = out$pair_id[which(inside)[1L]], decision = "include",
            nonce = 1))
        expect_identical(band_counts()$n_judged, 1L)
        expect_identical(band_counts()$n_left, sum(inside) - 1L)

        # Typed into the boxes, the band moves.
        session$setInputs(band_lo = 0, band_hi = 2)
        session$elapse(400)
        expect_identical(band_counts()$n, nrow(out))
        session$setInputs(note = "looked at the band", accept = 1)
    })
    th <- read_decisions(path)$thresholds
    expect_identical(th$band_score, "sn")
    expect_equal(c(th$band_lower, th$band_upper), c(0, 2))
    expect_true(is.na(th$audit_judged))
})

test_that("the queue can be drawn again around the cut, or as an audit", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, decisions = path, reviewer = "a", launch = FALSE,
                         max_pairs = 2)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "all", mode = "flag",
                          filter = "all")
        session$elapse(400)
        first <- rv_queue()
        expect_length(first, 2L)
        # A pair judged stays on screen wherever the cut goes.
        kept <- out$pair_id[first[1L]]
        session$setInputs(fs_decision = list(pair_id = kept,
                                             decision = "include", nonce = 1))
        session$setInputs(cut_1 = 2)
        session$elapse(400)
        session$setInputs(queue_mode = "cut", rebuild = 1)
        expect_length(rv_queue(), 2L)
        expect_true(first[1L] %in% rv_queue())
        expect_identical(rv_queue()[2L],
                         setdiff(review_order(out$sn, threshold = 2),
                                 first[1L])[1L])
        expect_setequal(table_data()$pair_id, out$pair_id[rv_queue()])

        # No estimate until a sample has been asked for.
        expect_null(audit())
        session$setInputs(cut_1 = 0)
        session$elapse(400)
        session$setInputs(queue_mode = "audit", audit_n = 2, rebuild = 2)
        sample <- rv_queue()
        expect_length(sample, 2L)
        expect_identical(audit()$n_sample, 2L)
        before <- audit()$n_not_link
        target <- out$pair_id[sample[2L]]
        session$setInputs(fs_decision = list(pair_id = target,
                                             decision = "exclude", nonce = 2))
        expect_identical(audit()$n_not_link, before + 1L)
        session$setInputs(note = "audited", accept = 1)
    })
    th <- read_decisions(path)$thresholds
    expect_identical(th$audit_sample, 2L)
    expect_gte(th$audit_not_link, 1L)
})

test_that("with weights, the cut is on the probability and the rest is evidence", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    set.seed(2)
    n <- 300
    a <- data.frame(frame_id = sprintf("E%03d", 1:n),
                    region = sample(20, n, TRUE),
                    surname = sample(c("SMITH", "JONES", "BROWN", "NGUYEN",
                                       "TAYLOR", "WILSON"), n, TRUE),
                    given = sample(c("JOHN", "MARY", "ROBERT", "ANNA",
                                     "DAVID", "LINDA"), n, TRUE),
                    stringsAsFactors = FALSE)
    b <- a[sample(n, 200), ]
    b$frame_id <- sprintf("C%03d", 1:200)
    b$surname[1:25] <- paste0(b$surname[1:25], "E")
    pairs <- rLink(a, b, block_1 = list(region),
                   compare = list(sn = jaro_winkler(surname),
                                  gn = jaro_winkler(given)))
    weighed <- suppressWarnings(rLink_weights(pairs))
    spec <- spec_for(weighed)
    expect_true(spec$modelled)
    expect_identical(spec$score_spec$score, "probability")
    expect_identical(spec$evidence_spec$score,
                     c("sn", "gn", "weight", "probability"))
    expect_equal(unname(spec$cuts), 0.5)
    # Asked for by name, the field scores are cut as before.
    expect_identical(spec_for(weighed, scores = c("sn", "gn"))$score_spec$score,
                     c("sn", "gn"))
    expect_false(spec_for(pairs)$modelled)

    # The card says what each field was worth, and it adds up to the weight.
    card <- fast.string:::.inspect_card(spec, 1L)
    expect_named(card$bits, c("sn", "gn"))
    expect_equal(sum(card$bits), weighed$weight[1L])

    app <- rLink_inspect(weighed, decisions = tempfile(), reviewer = "a",
                         launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0.5, rule = "all")
        session$elapse(400)
        expect_identical(pass_all(), weighed$probability >= 0.5)
        # The sweep can say how many of the kept pairs should be right.
        expect_true("est_precision" %in% names(sweep()))
        # A rule may still name the field scores.
        session$setInputs(rule = "expr", expr = "probability >= 0.5 & sn == 1")
        session$elapse(600)
        expect_identical(pass_all(),
                         weighed$probability >= 0.5 & weighed$sn == 1)
    })
})

test_that("saving remembers what it wrote, and still merges a second writer", {
    out <- scored()
    path <- new_decisions(out)
    spec <- spec_for(out, file = path, reviewer = "analyst")
    cache <- new.env()
    one <- list(event("analyst", out$pair_id[1L], "include", "x", 1L,
                      "2026-11-06T09:00:00.000Z", run_id = spec$run_id))
    fast.string:::.inspect_save(spec, one, list(), cache = cache)
    expect_false(is.null(cache$written))
    expect_identical(cache$stamp$size, file.info(path)$size)
    # Somebody else writes to the file in between.
    other <- fast.string:::.review_read_file(path)
    other$decisions <- c(other$decisions, list(
        event("qa", out$pair_id[2L], "exclude", "y", 1L,
              "2026-11-06T10:00:00.000Z")))
    fast.string:::.review_write(other, path)
    expect_false(identical(cache$stamp$size, file.info(path)$size))
    two <- c(one, list(event("analyst", out$pair_id[3L], "include", "z", 2L,
                             "2026-11-06T11:00:00.000Z",
                             run_id = spec$run_id)))
    fast.string:::.inspect_save(spec, two, list(), cache = cache)
    review <- read_decisions(path)
    expect_identical(nrow(review$events), 3L)
    expect_setequal(review$events$reviewer, c("analyst", "qa"))
    # With nobody else writing, what is remembered is what is on disk.
    expect_identical(
        length(cache$written$decisions),
        length(fast.string:::.review_read_file(path)$decisions))
    expect_identical(cache$stamp$size, file.info(path)$size)
})

test_that("a session is stamped with when it opened, not when it last saved", {
    out <- scored()
    path <- tempfile(fileext = ".json")
    spec <- spec_for(out, file = path, reviewer = "analyst")
    fast.string:::.inspect_save(spec, list(), list())
    first <- read_decisions(path)$runs$started
    Sys.sleep(0.05)
    fast.string:::.inspect_save(spec, list(), list())
    runs <- read_decisions(path)$runs
    expect_identical(runs$started, first)
    expect_identical(runs$started, spec$started)
    expect_gt(runs$ended, runs$started)
})
