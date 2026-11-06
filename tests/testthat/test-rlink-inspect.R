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

new_artifact <- function(x = scored(), ...) {
    path <- tempfile(fileext = ".json")
    rLink_inspect(x, artifact = path, reviewer = "analyst", launch = FALSE,
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
    expect_true(grepl("state.meta.cuts",
                      as.character(fast.string:::.inspect_js_rowclass()),
                      fixed = TRUE))
    js <- fast.string:::.inspect_js_bindings()
    expect_true(grepl("fs_decision", js, fixed = TRUE))
    expect_true(grepl("fs_comment", js, fixed = TRUE))
    # The nonce is what makes a repeated identical click fire at all.
    expect_true(grepl("nonce", js, fixed = TRUE))
})

test_that("every column gets a colDef, with the identity ones pinned", {
    skip_if_not_installed("reactable")
    spec <- fast.string:::.inspect_spec(
        scored(), artifact = tempfile(), reviewer = "a", scores = NULL,
        threshold = NULL, max_pairs = 100, page_size = 50, font_size = 11,
        order_by = c("review", "input", "score"), expression = "x")
    cols <- fast.string:::.inspect_columns(spec, names(spec$view))
    expect_true(all(vapply(cols, inherits, logical(1L), "colDef")))
    expect_true(all(c("_decision", "_comment") %in% names(cols)))
    expect_identical(cols$pair_id$sticky, "left")
    expect_false(cols[["_decision"]]$sortable)
    # Asking for fewer columns gets fewer, plus the two controls.
    few <- fast.string:::.inspect_columns(spec, c("pair_id", "sn"))
    expect_identical(sort(names(few)),
                     sort(c("pair_id", "sn", "_decision", "_comment")))
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

spec_for <- function(x, ...) {
    args <- list(x = x, artifact = tempfile(), reviewer = "a", scores = NULL,
                 threshold = NULL, max_pairs = 100, page_size = 50,
                 font_size = 11,
                 order_by = c("review", "input", "score"), expression = "x")
    do.call(fast.string:::.inspect_spec, utils::modifyList(args, list(...)))
}

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


# --- the artifact ----------------------------------------------------------

test_that("the artifact is written as soon as the app is built", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    skip_if_not_installed("jsonlite")
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(scored(), artifact = path, reviewer = "analyst",
                         launch = FALSE)
    expect_s3_class(app, "shiny.appobj")
    expect_true(file.exists(path))
    review <- rLink_review(path)
    expect_s3_class(review, "fs_review")
    expect_identical(nrow(review$runs), 1L)
    expect_identical(review$runs$reviewer, "analyst")
    expect_identical(review$runs$n_pairs, nrow(scored()))
    expect_identical(review$scores$score, c("sn", "gn"))
    expect_identical(nrow(review$decisions), 0L)
})

test_that("an unwritable path fails before any review is done", {
    skip_if_not_installed("shiny")
    expect_error(
        rLink_inspect(scored(), artifact = file.path(tempdir(), "nope", "a.json"),
                      launch = FALSE),
        "no directory")
    expect_error(rLink_inspect(scored(), artifact = ""), "single path")
})

test_that("a stated threshold is recorded as an accepted one, with its operator", {
    skip_if_not_installed("shiny")
    path <- new_artifact(threshold = c(sn = 0.8))
    review <- rLink_review(path)
    expect_identical(nrow(review$thresholds), 1L)
    expect_identical(review$thresholds$status, "accepted")
    expect_identical(review$cuts$score, "sn")
    expect_identical(review$cuts$operator, ">=")
    expect_equal(review$cuts$value, 0.8)
    # The consequence of the cut is snapshotted, so a reader need not re-run it.
    expect_false(is.na(review$thresholds$n_pairs))
})

test_that("full precision survives the round trip", {
    skip_if_not_installed("shiny")
    # jsonlite rounds to 4 significant digits by default, which would silently
    # round the very numbers the artifact exists to record.
    path <- new_artifact(threshold = c(sn = 0.881234567890123))
    expect_equal(rLink_review(path)$cuts$value, 0.881234567890123,
                 tolerance = 0)
})

test_that("a review reports the current verdict, the log, and the revisions", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "father and son", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("analyst", out$pair_id[1L], "include", "changed my mind", 2L,
              "2026-11-06T09:05:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "disagree", 1L,
              "2026-11-07T10:00:00.000Z"),
        event("qa", out$pair_id[2L], "include", "fine élève", 1L,
              "2026-11-07T10:01:00.000Z")))
    review <- rLink_review(path)
    expect_identical(nrow(review$events), 4L)
    expect_identical(nrow(review$decisions), 3L)
    mine <- review$decisions[review$decisions$reviewer == "analyst" &
                             review$decisions$pair_id == out$pair_id[1L], ]
    expect_identical(mine$decision, "include")
    expect_identical(mine$n_changes, 2L)
    expect_true(any(grepl("élève", review$events$comment)))
    expect_identical(nrow(rLink_review(path, reviewer = "qa")$decisions), 2L)
})

test_that("a review surfaces where two reviewers disagree", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "include", "same person", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "not convinced", 1L,
              "2026-11-07T10:00:00.000Z"),
        event("qa", out$pair_id[2L], "include", "agreed", 2L,
              "2026-11-07T10:01:00.000Z")))
    review <- rLink_review(path)
    expect_identical(nrow(review$disagreements), 1L)
    expect_identical(review$disagreements$pair_id, out$pair_id[1L])
    expect_identical(review$disagreements$reviewers, "analyst, qa")
    expect_identical(review$agreement$n_reviewers, 2L)
    expect_identical(review$agreement$n_disagreed, 1L)
    expect_output(print(review), "judged differently")
    expect_output(print(review), "Pair review")
})

test_that("an appended session never overwrites the one before it", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "mine", 1L,
              "2026-11-06T09:00:00.000Z")))
    # A second person opens the same file.
    rLink_inspect(out, artifact = path, reviewer = "qa", launch = FALSE)
    review <- rLink_review(path)
    expect_identical(nrow(review$runs), 2L)
    expect_identical(review$runs$reviewer, c("analyst", "qa"))
    # And the first person's decision is still there.
    expect_identical(nrow(review$events), 1L)
    expect_identical(review$events$reviewer, "analyst")
})

test_that("a changed input is reported rather than ignored", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out)
    expect_true(rLink_review(path, x = out)$status$pair_set_match)
    fewer <- rLink_review(path, x = out[-1L, ])$status
    expect_false(fewer$pair_set_match)
    expect_output(print(rLink_review(path, x = out[-1L, ])), "has changed")
    expect_output(print(rLink_review(path, x = out)), "match the ones")
})

test_that("a damaged or foreign artifact is refused by name", {
    skip_if_not_installed("jsonlite")
    path <- tempfile(fileext = ".json")
    writeLines("{ not json", path)
    expect_error(rLink_review(path), "not readable as JSON")
    writeLines('{"schema":"something/else","format_version":1}', path)
    expect_error(rLink_review(path), "not a fast.string review artifact")
    writeLines(sprintf('{"schema":"%s","format_version":99}',
                       fast.string:::.review_schema), path)
    expect_error(rLink_review(path), "reads up to")
    expect_error(rLink_review(tempfile()), "no review artifact")
    expect_error(rLink_review(1), "single path")
})

test_that("the previous version is kept beside a rewritten artifact", {
    skip_if_not_installed("shiny")
    path <- new_artifact()
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
    expect_identical(rLink_review(path)$events$comment, "café — yes")
})


# --- applying a review ----------------------------------------------------

test_that("rLink_apply() applies the recorded threshold with its operator", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out, threshold = c(sn = 0.9))
    links <- rLink_apply(out, path)
    expect_identical(as.vector(links), out$sn >= 0.9)
    expect_true(all(attr(links, "reason") %in%
                    c("threshold", "below threshold")))

    # A score left on its own scale is cut the other way, and applying it the
    # wrong way round would invert the whole linkage.
    raw <- rLink(enrol(), claims(), block_1 = list(fsubstr(surname, 1, 1)),
                 compare = list(d = levenshtein(surname)), normalise = FALSE)
    raw_path <- tempfile(fileext = ".json")
    rLink_inspect(raw, artifact = raw_path, reviewer = "a", launch = FALSE,
                  threshold = c(d = 2))
    expect_identical(rLink_review(raw_path)$cuts$operator, "<=")
    expect_identical(as.vector(rLink_apply(raw, raw_path)), raw$d <= 2)
})

test_that("rLink_apply() lets a per-pair verdict override the threshold", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out, threshold = c(sn = 0.9))
    write_events(path, list(
        # Included although it fails the cut, and excluded although it passes.
        event("a", out$pair_id[which.min(out$sn)], "include", "checked", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("a", out$pair_id[which.max(out$sn)], "exclude", "twins", 2L,
              "2026-11-06T09:01:00.000Z")))
    links <- rLink_apply(out, path)
    reason <- attr(links, "reason")
    expect_true(links[which.min(out$sn)])
    expect_identical(reason[which.min(out$sn)], "included")
    expect_false(links[which.max(out$sn)])
    expect_identical(reason[which.max(out$sn)], "excluded")
})

test_that("rLink_apply() resolves two reviewers by its rule", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out, threshold = c(sn = 0.9))
    write_events(path, list(
        event("analyst", out$pair_id[1L], "include", "same", 1L,
              "2026-11-06T09:00:00.000Z"),
        event("qa", out$pair_id[1L], "exclude", "not same", 1L,
              "2026-11-07T10:00:00.000Z")))
    expect_identical(attr(rLink_apply(out, path, rule = "last"),
                          "reason")[1L], "excluded")
    expect_identical(attr(rLink_apply(out, path, rule = "unanimous"),
                          "reason")[1L], "disputed")
    expect_identical(attr(rLink_apply(out, path, reviewer = "analyst"),
                          "reason")[1L], "included")
    # Under "unanimous" a disputed pair keeps whatever the threshold said.
    expect_identical(unname(rLink_apply(out, path, rule = "unanimous")[1L]),
                     out$sn[1L] >= 0.9)
})

test_that("rLink_apply() with no threshold links only what was included", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out)
    links <- rLink_apply(out, path)
    expect_false(any(links))
    expect_true(all(attr(links, "reason") == "no threshold"))
})

test_that("rLink_apply() refuses pairs it cannot cut", {
    skip_if_not_installed("shiny")
    out <- scored()
    path <- new_artifact(out, threshold = c(sn = 0.9))
    expect_error(rLink_apply(as.data.frame(out)[, c("pair_id", "gn")], path),
                 "which `x` does not have")
    expect_error(rLink_apply(as.data.frame(out)[, c("sn", "gn")], path),
                 "`pair_id` column")
    expect_error(rLink_apply(out, 1), "must be an rLink_review")
    expect_error(rLink_apply(list(), path), "must be a data frame")
})


# --- the server, headlessly ------------------------------------------------

test_that("a decision is recorded, kept, and saved", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- tempfile(fileext = ".json")
    app <- rLink_inspect(out, artifact = path, reviewer = "analyst",
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
    review <- rLink_review(path)
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
    app <- rLink_inspect(out, artifact = tempfile(), reviewer = "a",
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
    app <- rLink_inspect(out, artifact = path, reviewer = "a", launch = FALSE)
    shiny::testServer(app, {
        session$setInputs(cut_1 = 0, cut_2 = 0, rule = "all",
                          note = "everything passes here")
        session$elapse(300)
        session$setInputs(accept = 1)
        expect_length(rv_thresholds(), 1L)
    })
    review <- rLink_review(path)
    expect_identical(nrow(review$thresholds), 1L)
    expect_identical(review$thresholds$note, "everything passes here")
    expect_identical(review$thresholds$n_pairs, nrow(out))
    expect_identical(nrow(review$cuts), 2L)
})

test_that("reopening a review shows the earlier decisions", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("reactable")
    out <- scored()
    path <- new_artifact(out)
    write_events(path, list(
        event("analyst", out$pair_id[1L], "exclude", "earlier call", 1L,
              "2026-11-06T09:00:00.000Z")))
    app <- rLink_inspect(out, artifact = path, reviewer = "analyst",
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
    app <- rLink_inspect(out, artifact = tempfile(), reviewer = "a",
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
