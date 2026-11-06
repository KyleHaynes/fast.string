# The clerical review app. Everything that is a function of plain data lives in
# an .inspect_* helper so that it can be tested without a browser; the shiny
# pieces at the bottom only assemble those helpers.

.inspect_pkgs <- c("shiny", "reactable", "jsonlite")

# A blue single-hue sequential ramp. Deliberately not red-to-green: that
# excludes the ~8% of men with a colour vision deficiency, and it implies a
# verdict where there is only a score.
.inspect_ramp <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b")
.inspect_na_colour <- "#eeeeee"

# Dark always means "more like a match", so a column where low is the match has
# its ramp reversed. A missing score gets its own flat grey, because a missing
# date of birth is not a date of birth that disagrees and colouring them alike
# would produce wrong clerical decisions.
.inspect_colour <- function(value, min, max, direction = "higher") {
    out <- rep(.inspect_na_colour, length(value))
    ok <- !is.na(value)
    if (!any(ok)) return(out)
    span <- max - min
    at <- if (is.finite(span) && span > 0) (value[ok] - min) / span
          else rep(1, sum(ok))
    at <- pmin(1, pmax(0, at))
    if (identical(direction, "lower")) at <- 1 - at
    rgb <- grDevices::colorRamp(.inspect_ramp)(at)
    out[ok] <- grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L],
                              maxColorValue = 255)
    out
}

# White text once the fill is dark enough to need it.
.inspect_text_colour <- function(value, min, max, direction = "higher") {
    span <- max - min
    at <- ifelse(is.na(value), 0,
                 pmin(1, pmax(0, if (is.finite(span) && span > 0)
                     (value - min) / span else 1)))
    if (identical(direction, "lower")) at <- 1 - at
    ifelse(at > 0.6, "#ffffff", "#111111")
}

# Which columns hold scores, and which way each one is cut. rLink() records
# both, and the fallback for a table that lost the attribute is the same one
# summary.fs_pairs() uses.
.inspect_scores <- function(x, scores = NULL, compare = attr(x, "compare")) {
    nms <- names(x)
    if (!is.null(scores)) {
        if (!is.character(scores) || !length(scores) || anyNA(scores))
            stop("`scores` must be a character vector of column names.",
                 call. = FALSE)
        bad <- setdiff(scores, nms)
        if (length(bad))
            stop(sprintf("`scores` names columns that are not there: %s.",
                         paste0("`", bad, "`", collapse = ", ")),
                 call. = FALSE)
        wrong <- scores[!vapply(scores, function(s) {
            v <- x[[s]]
            is.numeric(v) || is.logical(v)
        }, logical(1L))]
        if (length(wrong))
            stop(sprintf("`scores` names columns that are not numeric: %s.",
                         paste0("`", wrong, "`", collapse = ", ")),
                 call. = FALSE)
        found <- scores
    } else {
        found <- .fs_score_columns(x, compare)
        if (!length(found))
            stop(paste0(
                "Cannot tell which columns hold scores. Give `scores`, or ",
                "pass an rLink() result with its `compare` attribute intact."),
                call. = FALSE)
    }
    rows <- lapply(found, function(s) {
        v <- as.double(x[[s]])
        ok <- v[!is.na(v)]
        at <- if (is.null(compare)) NA_integer_
              else match(s, as.character(compare$score))
        dir <- if (is.na(at) || is.null(compare$direction)) NA_character_
               else as.character(compare$direction)[at]
        data.frame(
            score = s,
            comparator = if (is.na(at)) NA_character_
                         else as.character(compare$comparator)[at],
            column_1 = if (is.na(at)) NA_character_
                       else as.character(compare$column_1)[at],
            column_2 = if (is.na(at)) NA_character_
                       else as.character(compare$column_2)[at],
            scale = if (is.na(at)) NA_character_
                    else as.character(compare$scale)[at],
            # Unknown direction is assumed higher-is-match, which is what
            # rLink() produces by default, and flagged so the app can say so.
            direction = if (identical(dir, "lower")) "lower" else "higher",
            direction_known = !is.na(dir),
            operator = if (identical(dir, "lower")) "<=" else ">=",
            observed_min = if (length(ok)) min(ok) else NA_real_,
            observed_max = if (length(ok)) max(ok) else NA_real_,
            n_missing = sum(is.na(v)),
            stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

# Where a slider should start: a 0-1 similarity has a conventional 0.85, and
# anything else starts in the middle of what was actually observed.
.inspect_default_cuts <- function(score_spec) {
    vapply(seq_len(nrow(score_spec)), function(k) {
        lo <- score_spec$observed_min[k]
        hi <- score_spec$observed_max[k]
        if (!is.finite(lo) || !is.finite(hi)) return(0)
        similarity <- identical(score_spec$direction[k], "higher") &&
            lo >= 0 && hi <= 1
        if (similarity) min(hi, max(lo, 0.85)) else (lo + hi) / 2
    }, numeric(1L))
}

# Which pairs pass every cut, or any of them. Computed over all the pairs, not
# only the ones on screen, so the counts the analyst reads describe the whole
# input.
.inspect_pass <- function(values, cuts, operators, rule = "all") {
    n <- if (length(values)) length(values[[1L]]) else 0L
    if (!length(values)) return(rep(TRUE, n))
    each <- lapply(seq_along(values), function(k) {
        v <- as.double(values[[k]])
        ok <- if (identical(operators[k], "<=")) v <= cuts[k] else v >= cuts[k]
        # A pair with no score passes no cut.
        ok & !is.na(v)
    })
    if (identical(rule, "any")) Reduce(`|`, each) else Reduce(`&`, each)
}

# The widget's `meta`: the score domains the gradient interpolates from, plus
# the maps the cell renderers read. It is a plain list, so it is testable, and
# pushing a new one with updateReactable() recolours the table without
# re-rendering it -- which is what preserves the reviewer's sort, filter and
# page position across every click.
.inspect_meta <- function(score_spec, cuts = NULL, rule = "all",
                          decisions = list(), comments = list()) {
    domains <- lapply(seq_len(nrow(score_spec)), function(k) list(
        min = if (is.finite(score_spec$observed_min[k]))
            score_spec$observed_min[k] else 0,
        max = if (is.finite(score_spec$observed_max[k]))
            score_spec$observed_max[k] else 1,
        direction = score_spec$direction[k],
        operator = score_spec$operator[k]))
    names(domains) <- score_spec$score
    cut_list <- if (is.null(cuts)) list() else as.list(cuts)
    list(domains = domains, cuts = cut_list, rule = rule,
         ramp = as.list(.inspect_ramp), na_colour = .inspect_na_colour,
         decisions = decisions, comments = comments)
}

.inspect_js_score <- function() reactable::JS("
function(rowInfo, column, state) {
  var d = state.meta.domains[column.id];
  if (!d) return null;
  var v = rowInfo.values[column.id];
  if (v === null || v === undefined || isNaN(v))
    return { background: state.meta.na_colour, color: '#777',
             fontStyle: 'italic' };
  var span = d.max - d.min;
  var t = span > 0 ? (v - d.min) / span : 1;
  t = Math.max(0, Math.min(1, t));
  if (d.direction === 'lower') t = 1 - t;
  var ramp = state.meta.ramp;
  var pos = t * (ramp.length - 1);
  var i = Math.min(ramp.length - 2, Math.floor(pos));
  var f = pos - i;
  function mix(a, b) {
    var pa = [parseInt(a.substr(1,2),16), parseInt(a.substr(3,2),16),
              parseInt(a.substr(5,2),16)];
    var pb = [parseInt(b.substr(1,2),16), parseInt(b.substr(3,2),16),
              parseInt(b.substr(5,2),16)];
    return 'rgb(' + Math.round(pa[0]+(pb[0]-pa[0])*f) + ',' +
                    Math.round(pa[1]+(pb[1]-pa[1])*f) + ',' +
                    Math.round(pa[2]+(pb[2]-pa[2])*f) + ')';
  }
  return { background: mix(ramp[i], ramp[i+1]),
           color: t > 0.6 ? '#fff' : '#111' };
}")

# A left border stripe for "fails the current cut". Pass and fail is a
# different question from how high the score is, so it gets a different visual
# channel rather than being folded into the gradient.
.inspect_js_rowclass <- function() reactable::JS("
function(rowInfo, state) {
  var cuts = state.meta.cuts, domains = state.meta.domains;
  var names = Object.keys(cuts);
  if (!names.length) return null;
  var any = false, all = true;
  for (var k = 0; k < names.length; k++) {
    var v = rowInfo.values[names[k]];
    var d = domains[names[k]] || { operator: '>=' };
    var ok = (v !== null && v !== undefined && !isNaN(v)) &&
             (d.operator === '<=' ? v <= cuts[names[k]] : v >= cuts[names[k]]);
    any = any || ok; all = all && ok;
  }
  var pass = state.meta.rule === 'any' ? any : all;
  return pass ? 'fs-pass' : 'fs-fail';
}")

.inspect_js_decision <- function() reactable::JS("
function(cellInfo, state) {
  var id = cellInfo.row['pair_id'];
  var d = state.meta.decisions[id] || cellInfo.row['_decision'] || 'undecided';
  function btn(v, label, title) {
    return '<button type=\"button\" class=\"fs-btn' +
      (d === v ? ' fs-on-' + v : '') + '\" data-fs-id=\"' + id +
      '\" data-fs-value=\"' + v + '\" title=\"' + title + '\">' +
      label + '</button>';
  }
  return '<span class=\"fs-btns\">' +
    btn('include', 'Y', 'A link') +
    btn('exclude', 'N', 'Not a link') +
    btn('undecided', '?', 'Undecided') + '</span>';
}")

.inspect_js_comment <- function() reactable::JS("
function(cellInfo, state) {
  var id = cellInfo.row['pair_id'];
  var txt = state.meta.comments[id];
  if (txt === undefined || txt === null) txt = cellInfo.row['_comment'] || '';
  var esc = String(txt).replace(/&/g, '&amp;').replace(/\"/g, '&quot;')
                       .replace(/</g, '&lt;');
  return '<input type=\"text\" class=\"fs-comment\" data-fs-id=\"' + id +
         '\" value=\"' + esc + '\" placeholder=\"why?\">';
}")

# One delegated listener on the whole table, added once. Delegation is what
# makes the controls survive paging, sorting, filtering and searching: it never
# binds to an individual row, so it does not matter which rows React has
# currently drawn.
#
# The nonce matters. Setting an input to a value it already holds does not fire
# in Shiny, so without it a reviewer who clicks "exclude", changes their mind,
# and clicks "exclude" again would silently fail the second time.
#
# Comments fire on a debounce as well as on change, because a comment typed and
# then abandoned by closing the tab would otherwise be lost -- which is data
# loss in a tool whose whole purpose is to record reasoning.
.inspect_js_bindings <- function() "
(function() {
  var timers = {};
  function send(name, payload) {
    payload.nonce = Math.random();
    Shiny.setInputValue(name, payload, { priority: 'event' });
  }
  document.addEventListener('click', function(e) {
    var b = e.target.closest ? e.target.closest('button.fs-btn') : null;
    if (!b) return;
    send('fs_decision', { pair_id: b.getAttribute('data-fs-id'),
                          decision: b.getAttribute('data-fs-value') });
  });
  function comment(el) {
    send('fs_comment', { pair_id: el.getAttribute('data-fs-id'),
                         comment: el.value });
  }
  document.addEventListener('input', function(e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains('fs-comment')) return;
    var id = el.getAttribute('data-fs-id');
    clearTimeout(timers[id]);
    timers[id] = setTimeout(function() { comment(el); }, 600);
  });
  document.addEventListener('change', function(e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains('fs-comment')) return;
    clearTimeout(el.getAttribute('data-fs-id'));
    comment(el);
  });
})();
"

# Scoped to #fs-inspect, so embedding the app in a larger page cannot be
# restyled by it. The table's own font comes from a CSS custom property, so
# changing the size re-renders one tiny style tag instead of the table.
.inspect_css <- function(font_size) sprintf("
#fs-inspect { --fs-font: %dpx; }
#fs-inspect, #fs-inspect .form-control, #fs-inspect .control-label,
#fs-inspect .shiny-input-container, #fs-inspect .btn,
#fs-inspect .irs, #fs-inspect .selectize-input {
  font-size: var(--fs-font);
}
#fs-inspect h4 { font-size: calc(var(--fs-font) + 4px); margin: 0 0 6px 0; }
#fs-inspect .shiny-input-container { margin-bottom: 4px; }
#fs-inspect .fs-head { display: flex; gap: 14px; flex-wrap: wrap;
  align-items: baseline; margin-bottom: 6px; }
#fs-inspect .fs-kpi { font-weight: 600; }
#fs-inspect .fs-note { background: #fff8e1; border-left: 3px solid #f0ad4e;
  padding: 4px 8px; margin: 4px 0; }
#fs-inspect .fs-ok { background: #e8f5e9; border-left: 3px solid #4caf50;
  padding: 4px 8px; margin: 4px 0; }
#fs-inspect .fs-path { font-family: monospace; color: #555; }
#fs-inspect .fs-btns { display: inline-flex; gap: 2px; }
#fs-inspect button.fs-btn { font-size: calc(var(--fs-font) - 1px);
  line-height: 1; padding: 1px 4px; border: 1px solid #bbb;
  background: #fafafa; cursor: pointer; border-radius: 2px; }
#fs-inspect button.fs-btn.fs-on-include { background: #2171b5; color: #fff;
  border-color: #2171b5; }
#fs-inspect button.fs-btn.fs-on-exclude { background: #b52131; color: #fff;
  border-color: #b52131; }
#fs-inspect button.fs-btn.fs-on-undecided { background: #777; color: #fff;
  border-color: #777; }
#fs-inspect input.fs-comment { width: 100%%; font-size: var(--fs-font);
  border: 1px solid #ddd; padding: 0 3px; }
#fs-inspect .fs-fail td { opacity: 0.55; }
#fs-inspect .fs-pass td { box-shadow: inset 3px 0 0 -1px #2171b5; }
#fs-inspect .fs-evidence { font-size: var(--fs-font); }
", as.integer(font_size))

# One colDef per column: scores gradient-filled, the identity columns pinned
# left so a row never loses its name when scrolled sideways, and the two
# control columns unsortable.
.inspect_columns <- function(spec, selected) {
    scores <- spec$score_spec$score
    nms <- names(spec$view)
    cols <- list()
    for (nm in intersect(selected, nms)) {
        if (nm %in% scores) {
            k <- match(nm, scores)
            cols[[nm]] <- reactable::colDef(
                name = nm, style = .inspect_js_score(),
                na = "–", align = "right", minWidth = 62,
                header = sprintf("%s %s", nm, spec$score_spec$operator[k]),
                format = reactable::colFormat(digits = 3))
        } else if (nm %in% c("pair_id", spec$index_cols)) {
            cols[[nm]] <- reactable::colDef(sticky = "left", minWidth = 70)
        } else {
            cols[[nm]] <- reactable::colDef(minWidth = 70)
        }
    }
    cols[["_decision"]] <- reactable::colDef(
        name = "link?", cell = .inspect_js_decision(), html = TRUE,
        sortable = FALSE, filterable = FALSE, width = 78, sticky = "left")
    cols[["_comment"]] <- reactable::colDef(
        name = "comment", cell = .inspect_js_comment(), html = TRUE,
        sortable = FALSE, filterable = FALSE, minWidth = 170)
    cols
}

.inspect_theme <- function() reactable::reactableTheme(
    cellPadding = "1px 5px",
    style = list(fontSize = "var(--fs-font)"),
    headerStyle = list(fontSize = "var(--fs-font)", fontWeight = 600))

# The pair beside itself: one row per compared field, with the value each input
# held and the score it earned. attr(x, "compare") is what makes this possible,
# because it records which two columns each score came from.
.inspect_evidence <- function(spec, pair_id) {
    at <- match(pair_id, spec$all_ids)
    if (is.na(at))
        stop(sprintf("`%s` is not one of these pairs.", pair_id),
             call. = FALSE)
    sfx <- spec$suffixes
    rows <- lapply(seq_len(nrow(spec$score_spec)), function(k) {
        s <- spec$score_spec$score[k]
        c1 <- spec$score_spec$column_1[k]
        c2 <- spec$score_spec$column_2[k]
        value <- function(col, side) {
            nm <- if (is.na(col) || is.null(sfx)) NA_character_
                  else paste0(col, sfx[side])
            if (is.na(nm) || !nm %in% names(spec$source)) NA_character_
            else as.character(spec$source[[nm]][at])
        }
        data.frame(field = if (is.na(c1)) s else
                       if (identical(c1, c2)) c1 else paste0(c1, " | ", c2),
                   value_1 = value(c1, 1L), value_2 = value(c2, 2L),
                   score = s,
                   value = as.double(spec$all[[s]][at]),
                   stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}

# Everything validated and prepared before shinyApp() is built, so a bad input
# is a plain error at the console rather than a grey box in a browser.
.inspect_spec <- function(x, artifact, reviewer, scores, threshold, max_pairs,
                          page_size, font_size, order_by, expression) {
    if (!is.data.frame(x)) stop("`x` must be a data frame.")
    if (!nrow(x)) stop("`x` has no pairs to review.")
    nms <- names(x)
    at <- .pair_columns(nms)
    if (!is.character(at))
        stop(paste0(
            "Cannot tell which columns hold the pair indices. `x` needs ",
            "`index_1`/`index_2`, as rLink() returns, or ",
            "`x_index`/`y_index`."), call. = FALSE)
    max_pairs <- .one_finite_number(max_pairs, "max_pairs")
    if (max_pairs < 1) stop("`max_pairs` must be at least 1.")
    page_size <- .one_finite_number(page_size, "page_size")
    if (page_size < 5 || page_size > 1000)
        stop(paste0("`page_size` must be between 5 and 1000. reactable draws ",
                    "every row of a page, so a larger page locks the ",
                    "browser."), call. = FALSE)
    font_size <- .one_finite_number(font_size, "font_size")
    if (font_size < 6 || font_size > 24)
        stop("`font_size` must be between 6 and 24.")
    order_by <- match.arg(order_by, c("review", "input", "score"))

    if (is.null(reviewer)) reviewer <- .inspect_user()
    if (!is.character(reviewer) || length(reviewer) != 1L ||
        is.na(reviewer) || !nzchar(reviewer))
        stop("`reviewer` must be a single non-empty name.")

    score_spec <- .inspect_scores(x, scores)
    source <- as.data.frame(x)

    # pair_id is what every decision is filed against, so its quality decides
    # whether the artifact is worth anything.
    if ("pair_id" %in% nms) {
        ids <- as.character(x[["pair_id"]])
        kind <- if (is.integer(x[["pair_id"]]) &&
                    identical(x[["pair_id"]], seq_len(nrow(x))))
            "sequence" else "composite"
    } else {
        ids <- paste0(x[[at[1L]]], "|", x[[at[2L]]])
        kind <- "index"
    }
    if (anyNA(ids))
        stop(sprintf(paste0(
            "%s of the `pair_id` values are missing, so those pairs cannot be ",
            "reviewed. Give rLink() a `pair_id` column that identifies every ",
            "record."), format(sum(is.na(ids)), big.mark = ",")),
            call. = FALSE)
    dup <- duplicated(ids)
    if (any(dup))
        stop(sprintf(paste0(
            "%s `pair_id` value%s repeated, so a decision could not say which ",
            "pair it was about: %s. Give rLink() a `pair_id` column that ",
            "identifies every record uniquely."),
            format(sum(dup), big.mark = ","),
            if (sum(dup) == 1L) " is" else "s are",
            paste0("`", utils::head(unique(ids[dup]), 5L), "`",
                   collapse = ", ")), call. = FALSE)

    cuts <- .inspect_default_cuts(score_spec)
    names(cuts) <- score_spec$score
    if (!is.null(threshold)) {
        if (!is.numeric(threshold) || is.null(names(threshold)) ||
            anyNA(threshold))
            stop(paste0("`threshold` must be a named numeric vector, one cut ",
                        'per score, e.g. c(surname_jw = 0.88).'),
                 call. = FALSE)
        bad <- setdiff(names(threshold), score_spec$score)
        if (length(bad))
            stop(sprintf("`threshold` names scores that are not there: %s.",
                         paste0("`", bad, "`", collapse = ", ")),
                 call. = FALSE)
        cuts[names(threshold)] <- as.double(threshold)
    }

    # The analysis reads every pair; only the table is capped. Thresholds, the
    # sweep and the counts therefore describe the whole input, and the UI says
    # outright how much of it is on screen.
    all_values <- lapply(score_spec$score, function(s) as.double(x[[s]]))
    names(all_values) <- score_spec$score
    all <- as.data.frame(all_values)
    names(all) <- score_spec$score

    priority <- switch(
        order_by,
        review = review_order(all[[1L]], threshold = cuts[[1L]]),
        score = order(-all[[1L]]),
        input = seq_len(nrow(x)))
    n_shown <- min(nrow(x), as.integer(max_pairs))
    shown <- priority[seq_len(n_shown)]

    view <- source[shown, , drop = FALSE]
    rownames(view) <- NULL
    view$pair_id <- ids[shown]
    view <- view[, union("pair_id", names(view)), drop = FALSE]

    # Fingerprinted with the identifiers attached, so a table that built them
    # from its indices still has something to hash.
    with_ids <- source
    with_ids[["pair_id"]] <- ids

    list(all = all, all_ids = ids, source = source, view = view,
         shown = shown, score_spec = score_spec, index_cols = at,
         suffixes = sub("^index", "", at), cuts = cuts, reviewer = reviewer,
         artifact = artifact, n_pairs = nrow(x), n_shown = n_shown,
         truncated = n_shown < nrow(x), pair_id_source = kind,
         page_size = as.integer(page_size), font_size = as.integer(font_size),
         order_by = order_by, expression = expression,
         fingerprints = .review_fingerprints(with_ids, score_spec$score,
                                             "pair_id"),
         run_id = .review_run_id())
}

.inspect_user <- function() {
    who <- tryCatch(Sys.info()[["user"]], error = function(e) "")
    if (is.null(who) || is.na(who) || !nzchar(who)) who <- Sys.getenv("USERNAME")
    if (!nzchar(who)) who <- Sys.getenv("USER")
    if (!nzchar(who)) "unknown" else who
}

.inspect_host <- function()
    tryCatch(Sys.info()[["nodename"]], error = function(e) NA_character_)

# The run header, written as soon as the app is built. Eager, so an unwritable
# path fails now rather than after an hour of review, and so the file records
# that this data was opened even if nothing was decided.
.inspect_run <- function(spec) list(
    run_id = spec$run_id, reviewer = spec$reviewer,
    started = .review_now(), ended = NULL,
    n_pairs = spec$n_pairs, n_shown = spec$n_shown,
    truncated = spec$truncated, order_by = spec$order_by,
    pair_id_source = spec$pair_id_source, expression = spec$expression,
    package_version = as.character(utils::packageVersion("fast.string")),
    r_version = R.version.string, host = .inspect_host(),
    columns = as.list(names(spec$source)),
    fp_pair_set = spec$fingerprints$pair_set,
    fp_scores = spec$fingerprints$scores,
    fp_columns = spec$fingerprints$columns,
    scores = lapply(seq_len(nrow(spec$score_spec)), function(k)
        as.list(spec$score_spec[k, ])))

.inspect_threshold_event <- function(spec, cuts, rule, status, note, seq,
                                     effect = NULL) {
    list(event_id = paste0(spec$run_id, "-t", sprintf("%06d", seq)),
         seq = seq, run_id = spec$run_id, reviewer = spec$reviewer,
         at = .review_now(), status = status, rule = rule,
         cuts = lapply(seq_along(cuts), function(k) list(
             score = names(cuts)[k],
             operator = spec$score_spec$operator[
                 match(names(cuts)[k], spec$score_spec$score)],
             value = unname(cuts[k]))),
         n_pairs = if (is.null(effect)) NULL else effect$n_pairs,
         n_one_to_one = if (is.null(effect)) NULL else effect$n_one_to_one,
         n_conflict_1 = if (is.null(effect)) NULL else effect$n_conflict_1,
         n_conflict_2 = if (is.null(effect)) NULL else effect$n_conflict_2,
         note = note)
}

# What the current cut would actually do, over every pair.
.inspect_effect <- function(spec, cuts, rule) {
    pass <- .inspect_pass(spec$all, cuts, spec$score_spec$operator, rule)
    i <- spec$source[[spec$index_cols[1L]]][pass]
    j <- spec$source[[spec$index_cols[2L]]][pass]
    t1 <- if (length(i)) tabulate(i, max(i)) else integer()
    t2 <- if (length(j)) tabulate(j, max(j)) else integer()
    list(n_pairs = sum(pass),
         n_linked_1 = sum(t1 > 0L), n_linked_2 = sum(t2 > 0L),
         n_one_to_one = if (length(i)) sum(t1[i] == 1L & t2[j] == 1L) else 0L,
         n_conflict_1 = sum(t1 > 1L), n_conflict_2 = sum(t2 > 1L))
}


# --- the app ---------------------------------------------------------------

.inspect_ui <- function(spec) {
    scores <- spec$score_spec
    sliders <- lapply(seq_len(nrow(scores)), function(k) {
        lo <- scores$observed_min[k]
        hi <- scores$observed_max[k]
        if (!is.finite(lo) || !is.finite(hi)) { lo <- 0; hi <- 1 }
        # A cut the caller named may sit outside the range these pairs
        # happen to cover, and the slider still has to be able to show it.
        lo <- min(lo, spec$cuts[k], na.rm = TRUE)
        hi <- max(hi, spec$cuts[k], na.rm = TRUE)
        if (hi <= lo) hi <- lo + 1
        shiny::tagList(
            shiny::sliderInput(
                paste0("cut_", k),
                label = sprintf("%s %s", scores$score[k], scores$operator[k]),
                min = lo, max = hi, value = unname(spec$cuts[k]),
                step = signif((hi - lo) / 100, 2)),
            if (!scores$direction_known[k])
                shiny::tags$div(class = "fs-note", sprintf(paste0(
                    "The direction of `%s` was not detected; it is being read ",
                    "as higher-is-a-match."), scores$score[k])))
    })
    all_cols <- names(spec$view)
    shiny::fluidPage(shiny::tags$div(
        id = "fs-inspect",
        shiny::tags$style(shiny::HTML(.inspect_css(spec$font_size))),
        shiny::uiOutput("fs_font"),
        shiny::tags$script(shiny::HTML(.inspect_js_bindings())),
        shiny::h4("rLink pair review"),
        shiny::tags$div(
            class = "fs-head",
            shiny::tags$span(sprintf("Reviewer: %s", spec$reviewer)),
            shiny::tags$span(class = "fs-path", spec$artifact),
            shiny::uiOutput("fs_kpi", inline = TRUE)),
        if (spec$truncated) shiny::tags$div(class = "fs-note", sprintf(
            paste0("Showing the %s pairs most worth a look, of %s. Every ",
                   "count below is still computed over all %s."),
            format(spec$n_shown, big.mark = ","),
            format(spec$n_pairs, big.mark = ","),
            format(spec$n_pairs, big.mark = ","))),
        if (identical(spec$pair_id_source, "sequence"))
            shiny::tags$div(class = "fs-note", paste0(
                "`pair_id` is a row number, so these decisions will not find ",
                "their pairs again if the pairs are rebuilt. Give rLink() a ",
                "`pair_id` column for a review that survives a re-run.")),
        if (identical(spec$pair_id_source, "index"))
            shiny::tags$div(class = "fs-note", paste0(
                "`pair_id` was built from the row indices, so these ",
                "decisions hold only while both inputs keep their row order.")),
        shiny::uiOutput("fs_changed"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 3,
                shiny::tags$strong("Thresholds"), sliders,
                shiny::radioButtons("rule", "A pair is a link when",
                                    c("every score passes" = "all",
                                      "any score passes" = "any"),
                                    selected = "all"),
                shiny::radioButtons("mode", "Pairs failing the cut",
                                    c("shown, dimmed" = "flag",
                                      "hidden" = "filter"),
                                    selected = "flag"),
                shiny::textAreaInput("note", "Why this cut?", rows = 2),
                shiny::actionButton("accept", "Record this threshold"),
                shiny::tags$hr(),
                shiny::radioButtons("filter", "Show",
                                    c("all pairs" = "all",
                                      "undecided" = "undecided",
                                      "marked a link" = "include",
                                      "marked not a link" = "exclude"),
                                    selected = "all"),
                shiny::actionButton("rerank", "Re-rank by how undecided"),
                shiny::tags$hr(),
                shiny::selectizeInput(
                    "columns", "Columns, in order", choices = all_cols,
                    selected = all_cols, multiple = TRUE,
                    options = list(plugins = list("remove_button"))),
                shiny::fluidRow(
                    shiny::column(6, shiny::numericInput(
                        "page_size", "Rows/page", spec$page_size,
                        min = 5, max = 1000, step = 25)),
                    shiny::column(6, shiny::numericInput(
                        "font_size", "Font px", spec$font_size,
                        min = 6, max = 24, step = 1))),
                shiny::tags$hr(),
                shiny::actionButton("save", "Save now"),
                shiny::uiOutput("fs_saved")),
            shiny::mainPanel(
                width = 9,
                reactable::reactableOutput("tbl"),
                shiny::tags$hr(),
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        "Threshold sweep",
                        shiny::plotOutput("sweep_plot", height = "260px"),
                        shiny::tableOutput("effect")),
                    shiny::tabPanel(
                        "Score distributions",
                        shiny::plotOutput("hist_plot", height = "320px")),
                    shiny::tabPanel(
                        "This session",
                        shiny::tableOutput("events")))))))
}

.inspect_server <- function(spec) function(input, output, session) {
    n_scores <- nrow(spec$score_spec)
    operators <- spec$score_spec$operator
    score_names <- spec$score_spec$score
    index_1 <- spec$source[[spec$index_cols[1L]]]
    index_2 <- spec$source[[spec$index_cols[2L]]]

    seeded <- .inspect_seed(spec)
    rv_decisions <- shiny::reactiveVal(seeded$decisions)
    rv_comments <- shiny::reactiveVal(seeded$comments)
    rv_events <- shiny::reactiveVal(list())
    rv_thresholds <- shiny::reactiveVal(list())
    rv_seq <- shiny::reactiveVal(0L)
    rv_touched <- shiny::reactiveVal(list(decisions = list(),
                                          comments = list()))
    rv_order <- shiny::reactiveVal(seq_len(spec$n_shown))
    rv_saved <- shiny::reactiveVal(NULL)

    next_seq <- function() {
        n <- rv_seq() + 1L
        rv_seq(n)
        n
    }

    cuts <- shiny::debounce(shiny::reactive({
        out <- vapply(seq_len(n_scores), function(k) {
            v <- input[[paste0("cut_", k)]]
            if (is.null(v)) unname(spec$cuts[k]) else as.double(v)
        }, numeric(1L))
        names(out) <- score_names
        out
    }), 250)

    rule <- shiny::reactive(if (is.null(input$rule)) "all" else input$rule)

    pass_all <- shiny::reactive(
        .inspect_pass(spec$all, cuts(), operators, rule()))

    effect <- shiny::reactive(.inspect_effect(spec, cuts(), rule()))

    state_for <- function(ids) {
        held <- rv_decisions()
        notes <- rv_comments()
        list(decision = vapply(ids, function(id) {
                 v <- held[[id]]
                 if (is.null(v)) "undecided" else v
             }, "", USE.NAMES = FALSE),
             comment = vapply(ids, function(id) {
                 v <- notes[[id]]
                 if (is.null(v)) "" else v
             }, "", USE.NAMES = FALSE))
    }

    table_rows <- shiny::reactive({
        rows <- rv_order()
        # Taken only in "filter" mode, so in the default mode moving a slider
        # does not invalidate the table and throw away the reviewer's place.
        if (identical(input$mode, "filter"))
            rows <- rows[pass_all()[spec$shown][rows]]
        pick <- input$filter
        if (!is.null(pick) && !identical(pick, "all")) {
            held <- shiny::isolate(state_for(spec$view$pair_id[rows])$decision)
            rows <- rows[held == pick]
        }
        rows
    })

    table_data <- shiny::reactive({
        rows <- table_rows()
        chosen <- if (is.null(input$columns)) names(spec$view)
                  else input$columns
        keep <- intersect(unique(c("pair_id", chosen)), names(spec$view))
        out <- spec$view[rows, keep, drop = FALSE]
        rownames(out) <- NULL
        # Read with isolate(), never depended on: decisions reach the widget
        # through updateReactable(meta = ) instead, so a click never
        # re-renders the table and never loses the sort, filter or page.
        held <- shiny::isolate(state_for(out$pair_id))
        out[["_decision"]] <- held$decision
        out[["_comment"]] <- held$comment
        out
    })

    output$tbl <- reactable::renderReactable({
        data <- table_data()
        page <- if (is.null(input$page_size)) spec$page_size
                else max(5L, min(1000L, as.integer(input$page_size)))
        reactable::reactable(
            data,
            columns = .inspect_columns(
                list(view = data, score_spec = spec$score_spec,
                     index_cols = spec$index_cols), names(data)),
            meta = .inspect_meta(spec$score_spec, shiny::isolate(cuts()),
                                 shiny::isolate(rule()),
                                 shiny::isolate(rv_touched()$decisions),
                                 shiny::isolate(rv_touched()$comments)),
            rowClass = .inspect_js_rowclass(),
            sortable = TRUE, resizable = TRUE, filterable = TRUE,
            searchable = TRUE, compact = TRUE, highlight = TRUE,
            wrap = FALSE, bordered = TRUE, pagination = TRUE,
            defaultPageSize = page, showPageSizeOptions = TRUE,
            pageSizeOptions = c(25, 50, 100, 250, 500, 1000),
            theme = .inspect_theme(), class = "fs-table")
    })

    push_meta <- function() {
        touched <- rv_touched()
        reactable::updateReactable("tbl", meta = list(
            cuts = as.list(cuts()), rule = rule(),
            decisions = touched$decisions, comments = touched$comments),
            session = session)
    }

    shiny::observeEvent(cuts(), push_meta(), ignoreInit = TRUE)
    shiny::observeEvent(rule(), push_meta(), ignoreInit = TRUE)

    record <- function(pair_id, decision = NULL, comment = NULL) {
        at <- match(pair_id, spec$all_ids)
        if (is.na(at)) return(invisible(NULL))
        if (is.null(decision)) {
            held <- rv_decisions()[[pair_id]]
            decision <- if (is.null(held)) "undecided" else held
        } else {
            held <- rv_decisions()
            held[[pair_id]] <- decision
            rv_decisions(held)
        }
        if (!is.null(comment)) {
            notes <- rv_comments()
            notes[[pair_id]] <- comment
            rv_comments(notes)
        }
        note <- rv_comments()[[pair_id]]
        n <- next_seq()
        rv_events(c(rv_events(), list(list(
            event_id = paste0(spec$run_id, "-d", sprintf("%06d", n)),
            seq = n, run_id = spec$run_id, reviewer = spec$reviewer,
            at = .review_now(), pair_id = pair_id,
            index_1 = as.integer(index_1[at]),
            index_2 = as.integer(index_2[at]),
            decision = decision,
            comment = if (is.null(note)) NULL else note,
            scores_at_decision = .named(
                lapply(score_names, function(s) spec$all[[s]][at]),
                score_names)))))
        touched <- rv_touched()
        touched$decisions[[pair_id]] <- decision
        touched$comments[[pair_id]] <- if (is.null(note)) "" else note
        rv_touched(touched)
        reactable::updateReactable("tbl", meta = list(
            decisions = touched$decisions, comments = touched$comments),
            session = session)
        invisible(NULL)
    }

    shiny::observeEvent(input$fs_decision, {
        info <- input$fs_decision
        if (is.null(info$pair_id) || is.null(info$decision)) return()
        if (!info$decision %in% c("include", "exclude", "undecided")) return()
        record(as.character(info$pair_id),
               decision = as.character(info$decision))
    })

    shiny::observeEvent(input$fs_comment, {
        info <- input$fs_comment
        if (is.null(info$pair_id)) return()
        record(as.character(info$pair_id),
               comment = as.character(if (is.null(info$comment)) ""
                                      else info$comment))
    })

    shiny::observeEvent(input$accept, {
        rv_thresholds(c(rv_thresholds(), list(.inspect_threshold_event(
            spec, cuts(), rule(), "accepted",
            if (is.null(input$note)) "" else input$note, next_seq(),
            effect()))))
        save_now()
    })

    shiny::observeEvent(input$rerank, {
        rv_order(review_order(spec$all[[score_names[1L]]][spec$shown],
                              threshold = unname(cuts()[1L])))
    })

    output$fs_font <- shiny::renderUI({
        size <- if (is.null(input$font_size)) spec$font_size
                else max(6L, min(24L, as.integer(input$font_size)))
        shiny::tags$style(shiny::HTML(
            sprintf("#fs-inspect { --fs-font: %dpx; }", size)))
    })

    output$fs_kpi <- shiny::renderUI({
        held <- unlist(rv_decisions(), use.names = FALSE)
        if (is.null(held)) held <- character()
        e <- effect()
        shiny::tagList(
            shiny::tags$span(class = "fs-kpi", sprintf(
                "%s of %s pairs pass", format(e$n_pairs, big.mark = ","),
                format(spec$n_pairs, big.mark = ","))),
            shiny::tags$span(sprintf("%s clean 1:1",
                                     format(e$n_one_to_one, big.mark = ","))),
            shiny::tags$span(sprintf(
                "%s contested",
                format(e$n_conflict_1 + e$n_conflict_2, big.mark = ","))),
            shiny::tags$span(sprintf("marked: %d link, %d not",
                                     sum(held == "include"),
                                     sum(held == "exclude"))))
    })

    output$fs_changed <- shiny::renderUI({
        if (is.null(seeded$changed)) return(NULL)
        shiny::tags$div(class = "fs-note", seeded$changed)
    })

    output$fs_saved <- shiny::renderUI({
        msg <- rv_saved()
        if (is.null(msg)) return(NULL)
        shiny::tags$div(
            class = if (startsWith(msg, "Could not")) "fs-note" else "fs-ok",
            msg)
    })

    output$effect <- shiny::renderTable({
        e <- effect()
        data.frame(measure = names(e),
                   value = format(unlist(e), big.mark = ","),
                   stringsAsFactors = FALSE)
    })

    output$sweep_plot <- shiny::renderPlot({
        s <- threshold_sweep(
            data.frame(index_1 = index_1, index_2 = index_2),
            spec$all[[score_names[1L]]], operator = operators[1L])
        if (nrow(s) < 2L) {
            graphics::plot.new()
            graphics::text(0.5, 0.5, "Too few distinct scores to sweep.")
            return(invisible(NULL))
        }
        plot.fs_sweep(s, threshold = unname(cuts()[1L]))
    })

    output$hist_plot <- shiny::renderPlot({
        frame <- spec$all
        frame[[spec$index_cols[1L]]] <- index_1
        frame[[spec$index_cols[2L]]] <- index_2
        plot.fs_pairs(
            structure(frame, class = c("fs_pairs", "data.frame"),
                      compare = attr(spec$source, "compare")),
            score = score_names, threshold = unname(cuts()))
    })

    output$events <- shiny::renderTable({
        e <- rv_events()
        if (!length(e)) return(data.frame(message = "No decisions yet."))
        df <- .review_events_df(e)
        utils::tail(df[, c("at", "pair_id", "decision", "comment")], 20L)
    })

    save_now <- function() {
        msg <- tryCatch({
            .inspect_save(spec, rv_events(), rv_thresholds())
            sprintf("Saved %s", format(Sys.time(), "%H:%M:%S"))
        }, error = function(cnd)
            paste0("Could not save: ", conditionMessage(cnd)))
        rv_saved(msg)
        invisible(NULL)
    }

    autosave <- shiny::throttle(
        shiny::reactive(length(rv_events()) + length(rv_thresholds())), 2000)
    shiny::observeEvent(autosave(), save_now(), ignoreInit = TRUE)
    shiny::observeEvent(input$save, save_now())
    # The last clicks must not be lost by closing the tab.
    session$onSessionEnded(function() try(save_now(), silent = TRUE))
}

# Decisions already in the artifact, so reopening a review shows the earlier
# work instead of starting again, and a moved fingerprint is reported rather
# than ignored.
.inspect_seed <- function(spec) {
    empty <- list(decisions = list(), comments = list(), changed = NULL)
    if (!file.exists(spec$artifact)) return(empty)
    held <- tryCatch(rLink_review(spec$artifact), error = function(cnd) NULL)
    if (is.null(held)) return(empty)
    out <- empty
    mine <- held$decisions
    for (k in seq_len(nrow(mine))) {
        id <- mine$pair_id[k]
        out$decisions[[id]] <- mine$decision[k]
        if (!is.na(mine$comment[k])) out$comments[[id]] <- mine$comment[k]
    }
    if (nrow(held$runs)) {
        last <- held$runs[nrow(held$runs), ]
        moved <- !identical(last$fp_pair_set, spec$fingerprints$pair_set) ||
            !identical(last$fp_scores, spec$fingerprints$scores)
        if (moved) {
            seen <- mine$pair_id
            gone <- sum(!seen %in% spec$all_ids)
            fresh <- sum(!spec$all_ids %in% seen)
            out$changed <- sprintf(paste0(
                "The data has changed since this file was last written: %s ",
                "reviewed pair%s no longer here, and %s pair%s new and ",
                "undecided. Decisions are matched by `pair_id`, so the rest ",
                "still apply."),
                format(gone, big.mark = ","),
                if (gone == 1L) " is" else "s are",
                format(fresh, big.mark = ","),
                if (fresh == 1L) " is" else "s are")
        }
    }
    out
}

# Read, merge, write. Re-reading before every write means two reviewers sharing
# a file add to each other's work rather than overwriting it, with no lockfile
# to go stale on a network share.
.inspect_save <- function(spec, events, thresholds) {
    mine <- .review_new()
    run <- .inspect_run(spec)
    run$ended <- .review_now()
    mine$runs <- list(run)
    mine$thresholds <- thresholds
    mine$decisions <- events
    held <- if (file.exists(spec$artifact))
        tryCatch(.review_read_file(spec$artifact), error = function(cnd) NULL)
        else NULL
    .review_write(if (is.null(held)) mine else .review_merge(held, mine),
                  spec$artifact)
}


#' Review candidate pairs in a browser
#'
#' Opens an interactive table of candidate pairs for choosing a threshold and
#' accepting or rejecting individual pairs, and records every judgement --- with
#' a comment --- in a JSON artifact that someone else can later read with
#' [rLink_review()]. It is the step between scoring pairs and believing the
#' result, and the artifact is what turns an analyst's afternoon of judgement
#' calls into something a second person can check.
#'
#' The workflow it is built for:
#'
#' ```
#' pairs <- rLink(enrol, claims, block_1 = ..., compare = ...)
#' rLink_inspect(pairs)                      # choose a cut, mark exceptions
#' review <- rLink_review("rLink_review.json")  # the QA pass reads this
#' links <- rLink_apply(pairs, review)       # and this applies it
#' ```
#'
#' Score columns are filled with a colour gradient, dark meaning more like a
#' match --- reversed automatically for a score where low is the match, so a
#' distance left unnormalised still reads the right way round. One slider per
#' score sets the cut, and pairs failing it are dimmed rather than hidden,
#' because a false negative you cannot see is a false negative you will not
#' find. Every count in the header and in the sweep is computed over *all* the
#' pairs, not only the ones on screen.
#'
#' @section What is written:
#' The artifact is append-only. A second reviewer's session is added to it, not
#' written over it, so a disagreement between two reviewers is preserved and
#' visible --- which is the point of having the file at all. Decisions are
#' filed against `pair_id`, so they still find their pairs after `rLink()` is
#' re-run with different blocking, provided `pair_id` was built from record
#' identifiers rather than row numbers. See *Pair identifiers* in [rLink()].
#'
#' @section Limits:
#' At most `max_pairs` rows are put on screen; the rest of the analysis still
#' reads every pair, and the app says so. reactable draws every row of a page,
#' so `page_size` is capped at 1000 --- there is no "show all rows", because at
#' these sizes it would lock the browser tab rather than show you anything.
#' Columns can be reordered and hidden from the sidebar; dragging a table
#' *header* is not something reactable can do.
#'
#' @param x Candidate pairs: an [rLink()] result, or any data frame with
#'   `index_1`/`index_2` or `x_index`/`y_index` columns.
#' @param artifact Path to the JSON review file. Read and appended to when it
#'   exists, created when it does not. Written as soon as the app is built, so
#'   an unwritable path fails now rather than after an hour of review.
#' @param reviewer Who is reviewing. Defaults to the logged-in user, and is the
#'   name every decision is attributed to.
#' @param scores Score columns to threshold, or `NULL` to read them from
#'   `attr(x, "compare")`, falling back to the numeric columns that are neither
#'   indices nor carried values.
#' @param threshold Optional named numeric vector of starting cuts, one per
#'   score, as `c(surname_jw = 0.88)`. Recorded in the artifact as an accepted
#'   threshold, so a scripted cut-off is logged in the same format as a
#'   hand-chosen one.
#' @param max_pairs Most rows to put on screen, `20000` by default.
#' @param page_size Rows per page, between 5 and 1000.
#' @param font_size Base font in pixels, adjustable in the app.
#' @param order_by Initial row order: `"review"` puts the most undecided pairs
#'   first, `"score"` the highest-scoring, `"input"` leaves them as given.
#' @param launch Whether to run the app. Defaults to [interactive()], so the
#'   artifact is still prepared when this is called from a script or a test.
#' @return The `shiny.appobj`, invisibly, so it can be run later or inspected.
#' @seealso [rLink_review()] to read the artifact, [rLink_apply()] to apply it,
#'   [threshold_sweep()] for the same evidence at the console, and
#'   [review_order()] for the ordering the queue uses.
#' @family record linkage review functions
#' @examplesIf interactive()
#' enrol <- data.frame(
#'     frame_id = c("E1", "E2", "E3", "E4"),
#'     surname  = c("SMITH", "JONES", "BROWN", "SMYTH"),
#'     given    = c("JOHN", "MARY", "ROB", "JON")
#' )
#' claims <- data.frame(
#'     frame_id = c("C1", "C2", "C3"),
#'     surname  = c("SMITH", "JONSE", "BROWNE"),
#'     given    = c("JON", "MARY", "ROBERT")
#' )
#' pairs <- rLink(enrol, claims, block_1 = list(fsubstr(surname, 1, 1)),
#'                compare = list(sn = jaro_winkler(surname),
#'                               gn = jaro_winkler(given)))
#' rLink_inspect(pairs, artifact = file.path(tempdir(), "review.json"))
#' @export
rLink_inspect <- function(x, artifact = "rLink_review.json", reviewer = NULL,
                          scores = NULL, threshold = NULL, max_pairs = 20000L,
                          page_size = 100L, font_size = 11L,
                          order_by = c("review", "input", "score"),
                          launch = interactive()) {
    .review_require(.inspect_pkgs)
    .check_flag(launch, "launch")
    if (!is.character(artifact) || length(artifact) != 1L || is.na(artifact) ||
        !nzchar(artifact))
        stop("`artifact` must be a single path.")
    expression <- paste(deparse(substitute(x)), collapse = " ")
    spec <- .inspect_spec(x, artifact = artifact, reviewer = reviewer,
                          scores = scores, threshold = threshold,
                          max_pairs = max_pairs, page_size = page_size,
                          font_size = font_size, order_by = order_by,
                          expression = expression)

    # The run header goes down now, with any threshold the caller stated, so a
    # bad path is an error here rather than a lost afternoon, and so the file
    # records that this data was opened even if nothing is decided.
    opening <- if (is.null(threshold)) list() else
        list(.inspect_threshold_event(
            spec, spec$cuts[names(threshold)], "all", "accepted",
            "Given to rLink_inspect() when the review was opened.", 0L,
            .inspect_effect(spec, spec$cuts, "all")))
    .inspect_save(spec, list(), opening)

    app <- shiny::shinyApp(ui = .inspect_ui(spec),
                           server = .inspect_server(spec))
    if (launch) return(shiny::runApp(app))
    invisible(app)
}
