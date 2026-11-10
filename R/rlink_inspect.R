# The clerical review app. Everything that is a function of plain data lives in
# an .inspect_* helper so that it can be tested without a browser; the shiny
# pieces at the bottom only assemble those helpers.

.inspect_pkgs <- c("shiny", "reactable", "jsonlite")

# A blue single-hue sequential ramp. Deliberately not red-to-green: that
# excludes the ~8% of men with a colour vision deficiency, and it implies a
# verdict where there is only a score.
.inspect_ramp <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b")
.inspect_na_colour <- "#eeeeee"

# An en dash for a score there is no value for. Built from its code point so
# that the source stays ASCII: a literal here is a portability WARNING, and a
# backslash-u escape is liable to be turned back into the character by
# whatever next edits this file.
.inspect_na_label <- intToUtf8(8211L)

# A value as it should read in the evidence pane. A missing value and an empty
# string are different facts about a record -- one was never collected, the
# other was collected blank -- and a clerical reviewer deciding whether two
# people are the same person needs to be able to tell them apart.
.inspect_shown <- function(value) {
    if (is.na(value)) .inspect_na_label
    else if (!nzchar(value)) "(empty)"
    else value
}

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

# The scope a pair is actually in, which is the threshold's verdict with the
# reviewer's own judgements laid over it. This is the function the whole app
# now reads -- the header counts, the in scope / out of scope blocks, the
# "what this cut does" panel, the row dimming and the threshold it records --
# because a review tool whose numbers describe only the cut is telling the
# reviewer about a scope that is not the one their work produces.
#
# It is deliberately the same resolution apply_decisions() performs: the
# threshold decides the pairs nobody looked at, and a per-pair verdict
# overrides it either way. So the count on screen is the count the next run
# gets, and a reviewer can include a pair the cut threw out and watch the
# header agree with them.
.inspect_scope <- function(pass, ids, decisions) {
    out <- pass
    if (!length(decisions)) return(out)
    judged <- names(decisions)
    if (is.null(judged)) return(out)
    # vapply over the names rather than unlist(), which drops a NULL or an
    # empty element and so silently shifts every verdict after it onto the
    # wrong pair. Nothing writes such an element today; a wrong verdict is
    # too expensive to leave resting on that.
    verdict <- vapply(judged, function(id) {
        v <- decisions[[id]]
        if (length(v) != 1L || is.na(v)) NA_character_ else as.character(v)
    }, NA_character_, USE.NAMES = FALSE)
    keep <- !is.na(verdict) & verdict %in% c("include", "exclude")
    if (!any(keep)) return(out)
    at <- match(judged[keep], ids)
    hit <- !is.na(at)
    if (!any(hit)) return(out)
    out[at[hit]] <- verdict[keep][hit] == "include"
    out
}

# How the scope was arrived at, so the app can say what the overrides did
# rather than only that the total moved. "forced in" and "forced out" are the
# pairs where the reviewer and the cut disagree, which is the set worth
# re-reading before a linkage is signed off.
.inspect_scope_counts <- function(pass, scope) {
    list(n_scope = sum(scope), n_threshold = sum(pass),
         n_forced_in = sum(scope & !pass), n_forced_out = sum(!scope & pass))
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

# Dimming for "out of scope". Pass and fail is a different question from how
# high a score is, so it gets its own visual channel rather than being folded
# into the gradient.
#
# It reads the row's own `_scope`, which R computed, rather than recomputing
# the cut in the browser. Recomputing was wrong as soon as a rule expression
# could decide scope instead of the sliders -- the browser would still be
# comparing against the cuts and dimming the wrong rows. A judgement is read
# from `meta` on top, so an overridden row stops being dimmed the moment it is
# judged.
.inspect_js_rowclass <- function() reactable::JS("
function(rowInfo, state) {
  if (!rowInfo || !rowInfo.values) return null;
  var scope = rowInfo.values['_scope'];
  if (scope === undefined || scope === null) return null;
  var id = rowInfo.values['pair_id'];
  var d = (state.meta.decisions || {})[id] ||
          rowInfo.values['_decision'] || 'undecided';
  var inScope = (d === 'include') ? true :
                (d === 'exclude') ? false :
                (scope === 'in scope');
  var cls = inScope ? 'fs-pass' : 'fs-fail';
  // A row that was forced across the cut keeps a mark of its own, so the
  // overrides are findable by eye on a page of several hundred rows. Marked
  // only when the cut's own verdict is actually known -- see the scope cell.
  var raw = rowInfo.values['_threshold'];
  if ((raw === 'pass' || raw === 'fail') &&
      (d === 'include' || d === 'exclude') &&
      inScope !== (raw === 'pass'))
    cls += ' fs-forced-row';
  return cls;
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
# One delegated listener on the whole document, added once. Delegation is what
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
#
# Ctrl/cmd-click adds one row to the marked set and shift-click adds a range.
# The range is read off `data-fs-id` -- the pair's own identifier, carried by
# every row's buttons -- never off a row position. reactable's `selected`
# indices mean position in the *data*, not on screen, and the two part company
# the moment anything is sorted, filtered or paged; the identifier does not.
.inspect_js_bindings <- function() "
(function() {
  var timers = {}, anchor = null, marked = {};
  function send(name, payload) {
    payload.nonce = Math.random();
    Shiny.setInputValue(name, payload, { priority: 'event' });
  }
  function rowId(el) {
    var row = el.closest ? el.closest('.rt-tr') : null;
    if (!row) return null;
    var btn = row.querySelector('button.fs-btn');
    return btn ? btn.getAttribute('data-fs-id') : null;
  }
  function visibleIds() {
    var out = [];
    document.querySelectorAll('#tbl .rt-tbody .rt-tr').forEach(function(r) {
      var b = r.querySelector('button.fs-btn');
      if (b) out.push(b.getAttribute('data-fs-id'));
    });
    return out;
  }
  function paintMarks() {
    document.querySelectorAll('#tbl .rt-tbody .rt-tr').forEach(function(r) {
      var b = r.querySelector('button.fs-btn');
      var on = b && marked[b.getAttribute('data-fs-id')];
      r.classList.toggle('fs-marked', !!on);
    });
  }
  function pushMarks() {
    send('fs_marked', { pair_ids: Object.keys(marked) });
    paintMarks();
  }
  // Capture phase, so this runs before reactable's own cell handlers.
  document.addEventListener('click', function(e) {
    if (!e.target.closest) return;
    if (e.target.closest('button.fs-btn') ||
        e.target.closest('input.fs-comment')) return;
    var cell = e.target.closest('#tbl .rt-td');
    if (!cell) return;
    var id = rowId(cell);
    if (!id) return;
    if (e.shiftKey) {
      e.preventDefault(); e.stopPropagation();
      var ids = visibleIds();
      var a = ids.indexOf(anchor), b = ids.indexOf(id);
      if (a < 0) a = b;
      for (var i = Math.min(a, b); i <= Math.max(a, b); i++)
        marked[ids[i]] = true;
      pushMarks();
    } else if (e.ctrlKey || e.metaKey) {
      e.preventDefault(); e.stopPropagation();
      if (marked[id]) delete marked[id]; else marked[id] = true;
      anchor = id;
      pushMarks();
    } else {
      anchor = id;
      send('fs_focus', { pair_id: id });
    }
  }, true);
  // Shift-clicking a table would otherwise highlight text across it.
  document.addEventListener('mousedown', function(e) {
    if (e.shiftKey && e.target.closest && e.target.closest('#tbl'))
      e.preventDefault();
  });
  Shiny.addCustomMessageHandler('fs_clear_marks', function(msg) {
    marked = {}; anchor = null; paintMarks();
  });
  // y / n / u judge the row last clicked, or the whole marked set when there
  // is one. Clerical review is thousands of small verdicts, and the hand
  // leaving the keyboard for the mouse on every one of them is most of the
  // cost. Ignored while a text box has focus, so typing 'n' in a comment
  // stays typing.
  document.addEventListener('keydown', function(e) {
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    var t = e.target;
    if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' ||
              t.tagName === 'SELECT' || t.isContentEditable)) return;
    var k = e.key ? e.key.toLowerCase() : '';
    var value = k === 'y' ? 'include' :
                k === 'n' ? 'exclude' :
                k === 'u' ? 'undecided' : null;
    if (!value) return;
    var ids = Object.keys(marked);
    if (!ids.length && anchor) ids = [anchor];
    if (!ids.length) return;
    e.preventDefault();
    send('fs_keyjudge', { pair_ids: ids, decision: value });
  });
  // Re-paint after reactable redraws a page.
  var repaint = null;
  document.addEventListener('click', function() {
    clearTimeout(repaint); repaint = setTimeout(paintMarks, 60);
  });
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
    clearTimeout(timers[el.getAttribute('data-fs-id')]);
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
#fs-inspect .irs, #fs-inspect .selectize-input, #fs-inspect .accordion-button {
  font-size: var(--fs-font);
}
#fs-inspect .shiny-input-container { margin-bottom: 4px; }
#fs-inspect .accordion-button { padding: 4px 10px; }
#fs-inspect .accordion-body { padding: 6px 8px; }
#fs-inspect .card-body { padding: 6px 8px; }
#fs-inspect .fs-head { display: flex; gap: 14px; flex-wrap: wrap;
  align-items: baseline; margin-bottom: 4px; }
#fs-inspect .fs-kpi { font-weight: 600; }
#fs-inspect .fs-kpi-forced { color: #8a4b00; }
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
#fs-inspect .rt-tr.fs-fail .rt-td { opacity: 0.55; }
#fs-inspect .rt-tr.fs-marked .rt-td { background: #fff3cd !important;
  box-shadow: inset 0 -1px 0 0 #e0a800; }
#fs-inspect .fs-scope-in { color: #0b6b2e; font-weight: 600; }
#fs-inspect .fs-scope-out { color: #8a8a8a; }
#fs-inspect .fs-scope-in.fs-forced, #fs-inspect .fs-scope-out.fs-forced {
  color: #8a4b00; font-weight: 600; }
#fs-inspect .rt-tr.fs-forced-row .rt-td:first-child {
  box-shadow: inset 3px 0 0 0 #d98300; }
#fs-inspect .rt-tr.fs-forced-row .rt-td { opacity: 1; }
#fs-inspect .fs-mono { font-family: monospace; }
#fs-inspect table.fs-ev { width: 100%%; border-collapse: collapse;
  margin: 4px 0; }
#fs-inspect table.fs-ev th { text-align: left; font-weight: 600;
  padding: 1px 4px 1px 0; vertical-align: top; white-space: nowrap; }
#fs-inspect table.fs-ev td { padding: 1px 4px; vertical-align: top;
  border-top: 1px solid #eee; word-break: break-word; }
#fs-inspect table.fs-ev td.fs-ev-score { text-align: right;
  font-variant-numeric: tabular-nums; white-space: nowrap; }
#fs-inspect .fs-ev-verdict { color: #555; margin-bottom: 2px; }
#fs-inspect .fs-ev-act { display: flex; gap: 3px; flex-wrap: wrap; }
#fs-inspect .rt-table { font-variant-numeric: tabular-nums; }
", as.integer(font_size))

# One colDef per column: scores gradient-filled, the identity and scope columns
# pinned left so a row never loses its name when scrolled sideways, and the two
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
                na = .inspect_na_label, align = "right", minWidth = 62,
                header = sprintf("%s %s", nm, spec$score_spec$operator[k]),
                format = reactable::colFormat(digits = 3))
        } else if (nm %in% c("pair_id", spec$index_cols)) {
            cols[[nm]] <- reactable::colDef(sticky = "left", minWidth = 70)
        } else {
            cols[[nm]] <- reactable::colDef(minWidth = 70)
        }
    }
    cols[["_scope"]] <- reactable::colDef(
        name = "scope", sticky = "left", minWidth = 112,
        cell = .inspect_js_scope(), html = TRUE,
        # Grouped on, so the header shows the count of each block.
        aggregate = "count")
    # The cut's own verdict, which the scope cell and the row class read to
    # tell a confirmation from an override. It has to be in the data for the
    # renderers to reach it, and it has nothing to say to the reviewer that
    # the scope cell does not already say, so it is not drawn.
    cols[["_threshold"]] <- reactable::colDef(show = FALSE)
    cols[["_decision"]] <- reactable::colDef(
        name = "link?", cell = .inspect_js_decision(), html = TRUE,
        sortable = FALSE, filterable = FALSE, width = 78, sticky = "left")
    cols[["_comment"]] <- reactable::colDef(
        name = "comment", cell = .inspect_js_comment(), html = TRUE,
        sortable = FALSE, filterable = FALSE, minWidth = 170)
    cols
}

# "In scope" is the single thing a reviewer most needs to see at a glance. The
# cell reads the live judgement out of `meta` first and the threshold's own
# verdict only as the fallback, so marking a pair turns its scope cell over
# immediately -- without re-rendering the table, which is what would cost the
# reviewer their place. A judged pair says so, because an override is a
# different kind of fact from a score clearing a bar.
.inspect_js_scope <- function() reactable::JS("
function(cellInfo, state) {
  var id = cellInfo.row['pair_id'];
  var d = (state.meta.decisions || {})[id] ||
          cellInfo.row['_decision'] || 'undecided';
  // The cut's own verdict for this row, which is what tells a confirmation
  // from an override. `_threshold` is in the data but not drawn; if a future
  // reactable stops putting a hidden column in the row object it reads as
  // undefined, and the cell then falls back to saying only that the pair was
  // judged -- less informative, never wrong.
  var raw = cellInfo.row['_threshold'];
  var known = (raw === 'pass' || raw === 'fail');
  var passed = raw === 'pass';
  var judged = (d === 'include' || d === 'exclude');
  var inScope = (d === 'include') ? true :
                (d === 'exclude') ? false :
                (known ? passed : cellInfo.value === 'in scope');
  var label = inScope ? '\\u25cf in scope' : '\\u25cb out';
  var cls = inScope ? 'fs-scope-in' : 'fs-scope-out';
  // Agreeing with the cut and overruling it are different facts, and only
  // the second one is a decision somebody has to be able to defend later.
  // Saying 'judged' for both buried the overrides among the confirmations.
  if (judged && known && inScope !== passed) {
    label += inScope ? ' (forced in)' : ' (forced out)';
    cls += ' fs-forced';
  } else if (judged && known) {
    label += ' (confirmed)';
  } else if (judged) {
    label += ' (judged)';
  }
  return '<span class=\"' + cls + '\">' + label + '</span>';
}")

.inspect_theme <- function() reactable::reactableTheme(
    cellPadding = "1px 5px",
    style = list(fontSize = "var(--fs-font)"),
    highlightColor = "#f3f7fb",
    headerStyle = list(fontSize = "var(--fs-font)", fontWeight = 600,
                       background = "#f7f9fa",
                       borderBottom = "2px solid #d8dee4"),
    rowSelectedStyle = list(backgroundColor = "#eef2ff",
                            boxShadow = "inset 2px 0 0 0 #4a6cf7"))

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
.inspect_spec <- function(x, file, reviewer, scores, threshold, max_pairs,
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
    # whether the decisions file is worth anything.
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

    # "score" means most-like-a-match first, which for a column where low is
    # the match -- an unnormalised edit distance, say -- is ascending. Sorting
    # it descending regardless put the worst pairs in the review queue and the
    # best ones past `max_pairs`, which is the exact opposite of the point.
    # "review" is unaffected: it ranks by distance from the cut either way.
    best_first <- if (identical(score_spec$direction[1L], "lower"))
        order(all[[1L]], na.last = TRUE)
    else order(-all[[1L]], na.last = TRUE)
    priority <- switch(
        order_by,
        review = review_order(all[[1L]], threshold = cuts[[1L]]),
        score = best_first,
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
         file = file, n_pairs = nrow(x), n_shown = n_shown,
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
                                     effect = NULL, expr = NULL) {
    list(event_id = paste0(spec$run_id, "-t", sprintf("%06d", seq)),
         seq = seq, run_id = spec$run_id, reviewer = spec$reviewer,
         at = .review_now(), status = status, rule = rule,
         # The cuts are recorded alongside a rule expression, not instead of
         # it: they are what the sliders said, and they are what
         # apply_decisions() falls back to if the rule ever stops working.
         expr = if (is.null(expr) || !nzchar(expr)) NULL else expr,
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

# What a scope does to the records: how many of each side it links, how many
# are cleanly one-to-one, and how many are contested. Takes the scope rather
# than the cut, so the one function serves the threshold on its own and the
# threshold with the reviewer's overrides laid over it -- and so the app and
# the eagerly written opening event cannot drift apart, which they did when
# each computed its own.
.inspect_effect_of <- function(index_1, index_2, scope) {
    i <- index_1[scope]
    j <- index_2[scope]
    t1 <- if (length(i)) tabulate(i, max(i)) else integer()
    t2 <- if (length(j)) tabulate(j, max(j)) else integer()
    list(n_pairs = sum(scope),
         n_linked_1 = sum(t1 > 0L), n_linked_2 = sum(t2 > 0L),
         n_one_to_one = if (length(i)) sum(t1[i] == 1L & t2[j] == 1L) else 0L,
         n_conflict_1 = sum(t1 > 1L), n_conflict_2 = sum(t2 > 1L))
}

# What the current cut would do, over every pair, with no judgements applied.
.inspect_effect <- function(spec, cuts, rule, decisions = list()) {
    pass <- .inspect_pass(spec$all, cuts, spec$score_spec$operator, rule)
    scope <- .inspect_scope(pass, spec$all_ids, decisions)
    .inspect_effect_of(spec$source[[spec$index_cols[1L]]],
                       spec$source[[spec$index_cols[2L]]], scope)
}


# --- choosing the cut ------------------------------------------------------

# Where the sweep says the cut belongs. Lowering a threshold buys pairs and
# eventually buys contested records: the point to stop is the last cut before
# contested records start accumulating faster than clean one-to-one links.
# Returned with the reasoning, because a number a reviewer cannot justify is
# not much better than a guess.
.inspect_suggest <- function(sweep) {
    if (!nrow(sweep)) return(NULL)
    # Walk from the strictest cut and score each step by what it bought.
    gain <- diff(c(0, sweep$n_one_to_one))
    cost <- diff(c(0, sweep$n_conflict_1 + sweep$n_conflict_2))
    if (!length(gain) || all(gain <= 0 & cost <= 0)) {
        at <- which.max(sweep$n_one_to_one)
        return(list(threshold = sweep$threshold[at], at = at,
                    why = sprintf(paste0(
                        "Every cut-off behaves the same way here, so this is ",
                        "simply the one with the most clean one-to-one links ",
                        "(%s)."),
                        format(sweep$n_one_to_one[at], big.mark = ","))))
    }
    worth <- gain - cost
    # The last step still worth taking, then the cut just before it turns.
    good <- which(worth > 0)
    at <- if (length(good)) max(good) else which.max(sweep$n_one_to_one)
    list(threshold = sweep$threshold[at], at = at,
         why = sprintf(paste0(
             "At %.4g the sweep has %s clean one-to-one links and %s ",
             "contested records. Going one step looser adds %s link%s but %s ",
             "contested record%s, so this is the last cut that pays for ",
             "itself."),
             sweep$threshold[at],
             format(sweep$n_one_to_one[at], big.mark = ","),
             format(sweep$n_conflict_1[at] + sweep$n_conflict_2[at],
                    big.mark = ","),
             format(max(0, if (at < nrow(sweep)) gain[min(at + 1L,
                 length(gain))] else 0), big.mark = ","),
             if (at < nrow(sweep) && gain[min(at + 1L, length(gain))] == 1)
                 "" else "s",
             format(max(0, if (at < nrow(sweep)) cost[min(at + 1L,
                 length(cost))] else 0), big.mark = ","),
             if (at < nrow(sweep) && cost[min(at + 1L, length(cost))] == 1)
                 "" else "s"))
}


# --- the app ---------------------------------------------------------------

.inspect_ui <- function(spec) {
    scores <- spec$score_spec
    sliders <- lapply(seq_len(nrow(scores)), function(k) {
        lo <- scores$observed_min[k]
        hi <- scores$observed_max[k]
        if (!is.finite(lo) || !is.finite(hi)) { lo <- 0; hi <- 1 }
        lo <- min(lo, spec$cuts[k], na.rm = TRUE)
        hi <- max(hi, spec$cuts[k], na.rm = TRUE)
        if (hi <= lo) hi <- lo + 1
        shiny::tagList(
            shiny::tags$div(
                style = "display:flex; gap:6px; align-items:flex-end;",
                shiny::div(style = "flex:1 1 auto;", shiny::sliderInput(
                    paste0("cut_", k),
                    label = sprintf("%s %s", scores$score[k],
                                    scores$operator[k]),
                    min = lo, max = hi, value = unname(spec$cuts[k]),
                    step = signif((hi - lo) / 100, 2), width = "100%")),
                shiny::div(style = "flex:0 0 86px;", shiny::numericInput(
                    paste0("num_", k), label = NULL,
                    value = unname(spec$cuts[k]),
                    min = lo, max = hi, step = signif((hi - lo) / 1000, 2),
                    width = "86px"))),
            if (!scores$direction_known[k])
                shiny::tags$div(class = "fs-note", sprintf(paste0(
                    "The direction of `%s` was not detected; it is being read ",
                    "as higher-is-a-match."), scores$score[k])))
    })

    sidebar <- bslib::sidebar(
        width = 330, open = "open", id = "side",
        bslib::accordion(
            multiple = TRUE, class = "fs-acc",
            bslib::accordion_panel(
                "Threshold", value = "p_thresh", icon = NULL,
                sliders,
                shiny::radioButtons("rule", "A pair is in scope when",
                                    c("every score passes" = "all",
                                      "any score passes" = "any",
                                      "this rule holds" = "expr"),
                                    selected = "all"),
                shiny::conditionalPanel(
                    "input.rule == 'expr'",
                    shiny::textAreaInput(
                        "expr", NULL, rows = 2,
                        placeholder = "sn >= 0.9 | (sn >= 0.8 & gn >= 0.95)"),
                    shiny::uiOutput("fs_expr_check")),
                shiny::actionButton("suggest", "Suggest a cut",
                                    class = "btn-sm"),
                shiny::uiOutput("fs_suggest"),
                shiny::tags$hr(),
                shiny::textAreaInput("note", "Why this cut?", rows = 2),
                shiny::actionButton("accept", "Record this threshold",
                                    class = "btn-sm btn-primary")),
            # The pair beside itself, for the row last clicked. This is the
            # thing a clerical reviewer actually decides on -- "SMITH, JOHN,
            # 1980-01-02" against "SMYTHE, JON, 1980-01-02" -- and a row of
            # scores is a summary of it, not a substitute. It is the first
            # panel after the threshold because it is read on every pair,
            # whereas the cut is set a handful of times in a session.
            bslib::accordion_panel(
                "The pair", value = "p_pair",
                shiny::helpText("Click a row to put it here."),
                shiny::uiOutput("fs_evidence")),
            bslib::accordion_panel(
                "Marked rows", value = "p_marked",
                shiny::helpText(paste0(
                    "Ctrl-click rows to mark them, shift-click for a range, ",
                    "then judge them all at once. Or press y, n or u to ",
                    "judge the marked rows -- or, with none marked, the row ",
                    "last clicked.")),
                shiny::uiOutput("fs_marked_count"),
                shiny::actionButton("mark_include", "Mark as links",
                                    class = "btn-sm"),
                shiny::actionButton("mark_exclude", "Mark as not links",
                                    class = "btn-sm"),
                shiny::actionButton("mark_clear", "Clear marks",
                                    class = "btn-sm")),
            bslib::accordion_panel(
                "Rows and columns", value = "p_cols",
                shiny::radioButtons("mode", "Out of scope rows",
                                    c("shown, dimmed" = "flag",
                                      "hidden" = "filter"),
                                    selected = "flag"),
                shiny::radioButtons("filter", "Show",
                                    c("all pairs" = "all",
                                      "in scope" = "in_scope",
                                      "out of scope" = "out_scope",
                                      "undecided" = "undecided",
                                      "marked a link" = "include",
                                      "marked not a link" = "exclude",
                                      "overriding the cut" = "override"),
                                    selected = "all"),
                shiny::checkboxInput("group_scope",
                                     "Group by in scope / out", TRUE),
                shiny::actionButton("rerank", "Re-rank by how undecided",
                                    class = "btn-sm"),
                shiny::tags$hr(),
                shiny::tags$strong("Columns"),
                shiny::helpText("Drag between the lists to show, hide and reorder."),
                shiny::uiOutput("fs_col_order"),
                shiny::fluidRow(
                    shiny::column(6, shiny::numericInput(
                        "page_size", "Rows/page", spec$page_size,
                        min = 5, max = 1000, step = 25)),
                    shiny::column(6, shiny::numericInput(
                        "font_size", "Font px", spec$font_size,
                        min = 6, max = 24, step = 1)))),
            bslib::accordion_panel(
                "Save and finish", value = "p_save",
                shiny::tags$div(class = "fs-path", spec$file),
                shiny::actionButton("save", "Save now", class = "btn-sm"),
                shiny::actionButton("exit", "Finish and close",
                                    class = "btn-sm btn-success"),
                shiny::uiOutput("fs_saved"))))

    bslib::page_sidebar(
        title = "rLink pair review",
        sidebar = sidebar,
        fillable = FALSE,
        shiny::tags$div(
            id = "fs-inspect",
            shiny::tags$style(shiny::HTML(.inspect_css(spec$font_size))),
            shiny::uiOutput("fs_font"),
            shiny::tags$script(shiny::HTML(.inspect_js_bindings())),
            shiny::tags$div(
                class = "fs-head",
                shiny::tags$span(sprintf("Reviewer: %s", spec$reviewer)),
                shiny::uiOutput("fs_kpi", inline = TRUE)),
            if (spec$truncated) shiny::tags$div(class = "fs-note", sprintf(
                paste0("Showing the %s pairs most worth a look, of %s. Every ",
                       "count below is still computed over all %s."),
                format(spec$n_shown, big.mark = ","),
                format(spec$n_pairs, big.mark = ","),
                format(spec$n_pairs, big.mark = ","))),
            if (identical(spec$pair_id_source, "sequence"))
                shiny::tags$div(class = "fs-note", paste0(
                    "`pair_id` is a row number, so these decisions will not ",
                    "find their pairs again if the pairs are rebuilt. Give ",
                    "rLink() a `pair_id` column for a review that survives a ",
                    "re-run.")),
            if (identical(spec$pair_id_source, "index"))
                shiny::tags$div(class = "fs-note", paste0(
                    "`pair_id` was built from the row indices, so these ",
                    "decisions hold only while both inputs keep their row ",
                    "order.")),
            shiny::uiOutput("fs_changed"),
            reactable::reactableOutput("tbl"),
            shiny::tags$br(),
            bslib::accordion(
                multiple = TRUE, open = FALSE, class = "fs-acc",
                bslib::accordion_panel(
                    "Threshold sweep", value = "a_sweep",
                    # Which score to sweep. The sweep used to be hardwired
                    # to the first one, which left every other cut in a
                    # multi-score linkage to be guessed -- and it is the
                    # second and third fields, not the first, where a
                    # reviewer has least idea where the cut belongs.
                    if (nrow(scores) > 1L) shiny::selectInput(
                        "sweep_score", "Sweep", choices = scores$score,
                        selected = scores$score[1L], width = "100%"),
                    shiny::helpText(
                        "Click a row to move that score's cut there."),
                    shiny::plotOutput("sweep_plot", height = "240px"),
                    reactable::reactableOutput("sweep_tbl")),
                bslib::accordion_panel(
                    "Score distributions", value = "a_hist",
                    shiny::plotOutput("hist_plot", height = "300px")),
                bslib::accordion_panel(
                    "What this cut does", value = "a_effect",
                    shiny::tableOutput("effect")),
                bslib::accordion_panel(
                    "This session", value = "a_events",
                    shiny::tableOutput("events")))))
}

.inspect_server <- function(spec) function(input, output, session) {
    n_scores <- nrow(spec$score_spec)
    operators <- spec$score_spec$operator
    score_names <- spec$score_spec$score
    index_1 <- spec$source[[spec$index_cols[1L]]]
    index_2 <- spec$source[[spec$index_cols[2L]]]
    control_cols <- c("_scope", "_decision", "_comment")
    data_cols <- setdiff(names(spec$view), control_cols)

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
    rv_marked <- shiny::reactiveVal(character())
    rv_suggest <- shiny::reactiveVal(NULL)
    rv_focus <- shiny::reactiveVal(NULL)

    next_seq <- function() {
        n <- rv_seq() + 1L
        rv_seq(n)
        n
    }

    # --- the cut ----------------------------------------------------------
    # The slider and the numeric box are two views of one number. R holds the
    # number and each widget writes to it, rather than one widget reading the
    # other: a cut must not depend on the browser echoing a value back, both
    # because the echo can lag behind a fast drag and because a round trip
    # through the client is not something that can be reasoned about or tested.
    rv_cuts <- shiny::reactiveVal(spec$cuts)

    set_cut <- function(k, value, from) {
        if (is.null(value) || !is.finite(value)) return(invisible(NULL))
        held <- rv_cuts()
        if (isTRUE(all.equal(unname(held[k]), as.double(value))))
            return(invisible(NULL))
        held[k] <- as.double(value)
        rv_cuts(held)
        # Only the other view needs telling.
        if (!identical(from, "slider"))
            shiny::updateSliderInput(session, paste0("cut_", k), value = value)
        if (!identical(from, "box"))
            shiny::updateNumericInput(session, paste0("num_", k),
                                      value = value)
        invisible(NULL)
    }

    # Deliberately not ignoreInit: set_cut() returns early when the value has
    # not moved, so the first firing -- the widgets reporting the values they
    # were built with -- is a no-op. Skipping it instead would mean the first
    # change a caller makes is swallowed, which is exactly what happens under
    # shiny::testServer(), where the inputs start unset.
    for (k in seq_len(n_scores)) local({
        kk <- k
        shiny::observeEvent(input[[paste0("cut_", kk)]],
                            set_cut(kk, input[[paste0("cut_", kk)]], "slider"))
        shiny::observeEvent(input[[paste0("num_", kk)]],
                            set_cut(kk, input[[paste0("num_", kk)]], "box"))
    })

    cuts <- shiny::debounce(shiny::reactive(rv_cuts()), 250)

    rule <- shiny::reactive(if (is.null(input$rule)) "all" else input$rule)

    rule_expr <- shiny::debounce(shiny::reactive(
        if (identical(rule(), "expr")) input$expr else NULL), 400)

    # The rule is checked as it is typed, so a reviewer finds out now rather
    # than when a QA pass applies it months later.
    expr_pass <- shiny::reactive({
        text <- rule_expr()
        if (is.null(text) || !nzchar(trimws(text))) return(NULL)
        frame <- spec$all
        frame$pair_id <- spec$all_ids
        suppressWarnings(.decisions_rule_eval(text, frame))
    })

    output$fs_expr_check <- shiny::renderUI({
        text <- rule_expr()
        if (is.null(text) || !nzchar(trimws(text))) return(NULL)
        got <- expr_pass()
        if (is.null(got))
            return(shiny::tags$div(class = "fs-note", paste0(
                "That rule cannot be used yet. Name score columns and ",
                "combine them with >=, <=, &, | and parentheses.")))
        shiny::tags$div(class = "fs-ok", sprintf(
            "%s of %s pairs match that rule.",
            format(sum(got), big.mark = ","),
            format(length(got), big.mark = ",")))
    })

    pass_all <- shiny::reactive({
        if (identical(rule(), "expr")) {
            got <- expr_pass()
            if (!is.null(got)) return(got)
        }
        .inspect_pass(spec$all, cuts(), operators,
                      if (identical(rule(), "any")) "any" else "all")
    })

    # The cut's verdict with the reviewer's judgements laid over it: what
    # apply_decisions() will produce from this session, and therefore the only
    # scope the numbers on screen have any business describing.
    scope_all <- shiny::reactive(
        .inspect_scope(pass_all(), spec$all_ids, rv_decisions()))

    # Counts over the effective scope, not the bare cut. A reviewer who
    # includes a pair the cut threw out sees the total move, which is the
    # whole point of being able to override it.
    effect <- shiny::reactive(.inspect_effect_of(index_1, index_2, scope_all()))

    # The cut's own counts, with no judgements applied. This is what goes in
    # a recorded threshold event: that event is a record of the *threshold*,
    # and its documented meaning is the consequence of the cut, so a reader
    # need not re-run it. Storing the reviewed counts there instead would
    # make the stored number depend on how far through the review the analyst
    # happened to be when they pressed the button, which is noise rather than
    # information. The reviewed counts are in the header and the panel, where
    # they are live.
    cut_effect <- shiny::reactive(
        .inspect_effect_of(index_1, index_2, pass_all()))

    # What the overrides did on their own, so the header can report them.
    overrides <- shiny::reactive(
        .inspect_scope_counts(pass_all(), scope_all()))

    # The same scope, but depending on the cut and *not* on the judgements:
    # what the table's grouping and row filtering read. Moving a slider has to
    # regroup; clicking a verdict must not, because a re-render loses the
    # reviewer's sort, filter and page. The cell renderer picks up the
    # judgement from `meta` in the meantime.
    scope_for_table <- function()
        .inspect_scope(pass_all(), spec$all_ids,
                       shiny::isolate(rv_decisions()))

    # Which score the sweep, the suggestion and the re-rank are about. One
    # score, not all of them, because a sweep is a curve in one dimension;
    # but a *chosen* one rather than always the first.
    swept <- shiny::reactive({
        pick <- input$sweep_score
        at <- if (is.null(pick)) NA_integer_ else match(pick, score_names)
        if (is.na(at)) 1L else at
    })

    # The sweep is a curve and the cut is a marker on it, so it is computed
    # once per score rather than on every slider tick.
    sweep <- shiny::reactive({
        k <- swept()
        threshold_sweep(
            data.frame(index_1 = index_1, index_2 = index_2),
            spec$all[[score_names[k]]], operator = operators[k])
    })

    shiny::observeEvent(input$suggest, {
        s <- .inspect_suggest(sweep())
        rv_suggest(s)
        if (!is.null(s)) set_cut(swept(), s$threshold, "suggest")
    })

    output$fs_suggest <- shiny::renderUI({
        s <- rv_suggest()
        if (is.null(s)) return(NULL)
        shiny::tags$div(class = "fs-ok", s$why)
    })

    # --- the table --------------------------------------------------------
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

    # Drag-to-reorder, drag-to-hide. The table reads col_order() rather than
    # the widget's own inputs, so if sortable never reports back -- it needs
    # shiny >= 1.9.0 to do so -- the table still draws with every column.
    output$fs_col_order <- shiny::renderUI({
        sortable::bucket_list(
            header = NULL, group_name = "fs_cols", orientation = "vertical",
            sortable::add_rank_list(text = "Shown", labels = data_cols,
                                    input_id = "cols_shown"),
            sortable::add_rank_list(text = "Hidden", labels = character(),
                                    input_id = "cols_hidden"))
    })
    # Shiny never runs a renderUI while its output is hidden, and this one
    # lives inside a collapsed accordion panel -- so without this the picker
    # is never drawn and input$cols_shown stays NULL for ever.
    shiny::outputOptions(output, "fs_col_order", suspendWhenHidden = FALSE)

    col_order <- shiny::reactive({
        shown <- input$cols_shown
        if (is.null(shown) || !length(shown)) return(data_cols)
        keep <- intersect(as.character(shown), data_cols)
        if (!length(keep)) data_cols else keep
    })

    table_rows <- shiny::reactive({
        rows <- rv_order()
        # Hiding out-of-scope rows hides them by *effective* scope, so a pair
        # included by hand does not vanish the moment it is included. Judging
        # a pair and watching it disappear is how a reviewer loses confidence
        # that the tool recorded what they said.
        if (identical(input$mode, "filter"))
            rows <- rows[scope_for_table()[spec$shown][rows]]
        pick <- input$filter
        if (!is.null(pick) && !identical(pick, "all")) {
            held <- shiny::isolate(state_for(spec$view$pair_id[rows])$decision)
            rows <- switch(
                pick,
                # The set worth re-reading before signing off: every pair
                # where the reviewer and the cut disagree.
                override = {
                    pass <- pass_all()[spec$shown][rows]
                    rows[(held == "include" & !pass) |
                         (held == "exclude" & pass)]
                },
                in_scope = rows[scope_for_table()[spec$shown][rows]],
                out_scope = rows[!scope_for_table()[spec$shown][rows]],
                rows[held == pick])
        }
        rows
    })

    table_data <- shiny::reactive({
        rows <- table_rows()
        keep <- intersect(unique(c("pair_id", col_order())),
                          names(spec$view))
        out <- spec$view[rows, keep, drop = FALSE]
        rownames(out) <- NULL
        held <- shiny::isolate(state_for(out$pair_id))
        # The blocks the table groups into are the *effective* scope -- the
        # cut with the judgements already applied -- and this reactive depends
        # on the cut, so moving a slider regroups, which is what you expect,
        # having just moved it.
        #
        # A per-pair judgement does not regroup. It cannot: grouping reads the
        # data, and re-rendering the table on every click would throw away the
        # reviewer's sort, filter and page position, which is the fastest way
        # to make someone stop using a review tool. The scope *cell* is drawn
        # from `meta` instead, so an overridden pair turns over the instant it
        # is judged and says so; it moves to the other block at the next
        # regroup.
        #
        # `_threshold` carries the cut's own verdict alongside it, because the
        # cell has to be able to say whether a judgement agreed with the cut
        # or overruled it, and after a regroup `_scope` no longer shows that.
        pass <- pass_all()[spec$shown][rows]
        out[["_scope"]] <- ifelse(scope_for_table()[spec$shown][rows],
                                  "in scope", "out of scope")
        out[["_threshold"]] <- ifelse(pass, "pass", "fail")
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
            groupBy = if (isTRUE(input$group_scope)) "_scope" else NULL,
            meta = .inspect_meta(spec$score_spec, shiny::isolate(cuts()),
                                 shiny::isolate(rule()),
                                 shiny::isolate(rv_touched()$decisions),
                                 shiny::isolate(rv_touched()$comments)),
            rowClass = .inspect_js_rowclass(),
            sortable = TRUE, resizable = TRUE, filterable = TRUE,
            searchable = TRUE, compact = TRUE, highlight = TRUE,
            wrap = FALSE, bordered = TRUE, pagination = TRUE,
            defaultExpanded = TRUE,
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

    # --- decisions --------------------------------------------------------
    record <- function(pair_id, decision = NULL, comment = NULL,
                       push = TRUE) {
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
        if (push)
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

    # --- the focused pair -------------------------------------------------
    # Clicking a row puts that pair in the sidebar, field by field, with the
    # value each input held beside the score it earned. The browser has been
    # sending this since the app was first written; nothing listened for it,
    # so a click did nothing and .inspect_evidence() was unreachable.
    shiny::observeEvent(input$fs_focus, {
        id <- as.character(input$fs_focus$pair_id)
        if (!length(id) || is.na(id) || !id %in% spec$all_ids) return()
        rv_focus(id)
    })

    output$fs_evidence <- shiny::renderUI({
        id <- rv_focus()
        if (is.null(id)) return(NULL)
        ev <- tryCatch(.inspect_evidence(spec, id), error = function(cnd) NULL)
        if (is.null(ev)) return(NULL)
        held <- rv_decisions()[[id]]
        pass <- pass_all()[match(id, spec$all_ids)]
        verdict <- if (is.null(held) || identical(held, "undecided"))
            sprintf("undecided; the cut says %s",
                    if (isTRUE(pass)) "in scope" else "out of scope")
        else if (identical(held, "include") == isTRUE(pass))
            sprintf("judged %s, agreeing with the cut",
                    if (identical(held, "include")) "a link" else "not a link")
        else sprintf("judged %s, overriding the cut",
                     if (identical(held, "include")) "a link"
                     else "not a link")
        rows <- lapply(seq_len(nrow(ev)), function(k) shiny::tags$tr(
            shiny::tags$th(ev$field[k]),
            shiny::tags$td(class = "fs-mono", .inspect_shown(ev$value_1[k])),
            shiny::tags$td(class = "fs-mono", .inspect_shown(ev$value_2[k])),
            shiny::tags$td(class = "fs-mono fs-ev-score",
                           if (is.na(ev$value[k])) .inspect_na_label
                           else formatC(ev$value[k], digits = 3,
                                        format = "g"))))
        # What anyone else said about this pair, where the file holds a
        # previous reviewer's pass. Shown, never pre-filled: a QA reviewer
        # should know the analyst called it a link, and should still have to
        # say so themselves for the file to record an agreement.
        theirs <- seeded$others[seeded$others$pair_id == id, , drop = FALSE]
        shiny::tagList(
            shiny::tags$div(class = "fs-mono", id),
            shiny::tags$div(class = "fs-ev-verdict", verdict),
            if (nrow(theirs)) shiny::tags$div(
                class = "fs-note",
                lapply(seq_len(nrow(theirs)), function(k) shiny::tags$div(
                    sprintf("%s: %s%s", theirs$reviewer[k],
                            if (identical(theirs$decision[k], "include"))
                                "a link" else "not a link",
                            if (!is.na(theirs$comment[k]) &&
                                nzchar(theirs$comment[k]))
                                paste0(" -- ", theirs$comment[k]) else "")))),
            shiny::tags$table(class = "fs-ev", rows),
            # Judging from here as well as from the row saves the reviewer
            # travelling back to a row they have just finished reading.
            shiny::tags$div(
                class = "fs-ev-act",
                shiny::actionButton("focus_include", "A link",
                                    class = "btn-sm"),
                shiny::actionButton("focus_exclude", "Not a link",
                                    class = "btn-sm"),
                shiny::actionButton("focus_undecided", "Undecided",
                                    class = "btn-sm")))
    })
    shiny::outputOptions(output, "fs_evidence", suspendWhenHidden = FALSE)

    focus_judge <- function(decision) {
        id <- rv_focus()
        if (is.null(id)) return(invisible(NULL))
        record(id, decision = decision)
    }
    shiny::observeEvent(input$focus_include, focus_judge("include"))
    shiny::observeEvent(input$focus_exclude, focus_judge("exclude"))
    shiny::observeEvent(input$focus_undecided, focus_judge("undecided"))

    # y / n / u from the keyboard. Routed through the same record() as a click
    # so the event log cannot tell them apart, which is what makes the audit
    # trail worth keeping.
    shiny::observeEvent(input$fs_keyjudge, {
        info <- input$fs_keyjudge
        decision <- as.character(info$decision)
        if (!length(decision) ||
            !decision %in% c("include", "exclude", "undecided")) return()
        ids <- intersect(as.character(unlist(info$pair_ids)), spec$all_ids)
        if (!length(ids)) return()
        for (id in ids) record(id, decision = decision, push = FALSE)
        touched <- rv_touched()
        reactable::updateReactable("tbl", meta = list(
            decisions = touched$decisions, comments = touched$comments),
            session = session)
    })

    # --- marked rows ------------------------------------------------------
    shiny::observeEvent(input$fs_marked, {
        ids <- as.character(input$fs_marked$pair_ids)
        rv_marked(intersect(ids, spec$all_ids))
    })

    output$fs_marked_count <- shiny::renderUI({
        n <- length(rv_marked())
        shiny::tags$div(class = if (n) "fs-ok" else NULL,
                        sprintf("%s row%s marked", format(n, big.mark = ","),
                                if (n == 1L) "" else "s"))
    })

    mark_all <- function(decision) {
        ids <- rv_marked()
        if (!length(ids)) return(invisible(NULL))
        for (id in ids) record(id, decision = decision, push = FALSE)
        touched <- rv_touched()
        reactable::updateReactable("tbl", meta = list(
            decisions = touched$decisions, comments = touched$comments),
            session = session)
        rv_marked(character())
        session$sendCustomMessage("fs_clear_marks", list(clear = TRUE))
        invisible(NULL)
    }

    shiny::observeEvent(input$mark_include, mark_all("include"))
    shiny::observeEvent(input$mark_exclude, mark_all("exclude"))
    shiny::observeEvent(input$mark_clear, {
        rv_marked(character())
        session$sendCustomMessage("fs_clear_marks", list(clear = TRUE))
    })

    # --- recording a threshold -------------------------------------------
    shiny::observeEvent(input$accept, {
        rv_thresholds(c(rv_thresholds(), list(.inspect_threshold_event(
            spec, cuts(), if (identical(rule(), "expr")) "all" else rule(),
            "accepted", if (is.null(input$note)) "" else input$note,
            next_seq(), cut_effect(),
            expr = if (identical(rule(), "expr")) rule_expr() else NULL))))
        save_now()
    })

    shiny::observeEvent(input$rerank, {
        k <- swept()
        rv_order(review_order(spec$all[[score_names[k]]][spec$shown],
                              threshold = unname(cuts()[k])))
    })

    # --- panels -----------------------------------------------------------
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
        o <- overrides()
        shiny::tagList(
            shiny::tags$span(class = "fs-kpi", sprintf(
                "%s of %s in scope", format(e$n_pairs, big.mark = ","),
                format(spec$n_pairs, big.mark = ","))),
            # Spelled out whenever the reviewer and the cut part company, so
            # the headline total is always accountable: this many from the
            # cut, this many forced across it by hand.
            if (o$n_forced_in || o$n_forced_out) shiny::tags$span(
                class = "fs-kpi-forced",
                sprintf("(%s by the cut, %s forced in, %s forced out)",
                        format(o$n_threshold, big.mark = ","),
                        format(o$n_forced_in, big.mark = ","),
                        format(o$n_forced_out, big.mark = ","))),
            shiny::tags$span(sprintf("%s clean 1:1",
                                     format(e$n_one_to_one, big.mark = ","))),
            shiny::tags$span(sprintf(
                "%s contested",
                format(e$n_conflict_1 + e$n_conflict_2, big.mark = ","))),
            shiny::tags$span(sprintf("judged: %d link, %d not",
                                     sum(held == "include"),
                                     sum(held == "exclude"))))
    })

    output$fs_changed <- shiny::renderUI({
        shiny::tagList(
            if (!is.null(seeded$changed))
                shiny::tags$div(class = "fs-note", seeded$changed),
            # Said once, at the top, because it changes how the session
            # should be read: this is a second pass over pairs somebody has
            # already judged, and the screen is deliberately not pre-filled
            # with their answers.
            if (nrow(seeded$others)) shiny::tags$div(
                class = "fs-note", sprintf(paste0(
                    "%s of these pairs have already been judged by %s. Those ",
                    "verdicts are shown against each pair but not filled in ",
                    "for you, so your pass is recorded as your own."),
                    format(length(unique(seeded$others$pair_id)),
                           big.mark = ","),
                    paste(sort(unique(seeded$others$reviewer)),
                          collapse = ", "))))
    })

    output$fs_saved <- shiny::renderUI({
        msg <- rv_saved()
        if (is.null(msg)) return(NULL)
        shiny::tags$div(
            class = if (startsWith(msg, "Could not")) "fs-note" else "fs-ok",
            msg)
    })

    # The cut and the review side by side, because the question the panel is
    # read to answer is "what am I actually shipping", and with overrides in
    # play the cut alone no longer answers it.
    output$effect <- shiny::renderTable({
        cut_only <- cut_effect()
        reviewed <- effect()
        o <- overrides()
        data.frame(
            measure = c(names(reviewed), "forced in", "forced out"),
            threshold = format(c(unlist(cut_only), NA, NA), big.mark = ","),
            reviewed = format(c(unlist(reviewed), o$n_forced_in,
                                o$n_forced_out), big.mark = ","),
            stringsAsFactors = FALSE)
    })

    output$sweep_plot <- shiny::renderPlot({
        s <- sweep()
        if (nrow(s) < 2L) {
            graphics::plot.new()
            graphics::text(0.5, 0.5, "Too few distinct scores to sweep.")
            return(invisible(NULL))
        }
        plot.fs_sweep(s, threshold = unname(cuts()[swept()]))
    })

    output$sweep_tbl <- reactable::renderReactable({
        s <- as.data.frame(sweep())
        reactable::reactable(
            s, compact = TRUE, bordered = TRUE, highlight = TRUE,
            defaultPageSize = 12, showPageSizeOptions = FALSE,
            selection = "single", onClick = "select",
            theme = .inspect_theme(), class = "fs-table")
    })

    # Clicking a sweep row moves the cut there: the evidence and the control
    # become the same object.
    shiny::observeEvent(
        reactable::getReactableState("sweep_tbl", "selected"), {
        at <- reactable::getReactableState("sweep_tbl", "selected")
        if (is.null(at) || !length(at)) return()
        s <- sweep()
        if (at[1L] > nrow(s)) return()
        set_cut(swept(), s$threshold[at[1L]], "sweep")
    }, ignoreInit = TRUE)

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

    # --- saving and finishing --------------------------------------------
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

    shiny::observeEvent(input$exit, {
        save_now()
        # Hands the decisions object back to whoever called runApp(), which is
        # what makes `output <- rLink_inspect(x)` work.
        shiny::stopApp(.inspect_result(spec))
    })
    # The last clicks must not be lost by closing the tab.
    session$onSessionEnded(function() try(save_now(), silent = TRUE))
}

# What the app hands back: the decisions themselves, checked against the pairs
# they were made on.
.inspect_result <- function(spec) {
    frame <- data.frame(pair_id = spec$all_ids, stringsAsFactors = FALSE)
    for (s in spec$score_spec$score) frame[[s]] <- spec$all[[s]]
    tryCatch(read_decisions(spec$file, x = frame),
             error = function(cnd) tryCatch(read_decisions(spec$file),
                                            error = function(e) NULL))
}
# Decisions already in the file, so reopening a review shows the earlier
# work instead of starting again, and a moved fingerprint is reported rather
# than ignored.
.inspect_no_others <- function()
    data.frame(reviewer = character(), pair_id = character(),
               decision = character(), comment = character(),
               stringsAsFactors = FALSE)

.inspect_seed <- function(spec) {
    empty <- list(decisions = list(), comments = list(), changed = NULL,
                  others = .inspect_no_others())
    if (!file.exists(spec[["file"]])) return(empty)
    held <- tryCatch(read_decisions(spec[["file"]]), error = function(cnd) NULL)
    if (is.null(held)) return(empty)
    out <- empty
    # This reviewer's own verdicts, and nobody else's. `$decisions` holds one
    # row per reviewer *and* pair, so seeding from all of it filled a second
    # reviewer's screen with the first one's answers -- with whichever of the
    # two happened to come last in the frame winning each pair. For a QA pass,
    # which is the main reason the file is shared at all, that is the one
    # thing the tool must not do: an independent check cannot be independent
    # if it opens pre-answered. Others' verdicts are reported separately
    # instead, as something to weigh rather than something already decided.
    everyone <- held$decisions
    mine <- everyone[everyone$reviewer %in% spec$reviewer, , drop = FALSE]
    for (k in seq_len(nrow(mine))) {
        id <- mine$pair_id[k]
        out$decisions[[id]] <- mine$decision[k]
        if (!is.na(mine$comment[k])) out$comments[[id]] <- mine$comment[k]
    }
    others <- everyone[!everyone$reviewer %in% spec$reviewer &
                       !is.na(everyone$decision) &
                       everyone$decision != "undecided", , drop = FALSE]
    out$others <- others[others$pair_id %in% spec$all_ids, , drop = FALSE]
    if (nrow(held$runs)) {
        last <- held$runs[nrow(held$runs), ]
        moved <- !identical(last$fp_pair_set, spec$fingerprints$pair_set) ||
            !identical(last$fp_scores, spec$fingerprints$scores)
        if (moved) {
            seen <- everyone$pair_id
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
    held <- if (file.exists(spec[["file"]]))
        tryCatch(.review_read_file(spec[["file"]]), error = function(cnd) NULL)
        else NULL
    .review_write(if (is.null(held)) mine else .review_merge(held, mine),
                  spec[["file"]])
}

#' Review candidate pairs in a browser
#'
#' Opens an interactive table of candidate pairs for choosing a threshold and
#' accepting or rejecting individual pairs, and records every judgement --- with
#' a comment --- in a JSON file that someone else can later read with
#' [read_decisions()]. It is the step between scoring pairs and believing the
#' result, and that file is what turns an analyst's afternoon of judgement
#' calls into something a second person can check and replay.
#'
#' The workflow it is built for:
#'
#' ```
#' pairs <- rLink(enrol, claims, block_1 = ..., compare = ...)
#' out <- rLink_inspect(pairs)          # choose a cut, mark the exceptions
#' out                                  # prints the R to reproduce and apply
#' pairs <- apply_decisions(pairs, "rLink_decisions.json")
#' ```
#'
#' Closing the app with **Finish and close** returns the [read_decisions()]
#' object, so `out` holds the whole review; printing it ends with the two lines
#' to paste into the script so the next run --- a colleague's QA pass, or yours
#' in six months --- reproduces exactly these decisions.
#'
#' @section What you see:
#' Score columns are filled with a colour gradient, dark meaning more like a
#' match --- reversed automatically for a score where low is the match, so a
#' distance left unnormalised still reads the right way round. The table is
#' grouped into **in scope** and **out of scope** blocks, so the consequence of
#' the current cut is the first thing visible, and out-of-scope rows are dimmed
#' rather than hidden, because a false negative you cannot see is a false
#' negative you will not find.
#'
#' Clicking a row puts that pair in the sidebar field by field --- the value
#' each input held on each side, beside the score it earned --- because what a
#' clerical decision is actually made on is `SMITH / JOHN / 1980-01-02`
#' against `SMYTHE / JON / 1980-01-02`, and a row of numbers is a summary of
#' that rather than a substitute for it.
#'
#' Every count in the header, the sweep and the panels is computed over *all*
#' the pairs, not only the ones on screen.
#'
#' @section Choosing the cut:
#' One slider and one numeric box per score, kept in step, so a cut can be
#' dragged or typed exactly.
#'
#' The **Threshold sweep** panel shows what every cut-off of one score would
#' do, and which score that is can be chosen --- it is the second and third
#' fields, not the first, where a reviewer has least idea where the cut
#' belongs. **Suggest a cut** reads that sweep and proposes the last cut-off
#' that buys more clean one-to-one links than it does contested records, and
#' says so in a sentence. Clicking a row of the sweep table moves that
#' score's cut there, so the evidence and the control are the same object.
#'
#' Beyond `all` and `any`, a rule such as
#' `sn >= 0.9 | (sn >= 0.8 & gn >= 0.95)` can be written out; it is checked as
#' you type, stored with the threshold, and re-applied by
#' [apply_decisions()].
#'
#' @section Judging pairs, and overriding the cut:
#' Three buttons and a comment box on every row, and the same three in the
#' sidebar for the pair being read. `y`, `n` and `u` judge from the keyboard.
#' Ctrl-click (cmd-click) marks rows and shift-click marks a range, and the
#' marked set can then be judged in one action --- which is how a block of
#' obvious links gets dealt with without 200 clicks. Marks follow the pair,
#' not the row position, so they survive sorting, filtering and paging.
#'
#' **A judgement beats the threshold, both ways.** The cut decides the pairs
#' nobody looks at; marking a pair a link brings it into scope even though its
#' scores failed, and marking one not a link takes it out even though they
#' passed. That is what the threshold is for --- it is a way of not having to
#' look at a million pairs, not a verdict --- and it is the order
#' [apply_decisions()] resolves them in, so the numbers on screen are the
#' numbers the next run produces.
#'
#' Because an override is a decision somebody has to be able to defend, it is
#' reported as one rather than folded into the total. The scope cell says
#' **forced in** or **forced out** where the judgement contradicts the cut and
#' **confirmed** where it agrees; those rows carry a marker down their left
#' edge; the header breaks the total into what the cut gave and what was forced
#' either way; **What this cut does** shows the cut's counts beside the
#' reviewed ones; and *Show > overriding the cut* lists exactly that set,
#' which is the page to re-read before a linkage is signed off.
#'
#' @section What is written:
#' The decisions file is append-only. A second reviewer's session is added to
#' it, not written over it, so a disagreement between two reviewers is
#' preserved and visible --- which is the point of having the file at all.
#' Decisions are filed against `pair_id`, so they still find their pairs after
#' `rLink()` is re-run with different blocking, provided `pair_id` was built
#' from record identifiers rather than row numbers. See *Pair identifiers* in
#' [rLink()].
#'
#' Reopening a file restores **your own** verdicts and comments. Another
#' reviewer's are shown against each pair but never filled in for you: a QA
#' pass that opens pre-answered is not a check. Who else judged what, and
#' where you and they disagree, is then in [read_decisions()]`$disagreements`.
#'
#' @section Limits:
#' At most `max_pairs` rows are put on screen, chosen by [review_order()]; the
#' rest of the analysis still reads every pair, and the app says so. reactable
#' draws every row of a page, so `page_size` is capped at 1000 --- there is no
#' "show all rows", because at these sizes it would lock the browser tab rather
#' than show you anything. Columns are reordered and hidden by dragging them
#' between the Shown and Hidden lists in the sidebar; reactable cannot drag a
#' table *header*.
#'
#' @param x Candidate pairs: an [rLink()] result, or any data frame with
#'   `index_1`/`index_2` or `x_index`/`y_index` columns.
#' @param decisions Path to the JSON decisions file. Read and appended to when
#'   it exists, created when it does not. Written as soon as the app is built,
#'   so an unwritable path fails now rather than after an hour of review.
#' @param reviewer Who is reviewing. Defaults to the logged-in user, and is the
#'   name every decision is attributed to.
#' @param scores Score columns to threshold, or `NULL` to read them from
#'   `attr(x, "compare")`, falling back to the numeric columns that are neither
#'   indices nor carried values.
#' @param threshold Optional named numeric vector of starting cuts, as
#'   `c(surname_jw = 0.88)`. Scores it does not name keep their default cut.
#'   Recorded in the file as an accepted threshold --- all of the cuts in
#'   force, not only the ones named --- so a scripted cut-off is logged in the
#'   same format as a hand-chosen one and [apply_decisions()] reproduces what
#'   was on screen.
#' @param max_pairs Most rows to put on screen, `20000` by default.
#' @param page_size Rows per page, between 5 and 1000.
#' @param font_size Base font in pixels, adjustable in the app.
#' @param order_by Initial row order: `"review"` puts the pairs nearest the
#'   cut --- the ones a threshold cannot settle --- first, `"score"` puts the
#'   most match-like first (which for a score where low is the match is
#'   ascending), and `"input"` leaves them as given.
#' @param launch Whether to run the app. Defaults to [interactive()], so the
#'   file is still written when this is called from a script or a test.
#' @return An [read_decisions()] object of class `fs_decisions`, holding the
#'   review and the R needed to reproduce it. Returned when the app closes; with
#'   `launch = FALSE` the `shiny.appobj` is returned invisibly instead, so it
#'   can be run or inspected later.
#' @seealso [read_decisions()] to read the file back, [apply_decisions()] to
#'   apply it, [threshold_sweep()] for the same evidence at the console, and
#'   [review_order()] for the ordering the queue uses.
#' @family record linkage review functions
#' @examplesIf interactive()
#' # A hospital admissions extract and a cancer registry notification file,
#' # to be linked on name and date of birth. The hard pairs are the ones no
#' # threshold settles: a married name against a maiden name, a transposed
#' # date of birth, a nickname, and two different people who happen to share
#' # a common surname and a birth year.
#' admissions <- data.frame(
#'     frame_id = c("ADM-1001", "ADM-1002", "ADM-1003", "ADM-1004",
#'                  "ADM-1005", "ADM-1006"),
#'     surname  = c("NGUYEN", "OBRIEN", "SMITH", "PAPADOPOULOS",
#'                  "SMITH", "WILLIAMS"),
#'     given    = c("THI MINH", "MARGARET", "ROBERT", "ELENI",
#'                  "ROBERT", "WILLIAM"),
#'     dob      = as.Date(c("1962-03-14", "1948-11-02", "1979-07-23",
#'                          "1955-01-30", "1979-12-23", "1990-06-08")),
#'     sex      = c("F", "F", "M", "F", "M", "M"),
#'     postcode = c("3121", "2035", "4000", "5061", "4001", "6000")
#' )
#' registry <- data.frame(
#'     frame_id = c("REG-77", "REG-78", "REG-79", "REG-80", "REG-81"),
#'     surname  = c("NGUYEN", "O BRIEN", "SMITH", "PAPADOPOULOS", "SMYTH"),
#'     given    = c("MINH", "MARGARET MARY", "BOB", "HELEN", "ROBERT"),
#'     dob      = as.Date(c("1962-03-14", "1948-11-02", "1979-07-23",
#'                          "1955-01-30", "1979-07-23")),
#'     sex      = c("F", "F", "M", "F", "M"),
#'     postcode = c("3121", "2035", "4000", "5061", "4000")
#' )
#'
#' # Block on the year of birth so that a wrong day or month cannot hide a
#' # pair, then score the names, the date and the postcode separately: a
#' # clerical reviewer needs to see which field disagreed, not one number.
#' pairs <- rLink(
#'     admissions, registry,
#'     block_1 = list(format_date(dob, "y")),
#'     compare = list(
#'         surname  = jaro_winkler(surname),
#'         given    = jaro_winkler_tokens(given),
#'         dob      = date_compare(dob),
#'         postcode = (function(a, b) as.numeric(a == b))(postcode)
#'     )
#' )
#'
#' # Open the review at a deliberately strict cut. Everything above it is a
#' # link without anyone looking; the review is about the pairs below it.
#' out <- rLink_inspect(
#'     pairs, threshold = c(surname = 0.92, given = 0.85),
#'     decisions = file.path(tempdir(), "cancer_registry_review.json")
#' )
#'
#' # In the app: click the ADM-1002 / REG-78 row to see "OBRIEN / MARGARET"
#' # against "O BRIEN / MARGARET MARY", and mark it a link -- it comes into
#' # scope even though the surname score fell short of 0.92. Then click the
#' # ADM-1005 / REG-81 row, see that the dates of birth are five months apart
#' # in different postcodes, and mark it not a link even though both names
#' # score above the cut. Each override is reported as one rather than
#' # disappearing into the total, and each survives into:
#' out
#' pairs <- apply_decisions(
#'     pairs, file.path(tempdir(), "cancer_registry_review.json"))
#' pairs[, c("pair_id", "link", "link_reason")]
#' @export
rLink_inspect <- function(x, decisions = "rLink_decisions.json",
                          reviewer = NULL, scores = NULL, threshold = NULL,
                          max_pairs = 20000L, page_size = 100L,
                          font_size = 11L,
                          order_by = c("review", "input", "score"),
                          launch = interactive()) {
    .review_require(c(.inspect_pkgs, "bslib", "sortable"))
    .check_flag(launch, "launch")
    if (!is.character(decisions) || length(decisions) != 1L ||
        is.na(decisions) || !nzchar(decisions))
        stop("`decisions` must be a single path.")
    expression <- paste(deparse(substitute(x)), collapse = " ")
    spec <- .inspect_spec(x, file = decisions, reviewer = reviewer,
                          scores = scores, threshold = threshold,
                          max_pairs = max_pairs, page_size = page_size,
                          font_size = font_size, order_by = order_by,
                          expression = expression)

    # The run header goes down now, with any threshold the caller stated, so a
    # bad path is an error here rather than a lost afternoon, and so the file
    # records that this data was opened even if nothing is decided.
    # Every cut, not only the ones the caller named. `threshold` may set one
    # score and leave the rest at their defaults, and those defaults are in
    # force on screen; recording only the named ones wrote a threshold that
    # apply_decisions() would apply differently from what the reviewer saw --
    # and that it would apply with `rule = "all"` over a shorter list, which
    # silently widens the scope. The effect counts beside it were already
    # computed over `spec$cuts`, so the two disagreed within the one event.
    opening <- if (is.null(threshold)) list() else
        list(.inspect_threshold_event(
            spec, spec$cuts, "all", "accepted",
            "Given to rLink_inspect() when the review was opened.", 0L,
            .inspect_effect(spec, spec$cuts, "all")))
    .inspect_save(spec, list(), opening)

    app <- shiny::shinyApp(ui = .inspect_ui(spec),
                           server = .inspect_server(spec))
    if (!launch) return(invisible(app))

    # stopApp() hands back whatever "Finish and close" passed it; closing the
    # browser tab instead ends the session without a value, so the review is
    # read back here rather than lost.
    result <- shiny::runApp(app)
    if (is.null(result) || !inherits(result, "fs_decisions"))
        result <- .inspect_result(spec)
    cli::cli_alert_success(
        "Decisions saved to {.file {normalizePath(decisions, winslash = '/', mustWork = FALSE)}}.")
    cli::cli_alert_info(paste0(
        "Print the returned object for the R that reproduces and applies ",
        "them."))
    result
}
