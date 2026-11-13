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

# The identifier goes into an attribute, so it is escaped on the way: one
# holding a double quote would otherwise end the attribute early, and the
# verdict would be filed against the part of the name before the quote --
# which is no pair, so it was dropped without a word.
.inspect_js_decision <- function() reactable::JS("
function(cellInfo, state) {
  var id = cellInfo.row['pair_id'];
  var d = state.meta.decisions[id] || cellInfo.row['_decision'] || 'undecided';
  var safe = String(id).replace(/&/g, '&amp;').replace(/\"/g, '&quot;')
                       .replace(/</g, '&lt;');
  function btn(v, label, title) {
    return '<button type=\"button\" class=\"fs-btn' +
      (d === v ? ' fs-on-' + v : '') + '\" data-fs-id=\"' + safe +
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
  function attr(s) {
    return String(s).replace(/&/g, '&amp;').replace(/\"/g, '&quot;')
                    .replace(/</g, '&lt;');
  }
  return '<input type=\"text\" class=\"fs-comment\" data-fs-id=\"' +
         attr(id) + '\" value=\"' + attr(txt) +
         '\" placeholder=\"why?\">';
}")

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
#
# The focused row is the one the keyboard acts on, and it is a property of the
# pair for the same reason the marks are. reactable redraws its rows whenever
# it pages, sorts or is handed new data, and a redraw forgets every class set
# on a row from outside, so a MutationObserver repaints the focus and the marks
# after each one. It replaces a timer that repainted 60ms after any click,
# which missed every redraw a click did not cause.
#
# Clerical review is thousands of small verdicts, and the hand leaving the
# keyboard for the mouse on each of them is most of what it costs. So: j and k
# (or the arrows) move the focus, turning the page at either end; y, n and u
# judge the focused row -- or the marked set, when there is one -- and move on
# to the next row nobody has judged; 1 to 9 file a stock reason; x marks; c
# goes to the comment box; ctrl-z takes the last action back. All of it is
# ignored while a text box has the focus, so typing 'n' in a comment stays
# typing.
.inspect_js_bindings <- function() "
(function() {
  var timers = {}, anchor = null, marked = {}, advance = true, edge = null;
  function send(name, payload) {
    payload.nonce = Math.random();
    Shiny.setInputValue(name, payload, { priority: 'event' });
  }
  function idOf(row) {
    var btn = row ? row.querySelector('button.fs-btn') : null;
    return btn ? btn.getAttribute('data-fs-id') : null;
  }
  function rowId(el) {
    return idOf(el.closest ? el.closest('.rt-tr') : null);
  }
  // The rows that are pairs, in the order they are on screen. A group header
  // is a row too, and has no buttons.
  function rows() {
    var out = [];
    document.querySelectorAll('#tbl .rt-tbody .rt-tr').forEach(function(r) {
      if (idOf(r)) out.push(r);
    });
    return out;
  }
  function visibleIds() { return rows().map(idOf); }
  function judged(row) {
    return !!row.querySelector('button.fs-on-include, button.fs-on-exclude');
  }
  function paint() {
    rows().forEach(function(r) {
      var id = idOf(r);
      r.classList.toggle('fs-marked', !!marked[id]);
      r.classList.toggle('fs-focus', id === anchor);
    });
  }
  function pushMarks() {
    send('fs_marked', { pair_ids: Object.keys(marked) });
    paint();
  }
  function focus(row, quiet) {
    var id = idOf(row);
    if (!id) return;
    anchor = id;
    paint();
    if (row.scrollIntoView) row.scrollIntoView({ block: 'nearest' });
    if (!quiet) send('fs_focus', { pair_id: id });
  }
  function turn(delta) {
    var b = document.querySelector(
      delta > 0 ? '#tbl .rt-next-button' : '#tbl .rt-prev-button');
    if (!b || b.disabled) return false;
    b.click();
    return true;
  }
  function at() {
    var list = rows();
    for (var i = 0; i < list.length; i++) if (idOf(list[i]) === anchor) return i;
    return -1;
  }
  function move(delta) {
    var list = rows();
    if (!list.length) return;
    var i = at();
    if (i < 0) { focus(list[delta > 0 ? 0 : list.length - 1]); return; }
    var to = i + delta;
    if (to >= 0 && to < list.length) { focus(list[to]); return; }
    // Off the end of the page: turn it, and land on the near edge of the
    // next one once it has been drawn.
    if (turn(delta)) edge = delta > 0 ? 'first' : 'last';
  }
  function nextUndecided() {
    var list = rows(), i = at();
    for (var k = i + 1; k < list.length; k++)
      if (!judged(list[k])) { focus(list[k]); return; }
    if (turn(1)) edge = 'undecided';
  }
  function settle() {
    if (edge) {
      var list = rows();
      if (list.length) {
        var pick = edge === 'last' ? list[list.length - 1] : list[0];
        if (edge === 'undecided')
          for (var k = 0; k < list.length; k++)
            if (!judged(list[k])) { pick = list[k]; break; }
        edge = null;
        focus(pick);
        return;
      }
    }
    paint();
  }
  // Repaint after every redraw of the table, whatever caused it.
  var redraw = null;
  function watch() {
    if (!document.body || !window.MutationObserver) return;
    new MutationObserver(function(changes) {
      for (var i = 0; i < changes.length; i++) {
        var t = changes[i].target;
        if (t && t.closest && t.closest('#tbl')) {
          clearTimeout(redraw);
          redraw = setTimeout(settle, 30);
          return;
        }
      }
    }).observe(document.body, { childList: true, subtree: true });
  }
  if (document.body) watch();
  else document.addEventListener('DOMContentLoaded', watch);

  // Capture phase, so this runs before reactable's own cell handlers.
  document.addEventListener('click', function(e) {
    if (!e.target.closest) return;
    var rival = e.target.closest('.fs-rival');
    if (rival) {
      e.preventDefault();
      anchor = rival.getAttribute('data-fs-id');
      paint();
      send('fs_focus', { pair_id: anchor });
      return;
    }
    var chip = e.target.closest('.fs-reason');
    if (chip) {
      e.preventDefault();
      send('fs_reason', { pair_ids: [chip.getAttribute('data-fs-id')],
                          index: parseInt(chip.getAttribute('data-fs-reason'), 10) });
      return;
    }
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
      paint();
      send('fs_focus', { pair_id: id });
    }
  }, true);
  // Shift-clicking a table would otherwise highlight text across it.
  document.addEventListener('mousedown', function(e) {
    if (e.shiftKey && e.target.closest && e.target.closest('#tbl'))
      e.preventDefault();
  });
  Shiny.addCustomMessageHandler('fs_clear_marks', function(msg) {
    marked = {}; paint();
  });
  // The server moving the focus: a rival opened from the card, or an undo.
  Shiny.addCustomMessageHandler('fs_set_focus', function(msg) {
    anchor = msg.pair_id; paint();
  });
  Shiny.addCustomMessageHandler('fs_options', function(msg) {
    if (msg.advance !== undefined) advance = !!msg.advance;
  });

  function typing(t) {
    return t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' ||
                 t.tagName === 'SELECT' || t.isContentEditable);
  }
  function targets() {
    var ids = Object.keys(marked);
    if (!ids.length && anchor) ids = [anchor];
    return ids;
  }
  document.addEventListener('keydown', function(e) {
    var t = e.target;
    var k = e.key ? e.key.toLowerCase() : '';
    if (typing(t)) {
      // Escape hands the keyboard back to the table.
      if (k === 'escape' && t.blur) t.blur();
      return;
    }
    if ((e.ctrlKey || e.metaKey) && !e.altKey && k === 'z') {
      e.preventDefault();
      send('fs_undo', {});
      return;
    }
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    if (k === 'j' || k === 'arrowdown') { e.preventDefault(); move(1); return; }
    if (k === 'k' || k === 'arrowup') { e.preventDefault(); move(-1); return; }
    if (k === '?') { e.preventDefault(); send('fs_help', {}); return; }
    if (k === 'x') {
      if (!anchor) return;
      e.preventDefault();
      if (marked[anchor]) delete marked[anchor]; else marked[anchor] = true;
      pushMarks();
      return;
    }
    if (k === 'c') {
      var box = document.getElementById('fs_card_comment');
      if (box) { e.preventDefault(); box.focus(); box.select(); }
      return;
    }
    if (k.length === 1 && k >= '1' && k <= '9') {
      var who = targets();
      if (!who.length) return;
      e.preventDefault();
      send('fs_reason', { pair_ids: who, index: parseInt(k, 10) });
      return;
    }
    var value = k === 'y' ? 'include' :
                k === 'n' ? 'exclude' :
                k === 'u' ? 'undecided' : null;
    if (!value) return;
    var ids = targets();
    if (!ids.length) return;
    e.preventDefault();
    var single = !Object.keys(marked).length;
    send('fs_keyjudge', { pair_ids: ids, decision: value });
    // A marked set is judged once, like the buttons: left marked, the next
    // key meant for one row would be applied to all of them again.
    if (!single) { marked = {}; pushMarks(); return; }
    // Only a real verdict moves on: 'u' is taking one back, and the reviewer
    // is still looking at that pair.
    if (advance && value !== 'undecided') nextUndecided();
  });
  document.addEventListener('click', function(e) {
    var b = e.target.closest ? e.target.closest('button.fs-btn') : null;
    if (!b) return;
    send('fs_decision', { pair_id: b.getAttribute('data-fs-id'),
                          decision: b.getAttribute('data-fs-value') });
  });
  // `live` says the reviewer is still typing. The server files the comment
  // either way, but only redraws around it once they have left the box:
  // redrawing a text box under a cursor takes the cursor away.
  function comment(el, live) {
    send('fs_comment', { pair_id: el.getAttribute('data-fs-id'),
                         comment: el.value, live: live });
  }
  document.addEventListener('input', function(e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains('fs-comment')) return;
    var id = el.getAttribute('data-fs-id');
    clearTimeout(timers[id]);
    timers[id] = setTimeout(function() { comment(el, true); }, 600);
  });
  document.addEventListener('change', function(e) {
    var el = e.target;
    if (!el.classList || !el.classList.contains('fs-comment')) return;
    clearTimeout(timers[el.getAttribute('data-fs-id')]);
    comment(el, false);
  });
})();
"

# Scoped to #fs-inspect, so embedding the app in a larger page cannot be
# restyled by it. The table's own font comes from a CSS custom property, so
# changing the size re-renders one tiny style tag instead of the table.
#
# The sidebar is not inside #fs-inspect -- bslib puts it beside the page's
# content, not within it -- so every rule is written a second time for the
# class the sidebar is given. Without that the font size, the compact spacing
# and the note boxes these rules were written for reached none of the
# controls they were written about.
.inspect_css <- function(font_size) {
    css <- .inspect_css_rules(font_size)
    paste0(css, gsub("#fs-inspect", ".fs-side", css, fixed = TRUE))
}

.inspect_css_rules <- function(font_size) sprintf("
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
  align-items: baseline; margin-bottom: 4px; position: sticky; top: 0;
  z-index: 6; background: #fff; padding: 3px 0;
  border-bottom: 1px solid #e6eaee; }
#fs-inspect .fs-head #fs_kpi { display: inline-flex; gap: 14px;
  flex-wrap: wrap; align-items: baseline; }
#fs-inspect .fs-kpi { font-weight: 600; }
#fs-inspect .fs-kpi-forced { color: #8a4b00; }
#fs-inspect .fs-kpi-band { color: #1b4f8a; }
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
#fs-inspect .rt-tr.fs-focus .rt-td { opacity: 1;
  box-shadow: inset 0 1px 0 0 #1b4f8a, inset 0 -1px 0 0 #1b4f8a; }
#fs-inspect .rt-tr.fs-focus .rt-td:first-child {
  box-shadow: inset 3px 0 0 0 #1b4f8a, inset 0 1px 0 0 #1b4f8a,
              inset 0 -1px 0 0 #1b4f8a; }
#fs-inspect .fs-scope-in { color: #0b6b2e; font-weight: 600; }
#fs-inspect .fs-scope-out { color: #8a8a8a; }
#fs-inspect .fs-scope-in.fs-forced, #fs-inspect .fs-scope-out.fs-forced {
  color: #8a4b00; font-weight: 600; }
#fs-inspect .rt-tr.fs-forced-row .rt-td:first-child {
  box-shadow: inset 3px 0 0 0 #d98300; }
#fs-inspect .rt-tr.fs-forced-row .rt-td { opacity: 1; }
#fs-inspect .fs-mono { font-family: monospace; }
#fs-inspect .fs-main { display: grid; gap: 8px; align-items: start;
  grid-template-columns: minmax(0, 1fr) minmax(300px, 30%%); }
#fs-inspect .fs-card { position: sticky; top: 34px; overflow: auto;
  max-height: calc(100vh - 44px); border: 1px solid #d8dee4;
  border-radius: 4px; padding: 6px 8px; background: #fff; }
#fs-inspect .fs-card h6 { font-size: var(--fs-font); font-weight: 700;
  margin: 8px 0 2px; }
#fs-inspect .fs-card-id { font-family: monospace; font-weight: 600;
  word-break: break-all; }
#fs-inspect .fs-card-meta { color: #666; }
#fs-inspect table.fs-ev { width: 100%%; border-collapse: collapse;
  margin: 4px 0; }
#fs-inspect table.fs-ev th { text-align: left; font-weight: 600;
  padding: 1px 4px 1px 0; vertical-align: top; white-space: nowrap; }
#fs-inspect table.fs-ev td { padding: 1px 4px; vertical-align: top;
  border-top: 1px solid #eee; word-break: break-word; }
#fs-inspect table.fs-ev td.fs-ev-score { text-align: right;
  font-variant-numeric: tabular-nums; white-space: nowrap; }
#fs-inspect table.fs-ev tr.fs-ev-other th { font-weight: 400; color: #555; }
#fs-inspect table.fs-ev thead th { color: #666; font-weight: 400;
  border-bottom: 1px solid #d8dee4; }
#fs-inspect mark.fs-diff { background: #ffd8a8; color: inherit;
  padding: 0; border-radius: 2px; }
#fs-inspect .fs-bits { color: #666; }
#fs-inspect .fs-ev-verdict { color: #555; margin-bottom: 2px; }
#fs-inspect .fs-ev-act { display: flex; gap: 3px; flex-wrap: wrap;
  margin: 4px 0; }
#fs-inspect a.fs-rival { cursor: pointer; text-decoration: underline;
  font-family: monospace; }
#fs-inspect button.fs-reason { font-size: calc(var(--fs-font) - 1px);
  padding: 0 5px; border: 1px solid #bbb; background: #f4f6f8;
  border-radius: 9px; cursor: pointer; }
#fs-inspect kbd { font-size: calc(var(--fs-font) - 1px); padding: 0 3px;
  background: #eef0f2; color: #222; border: 1px solid #c8ccd0;
  border-radius: 3px; }
#fs-inspect .rt-table { font-variant-numeric: tabular-nums; }
@media (max-width: 1100px) {
  #fs-inspect .fs-main { grid-template-columns: minmax(0, 1fr); }
  #fs-inspect .fs-card { position: static; max-height: none; }
}
", as.integer(font_size))

# One colDef per column: scores gradient-filled, the identity and scope columns
# pinned left so a row never loses its name when scrolled sideways, and the two
# control columns unsortable.
.inspect_columns <- function(spec, selected) {
    cut_on <- spec$score_spec$score
    # Every score is drawn as one, with its gradient; only the ones a cut is
    # set on carry the cut's operator in their header.
    scores <- union(cut_on, spec$evidence_spec$score)
    nms <- names(spec$view)
    cols <- list()
    for (nm in intersect(selected, nms)) {
        if (nm %in% scores) {
            k <- match(nm, cut_on)
            cols[[nm]] <- reactable::colDef(
                name = nm, style = .inspect_js_score(),
                na = .inspect_na_label, align = "right", minWidth = 62,
                header = if (is.na(k)) nm else
                    sprintf("%s %s", nm, spec$score_spec$operator[k]),
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

# --- the pair card ---------------------------------------------------------

# Which fields the card shows, and in what order: the compared ones first, with
# the score each earned, then everything else that was carried under both
# suffixes, then whatever only one input has. Worked out once per review,
# because it is a property of the columns and not of any one pair.
#
# A reviewer deciding whether two records are one person reads the address and
# the sex as well as the three fields somebody chose to score, and the evidence
# pane used to show only those three.
.inspect_card_fields <- function(source, evidence_spec, suffixes, lead) {
    nms <- names(source)
    rows <- list()
    used <- character()
    two_sided <- length(suffixes) == 2L
    # Compared fields, one row per pair of columns: jaro_winkler(surname) and
    # levenshtein(surname) are two scores of one field, not two fields.
    for (k in seq_len(nrow(evidence_spec))) {
        s <- evidence_spec$score[k]
        c1 <- evidence_spec$column_1[k]
        c2 <- evidence_spec$column_2[k]
        n1 <- if (is.na(c1) || !two_sided) NA_character_
              else paste0(c1, suffixes[1L])
        n2 <- if (is.na(c2) || !two_sided) NA_character_
              else paste0(c2, suffixes[2L])
        if (!is.na(n1) && !n1 %in% nms) n1 <- NA_character_
        if (!is.na(n2) && !n2 %in% nms) n2 <- NA_character_
        bare <- is.na(c1) && is.na(c2)
        field <- if (bare) s
                 else if (identical(c1, c2)) c1 else paste0(c1, " | ", c2)
        key <- paste(field, n1, n2)
        if (!is.null(rows[[key]])) {
            rows[[key]]$scores <- c(rows[[key]]$scores, s)
        } else {
            rows[[key]] <- list(field = field, col_1 = n1, col_2 = n2,
                                scores = s,
                                kind = if (bare) "score" else "compared")
            used <- c(used, n1, n2)
        }
    }
    rest <- setdiff(nms, c(used, lead, evidence_spec$score))
    if (two_sided) {
        strip <- function(x, suffix)
            substr(x, 1L, nchar(x) - nchar(suffix))
        base_1 <- strip(rest[endsWith(rest, suffixes[1L])], suffixes[1L])
        base_2 <- strip(rest[endsWith(rest, suffixes[2L])], suffixes[2L])
        for (b in union(base_1, base_2))
            rows[[paste("carried", b)]] <- list(
                field = b,
                col_1 = if (b %in% base_1) paste0(b, suffixes[1L])
                        else NA_character_,
                col_2 = if (b %in% base_2) paste0(b, suffixes[2L])
                        else NA_character_,
                scores = character(), kind = "carried")
    }
    # Compared, then the scores that are about no one field, then the rest.
    kinds <- vapply(rows, `[[`, "", "kind")
    unname(rows[order(match(kinds, c("compared", "score", "carried")))])
}

.inspect_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
}

# The two values of a field with what differs between them marked, as HTML.
# "MACDONALD" against "MCDONALD" is one dropped letter and
# "CHOWDHURY" against "CHOUDHARI" is three substitutions, and seeing which
# is the difference between a typing error and another person. edit_ops()
# already knows; this only draws what it says.
#
# Values are compared without regard to case, as rLink() compares them by
# default, so "Smith" against "SMITH" is not painted as five differences.
# Long values are left alone: an alignment of two addresses is noise.
.inspect_diff <- function(a, b, max_chars = 120L) {
    plain <- list(a = .inspect_html(.inspect_shown(a)),
                  b = .inspect_html(.inspect_shown(b)))
    if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(plain)
    if (nchar(a) > max_chars || nchar(b) > max_chars) return(plain)
    ua <- toupper(a)
    ub <- toupper(b)
    # Upper-casing can change a string's length (a sharp s becomes two
    # letters), and the positions would then be into the wrong string.
    if (nchar(ua) != nchar(a) || nchar(ub) != nchar(b)) {
        ua <- a
        ub <- b
    }
    if (identical(ua, ub)) return(plain)
    ops <- tryCatch(edit_ops(ua, ub, type = "opcodes"),
                    error = function(cnd) NULL)
    if (is.null(ops) || !nrow(ops)) return(plain)
    side <- function(value, from, to) {
        piece <- substring(value, from, to)
        html <- .inspect_html(piece)
        changed <- ops$type != "equal" & nzchar(piece)
        html[changed] <- paste0("<mark class=\"fs-diff\">", html[changed],
                                "</mark>")
        paste(html, collapse = "")
    }
    list(a = side(a, ops$a_start, ops$a_end),
         b = side(b, ops$b_start, ops$b_end))
}

# What a fitted model made of one pair, field by field: the agreement level
# each score reached and the bits that level is worth. They add up to the
# pair's weight, which is the point -- a weight of 14 is a number, and "the
# date of birth gave it 8 and the surname took 3 away" is a reason.
.inspect_bits <- function(model, values) {
    if (is.null(model) || is.null(model$fit)) return(NULL)
    scores <- intersect(model$scores, names(values))
    if (!length(scores)) return(NULL)
    out <- vapply(scores, function(s) {
        v <- as.double(values[[s]])
        if (is.na(v)) return(NA_real_)
        level <- compare_levels(v, model$breaks[[s]],
                                decreasing = isTRUE(model$decreasing[[s]]))
        m <- model$fit$m[[s]]
        u <- model$fit$u[[s]]
        if (level + 1L > length(m)) return(NA_real_)
        log2(m[level + 1L] / u[level + 1L])
    }, numeric(1L))
    names(out) <- scores
    out
}

# Everything the card says about one pair, as plain data: `at` is its row among
# all the pairs.
.inspect_card <- function(spec, at) {
    source <- spec$source
    text <- function(col) if (is.na(col)) NA_character_ else {
        v <- source[[col]][at]
        if (inherits(v, "Date")) format(v) else as.character(v)
    }
    values <- lapply(spec$evidence_spec$score, function(s)
        as.double(spec$evidence[[s]][at]))
    names(values) <- spec$evidence_spec$score
    bits <- .inspect_bits(spec$model, values)
    rows <- lapply(spec$card_fields, function(f) list(
        field = f$field, kind = f$kind,
        value_1 = text(f$col_1), value_2 = text(f$col_2),
        has_1 = !is.na(f$col_1), has_2 = !is.na(f$col_2),
        scores = values[f$scores],
        bits = if (is.null(bits)) NULL
               else bits[intersect(f$scores, names(bits))]))
    lead <- lapply(intersect(c("pass", "block_key"), names(source)),
                   function(nm) as.character(source[[nm]][at]))
    names(lead) <- intersect(c("pass", "block_key"), names(source))
    list(pair_id = spec$all_ids[at], lead = lead, rows = rows, bits = bits)
}

# The other candidates for the same two records. Whether a pair is a link is
# rarely a question about that pair alone: it is whether this is the *best*
# candidate for the record, and a reviewer shown one pair at a time cannot
# tell that the claim they are about to link has a better match two pages
# down.
#
# Deduplicating, a record can be on either side of a pair, so both sides are
# searched for each record; linking two files, the same row number on the
# other side is a different record and must not be.
.inspect_rivals <- function(index_1, index_2, at, dedupe = FALSE) {
    i <- index_1[at]
    j <- index_2[at]
    with_1 <- if (dedupe) which(index_1 == i | index_2 == i)
              else which(index_1 == i)
    with_2 <- if (dedupe) which(index_1 == j | index_2 == j)
              else which(index_2 == j)
    rows <- setdiff(union(with_1, with_2), at)
    data.frame(row = rows,
               shares = ifelse(rows %in% with_1, 1L, 2L),
               stringsAsFactors = FALSE)
}


# --- the review band, the queue and the audit sample -----------------------

# Which pairs are in the review band: scoring between the two limits on the
# score being reviewed. The band is where a threshold cannot be trusted and a
# person has to look -- above it the pairs are taken as links, below it as
# not, and how many are left inside it is how much work is left.
.inspect_in_band <- function(v, band) {
    if (is.null(band) || length(band) != 2L || anyNA(band))
        return(rep(FALSE, length(v)))
    v <- as.double(v)
    !is.na(v) & v >= min(band) & v <= max(band)
}

# The audit sample: the `n` pairs passing the cut whose identifiers hash
# smallest. A hash is as good as a random number for choosing a sample and
# better in every other way -- the same pair is in or out whoever opens the
# review and whenever, the sample grows by adding pairs rather than by being
# redrawn, and the caller's random stream is not touched, which a package
# has no business doing. `hash_order` is the pairs in hash order, computed
# once.
.inspect_audit_rows <- function(hash_order, pass, n) {
    n <- max(0L, as.integer(n))
    if (!n) return(integer())
    passing <- hash_order[pass[hash_order]]
    passing[seq_len(min(n, length(passing)))]
}

# The share of the audited pairs that a reviewer judged not to be links,
# which estimates the false-link rate of everything the cut let through. The
# interval is Wilson's: it behaves at 0 of 40, where the usual one says the
# rate is exactly zero.
.inspect_audit <- function(verdict) {
    judged <- verdict %in% c(1L, 2L)
    n <- sum(judged)
    k <- sum(verdict[judged] == 2L)
    out <- list(n_sample = length(verdict), n_judged = n, n_not_link = k,
                rate = NA_real_, lower = NA_real_, upper = NA_real_)
    if (!n) return(out)
    z <- stats::qnorm(0.975)
    p <- k / n
    denom <- 1 + z^2 / n
    centre <- (p + z^2 / (2 * n)) / denom
    half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
    out$rate <- p
    out$lower <- max(0, centre - half)
    out$upper <- min(1, centre + half)
    out
}

# The cut-offs a sweep is drawn over. Every distinct score when there are few,
# which is exact and is what a small review wants. A continuous score over
# millions of pairs has millions of distinct values, and a sweep over all of
# them is a table of millions of rows sent to a browser to draw a curve two
# hundred points would draw as well; so it is swept at quantiles instead,
# which puts the cut-offs where the pairs are.
.inspect_sweep_grid <- function(v, n = 200L) {
    v <- as.double(v)
    v <- v[!is.na(v)]
    if (length(v) <= n) return(NULL)
    probe <- if (length(v) > 100000L)
        v[seq.int(1L, length(v), length.out = 100000L)] else v
    if (length(unique(probe)) <= n && length(unique(v)) <= n) return(NULL)
    unique(stats::quantile(v, probs = seq(0, 1, length.out = n), type = 1L,
                           names = FALSE))
}

# The cut's verdict with the reviewer's laid over it, from the verdicts held
# as one code per pair (1 a link, 2 not). It is .inspect_scope() without the
# lookup by name, which is what made every click cost a pass over every
# identifier.
#
# `judged` is the pairs that hold a verdict, when the caller knows them: then
# only those are looked at, and laying a few thousand verdicts over ten
# million pairs costs a few thousand comparisons rather than twenty million.
.inspect_scope_at <- function(pass, verdict, judged = NULL) {
    out <- pass
    if (is.null(judged)) {
        out[verdict == 1L] <- TRUE
        out[verdict == 2L] <- FALSE
        return(out)
    }
    said <- verdict[judged]
    out[judged[said == 1L]] <- TRUE
    out[judged[said == 2L]] <- FALSE
    out
}

.inspect_verdicts <- c("include", "exclude", "undecided")

# Everything validated and prepared before shinyApp() is built, so a bad input
# is a plain error at the console rather than a grey box in a browser.
.inspect_spec <- function(x, file, reviewer, scores, threshold, max_pairs,
                          page_size, font_size, order_by, expression,
                          band = NULL, reasons = NULL) {
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
    if (!is.null(reasons) &&
        (!is.character(reasons) || anyNA(reasons) || !all(nzchar(reasons)) ||
         anyDuplicated(reasons) || length(reasons) > 9L))
        stop(paste0("`reasons` must be up to nine different, non-empty ",
                    "strings: one for each of the keys 1 to 9."),
             call. = FALSE)

    # Two sets of scores, which were one until a model could be fitted. The
    # *evidence* is every score there is: each is drawn with its gradient and
    # shown on the card. The *threshold* scores are the ones with a slider.
    # With weights from rLink_weights() and nothing said otherwise, that is
    # the match probability alone -- which is the point of fitting a model:
    # one cut on one number, with the field scores as the reasons behind it.
    model <- attr(x, "model")
    if (!is.list(model) || is.null(model$fit)) model <- NULL
    modelled <- is.null(scores) && !is.null(model) &&
        "probability" %in% nms && is.numeric(x[["probability"]])
    score_spec <- .inspect_scores(x, if (modelled) "probability" else scores)
    evidence_spec <- tryCatch(.inspect_scores(x, NULL),
                              error = function(cnd) score_spec)
    extra <- setdiff(score_spec$score, evidence_spec$score)
    if (length(extra))
        evidence_spec <- rbind(
            evidence_spec,
            score_spec[match(extra, score_spec$score), , drop = FALSE])
    rownames(evidence_spec) <- NULL

    # The columns are shared with `x`, not copied: as.data.frame() on a
    # data.table of ten million pairs duplicates every one of them to hand
    # back the same values, and the review only ever reads.
    source <- lapply(seq_along(x), function(k) x[[k]])
    names(source) <- nms
    source <- structure(source, class = "data.frame",
                        row.names = .set_row_names(nrow(x)),
                        compare = attr(x, "compare"))

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
    # A probability's natural cut is an even chance, not the 0.85 that suits
    # a string similarity.
    if (modelled) {
        lo <- score_spec$observed_min[1L]
        hi <- score_spec$observed_max[1L]
        if (is.finite(lo) && is.finite(hi))
            cuts[["probability"]] <- min(hi, max(lo, 0.5))
    }
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
    if (!is.null(band)) {
        if (!is.numeric(band) || length(band) != 2L || anyNA(band) ||
            any(!is.finite(band)) || band[1L] == band[2L])
            stop(paste0("`band` must be two different numbers: the lower and ",
                        "upper limits of the scores to be reviewed by hand."),
                 call. = FALSE)
        band <- sort(as.double(band))
    }

    # The analysis reads every pair; only the table is capped. Thresholds, the
    # sweep and the counts therefore describe the whole input, and the UI says
    # outright how much of it is on screen.
    as_values <- function(names) {
        out <- lapply(names, function(s) as.double(x[[s]]))
        names(out) <- names
        structure(out, class = "data.frame",
                  row.names = .set_row_names(nrow(x)))
    }
    all <- as_values(score_spec$score)
    evidence <- as_values(evidence_spec$score)

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

    suffixes <- sub("^index", "", at)
    lead <- c("pair_id", at, "pass", "block_key")
    # The distributions are drawn once here and only the cut line is redrawn
    # as it moves: a histogram of ten million scores on every tick of a
    # slider was most of what made the slider feel heavy.
    hists <- lapply(evidence_spec$score, function(s) {
        v <- evidence[[s]]
        v <- v[is.finite(v)]
        if (!length(v)) return(NULL)
        graphics::hist(v, breaks = 50, plot = FALSE)
    })
    names(hists) <- evidence_spec$score

    list(all = all, evidence = evidence, all_ids = ids, source = source,
         view = view, shown = shown, score_spec = score_spec,
         evidence_spec = evidence_spec, index_cols = at,
         suffixes = suffixes, cuts = cuts, reviewer = reviewer,
         file = file, n_pairs = nrow(x), n_shown = n_shown,
         max_pairs = as.integer(max_pairs),
         truncated = n_shown < nrow(x), pair_id_source = kind,
         page_size = as.integer(page_size), font_size = as.integer(font_size),
         order_by = order_by, expression = expression,
         band = band, reasons = reasons, model = model, modelled = modelled,
         dedupe = isTRUE(attr(x, "dedupe")), hists = hists,
         card_fields = .inspect_card_fields(source, evidence_spec, suffixes,
                                            lead),
         fingerprints = .review_fingerprints(with_ids, score_spec$score,
                                             "pair_id"),
         run_id = .review_run_id(), started = .review_now())
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
    # When the review was opened, not when it was last saved: the header is
    # rewritten on every save, and stamping it afresh each time made every
    # session look as though it had started the moment it ended.
    started = if (is.null(spec$started)) .review_now() else spec$started,
    ended = NULL,
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

# `band` and `audit` are optional and additive: a file written before they
# existed has neither, and a reader that does not know them ignores them. They
# are filed with the threshold because they are what it was accepted on --
# "0.82, having looked at everything from 0.75 to 0.9 and found 3 wrong links
# in a sample of 200" is a different claim from "0.82".
.inspect_threshold_event <- function(spec, cuts, rule, status, note, seq,
                                     effect = NULL, expr = NULL, band = NULL,
                                     band_score = NULL, audit = NULL) {
    has_band <- !is.null(band) && length(band) == 2L && !anyNA(band)
    has_audit <- !is.null(audit) && isTRUE(audit$n_judged > 0)
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
         band_score = if (has_band) band_score,
         band_lower = if (has_band) min(band),
         band_upper = if (has_band) max(band),
         audit_sample = if (has_audit) audit$n_sample,
         audit_judged = if (has_audit) audit$n_judged,
         audit_not_link = if (has_audit) audit$n_not_link,
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

# What each key does, in the order a reviewer learns them. Shown in the
# sidebar and on `?`.
.inspect_keys <- function(reasons = NULL) {
    key <- function(k, what) shiny::tags$tr(
        shiny::tags$td(lapply(k, shiny::tags$kbd)), shiny::tags$td(what))
    shiny::tags$table(
        class = "fs-ev",
        key(c("j", "k"), "next and previous pair (the arrow keys too)"),
        key(c("y", "n"), "a link, not a link -- then on to the next undecided"),
        key("u", "undecided again"),
        key("x", "mark or unmark the pair; ctrl-click and shift-click too"),
        key("c", "write a comment (Esc to come back)"),
        if (length(reasons)) key(paste0("1-", length(reasons)),
                                 "file one of the stock reasons"),
        key("Ctrl+Z", "take the last action back"),
        key("?", "this list"))
}

.inspect_ui <- function(spec) {
    scores <- spec$score_spec
    sliders <- lapply(seq_len(nrow(scores)), function(k) {
        lo <- scores$observed_min[k]
        hi <- scores$observed_max[k]
        if (!is.finite(lo) || !is.finite(hi)) { lo <- 0; hi <- 1 }
        lo <- min(lo, spec$cuts[k], na.rm = TRUE)
        hi <- max(hi, spec$cuts[k], na.rm = TRUE)
        if (hi <= lo) hi <- lo + 1
        # The ends of the slider on round numbers that still hold the range:
        # the smallest score observed is a number like 0.0115710746272388,
        # and that was the label at the left-hand end.
        unit <- signif((hi - lo) / 100, 1)
        lo <- round(floor(lo / unit) * unit, 10)
        hi <- round(ceiling(hi / unit) * unit, 10)
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
        width = 330, open = "open", id = "side", class = "fs-side",
        bslib::accordion(
            multiple = TRUE, class = "fs-acc",
            open = c("p_thresh", "p_queue"),
            bslib::accordion_panel(
                "Threshold", value = "p_thresh", icon = NULL,
                if (spec$modelled) shiny::helpText(paste0(
                    "Weights from rLink_weights(): the cut is on the match ",
                    "probability, and the field scores are shown as the ",
                    "evidence behind it.")),
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
            # What to look at, which is a different question from where the
            # cut is. The band is the range of scores a threshold cannot be
            # trusted to settle; the queue is the pairs put on screen, and it
            # can be drawn again once the cut has moved away from where it
            # was when the review opened.
            bslib::accordion_panel(
                "Review queue", value = "p_queue",
                # Which score the band, the queue, the sweep and the
                # suggestion are about. One score rather than all of them,
                # because each of those is a question in one dimension; but a
                # chosen one, since it is the second and third fields where a
                # reviewer has least idea where the cut belongs.
                if (nrow(scores) > 1L) shiny::selectInput(
                    "sweep_score", "Score to review by",
                    choices = scores$score, selected = scores$score[1L],
                    width = "100%"),
                shiny::tags$strong("Review band"),
                shiny::helpText(paste0(
                    "Pairs scoring between these are the ones to look at by ",
                    "hand. Leave blank for none.")),
                shiny::fluidRow(
                    shiny::column(6, shiny::numericInput(
                        "band_lo", "from",
                        if (is.null(spec$band)) NA else spec$band[1L])),
                    shiny::column(6, shiny::numericInput(
                        "band_hi", "to",
                        if (is.null(spec$band)) NA else spec$band[2L]))),
                shiny::selectInput(
                    "queue_mode", "Put on screen",
                    c("the pairs nearest the cut" = "cut",
                      "the review band" = "band",
                      "an audit sample of the pairs in scope" = "audit"),
                    selected = "cut", width = "100%"),
                shiny::conditionalPanel(
                    "input.queue_mode == 'audit'",
                    shiny::numericInput("audit_n", "Pairs to sample", 200,
                                        min = 10, max = spec$max_pairs,
                                        step = 50),
                    shiny::helpText(paste0(
                        "A fixed, repeatable sample of the pairs passing the ",
                        "cut. Judge them and the header estimates how many ",
                        "of the links are wrong."))),
                shiny::actionButton("rebuild", "Rebuild the queue",
                                    class = "btn-sm btn-primary"),
                shiny::actionButton("rerank", "Re-rank by how undecided",
                                    class = "btn-sm"),
                shiny::uiOutput("fs_queue")),
            bslib::accordion_panel(
                "Marked rows", value = "p_marked",
                shiny::helpText(paste0(
                    "Ctrl-click rows to mark them, shift-click for a range, ",
                    "then judge them all at once. Or press y, n or u to ",
                    "judge the marked rows -- or, with none marked, the row ",
                    "in focus.")),
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
                                      "in the review band, undecided" = "band",
                                      "marked a link" = "include",
                                      "marked not a link" = "exclude",
                                      "overriding the cut" = "override"),
                                    selected = "all"),
                shiny::checkboxInput("group_scope",
                                     "Group by in scope / out", TRUE),
                shiny::checkboxInput("show_card",
                                     "The pair beside the table", TRUE),
                shiny::checkboxInput("advance",
                                     "Move on after judging from the keyboard",
                                     TRUE),
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
                "Keys", value = "p_keys",
                .inspect_keys(spec$reasons)),
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
                shiny::uiOutput("fs_kpi", inline = TRUE),
                shiny::actionButton("undo", "Undo", class = "btn-sm",
                                    title = "Take the last action back (Ctrl+Z)")),
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
            # The table and the pair it is on, side by side. The pair used to
            # live in a panel of the settings sidebar, 330 pixels wide and
            # closed by default, which is a poor place for the one thing a
            # clerical reviewer is actually reading.
            shiny::tags$div(
                class = "fs-main",
                shiny::tags$div(reactable::reactableOutput("tbl")),
                shiny::tags$div(
                    class = "fs-card",
                    shiny::uiOutput("fs_evidence"))),
            shiny::tags$br(),
            bslib::accordion(
                multiple = TRUE, open = FALSE, class = "fs-acc",
                bslib::accordion_panel(
                    "Threshold sweep", value = "a_sweep",
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
    n_all <- spec$n_pairs
    n_scores <- nrow(spec$score_spec)
    operators <- spec$score_spec$operator
    score_names <- spec$score_spec$score
    index_1 <- spec$source[[spec$index_cols[1L]]]
    index_2 <- spec$source[[spec$index_cols[2L]]]
    control_cols <- c("_scope", "_threshold", "_decision", "_comment")
    data_cols <- setdiff(names(spec$view), control_cols)

    # Which of all the pairs an identifier is. chmatch() rather than match():
    # match() builds a hash of the table on every call, and the table is
    # every pair there is.
    row_of <- function(ids) data.table::chmatch(as.character(ids),
                                                spec$all_ids)

    # --- what has been decided --------------------------------------------
    # The verdicts are one code per pair, held outside the reactive graph,
    # with a single counter to say they have changed. They used to be a named
    # list in a reactive value: every verdict copied the list, appended to
    # the event log by concatenation and looked its pair up among every
    # identifier, so marking five hundred rows was five hundred of each. Now
    # a batch is one assignment, and what depends on the verdicts reads a
    # vector.
    seeded <- .inspect_seed(spec)
    st <- new.env(parent = emptyenv())
    st$verdict <- integer(n_all)
    st$comments <- character()
    st$events <- vector("list", 64L)
    st$n_events <- 0L
    st$seq <- 0L
    st$touched_decisions <- character()
    st$touched_comments <- character()
    st$undo <- list()
    st$columns <- NULL
    st$hash_order <- NULL
    # Which pairs hold a verdict at all: a few thousand of however many
    # million, and the only ones whose scope can differ from the cut.
    st$judged <- integer()
    # What was last written and the text of every event in it, begun when
    # the review was opened, so the first save of the session encodes only
    # what the session has added.
    st$save_cache <- if (is.environment(spec$save_cache)) spec$save_cache
                     else new.env(parent = emptyenv())
    if (length(seeded$decisions)) {
        at <- row_of(names(seeded$decisions))
        code <- match(unlist(seeded$decisions, use.names = FALSE),
                      .inspect_verdicts)
        ok <- !is.na(at) & !is.na(code)
        st$verdict[at[ok]] <- code[ok]
        st$judged <- unique(at[ok])
    }
    if (length(seeded$comments))
        st$comments <- unlist(seeded$comments)

    # --- a second look ----------------------------------------------------
    # What anyone else has judged, as positions among the pairs. A review is
    # checked by a second person going over the first one's verdicts, and
    # the queue is the pairs nearest the cut -- which is where the first
    # reviewer worked, but not a list of what they decided. So the second
    # reviewer can ask for exactly those pairs, see how far through them
    # they are, and list the ones the two of them disagree on. Their own
    # verdicts still start blank: this says where to look, not what to say.
    others_at <- row_of(seeded$others$pair_id)
    others_said <- match(seeded$others$decision, .inspect_verdicts)
    others_rows <- unique(others_at)
    has_others <- length(others_rows) > 0L
    disagreeing <- function() {
        mine <- st$verdict[others_at]
        unique(others_at[mine %in% c(1L, 2L) & mine != others_said])
    }
    if (has_others) {
        shiny::updateRadioButtons(
            session, "filter", selected = "all",
            choices = c("all pairs" = "all", "in scope" = "in_scope",
                        "out of scope" = "out_scope",
                        "undecided" = "undecided",
                        "in the review band, undecided" = "band",
                        "marked a link" = "include",
                        "marked not a link" = "exclude",
                        "overriding the cut" = "override",
                        "judged by another reviewer" = "others",
                        "where I and another reviewer disagree" = "disagree"))
        shiny::updateSelectInput(
            session, "queue_mode", selected = "cut",
            choices = c("the pairs nearest the cut" = "cut",
                        "the review band" = "band",
                        "an audit sample of the pairs in scope" = "audit",
                        "the pairs another reviewer judged" = "others"))
    }

    # Two counters. `rv_tick` says something changed that the screen has to
    # show; `rv_quiet` says something was filed that it need not -- a comment
    # still being typed -- so that it is saved without redrawing the box it
    # is being typed into.
    rv_tick <- shiny::reactiveVal(0L)
    rv_quiet <- shiny::reactiveVal(0L)
    rv_thresholds <- shiny::reactiveVal(list())
    rv_saved <- shiny::reactiveVal(NULL)
    rv_marked <- shiny::reactiveVal(character())
    rv_suggest <- shiny::reactiveVal(NULL)
    rv_focus <- shiny::reactiveVal(NULL)
    rv_queue <- shiny::reactiveVal(spec$shown)
    rv_queue_note <- shiny::reactiveVal(NULL)

    # The same things under the names they have always had. Each reads the
    # counter, so anything built on one is redrawn when a verdict changes.
    rv_decisions <- function() {
        rv_tick()
        at <- which(st$verdict > 0L)
        out <- as.list(.inspect_verdicts[st$verdict[at]])
        names(out) <- spec$all_ids[at]
        out
    }
    rv_comments <- function() {
        rv_tick()
        rv_quiet()
        as.list(st$comments)
    }
    rv_events <- function() {
        rv_tick()
        rv_quiet()
        st$events[seq_len(st$n_events)]
    }
    rv_touched <- function() {
        rv_tick()
        list(decisions = as.list(st$touched_decisions),
             comments = as.list(st$touched_comments))
    }

    next_seq <- function() {
        st$seq <- st$seq + 1L
        st$seq
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
    # than when a QA pass applies it months later. It may name any score, not
    # only the ones with a slider.
    expr_pass <- shiny::reactive({
        text <- rule_expr()
        if (is.null(text) || !nzchar(trimws(text))) return(NULL)
        frame <- spec$evidence
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
    scope_all <- shiny::reactive({
        rv_tick()
        .inspect_scope_at(pass_all(), st$verdict, st$judged)
    })

    # Counting what a scope does to the records is a pass over every pair in
    # it. That is nothing on a hundred thousand pairs and a third of a second
    # on ten million, which is too long to wait on every keypress -- so past
    # a couple of million the header catches up once a second instead.
    paced <- function(r) if (n_all > 2e6) shiny::throttle(r, 1000) else r

    # Counts over the effective scope, not the bare cut. A reviewer who
    # includes a pair the cut threw out sees the total move, which is the
    # whole point of being able to override it.
    effect <- paced(shiny::reactive(
        .inspect_effect_of(index_1, index_2, scope_all())))

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
    # Counted from the judged pairs alone: the rest of the scope is the cut,
    # and how many pairs the cut passes only changes when the cut does.
    n_passing <- shiny::reactive(sum(pass_all()))
    overrides <- shiny::reactive({
        rv_tick()
        said <- st$verdict[st$judged]
        passed <- pass_all()[st$judged]
        n_in <- sum(said == 1L & !passed)
        n_out <- sum(said == 2L & passed)
        list(n_scope = n_passing() + n_in - n_out, n_threshold = n_passing(),
             n_forced_in = n_in, n_forced_out = n_out)
    })

    # The same scope, but depending on the cut and *not* on the judgements:
    # what the table's grouping and row filtering read. Moving a slider has to
    # regroup; clicking a verdict must not, because a re-render loses the
    # reviewer's sort, filter and page. The cell renderer picks up the
    # judgement from `meta` in the meantime. The verdicts are outside the
    # reactive graph, so reading them here takes no dependency on them.
    scope_for_table <- function()
        .inspect_scope_at(pass_all(), st$verdict, st$judged)

    # Which score the band, the queue, the sweep, the suggestion and the
    # re-rank are about.
    swept <- shiny::reactive({
        pick <- input$sweep_score
        at <- if (is.null(pick)) NA_integer_ else match(pick, score_names)
        if (is.na(at)) 1L else at
    })

    # The sweep is a curve and the cut is a marker on it, so it is computed
    # once per score rather than on every slider tick -- and over a grid of
    # cut-offs rather than every distinct score once there are too many of
    # those to draw.
    sweep <- shiny::reactive({
        k <- swept()
        v <- spec$all[[score_names[k]]]
        grid <- .inspect_sweep_grid(v)
        if (!is.null(grid))
            grid <- sort(grid, decreasing = !identical(operators[k], "<="))
        # With a match probability to hand, the sweep can also say how many
        # of the pairs each cut keeps are expected to be right.
        prob <- if (!is.null(spec$model) &&
                    "probability" %in% names(spec$evidence))
            spec$evidence[["probability"]] else NULL
        threshold_sweep(
            data.frame(index_1 = index_1, index_2 = index_2), v,
            thresholds = grid, probability = prob, operator = operators[k])
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

    # --- the review band --------------------------------------------------
    band <- shiny::debounce(shiny::reactive({
        lo <- input$band_lo
        hi <- input$band_hi
        if (is.null(lo) && is.null(hi)) return(spec$band)
        if (is.null(lo) || is.null(hi) || !is.finite(lo) || !is.finite(hi) ||
            lo == hi) return(NULL)
        sort(c(lo, hi))
    }), 300)

    in_band <- shiny::reactive(
        .inspect_in_band(spec$all[[score_names[swept()]]], band()))

    band_counts <- shiny::reactive({
        inside <- in_band()
        if (!any(inside)) return(NULL)
        rv_tick()
        judged <- st$verdict[inside] %in% c(1L, 2L)
        list(n = sum(inside), n_judged = sum(judged),
             n_left = sum(!judged))
    })

    # --- the audit sample -------------------------------------------------
    hash_order <- function() {
        if (is.null(st$hash_order))
            st$hash_order <- order(fhash(spec$all_ids, algo = "xxh64"),
                                   method = "radix")
        st$hash_order
    }

    audit_size <- shiny::reactive({
        n <- input$audit_n
        if (is.null(n) || !is.finite(n) || n < 1) 200L
        else as.integer(min(n, spec$max_pairs))
    })

    audit_rows <- shiny::reactive(
        .inspect_audit_rows(hash_order(), pass_all(), audit_size()))

    # Estimated only once the reviewer has asked for the sample: before that
    # the pairs in it are just pairs, and a rate from the few of them that
    # happened to be judged near the cut would be a rate from the hardest
    # cases presented as one from them all.
    rv_audited <- shiny::reactiveVal(FALSE)
    audit <- shiny::reactive({
        if (!rv_audited()) return(NULL)
        rows <- audit_rows()
        rv_tick()
        .inspect_audit(st$verdict[rows])
    })

    # --- the table --------------------------------------------------------
    state_for <- function(rows) {
        ids <- spec$all_ids[rows]
        code <- st$verdict[rows]
        code[code == 0L] <- 3L
        note <- unname(st$comments[ids])
        note[is.na(note)] <- ""
        list(decision = .inspect_verdicts[code], comment = note)
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

    # The rows on screen, as positions among all the pairs.
    table_rows <- shiny::reactive({
        rows <- rv_queue()
        # Hiding out-of-scope rows hides them by *effective* scope, so a pair
        # included by hand does not vanish the moment it is included. Judging
        # a pair and watching it disappear is how a reviewer loses confidence
        # that the tool recorded what they said.
        if (identical(input$mode, "filter"))
            rows <- rows[scope_for_table()[rows]]
        pick <- input$filter
        if (!is.null(pick) && !identical(pick, "all")) {
            held <- state_for(rows)$decision
            rows <- switch(
                pick,
                # The set worth re-reading before signing off: every pair
                # where the reviewer and the cut disagree.
                override = {
                    pass <- pass_all()[rows]
                    rows[(held == "include" & !pass) |
                         (held == "exclude" & pass)]
                },
                in_scope = rows[scope_for_table()[rows]],
                out_scope = rows[!scope_for_table()[rows]],
                band = rows[in_band()[rows] & held == "undecided"],
                others = rows[rows %in% others_rows],
                disagree = rows[rows %in% disagreeing()],
                rows[held == pick])
        }
        rows
    })

    table_data <- shiny::reactive({
        rows <- table_rows()
        keep <- intersect(unique(c("pair_id", col_order())), data_cols)
        out <- spec$source[rows, intersect(keep, names(spec$source)),
                           drop = FALSE]
        rownames(out) <- NULL
        # Always the identifier the decisions are filed against, which for a
        # table without one of its own was built from the indices.
        out[["pair_id"]] <- spec$all_ids[rows]
        out <- out[, keep, drop = FALSE]
        held <- state_for(rows)
        # The blocks the table groups into are the *effective* scope -- the
        # cut with the judgements already applied -- and this reactive depends
        # on the cut, so moving a slider regroups, which is what you expect,
        # having just moved it.
        #
        # A per-pair judgement does not regroup. It cannot: grouping reads the
        # data, and redrawing the table on every click would throw away the
        # reviewer's place in it. The scope *cell* is drawn from `meta`
        # instead, so an overridden pair turns over the instant it is judged
        # and says so; it moves to the other block at the next regroup.
        #
        # `_threshold` carries the cut's own verdict alongside it, because the
        # cell has to be able to say whether a judgement agreed with the cut
        # or overruled it, and after a regroup `_scope` no longer shows that.
        pass <- pass_all()[rows]
        out[["_scope"]] <- ifelse(scope_for_table()[rows],
                                  "in scope", "out of scope")
        out[["_threshold"]] <- ifelse(pass, "pass", "fail")
        out[["_decision"]] <- held$decision
        out[["_comment"]] <- held$comment
        # The verdict buttons first. reactable draws columns in the order the
        # data has them, and pinning a column only holds it once it has been
        # scrolled to -- so at the end of the data, where these used to be,
        # the buttons sat off the right-hand edge of any table wider than the
        # window, behind every carried column.
        out[, c("_decision", "_scope", "_threshold", keep, "_comment"),
            drop = FALSE]
    })

    grouped <- shiny::reactive(isTRUE(input$group_scope))
    page_rows <- shiny::reactive(
        if (is.null(input$page_size) || !is.finite(input$page_size))
            spec$page_size
        else max(5L, min(1000L, as.integer(input$page_size))))

    # The table is drawn when its *shape* changes -- the columns, the
    # grouping, the page length -- and at no other time. What is in it is
    # pushed into the table that is already there (see below), because a
    # table drawn afresh starts again at page one, unsorted and unfiltered,
    # and a reviewer who has sorted by surname and paged to the thirtieth
    # screen should not lose that for having moved a slider.
    output$tbl <- reactable::renderReactable({
        columns <- col_order()
        group <- grouped()
        page <- page_rows()
        data <- shiny::isolate(table_data())
        st$columns <- names(data)
        # What is drawn carries every verdict so far, so the live overlay
        # starts empty again.
        st$touched_decisions <- character()
        st$touched_comments <- character()
        reactable::reactable(
            data,
            columns = .inspect_columns(
                list(view = data, score_spec = spec$score_spec,
                     evidence_spec = spec$evidence_spec,
                     index_cols = spec$index_cols), names(data)),
            groupBy = if (group) "_scope" else NULL,
            # Without this a group is drawn whole, however long it is, and
            # "rows per page" stops meaning anything: two groups are two
            # rows, and twenty thousand pairs are one page.
            paginateSubRows = TRUE,
            meta = .inspect_meta(spec$evidence_spec, shiny::isolate(cuts()),
                                 shiny::isolate(rule())),
            rowClass = .inspect_js_rowclass(),
            sortable = TRUE, resizable = TRUE, filterable = TRUE,
            searchable = TRUE, compact = TRUE, highlight = TRUE,
            wrap = FALSE, bordered = TRUE, pagination = TRUE,
            defaultExpanded = TRUE,
            defaultPageSize = page, showPageSizeOptions = TRUE,
            pageSizeOptions = c(25, 50, 100, 250, 500, 1000),
            theme = .inspect_theme(), class = "fs-table")
    })

    # New contents for the table that is already drawn. reactable keeps the
    # sort and the filters across this; the page is put back by hand, held to
    # the last page there now is.
    push_data <- function() {
        data <- table_data()
        if (!identical(names(data), st$columns)) return(invisible(NULL))
        st$touched_decisions <- character()
        st$touched_comments <- character()
        page <- reactable::getReactableState("tbl", "page", session = session)
        size <- reactable::getReactableState("tbl", "pageSize",
                                             session = session)
        if (is.null(size) || !is.finite(size) || size < 1)
            size <- shiny::isolate(page_rows())
        last <- max(1L, as.integer(ceiling(nrow(data) / size)))
        reactable::updateReactable(
            "tbl", data = data, expanded = TRUE,
            page = if (is.null(page) || !is.finite(page)) NULL
                   else min(as.integer(page), last),
            meta = list(cuts = as.list(shiny::isolate(cuts())),
                        rule = shiny::isolate(rule()),
                        decisions = list(), comments = list()),
            session = session)
        invisible(NULL)
    }
    shiny::observeEvent(table_data(), push_data(), ignoreInit = TRUE)

    push_meta <- function() {
        # The overlay is every verdict since the table's contents were last
        # sent, and all of it goes down the wire on each click. Past a couple
        # of thousand it is cheaper to send the contents again, which carry
        # the verdicts themselves and empty it.
        if (length(st$touched_decisions) > 2000L) return(push_data())
        reactable::updateReactable("tbl", meta = list(
            decisions = as.list(st$touched_decisions),
            comments = as.list(st$touched_comments)), session = session)
    }

    # --- decisions --------------------------------------------------------
    # One or many pairs, one call: the verdicts are set together, the events
    # are filed together and the table is told once.
    record_many <- function(pair_ids, decision = NULL, comment = NULL,
                            push = TRUE, undoable = TRUE, quiet = FALSE) {
        pair_ids <- unique(as.character(pair_ids))
        at <- row_of(pair_ids)
        pair_ids <- pair_ids[!is.na(at)]
        at <- at[!is.na(at)]
        if (!length(at)) return(invisible(NULL))
        before <- list(rows = at, ids = pair_ids, verdict = st$verdict[at],
                       comment = unname(st$comments[pair_ids]))
        if (!is.null(decision)) {
            st$verdict[at] <- match(decision, .inspect_verdicts)
            st$judged <- union(st$judged, at)
            verdicts <- rep(decision, length(at))
        } else {
            code <- st$verdict[at]
            code[code == 0L] <- 3L
            verdicts <- .inspect_verdicts[code]
        }
        if (!is.null(comment)) st$comments[pair_ids] <- comment
        notes <- unname(st$comments[pair_ids])
        now <- .review_now()
        fresh <- lapply(seq_along(at), function(k) {
            n <- next_seq()
            list(event_id = paste0(spec$run_id, "-d", sprintf("%06d", n)),
                 seq = n, run_id = spec$run_id, reviewer = spec$reviewer,
                 at = now, pair_id = pair_ids[k],
                 index_1 = as.integer(index_1[at[k]]),
                 index_2 = as.integer(index_2[at[k]]),
                 decision = verdicts[k],
                 comment = if (is.na(notes[k])) NULL else notes[k],
                 scores_at_decision = .named(
                     lapply(score_names, function(s) spec$all[[s]][at[k]]),
                     score_names))
        })
        need <- st$n_events + length(fresh)
        if (need > length(st$events))
            length(st$events) <- max(need, 2L * length(st$events))
        st$events[st$n_events + seq_along(fresh)] <- fresh
        st$n_events <- need
        st$touched_decisions[pair_ids] <- verdicts
        st$touched_comments[pair_ids] <- ifelse(is.na(notes), "", notes)
        if (undoable) {
            st$undo <- c(st$undo, list(before))
            # Far enough back to recover from a slip, not a second history.
            if (length(st$undo) > 50L)
                st$undo <- st$undo[seq.int(length(st$undo) - 49L,
                                           length(st$undo))]
        }
        if (quiet) {
            rv_quiet(shiny::isolate(rv_quiet()) + 1L)
            return(invisible(NULL))
        }
        rv_tick(shiny::isolate(rv_tick()) + 1L)
        if (push) push_meta()
        invisible(NULL)
    }

    # Taking an action back is itself an action: the log is append-only, and
    # a review in which a verdict was reached and then withdrawn is a
    # different review from one in which it was never reached. So each pair
    # gets a new event restoring what it held before.
    undo <- function() {
        if (!length(st$undo)) return(invisible(NULL))
        last <- st$undo[[length(st$undo)]]
        st$undo[[length(st$undo)]] <- NULL
        code <- last$verdict
        code[code == 0L] <- 3L
        note <- last$comment
        note[is.na(note)] <- ""
        was <- paste(code, note, sep = "\r")
        for (state in unique(was)) {
            pick <- which(was == state)
            record_many(last$ids[pick],
                        decision = .inspect_verdicts[code[pick[1L]]],
                        comment = note[pick[1L]], push = FALSE,
                        undoable = FALSE)
        }
        push_meta()
        if (length(last$ids) == 1L) set_focus(last$ids)
        invisible(NULL)
    }
    shiny::observeEvent(input$undo, undo())
    shiny::observeEvent(input$fs_undo, undo())

    valid_decision <- function(d) length(d) == 1L && !is.na(d) &&
        d %in% .inspect_verdicts

    shiny::observeEvent(input$fs_decision, {
        info <- input$fs_decision
        if (is.null(info$pair_id) || is.null(info$decision)) return()
        decision <- as.character(info$decision)
        if (!valid_decision(decision)) return()
        record_many(as.character(info$pair_id), decision = decision)
    })

    shiny::observeEvent(input$fs_comment, {
        info <- input$fs_comment
        if (is.null(info$pair_id)) return()
        # Still being typed: filed, so that closing the tab loses nothing,
        # but neither redrawn around nor put on the undo stack a keystroke at
        # a time.
        live <- isTRUE(info$live)
        record_many(as.character(info$pair_id),
                    comment = as.character(if (is.null(info$comment)) ""
                                           else info$comment),
                    quiet = live, undoable = !live)
    })

    # A stock reason, from its key or its chip. It is the comment, so that a
    # file of decisions reads the same whether a reason was typed or picked.
    shiny::observeEvent(input$fs_reason, {
        info <- input$fs_reason
        k <- suppressWarnings(as.integer(info$index))
        if (!length(k) || is.na(k) || k < 1L || k > length(spec$reasons))
            return()
        ids <- intersect(as.character(unlist(info$pair_ids)), spec$all_ids)
        if (!length(ids)) return()
        record_many(ids, comment = spec$reasons[k])
    })

    # --- the focused pair -------------------------------------------------
    set_focus <- function(id) {
        rv_focus(id)
        session$sendCustomMessage("fs_set_focus", list(pair_id = id))
    }

    shiny::observeEvent(input$fs_focus, {
        id <- as.character(input$fs_focus$pair_id)
        if (!length(id) || is.na(id) || is.na(row_of(id))) return()
        rv_focus(id)
    })

    output$fs_evidence <- shiny::renderUI({
        id <- rv_focus()
        if (is.null(id))
            return(shiny::tags$div(
                class = "fs-card-meta",
                "Click a row, or press j, to put a pair here."))
        at <- row_of(id)
        if (is.na(at)) return(NULL)
        rv_tick()
        card <- .inspect_card(spec, at)
        code <- st$verdict[at]
        pass <- pass_all()[at]
        verdict <- if (!code %in% c(1L, 2L))
            sprintf("undecided; the cut says %s",
                    if (isTRUE(pass)) "in scope" else "out of scope")
        else if ((code == 1L) == isTRUE(pass))
            sprintf("judged %s, agreeing with the cut",
                    if (code == 1L) "a link" else "not a link")
        else sprintf("judged %s, overriding the cut",
                     if (code == 1L) "a link" else "not a link")

        number <- function(v) if (is.na(v)) .inspect_na_label
                              else formatC(v, digits = 3, format = "g")
        rows <- lapply(card$rows, function(r) {
            v1 <- if (r$has_1) r$value_1 else NA_character_
            v2 <- if (r$has_2) r$value_2 else NA_character_
            # What differs is marked in the fields that were compared. In the
            # others it would be marked nearly everywhere -- two files' record
            # identifiers never agree -- and a card that is mostly highlight
            # has stopped pointing at anything.
            marked <- if (identical(r$kind, "score")) list(a = "", b = "")
                      else if (identical(r$kind, "compared"))
                          .inspect_diff(v1, v2)
                      else list(a = .inspect_html(.inspect_shown(v1)),
                                b = .inspect_html(.inspect_shown(v2)))
            cell <- function(html, has) shiny::tags$td(
                class = "fs-mono", if (has) shiny::HTML(html) else "")
            score <- if (!length(r$scores)) "" else if (length(r$scores) == 1L)
                number(r$scores[[1L]])
            else paste(sprintf("%s %s", names(r$scores),
                               vapply(r$scores, number, "")), collapse = ", ")
            bits <- if (length(r$bits) && any(!is.na(r$bits)))
                sprintf("%+.1f", sum(r$bits, na.rm = TRUE)) else ""
            shiny::tags$tr(
                class = if (identical(r$kind, "carried")) "fs-ev-other",
                shiny::tags$th(r$field),
                cell(marked$a, r$has_1 && !identical(r$kind, "score")),
                cell(marked$b, r$has_2 && !identical(r$kind, "score")),
                shiny::tags$td(class = "fs-mono fs-ev-score", score),
                if (!is.null(card$bits))
                    shiny::tags$td(class = "fs-mono fs-ev-score fs-bits",
                                   bits))
        })

        # What anyone else said about this pair, where the file holds a
        # previous reviewer's pass. Shown, never pre-filled: a QA reviewer
        # should know the analyst called it a link, and should still have to
        # say so themselves for the file to record an agreement.
        theirs <- seeded$others[seeded$others$pair_id == id, , drop = FALSE]

        rivals <- .inspect_rivals(index_1, index_2, at, spec$dedupe)
        k <- swept()
        v <- spec$all[[score_names[k]]]
        if (nrow(rivals)) {
            best <- order(if (identical(operators[k], "<=")) v[rivals$row]
                          else -v[rivals$row], na.last = TRUE)
            rivals <- rivals[best, , drop = FALSE]
        }
        n_rivals <- nrow(rivals)
        rivals <- utils::head(rivals, 10L)
        scope <- shiny::isolate(scope_all())
        rival_rows <- lapply(seq_len(nrow(rivals)), function(r) {
            row <- rivals$row[r]
            said <- st$verdict[row]
            shiny::tags$tr(
                shiny::tags$td(shiny::tags$a(
                    class = "fs-rival", `data-fs-id` = spec$all_ids[row],
                    spec$all_ids[row])),
                shiny::tags$td(sprintf("same record %d", rivals$shares[r])),
                shiny::tags$td(class = "fs-mono fs-ev-score", number(v[row])),
                shiny::tags$td(
                    if (said == 1L) "a link" else if (said == 2L) "not a link"
                    else if (scope[row]) "in scope" else "out"))
        })

        note <- unname(st$comments[id])
        if (is.na(note)) note <- ""
        shiny::tagList(
            shiny::tags$div(class = "fs-card-id", id),
            if (length(card$lead)) shiny::tags$div(
                class = "fs-card-meta",
                paste(sprintf("%s %s", names(card$lead),
                              unlist(card$lead)), collapse = "  |  ")),
            shiny::tags$div(class = "fs-ev-verdict", verdict),
            shiny::tags$div(
                class = "fs-ev-act",
                shiny::actionButton("focus_include", "A link (y)",
                                    class = "btn-sm"),
                shiny::actionButton("focus_exclude", "Not a link (n)",
                                    class = "btn-sm"),
                shiny::actionButton("focus_undecided", "Undecided (u)",
                                    class = "btn-sm")),
            if (nrow(theirs)) shiny::tags$div(
                class = "fs-note",
                lapply(seq_len(nrow(theirs)), function(i) shiny::tags$div(
                    sprintf("%s: %s%s", theirs$reviewer[i],
                            if (identical(theirs$decision[i], "include"))
                                "a link" else "not a link",
                            if (!is.na(theirs$comment[i]) &&
                                nzchar(theirs$comment[i]))
                                paste0(" -- ", theirs$comment[i]) else "")))),
            shiny::tags$table(
                class = "fs-ev",
                shiny::tags$thead(shiny::tags$tr(
                    shiny::tags$th(""),
                    shiny::tags$th(sub("^_", "", spec$suffixes[1L])),
                    shiny::tags$th(sub("^_", "", spec$suffixes[2L])),
                    shiny::tags$th("score"),
                    if (!is.null(card$bits)) shiny::tags$th("bits"))),
                shiny::tags$tbody(rows)),
            shiny::tags$input(
                type = "text", class = "fs-comment", id = "fs_card_comment",
                `data-fs-id` = id, value = note,
                placeholder = "why? (press c)"),
            if (length(spec$reasons)) shiny::tags$div(
                class = "fs-ev-act",
                lapply(seq_along(spec$reasons), function(i) shiny::tags$button(
                    type = "button", class = "fs-reason",
                    `data-fs-id` = id, `data-fs-reason` = i,
                    sprintf("%d %s", i, spec$reasons[i])))),
            if (n_rivals) shiny::tagList(
                shiny::tags$h6(sprintf(
                    "%s other candidate%s for these two records",
                    format(n_rivals, big.mark = ","),
                    if (n_rivals == 1L) "" else "s")),
                shiny::tags$table(class = "fs-ev", shiny::tags$tbody(
                    rival_rows)),
                if (n_rivals > nrow(rivals)) shiny::tags$div(
                    class = "fs-card-meta",
                    sprintf("and %s more, lower down.",
                            format(n_rivals - nrow(rivals), big.mark = ","))))
            else shiny::tags$div(
                class = "fs-card-meta",
                "No other candidate for either record."))
    })
    shiny::outputOptions(output, "fs_evidence", suspendWhenHidden = FALSE)

    focus_judge <- function(decision) {
        id <- rv_focus()
        if (is.null(id)) return(invisible(NULL))
        record_many(id, decision = decision)
    }
    shiny::observeEvent(input$focus_include, focus_judge("include"))
    shiny::observeEvent(input$focus_exclude, focus_judge("exclude"))
    shiny::observeEvent(input$focus_undecided, focus_judge("undecided"))

    # y / n / u from the keyboard. Routed through the same record_many() as a
    # click so the event log cannot tell them apart, which is what makes the
    # audit trail worth keeping.
    shiny::observeEvent(input$fs_keyjudge, {
        info <- input$fs_keyjudge
        decision <- as.character(info$decision)
        if (!valid_decision(decision)) return()
        record_many(as.character(unlist(info$pair_ids)), decision = decision)
    })

    shiny::observeEvent(input$advance, session$sendCustomMessage(
        "fs_options", list(advance = isTRUE(input$advance))))

    shiny::observeEvent(input$fs_help, shiny::showModal(shiny::modalDialog(
        title = "Keys", easyClose = TRUE, footer = shiny::modalButton("Close"),
        shiny::tags$div(class = "fs-side", .inspect_keys(spec$reasons)))))

    # --- marked rows ------------------------------------------------------
    shiny::observeEvent(input$fs_marked, {
        ids <- as.character(unlist(input$fs_marked$pair_ids))
        rv_marked(ids[!is.na(row_of(ids))])
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
        record_many(ids, decision = decision)
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

    # --- the queue --------------------------------------------------------
    # Which pairs are on screen. It was fixed when the review opened, around
    # the cut the review opened with -- so a reviewer who moved the cut from
    # 0.85 to 0.7 went on being shown the pairs nearest 0.85, and the ones
    # the new cut could not settle were never put in front of them.
    shiny::observeEvent(input$rebuild, {
        k <- swept()
        v <- spec$all[[score_names[k]]]
        cut <- unname(cuts()[k])
        mode <- if (is.null(input$queue_mode)) "cut" else input$queue_mode
        judged <- which(st$verdict %in% c(1L, 2L))
        nearest <- function(rows) rows[review_order(v[rows], threshold = cut)]
        if (identical(mode, "audit")) {
            rows <- audit_rows()
            rv_audited(TRUE)
            note <- sprintf(paste0(
                "An audit sample: %s of the %s pairs passing the cut, chosen ",
                "by a hash of their identifiers. It is the same sample ",
                "whoever opens this review."),
                format(length(rows), big.mark = ","),
                format(sum(pass_all()), big.mark = ","))
        } else if (identical(mode, "others")) {
            if (!has_others) {
                rv_queue_note(paste0("Nobody else has judged any of these ",
                                     "pairs."))
                return()
            }
            rows <- utils::head(nearest(others_rows), spec$max_pairs)
            note <- sprintf(paste0(
                "The %s pair%s another reviewer judged, nearest the cut ",
                "first. Their verdict is on each pair, beside the table; ",
                "yours starts blank."),
                format(length(rows), big.mark = ","),
                if (length(rows) == 1L) "" else "s")
        } else if (identical(mode, "band")) {
            inside <- which(in_band())
            if (!length(inside)) {
                rv_queue_note(paste0("There is no review band, or no pair ",
                                     "scores inside it."))
                return()
            }
            rows <- utils::head(nearest(inside), spec$max_pairs)
            note <- sprintf(
                "The review band: %s of its %s pairs, nearest the cut first.",
                format(length(rows), big.mark = ","),
                format(length(inside), big.mark = ","))
        } else {
            # What has been judged stays on screen wherever the cut has gone:
            # a verdict the reviewer can no longer find is one they cannot
            # check.
            near <- review_order(v, threshold = cut)
            rows <- utils::head(unique(c(nearest(judged), near)),
                                spec$max_pairs)
            note <- sprintf(
                "The %s pairs nearest a cut of %s on `%s`.",
                format(length(rows), big.mark = ","),
                formatC(cut, digits = 4, format = "g"), score_names[k])
        }
        rv_queue(rows)
        rv_queue_note(note)
    })

    shiny::observeEvent(input$rerank, {
        k <- swept()
        rows <- rv_queue()
        rv_queue(rows[review_order(spec$all[[score_names[k]]][rows],
                                   threshold = unname(cuts()[k]))])
    })

    output$fs_queue <- shiny::renderUI({
        note <- rv_queue_note()
        if (is.null(note)) return(NULL)
        shiny::tags$div(class = "fs-ok", note)
    })

    # --- recording a threshold -------------------------------------------
    shiny::observeEvent(input$accept, {
        rv_thresholds(c(rv_thresholds(), list(.inspect_threshold_event(
            spec, cuts(), if (identical(rule(), "expr")) "all" else rule(),
            "accepted", if (is.null(input$note)) "" else input$note,
            next_seq(), cut_effect(),
            expr = if (identical(rule(), "expr")) rule_expr() else NULL,
            band = band(), band_score = score_names[swept()],
            audit = audit()))))
        save_now()
    })

    # --- panels -----------------------------------------------------------
    # One small style tag carries what the reviewer can change about the
    # layout, so changing it redraws nothing: the font size, and whether the
    # pair sits beside the table or the table has the full width.
    output$fs_font <- shiny::renderUI({
        size <- if (is.null(input$font_size) || !is.finite(input$font_size))
            spec$font_size
        else max(6L, min(24L, as.integer(input$font_size)))
        shiny::tags$style(shiny::HTML(paste0(
            sprintf("#fs-inspect, .fs-side { --fs-font: %dpx; }", size),
            if (isFALSE(input$show_card)) paste0(
                " #fs-inspect .fs-main { grid-template-columns: ",
                "minmax(0, 1fr); } #fs-inspect .fs-card { display: none; }"))))
    })

    output$fs_kpi <- shiny::renderUI({
        rv_tick()
        said <- tabulate(st$verdict, 3L)
        e <- effect()
        o <- overrides()
        b <- band_counts()
        a <- audit()
        second <- if (has_others) {
            mine <- st$verdict[others_at]
            both <- mine %in% c(1L, 2L)
            list(n = length(others_rows),
                 n_judged = length(unique(others_at[both])),
                 n_differ = length(disagreeing()))
        }
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
                                     said[1L], said[2L])),
            # How much of the work is left, which is the number a reviewer
            # wants on a Friday afternoon.
            if (!is.null(b)) shiny::tags$span(
                class = "fs-kpi-band",
                sprintf("band: %s pairs, %s judged, %s left",
                        format(b$n, big.mark = ","),
                        format(b$n_judged, big.mark = ","),
                        format(b$n_left, big.mark = ","))),
            if (!is.null(a)) shiny::tags$span(
                class = "fs-kpi-band",
                if (!a$n_judged) sprintf(
                    "audit: none of the %s sampled pairs judged yet",
                    format(a$n_sample, big.mark = ","))
                else sprintf(paste0(
                    "audit: %d of %d sampled links judged wrong -- about ",
                    "%.1f%% of what the cut lets through (95%% interval ",
                    "%.1f to %.1f)"),
                    a$n_not_link, a$n_judged, 100 * a$rate, 100 * a$lower,
                    100 * a$upper)),
            # A second look at somebody else's review: how much of it has
            # been gone over, and where the two reviewers part company.
            if (!is.null(second)) shiny::tags$span(
                class = "fs-kpi-band",
                sprintf(paste0("second look: %s of %s earlier verdicts gone ",
                               "over, %s differing"),
                        format(second$n_judged, big.mark = ","),
                        format(second$n, big.mark = ","),
                        format(second$n_differ, big.mark = ","))))
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
                    "for you, so your pass is recorded as your own. To go ",
                    "over exactly those pairs, rebuild the queue as \"the ",
                    "pairs another reviewer judged\"."),
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
        est <- intersect(c("est_tp", "est_fp", "est_fn", "est_precision",
                           "est_recall", "est_f_measure"), names(s))
        reactable::reactable(
            s, compact = TRUE, bordered = TRUE, highlight = TRUE,
            defaultPageSize = 12, showPageSizeOptions = FALSE,
            selection = "single", onClick = "select",
            columns = c(
                list(threshold = reactable::colDef(
                    format = reactable::colFormat(digits = 4))),
                .named(lapply(est, function(nm) reactable::colDef(
                    format = reactable::colFormat(digits = 3))), est)),
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

    # The bars were counted when the review opened; only the line is new.
    output$hist_plot <- shiny::renderPlot({
        held <- cuts()
        shown <- Filter(Negate(is.null), spec$hists)
        if (!length(shown)) {
            graphics::plot.new()
            graphics::text(0.5, 0.5, "No scores to draw.")
            return(invisible(NULL))
        }
        old <- graphics::par(mfrow = grDevices::n2mfrow(length(shown)),
                             mar = c(4, 4, 2, 1))
        on.exit(graphics::par(old), add = TRUE)
        for (s in names(shown)) {
            at <- match(s, spec$evidence_spec$score)
            graphics::plot(
                shown[[s]], main = s, col = "#c6dbef", border = "#6baed6",
                xlab = if (is.na(spec$evidence_spec$comparator[at])) "score"
                       else sprintf("%s (%s)",
                                    spec$evidence_spec$comparator[at],
                                    spec$evidence_spec$scale[at]))
            if (s %in% names(held))
                graphics::abline(v = held[[s]], lwd = 2, lty = 2)
        }
    })

    output$events <- shiny::renderTable({
        e <- rv_events()
        if (!length(e)) return(data.frame(message = "No decisions yet."))
        df <- .review_events_df(utils::tail(e, 20L))
        df[, c("at", "pair_id", "decision", "comment")]
    })

    # --- saving and finishing --------------------------------------------
    save_now <- function() {
        msg <- tryCatch({
            .inspect_save(spec, st$events[seq_len(st$n_events)],
                          shiny::isolate(rv_thresholds()),
                          cache = st$save_cache)
            sprintf("Saved %s", format(Sys.time(), "%H:%M:%S"))
        }, error = function(cnd)
            paste0("Could not save: ", conditionMessage(cnd)))
        rv_saved(msg)
        invisible(NULL)
    }

    autosave <- shiny::throttle(
        shiny::reactive(rv_tick() + rv_quiet() + length(rv_thresholds())),
        2000)
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
    # One verdict per pair, the last where a pair is somehow there twice.
    # Built as vectors: a named list grown a pair at a time looks every name
    # up among all the names before it, which on twenty thousand verdicts
    # was most of the time it took to reopen a review.
    mine <- mine[!duplicated(mine$pair_id, fromLast = TRUE), , drop = FALSE]
    out$decisions <- .named(as.list(mine$decision), mine$pair_id)
    noted <- !is.na(mine$comment)
    out$comments <- .named(as.list(mine$comment[noted]), mine$pair_id[noted])
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

# Read, merge, write. Merging with what is on disk before every write means
# two reviewers sharing a file add to each other's work rather than
# overwriting it, with no lockfile to go stale on a network share.
#
# `cache` is an environment that remembers what this session last wrote and
# what the file looked like afterwards. While the file still looks like that,
# nobody else has written to it and what is on disk is what is remembered, so
# it is not read and parsed again -- which an autosave every two seconds
# otherwise did to the whole of a file that only ever grows.
.inspect_save <- function(spec, events, thresholds, cache = NULL) {
    path <- spec[["file"]]
    mine <- .review_new()
    run <- .inspect_run(spec)
    run$ended <- .review_now()
    mine$runs <- list(run)
    mine$thresholds <- thresholds
    mine$decisions <- events
    stamp <- function() {
        info <- file.info(path)
        list(size = info$size, mtime = info$mtime)
    }
    held <- NULL
    if (file.exists(path)) {
        unchanged <- !is.null(cache) && !is.null(cache$written) &&
            identical(cache$stamp, stamp())
        held <- if (unchanged) cache$written else
            tryCatch(.review_read_file(path), error = function(cnd) NULL)
        # Somebody else has written to the file, and what they wrote is what
        # is there now -- including, conceivably, an event this session
        # remembers the text of differently. The remembered text is let go
        # and the file encoded from what was read.
        if (!unchanged && !is.null(cache)) cache$json <- NULL
    }
    merged <- if (is.null(held)) mine else .review_merge(held, mine)
    # The same environment remembers each event's text, so a save encodes
    # only the events that are new since the last one.
    .review_write(merged, path, cache)
    if (!is.null(cache)) {
        cache$written <- merged
        cache$stamp <- stamp()
    }
    invisible(path)
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
#' Beside the table is **the pair**: the row in focus, field by field. The
#' compared fields come first, each with the value either input held and the
#' score it earned, and with what differs between the two values marked ---
#' one dropped letter reads very differently from three substitutions. Then
#' every other column that was carried, because a reviewer deciding whether
#' two records are one person reads the address and the sex as well as the
#' three fields somebody chose to score. Below that are the **other
#' candidates for the same two records**, each a click away: whether a pair is
#' a link is rarely a question about that pair alone, but whether it is the
#' best candidate its records have.
#'
#' With weights from [rLink_weights()] the cut is on the match probability
#' alone, the field scores are shown as the evidence behind it, and the pair
#' also says how many bits each field contributed to the weight.
#'
#' Every count in the header, the sweep and the panels is computed over *all*
#' the pairs, not only the ones on screen. Moving a cut changes what is in the
#' table without redrawing it, so the sort, the filters and the page you were
#' on are kept.
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
#' Three buttons and a comment box on every row, and the same on the pair
#' beside the table. Ctrl-click (cmd-click) marks rows and shift-click marks
#' a range, and the marked set can then be judged in one action --- which is
#' how a block of obvious links gets dealt with without 200 clicks. Marks
#' follow the pair, not the row position, so they survive sorting, filtering
#' and paging.
#'
#' The whole review can be done from the keyboard, which is most of what
#' makes a few thousand verdicts bearable:
#'
#' | Key | |
#' |---|---|
#' | `j` `k`, or the arrows | the next and the previous pair, turning the page at either end |
#' | `y` `n` | a link, not a link --- then on to the next pair nobody has judged |
#' | `u` | undecided again |
#' | `x` | mark or unmark the pair in focus |
#' | `c` | write a comment (`Esc` to come back) |
#' | `1` to `9` | file one of the stock `reasons` as the comment |
#' | `Ctrl+Z` | take the last action back |
#' | `?` | show this list |
#'
#' Taking an action back is itself recorded: the log is append-only, and a
#' verdict reached and then withdrawn is a different history from one never
#' reached. A comment is saved as it is typed, so closing the tab loses
#' nothing.
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
#' @section The review queue, the band and the audit sample:
#' A threshold is a way of not having to look at every pair, and these three
#' are about which pairs to look at instead.
#'
#' The **review band** is a range of one score --- `band = c(0.75, 0.95)`, or
#' the two boxes in the sidebar --- inside which a cut cannot be trusted and
#' a person has to look. The header counts the pairs in it, how many have
#' been judged and how many are left, which is the review's progress bar; and
#' *Show > in the review band, undecided* lists what is left. The cut still
#' decides any pair nobody judges.
#'
#' The **queue** is the `max_pairs` pairs put on screen. It opens as the ones
#' nearest the cut the review opened with, and **Rebuild the queue** draws it
#' again around the cut as it now stands --- or as the review band, or as an
#' audit sample. Pairs already judged stay in it wherever the cut has gone.
#'
#' The **audit sample** answers the other question a linkage is asked: of the
#' pairs the cut lets through unseen, how many are wrong? It is a fixed
#' sample of them --- the ones whose identifiers hash smallest, so it is the
#' same sample whoever opens the review and does not disturb the random
#' number stream. Judge it and the header estimates the false-link rate with
#' a 95% interval. The band and the audit are both filed with a recorded
#' threshold, so the file says what the cut was accepted on.
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
#' The file is saved two seconds after anything changes, and again when the
#' app closes. A verdict is encoded once and its text kept, so a save costs
#' what the new verdicts cost and not what the whole file does: the
#' twenty-thousandth is saved as quickly as the first.
#'
#' @section A second look:
#' The usual check on a review is a second person going over the first
#' one's verdicts, and the queue --- the pairs nearest the cut --- is where
#' the first reviewer worked without being a list of what they decided. When
#' the file holds somebody else's verdicts on these pairs:
#'
#' * **Rebuild the queue** offers *the pairs another reviewer judged*, which
#'   puts exactly those on screen. Their verdict and comment are on each
#'   pair, beside the table; yours starts blank.
#' * The header counts how many of those earlier verdicts you have gone over
#'   and on how many the two of you differ.
#' * *Show > judged by another reviewer* and *Show > where I and another
#'   reviewer disagree* list them, the second being the pairs to talk about.
#'
#' @section Limits:
#' At most `max_pairs` rows are put on screen, chosen by [review_order()]; the
#' rest of the analysis still reads every pair, and the app says so. reactable
#' draws every row of a page, so `page_size` is capped at 1000 --- there is no
#' "show all rows", because at these sizes it would lock the browser tab rather
#' than show you anything. The threshold sweep is drawn over at most 200
#' cut-offs, placed at quantiles of the score once it has more distinct values
#' than that. Columns are reordered and hidden by dragging them
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
#' @param band Optional review band: the lower and upper limit, on the first
#'   score, of the pairs to be looked at by hand. It can be set and changed
#'   in the app. See *The review queue, the band and the audit sample*.
#' @param reasons Optional character vector of up to nine stock reasons, such
#'   as `c("married name", "twin", "typing error")`. Each is offered on the
#'   pair and bound to its number key, and is filed as the comment.
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
#' # The band says which surnames are worth a person's time, and the stock
#' # reasons are the ones this linkage keeps running into.
#' out <- rLink_inspect(
#'     pairs, threshold = c(surname = 0.92, given = 0.85),
#'     band = c(0.8, 0.95),
#'     reasons = c("married name", "nickname", "transposed date"),
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
                          launch = interactive(), band = NULL,
                          reasons = NULL) {
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
                          expression = expression, band = band,
                          reasons = reasons)

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
    spec$save_cache <- new.env(parent = emptyenv())
    .inspect_save(spec, list(), opening, cache = spec$save_cache)

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
