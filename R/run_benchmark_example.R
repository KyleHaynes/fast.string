# Test data mirrors bench.R's corpus style: fully vectorised (sprintf() +
# sample()), not a per-element random-character loop, so it stays fast even
# at n in the millions and needs no Suggested package (stringi et al.).
.bench_test_strings <- function(n) {
    words <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")
    x <- sprintf("row-%08d-%s-value", seq_len(n), sample(words, n, replace = TRUE))
    if (n >= 1000L) {
        na_idx <- sample.int(n, max(1L, n %/% 1000L))
        x[na_idx] <- NA_character_
    }
    x
}

# Name-like strings for the similarity/phonetic families, same vectorised
# approach, no stringi dependency.
.bench_test_names <- function(n) {
    first <- c(
        "JOHN", "JANE", "MARY", "ROBERT", "MICHAEL", "SARAH", "DAVID", "LAURA",
        "PETER", "ANNA", "JAMES", "LINDA", "PAUL", "KAREN", "MARK", "SUSAN",
        "STEVEN", "DONNA", "KEVIN", "LISA"
    )
    last <- c(
        "SMITH", "JONES", "WILLIAMS", "BROWN", "TAYLOR", "MILLER", "WILSON",
        "MOORE", "CLARK", "LEWIS", "WALKER", "HALL", "ALLEN", "YOUNG", "KING",
        "WRIGHT", "SCOTT", "GREEN", "BAKER", "ADAMS"
    )
    x <- paste(sample(first, n, replace = TRUE), sample(last, n, replace = TRUE))
    if (n >= 1000L) {
        na_idx <- sample.int(n, max(1L, n %/% 1000L))
        x[na_idx] <- NA_character_
    }
    x
}

.bench_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# One call, capturing both its elapsed time and its return value (needed for
# the correctness check afterwards) — system.time() alone discards the value.
# Errors are caught rather than propagated, since baselines call into
# Suggested packages this code doesn't control the installed version of.
.bench_run <- function(call_fn, reps) {
    times <- numeric(reps)
    val <- NULL
    for (i in seq_len(reps)) {
        t0 <- proc.time()[["elapsed"]]
        val <- tryCatch(call_fn(), error = function(e) {
            return(structure(list(message = conditionMessage(e)), class = "bench_error"))
        })
        times[i] <- proc.time()[["elapsed"]] - t0
        if (inherits(val, "bench_error")) {
            return(list(value = NULL, elapsed = times[i], ok = FALSE, error = val$message))
        }
    }
    list(value = val, elapsed = min(times), ok = TRUE, error = NULL)
}

# Different packages, different regex engines and string conventions (PCRE2
# here, base R's TRE/POSIX or perl=TRUE, ICU in stringi, ...), so speed
# numbers alone don't establish that two functions computed the same thing.
# NA positions must match exactly; non-NA values compare with a small
# floating-point tolerance if numeric, exactly (as text) otherwise.
.bench_values_match <- function(a, b) {
    a <- as.vector(a)
    b <- as.vector(b)
    if (length(a) != length(b)) return(FALSE)
    na_a <- is.na(a)
    na_b <- is.na(b)
    if (!identical(na_a, na_b)) return(FALSE)
    a <- a[!na_a]
    b <- b[!na_b]
    if (!length(a)) return(TRUE)
    if (is.numeric(a) && is.numeric(b)) {
        isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-6))
    } else {
        identical(as.character(a), as.character(b))
    }
}

# system.time()'s clock resolution is a few ms on most platforms, so a
# near-instant fast.string call often rounds to exactly 0. When that happens
# but the baseline's time was clearly measurable, report a conservative
# lower bound instead of a bogus infinite/undefined ratio.
.bench_speedup <- function(t_base, t_fast) {
    clock_res <- 0.01
    if (t_fast > 0) {
        list(speedup = t_base / t_fast, label = sprintf("%.1fx", t_base / t_fast))
    } else if (t_base > clock_res) {
        list(speedup = NA_real_, label = sprintf(">%.0fx (too fast to time precisely)", t_base / clock_res))
    } else {
        list(speedup = NA_real_, label = "too fast to measure at this n")
    }
}

# A comparison baseline: `expect_match = FALSE` marks a pairing that's
# known, by design, to compute something different (a different phonetic
# algorithm revision, a rule the two sides read differently, ...), so a
# mismatch there is reported as expected rather than as a red flag. `note`
# is shown alongside it either way.
.bench_baseline <- function(call, expect_match = TRUE, note = NULL) {
    list(call = call, expect_match = isTRUE(expect_match), note = note)
}

# Runs fast_call once (timed across `reps`), then every available baseline
# against it, printing a line per baseline: timing, speedup, and whether the
# two sides' *output* actually agreed — not just how fast each one was.
.bench_compare <- function(op_label, fast_call, baselines, reps) {
    if (!length(baselines)) {
        cli::cli_alert_warning("{op_label}: no comparison package installed for this operation — skipped.")
        return(NULL)
    }
    cli::cli_alert("Running {op_label} ...")
    fast_run <- .bench_run(fast_call, reps)
    if (!fast_run$ok) {
        cli::cli_alert_danger("{op_label}: fast.string call failed — {fast_run$error}")
        return(NULL)
    }

    rows <- lapply(names(baselines), function(bname) {
        bl <- baselines[[bname]]
        base_run <- .bench_run(bl$call, reps)
        if (!base_run$ok) {
            cli::cli_alert_danger("{op_label} vs {bname}: call failed — {base_run$error} (skipped)")
            return(NULL)
        }

        speed <- .bench_speedup(base_run$elapsed, fast_run$elapsed)
        matches <- .bench_values_match(fast_run$value, base_run$value)
        match_symbol <- if (matches) "match" else if (bl$expect_match) "DIFFERS!" else "differs (expected)"
        note_suffix <- if (!is.null(bl$note)) sprintf(" (%s)", bl$note) else ""

        txt <- sprintf(
            "%s vs %s: %.4fs → %.4fs (%s) | %s%s",
            op_label, bname, base_run$elapsed, fast_run$elapsed, speed$label, match_symbol, note_suffix
        )
        if (matches && !is.na(speed$speedup) && speed$speedup >= 1) {
            cli::cli_alert_success(txt)
        } else if (matches) {
            cli::cli_alert_warning(txt)
        } else if (bl$expect_match) {
            cli::cli_alert_danger(txt)
        } else {
            cli::cli_alert_info(txt)
        }

        data.frame(
            operation = op_label, baseline = bname,
            base_s = base_run$elapsed, fast_s = fast_run$elapsed,
            speedup = speed$speedup, speedup_label = speed$label,
            match = matches, expect_match = bl$expect_match,
            match_symbol = match_symbol, stringsAsFactors = FALSE
        )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
}

.bench_matching <- function(x, reps) {
    cli::cli_h2("Matching & substitution")
    rows <- list()

    grepl_baselines <- list("base::grepl()" = .bench_baseline(
        function() base::grepl("[0-9]{2}", x),
        expect_match = FALSE,
        note = "base::grepl() returns FALSE for NA input (its own documented exception); fgrepl() returns NA, like every other match function here"
    ))
    if (.bench_pkg("stringi")) {
        grepl_baselines[["stringi::stri_detect_regex()"]] <-
            .bench_baseline(function() stringi::stri_detect_regex(x, "[0-9]{2}"))
    }
    rows$grepl <- .bench_compare("fgrepl()", function() fgrepl("[0-9]{2}", x), grepl_baselines, reps)

    gsub_baselines <- list("base::gsub()" = .bench_baseline(function() base::gsub("[0-9]+", "#", x, perl = TRUE)))
    if (.bench_pkg("stringi")) {
        gsub_baselines[["stringi::stri_replace_all_regex()"]] <-
            .bench_baseline(function() stringi::stri_replace_all_regex(x, "[0-9]+", "#"))
    }
    rows$gsub <- .bench_compare("fgsub()", function() fgsub("[0-9]+", "#", x), gsub_baselines, reps)

    count_baselines <- list("gregexpr()+vapply()" = .bench_baseline(function() {
        vapply(base::gregexpr("[0-9]", x), function(m) {
            if (length(m) == 1L && is.na(m[1])) return(NA_integer_)
            if (length(m) == 1L && m[1] < 0L) return(0L)
            length(m)
        }, integer(1))
    }))
    if (.bench_pkg("stringi")) {
        count_baselines[["stringi::stri_count_regex()"]] <-
            .bench_baseline(function() stringi::stri_count_regex(x, "[0-9]"))
    }
    rows$fcount <- .bench_compare("fcount()", function() fcount("[0-9]", x), count_baselines, reps)

    rows
}

.bench_strings <- function(x, reps) {
    cli::cli_h2("String utilities")
    x_ws <- ifelse(is.na(x), NA_character_, paste0("  ", x, "\t\n"))
    rows <- list()

    trimws_baselines <- list("base::trimws()" = .bench_baseline(function() base::trimws(x_ws)))
    if (.bench_pkg("stringi")) {
        trimws_baselines[["stringi::stri_trim_both()"]] <-
            .bench_baseline(function() stringi::stri_trim_both(x_ws))
    }
    rows$trimws <- .bench_compare("ftrimws()", function() ftrimws(x_ws), trimws_baselines, reps)

    substr_baselines <- list("base::substr()" = .bench_baseline(function() base::substr(x, 1, 10)))
    if (.bench_pkg("stringi")) {
        substr_baselines[["stringi::stri_sub()"]] <-
            .bench_baseline(function() stringi::stri_sub(x, 1, 10))
    }
    rows$substr <- .bench_compare("fsubstr()", function() fsubstr(x, 1, 10), substr_baselines, reps)

    nchar_baselines <- list("base::nchar()" = .bench_baseline(function() base::nchar(x, "chars")))
    if (.bench_pkg("stringi")) {
        nchar_baselines[["stringi::stri_length()"]] <-
            .bench_baseline(function() stringi::stri_length(x))
    }
    rows$nchar <- .bench_compare("fnchar()", function() fnchar(x, "chars"), nchar_baselines, reps)

    chartr_baselines <- list("base::chartr()" = .bench_baseline(function() base::chartr("aeiou", "AEIOU", x)))
    rows$chartr <- .bench_compare("fchartr()", function() fchartr("aeiou", "AEIOU", x), chartr_baselines, reps)

    rows
}

.bench_dates <- function(n, reps) {
    cli::cli_h2("Dates & timestamps")
    days <- as.Date("1950-01-01") + sample(0:27000, n, replace = TRUE)
    date_strings <- format_date(days, "iso")
    stamps <- as.POSIXct("1990-01-01", tz = "UTC") + sample(0:1e9, n, replace = TRUE)
    stamp_strings <- format_datetime(stamps, "iso")

    list(
        as_date = .bench_compare(
            "fas.Date()", function() fas.Date(date_strings, "iso"),
            list("base::as.Date()" = .bench_baseline(function() base::as.Date(date_strings, format = "%Y-%m-%d"))),
            reps
        ),
        as_posixct = .bench_compare(
            "fas.POSIXct()", function() fas.POSIXct(stamp_strings, "iso"),
            list("base::as.POSIXct()" = .bench_baseline(function() base::as.POSIXct(stamp_strings, tz = "UTC"))),
            reps
        )
    )
}

# No base R equivalent exists for any of these — only run when at least one
# comparison package (stringdist and/or RecordLinkage) is installed.
.bench_similarity <- function(n, reps) {
    cli::cli_h2("String similarity & edit distance")
    if (!.bench_pkg("stringdist") && !.bench_pkg("RecordLinkage")) {
        cli::cli_alert_warning("Neither {.pkg stringdist} nor {.pkg RecordLinkage} is installed — skipping this family.")
        return(list())
    }
    a <- .bench_test_names(n)
    b <- rev(a)
    rows <- list()

    jw_baselines <- list()
    if (.bench_pkg("stringdist")) {
        jw_baselines[["stringdist::stringdist(jw)"]] <-
            .bench_baseline(function() 1 - stringdist::stringdist(a, b, method = "jw", p = 0.1))
    }
    if (.bench_pkg("RecordLinkage")) {
        jw_baselines[["RecordLinkage::jarowinkler()"]] <-
            .bench_baseline(
                function() RecordLinkage::jarowinkler(a, b),
                expect_match = FALSE,
                note = "matches exactly on near-duplicate pairs; RecordLinkage applies its prefix bonus at a lower similarity threshold, so scores diverge on largely-unrelated pairs like the random pairing used here"
            )
    }
    rows$jaro_winkler <- .bench_compare("jaro_winkler()", function() jaro_winkler(a, b), jw_baselines, reps)

    if (.bench_pkg("stringdist")) {
        rows$levenshtein <- .bench_compare(
            "levenshtein()", function() levenshtein(a, b),
            list("stringdist::stringdist(lv)" = .bench_baseline(function() stringdist::stringdist(a, b, method = "lv"))),
            reps
        )
        rows$osa <- .bench_compare(
            "osa_distance()", function() osa_distance(a, b),
            list("stringdist::stringdist(osa)" = .bench_baseline(function() stringdist::stringdist(a, b, method = "osa"))),
            reps
        )
        rows$dl <- .bench_compare(
            "damerau_levenshtein()", function() damerau_levenshtein(a, b),
            list("stringdist::stringdist(dl)" = .bench_baseline(function() stringdist::stringdist(a, b, method = "dl"))),
            reps
        )

        a_eq <- fsubstr(a, 1, 8)
        b_eq <- rev(a_eq)
        rows$hamming <- .bench_compare(
            "hamming()", function() hamming(a_eq, b_eq),
            list("stringdist::stringdist(hamming)" = .bench_baseline(function() stringdist::stringdist(a_eq, b_eq, method = "hamming"))),
            reps
        )
        rows$jaccard <- .bench_compare(
            "jaccard_index()", function() jaccard_index(a, b),
            list("stringdist::stringsim(jaccard)" = .bench_baseline(function() stringdist::stringsim(a, b, method = "jaccard", q = 2))),
            reps
        )
        rows$cosine <- .bench_compare(
            "cosine_similarity()", function() cosine_similarity(a, b),
            list("stringdist::stringsim(cosine)" = .bench_baseline(function() stringdist::stringsim(a, b, method = "cosine", q = 2))),
            reps
        )
    }
    rows
}

# Also no base R equivalent. Where the comparison package implements a
# different algorithm revision (documented in docs/06-benchmarks.qmd), the
# baseline is marked expect_match = FALSE so a mismatch there is reported as
# expected rather than as a correctness problem.
.bench_phonetic <- function(n, reps) {
    cli::cli_h2("Phonetic codes")
    if (!.bench_pkg("RecordLinkage") && !.bench_pkg("phonics")) {
        cli::cli_alert_warning("Neither {.pkg RecordLinkage} nor {.pkg phonics} is installed — skipping this family.")
        return(list())
    }
    # Single-token names: phonics's functions warn ("unknown characters
    # found") and return NA on the space in a two-word "FIRST LAST" name,
    # which would make every comparison below look like a mismatch for a
    # reason that has nothing to do with the phonetic algorithms themselves.
    names_x <- gsub(" ", "", .bench_test_names(n), fixed = TRUE)
    rows <- list()

    if (.bench_pkg("RecordLinkage")) {
        rows$soundex <- .bench_compare(
            "soundex()", function() soundex(names_x),
            list("RecordLinkage::soundex()" = .bench_baseline(
                function() RecordLinkage::soundex(names_x),
                expect_match = FALSE, note = "H/W-rule edge cases can legitimately disagree"
            )),
            reps
        )
    }
    if (.bench_pkg("phonics")) {
        rows$refined_soundex <- .bench_compare(
            "refined_soundex()", function() refined_soundex(names_x),
            list("phonics::refinedSoundex()" = .bench_baseline(
                function() phonics::refinedSoundex(names_x, maxCodeLen = 40)
            )),
            reps
        )
        rows$cologne <- .bench_compare(
            "cologne()", function() cologne(names_x),
            list("phonics::cologne()" = .bench_baseline(
                function() phonics::cologne(names_x),
                expect_match = FALSE, note = "differing reading of two under-specified umlaut rules"
            )),
            reps
        )
        rows$double_metaphone <- .bench_compare(
            "double_metaphone()", function() double_metaphone(names_x),
            list("phonics::metaphone()" = .bench_baseline(
                function() phonics::metaphone(names_x),
                expect_match = FALSE, note = "different algorithm (Double Metaphone vs Metaphone), and a different return shape"
            )),
            reps
        )
        rows$caverphone <- .bench_compare(
            "caverphone()", function() caverphone(names_x),
            list("phonics::caverphone()" = .bench_baseline(
                function() phonics::caverphone(names_x, maxCodeLen = 10),
                expect_match = FALSE, note = "different Caverphone revision"
            )),
            reps
        )
    }
    rows
}

.bench_print_table <- function(tbl) {
    cli::cli_h2("Summary")
    if (is.null(tbl) || !nrow(tbl)) {
        cli::cli_alert_warning("No comparisons were run.")
        return(invisible())
    }
    base_fmt <- sprintf("%.4f", tbl$base_s)
    fast_fmt <- sprintf("%.4f", tbl$fast_s)
    faster <- tbl$fast_s <= tbl$base_s
    speed_symbol <- ifelse(faster, cli::symbol$tick, cli::symbol$cross)

    cols <- list(
        Speed              = speed_symbol,
        Operation          = tbl$operation,
        Baseline           = tbl$baseline,
        `Baseline (s)`     = base_fmt,
        `fast.string (s)`  = fast_fmt,
        Speedup            = tbl$speedup_label,
        Match              = tbl$match_symbol
    )
    # Column widths and padding are computed from the plain (uncoloured)
    # text using nchar(type = "width") + strrep(), not sprintf("%-Ns", ...)
    # — sprintf's field width counts *bytes* for multi-byte UTF-8 text on
    # this platform, which under-pads a symbol like the tick/cross glyphs
    # and throws off alignment. Colour is applied only afterwards, to whole
    # already-padded cells, so the ANSI codes never disturb it either.
    pad_right <- function(txt, w) paste0(txt, strrep(" ", pmax(0L, w - nchar(txt, type = "width"))))
    widths <- vapply(names(cols), function(nm) {
        max(nchar(cols[[nm]], type = "width"), nchar(nm, type = "width"))
    }, integer(1))
    padded <- lapply(names(cols), function(nm) pad_right(cols[[nm]], widths[[nm]]))
    names(padded) <- names(cols)

    header <- cli::style_bold(paste(
        mapply(pad_right, names(cols), widths),
        collapse = "  "
    ))
    sep <- cli::col_grey(strrep("-", sum(widths) + 2L * (length(widths) - 1L)))

    speed_col <- ifelse(faster, cli::col_green(padded$Speed), cli::col_red(padded$Speed))
    match_col <- ifelse(
        tbl$match_symbol == "match", cli::col_green(padded$Match),
        ifelse(tbl$match_symbol == "DIFFERS!", cli::col_red(padded$Match), cli::col_cyan(padded$Match))
    )
    body <- paste(
        speed_col, padded$Operation, padded$Baseline, padded$`Baseline (s)`,
        padded$`fast.string (s)`, padded$Speedup, match_col,
        sep = "  "
    )

    cli::cli_verbatim(c(header, sep, body))
}

.bench_print_correctness_summary <- function(tbl) {
    cli::cli_h2("Correctness")
    if (is.null(tbl) || !nrow(tbl)) {
        cli::cli_alert_warning("No comparisons to check.")
        return(invisible())
    }
    total <- nrow(tbl)
    matched <- sum(tbl$match)
    expected_diff <- sum(!tbl$match & !tbl$expect_match)
    unexpected <- sum(!tbl$match & tbl$expect_match)

    if (matched == total) {
        cli::cli_alert_success("All {total} compared outputs matched exactly (within floating-point tolerance).")
    } else {
        cli::cli_alert_info("{matched}/{total} compared outputs matched.")
    }
    if (expected_diff > 0) {
        cli::cli_alert_info("{expected_diff} differ by design (different algorithm/revision) — see the Match column and notes above.")
    }
    if (unexpected > 0) {
        cli::cli_alert_danger("{unexpected} differ UNEXPECTEDLY — investigate before trusting these numbers. See the rows marked \"DIFFERS!\" above.")
    }
}

.bench_ask_n <- function(default) {
    cli::cli_text("How many test strings should each comparison use?")
    ans <- trimws(readline(sprintf("n (blank = %s) > ", format(default, big.mark = ",", scientific = FALSE))))
    if (!nzchar(ans)) return(default)
    val <- suppressWarnings(as.integer(ans))
    if (is.na(val) || val < 1L) {
        cli::cli_alert_warning("Not a positive whole number — using {format(default, big.mark = ',', scientific = FALSE)}.")
        return(default)
    }
    val
}

.bench_ask_family <- function(default) {
    cli::cli_text("Which function family should be benchmarked?")
    cli::cli_ul(c(
        "1: Matching & substitution ({.fn grepl}, {.fn gsub}, {.fn fcount}; + {.pkg stringi} if installed)",
        "2: String utilities ({.fn trimws}, {.fn substr}, {.fn nchar}, {.fn chartr}; + {.pkg stringi} if installed)",
        "3: Dates & timestamps ({.fn as.Date}, {.fn as.POSIXct})",
        "4: String similarity & edit distance ({.pkg stringdist} / {.pkg RecordLinkage} if installed)",
        "5: Phonetic codes ({.pkg RecordLinkage} / {.pkg phonics} if installed)",
        "6: All of the above"
    ))
    ans <- trimws(readline("1-6 (blank = all) > "))
    switch(ans,
        "1" = "matching", "2" = "strings", "3" = "dates",
        "4" = "similarity", "5" = "phonetic", "6" = "all",
        default
    )
}

.bench_ask_reps <- function(default) {
    cli::cli_text("How many timed repetitions per comparison (best-of-N is reported)?")
    ans <- trimws(readline(sprintf("reps (blank = %d) > ", default)))
    if (!nzchar(ans)) return(default)
    val <- suppressWarnings(as.integer(ans))
    if (is.na(val) || val < 1L) {
        cli::cli_alert_warning("Not a positive whole number — using {default}.")
        return(default)
    }
    val
}

.bench_resolve <- function(value, ask_fn, default, is_interactive) {
    if (!is.null(value)) return(value)
    if (isTRUE(is_interactive)) return(ask_fn(default))
    default
}

#' Interactive speed comparison: fast.string vs base R and other packages
#'
#' Runs a live, on-this-machine comparison of fast.string functions against
#' base R and — where installed — the alternative-package implementations
#' `docs/06-benchmarks.qmd` also compares against (`stringi` for matching and
#' string utilities; `stringdist` and `RecordLinkage` for string similarity
#' and edit distance; `RecordLinkage` and `phonics` for phonetic codes).
#' Comparisons whose package isn't installed are skipped, not errored.
#'
#' Every comparison is a **fresh random sample**, seeded by `seed`, and the
#' packages being compared can implement meaningfully different string or
#' regex conventions (PCRE2 here, base R's TRE/POSIX or `perl = TRUE`, ICU
#' in stringi, ...; different phonetic-algorithm revisions in `phonics`).
#' So a speed number alone doesn't establish the two sides computed the same
#' thing: after timing each comparison, this function also checks whether
#' fast.string's output and the baseline's output actually agree, and
#' reports that in the `Match` column and in a closing correctness summary.
#' A handful of comparisons are documented, known exceptions and are
#' labelled "differs (expected)" rather than flagged as a problem: several
#' phonetic comparisons implement a different algorithm/revision by design;
#' `base::grepl()` returns `FALSE` for `NA` input (its own documented
#' exception, unlike every other function compared here, which returns
#' `NA`); and `RecordLinkage::jarowinkler()` applies its prefix bonus at a
#' lower similarity threshold, so it agrees with [jaro_winkler()] exactly on
#' near-duplicate pairs but diverges on largely-unrelated ones. Anything
#' else that disagrees is flagged clearly — that's a real finding, worth
#' investigating rather than ignoring.
#'
#' In an interactive session it asks for the input size, function family,
#' and repetition count; any argument supplied explicitly (or every
#' argument, when called non-interactively) skips its prompt.
#'
#' Timings come from [system.time()] on your machine, for the input you
#' choose — they will differ from any number printed elsewhere. For a
#' reproducible, full-scale (3M-row) measurement across every function, see
#' `Rscript bench.R --full` or the benchmarks chapter at
#' <https://kylehaynes.github.io/fast.string/06-benchmarks.html>.
#'
#' @param n Number of test strings to generate. `NULL` (the default) prompts
#'   for a value in interactive sessions, or uses `100000` otherwise.
#' @param family One of `"matching"`, `"strings"`, `"dates"`, `"similarity"`,
#'   `"phonetic"`, or `"all"`. `NULL` prompts in interactive sessions, or
#'   uses `"all"` otherwise.
#' @param reps Number of timed repetitions per comparison; the fastest run
#'   is reported. `NULL` prompts in interactive sessions, or uses `1`
#'   otherwise.
#' @param seed Random seed for the generated test data, for reproducibility.
#' @return Invisibly, a data frame with one row per (operation, baseline)
#'   comparison: `operation`, `baseline`, `base_s`, `fast_s`, `speedup`,
#'   `speedup_label`, `match` (logical), `expect_match` (logical), and
#'   `match_symbol`.
#' @family benchmarking functions
#' @examplesIf interactive()
#' # Prompts for input size, function family, and repetitions:
#' run_benchmark_example()
#'
#' # Or skip every prompt by supplying all three:
#' run_benchmark_example(n = 200000, family = "matching", reps = 3)
#' @export
run_benchmark_example <- function(n = NULL, family = NULL, reps = NULL, seed = 1) {
    is_interactive <- base::interactive()
    n      <- .bench_resolve(n, .bench_ask_n, 100000L, is_interactive)
    family <- .bench_resolve(family, .bench_ask_family, "all", is_interactive)
    reps   <- .bench_resolve(reps, .bench_ask_reps, 1L, is_interactive)
    family <- match.arg(family, c("matching", "strings", "dates", "similarity", "phonetic", "all"))

    set.seed(seed)
    cli::cli_h1("fast.string benchmark: fast.string vs base R and other packages")
    cli::cli_alert_info(
        "n = {format(n, big.mark = ',', scientific = FALSE)} strings, family = {.val {family}}, reps = {.val {reps}}, seed = {.val {seed}}"
    )
    cli::cli_text(paste(
        "Test data is freshly {.strong randomly} generated for this run (seeded, so it's",
        "reproducible with the same {.arg seed}) — rerun to see natural variance. Different",
        "packages can use different regex engines or string conventions, so every",
        "comparison's {.emph output} is checked for equality after timing, not just its speed;",
        "see the {.strong Match} column and the closing {.strong Correctness} summary."
    ))
    optional <- c("stringi", "stringdist", "RecordLinkage", "phonics")
    have <- vapply(optional, .bench_pkg, logical(1))
    cli::cli_text(sprintf(
        "Optional comparison packages detected: %s.%s",
        if (any(have)) paste(optional[have], collapse = ", ") else "none",
        if (any(!have)) sprintf(" Not installed (skipped): %s.", paste(optional[!have], collapse = ", ")) else ""
    ))

    x <- .bench_test_strings(n)

    rows <- list()
    if (family %in% c("matching",   "all")) rows <- c(rows, .bench_matching(x, reps))
    if (family %in% c("strings",    "all")) rows <- c(rows, .bench_strings(x, reps))
    if (family %in% c("dates",      "all")) rows <- c(rows, .bench_dates(n, reps))
    if (family %in% c("similarity", "all")) rows <- c(rows, .bench_similarity(n, reps))
    if (family %in% c("phonetic",   "all")) rows <- c(rows, .bench_phonetic(n, reps))
    rows <- rows[!vapply(rows, is.null, logical(1))]

    tbl <- if (length(rows)) do.call(rbind, rows) else NULL
    if (!is.null(tbl)) rownames(tbl) <- NULL
    .bench_print_table(tbl)
    .bench_print_correctness_summary(tbl)

    cli::cli_alert_info(
        "Done. Rerun any time with {.fn run_benchmark_example}, or silence the base-function reminders with {.code options(fast.string.reminders = FALSE)}."
    )
    invisible(tbl)
}
