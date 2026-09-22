test_that("run_benchmark_example is registered in the function index", {
    expect_identical(
        unname(fast.string:::.index_functions()[["run_benchmark_example"]]),
        "interactive speed demo vs base R"
    )
})

# cli_verbatim() emits through message(), not stdout, so capture.output()
# alone sees nothing; this helper pulls the actual printed lines out via a
# message handler instead. The leading cli_h2("Summary") heading is a
# separate, differently-formatted message, so it's dropped here too —
# callers want just the table's own header/separator/data rows.
.capture_bench_table_lines <- function(tbl) {
    lines <- character()
    withCallingHandlers(
        fast.string:::.bench_print_table(tbl),
        message = function(m) {
            lines <<- c(lines, strsplit(conditionMessage(m), "\n")[[1]])
            invokeRestart("muffleMessage")
        }
    )
    lines <- lines[nzchar(trimws(lines))]
    lines[!grepl("Summary", lines, fixed = TRUE)]
}

test_that(".bench_print_table() keeps every printed line the same display width", {
    # Regression test: sprintf("%-Ns", ...) pads by *bytes*, not display
    # width, for multi-byte UTF-8 text on this platform, which silently
    # under-padded the tick/cross column and broke column alignment.
    tbl <- suppressMessages(run_benchmark_example(n = 2000, family = "matching", reps = 1, seed = 1))

    old <- options(cli.num_colors = 1)
    on.exit(options(old), add = TRUE)
    plain <- .capture_bench_table_lines(tbl)
    options(cli.num_colors = 256)
    coloured <- .capture_bench_table_lines(tbl)

    expect_length(unique(cli::ansi_nchar(plain, type = "width")), 1L)
    expect_length(unique(cli::ansi_nchar(coloured, type = "width")), 1L)
    expect_identical(cli::ansi_strip(coloured), plain)
})

test_that(".bench_print_table() marks a fast.string win with a tick and a loss with a cross", {
    tbl <- data.frame(
        operation = c("winning_op", "losing_op"), baseline = c("b1", "b2"),
        base_s = c(1, 1), fast_s = c(0.5, 2),
        speedup = c(2, 0.5), speedup_label = c("2.0x", "0.5x"),
        match = c(TRUE, TRUE), expect_match = c(TRUE, TRUE),
        match_symbol = c("match", "match"), stringsAsFactors = FALSE
    )
    old <- options(cli.num_colors = 1)
    on.exit(options(old), add = TRUE)
    lines <- .capture_bench_table_lines(tbl)

    # The tick/cross fallback glyphs ("v"/"x" without UTF-8 output support)
    # are common letters that can also appear elsewhere in a row (e.g. the
    # "x" in a "2.0x" speedup label), so check the leading column only,
    # not a whole-line substring search.
    leading_symbol <- function(line) sub("^(\\S+).*$", "\\1", line)
    win_line  <- lines[grepl("winning_op", lines, fixed = TRUE)]
    lose_line <- lines[grepl("losing_op", lines, fixed = TRUE)]
    expect_identical(leading_symbol(win_line), cli::symbol$tick)
    expect_identical(leading_symbol(lose_line), cli::symbol$cross)
})

test_that(".bench_resolve() skips the ask function when a value is supplied", {
    asked <- FALSE
    ask_fn <- function(default) { asked <<- TRUE; default }

    expect_identical(fast.string:::.bench_resolve(42, ask_fn, 1, is_interactive = TRUE), 42)
    expect_false(asked)
    expect_identical(fast.string:::.bench_resolve(42, ask_fn, 1, is_interactive = FALSE), 42)
    expect_false(asked)
})

test_that(".bench_resolve() asks only when the value is NULL and interactive", {
    asked <- FALSE
    ask_fn <- function(default) { asked <<- TRUE; 999 }

    expect_identical(fast.string:::.bench_resolve(NULL, ask_fn, 1, is_interactive = TRUE), 999)
    expect_true(asked)

    asked <- FALSE
    expect_identical(fast.string:::.bench_resolve(NULL, ask_fn, 1, is_interactive = FALSE), 1)
    expect_false(asked)
})

test_that(".bench_ask_n() parses a valid answer and falls back on blank/invalid input", {
    suppressMessages({
        testthat::local_mocked_bindings(readline = function(prompt = "") "12345", .package = "base")
        expect_identical(fast.string:::.bench_ask_n(999L), 12345L)

        testthat::local_mocked_bindings(readline = function(prompt = "") "", .package = "base")
        expect_identical(fast.string:::.bench_ask_n(777L), 777L)

        testthat::local_mocked_bindings(readline = function(prompt = "") "not a number", .package = "base")
        expect_identical(fast.string:::.bench_ask_n(777L), 777L)
    })
})

test_that(".bench_ask_family() maps menu numbers and falls back to 'all'", {
    suppressMessages({
        testthat::local_mocked_bindings(readline = function(prompt = "") "1", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "matching")

        testthat::local_mocked_bindings(readline = function(prompt = "") "3", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "dates")

        testthat::local_mocked_bindings(readline = function(prompt = "") "4", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "similarity")

        testthat::local_mocked_bindings(readline = function(prompt = "") "5", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "phonetic")

        testthat::local_mocked_bindings(readline = function(prompt = "") "", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "all")

        testthat::local_mocked_bindings(readline = function(prompt = "") "banana", .package = "base")
        expect_identical(fast.string:::.bench_ask_family("all"), "all")
    })
})

test_that(".bench_ask_reps() parses a valid answer and falls back on blank/invalid input", {
    suppressMessages({
        testthat::local_mocked_bindings(readline = function(prompt = "") "5", .package = "base")
        expect_identical(fast.string:::.bench_ask_reps(1L), 5L)

        testthat::local_mocked_bindings(readline = function(prompt = "") "", .package = "base")
        expect_identical(fast.string:::.bench_ask_reps(1L), 1L)

        testthat::local_mocked_bindings(readline = function(prompt = "") "0", .package = "base")
        expect_identical(fast.string:::.bench_ask_reps(1L), 1L)
    })
})

test_that(".bench_test_strings() generates the right length, type, and some NAs", {
    x <- fast.string:::.bench_test_strings(5000L)
    expect_type(x, "character")
    expect_length(x, 5000L)
    expect_true(any(is.na(x)))
    expect_true(any(grepl("^row-", x[!is.na(x)])))
})

test_that(".bench_test_names() generates name-like strings with some NAs", {
    x <- fast.string:::.bench_test_names(5000L)
    expect_type(x, "character")
    expect_length(x, 5000L)
    expect_true(any(is.na(x)))
    expect_true(all(grepl("^[A-Z]+ [A-Z]+$", x[!is.na(x)])))
})

test_that(".bench_pkg() reflects whether a namespace is installed", {
    expect_true(fast.string:::.bench_pkg("base"))
    expect_false(fast.string:::.bench_pkg("not.a.real.package.xyz"))
})

test_that(".bench_values_match() agrees on identical vectors and disagrees on different ones", {
    expect_true(fast.string:::.bench_values_match(c(1, 2, 3), c(1, 2, 3)))
    expect_true(fast.string:::.bench_values_match(c("a", "b"), c("a", "b")))
    expect_false(fast.string:::.bench_values_match(c(1, 2, 3), c(1, 2, 4)))
    expect_false(fast.string:::.bench_values_match(c("a", "b"), c("a", "c")))
})

test_that(".bench_values_match() tolerates floating-point noise but not real differences", {
    expect_true(fast.string:::.bench_values_match(1 / 3, 0.3333333333333))
    expect_false(fast.string:::.bench_values_match(0.5, 0.6))
})

test_that(".bench_values_match() requires NA positions to match exactly", {
    expect_true(fast.string:::.bench_values_match(c(1, NA, 3), c(1, NA, 3)))
    expect_false(fast.string:::.bench_values_match(c(1, NA, 3), c(1, 2, 3)))
    expect_false(fast.string:::.bench_values_match(c(NA, NA), c(1, NA)))
})

test_that(".bench_values_match() treats a length mismatch as a non-match, not an error", {
    expect_false(fast.string:::.bench_values_match(1:3, 1:4))
    expect_false(fast.string:::.bench_values_match(
        data.frame(a = 1:2, b = 3:4), 1:9
    ))
})

test_that(".bench_speedup() computes a plain ratio, a lower bound, or 'too fast to measure'", {
    plain <- fast.string:::.bench_speedup(t_base = 1, t_fast = 0.5)
    expect_identical(plain$speedup, 2)
    expect_identical(plain$label, "2.0x")

    bound <- fast.string:::.bench_speedup(t_base = 1, t_fast = 0)
    expect_true(is.na(bound$speedup))
    expect_match(bound$label, "^>[0-9]+x")

    neither <- fast.string:::.bench_speedup(t_base = 0, t_fast = 0)
    expect_true(is.na(neither$speedup))
    expect_identical(neither$label, "too fast to measure at this n")
})

test_that(".bench_baseline() builds the expected structure", {
    bl <- fast.string:::.bench_baseline(function() 1, expect_match = FALSE, note = "x")
    expect_type(bl$call, "closure")
    expect_false(bl$expect_match)
    expect_identical(bl$note, "x")

    bl2 <- fast.string:::.bench_baseline(function() 1)
    expect_true(bl2$expect_match)
    expect_null(bl2$note)
})

test_that(".bench_run() captures both the timing and the return value", {
    run <- fast.string:::.bench_run(function() 42, reps = 2)
    expect_identical(run$value, 42)
    expect_true(run$elapsed >= 0)
    expect_true(run$ok)
    expect_null(run$error)
})

test_that(".bench_run() catches an error instead of propagating it", {
    run <- fast.string:::.bench_run(function() stop("boom"), reps = 1)
    expect_false(run$ok)
    expect_match(run$error, "boom")
})

test_that(".bench_compare() labels a genuine mismatch as DIFFERS! when expect_match is TRUE", {
    row <- suppressMessages(fast.string:::.bench_compare(
        "op", function() 1:3,
        list("wrong" = fast.string:::.bench_baseline(function() 1:4)),
        reps = 1
    ))
    expect_false(row$match)
    expect_identical(row$match_symbol, "DIFFERS!")
})

test_that(".bench_compare() labels a known mismatch as expected when expect_match is FALSE", {
    row <- suppressMessages(fast.string:::.bench_compare(
        "op", function() 1:3,
        list("wrong" = fast.string:::.bench_baseline(function() 1:4, expect_match = FALSE, note = "by design")),
        reps = 1
    ))
    expect_false(row$match)
    expect_identical(row$match_symbol, "differs (expected)")
})

test_that(".bench_compare() reports a genuine match", {
    row <- suppressMessages(fast.string:::.bench_compare(
        "op", function() 1:3,
        list("right" = fast.string:::.bench_baseline(function() 1:3)),
        reps = 1
    ))
    expect_true(row$match)
    expect_identical(row$match_symbol, "match")
})

test_that(".bench_compare() skips gracefully when no baselines are available", {
    expect_null(suppressMessages(fast.string:::.bench_compare("op", function() 1, list(), reps = 1)))
})

test_that(".bench_compare() skips a baseline that errors, instead of propagating", {
    row <- suppressMessages(fast.string:::.bench_compare(
        "op", function() 1,
        list(
            "broken" = fast.string:::.bench_baseline(function() stop("nope")),
            "fine"    = fast.string:::.bench_baseline(function() 1)
        ),
        reps = 1
    ))
    expect_identical(nrow(row), 1L)
    expect_identical(row$baseline, "fine")
})

test_that("run_benchmark_example() runs each family non-interactively with explicit arguments", {
    matching <- suppressMessages(
        run_benchmark_example(n = 5000, family = "matching", reps = 1, seed = 1)
    )
    expect_s3_class(matching, "data.frame")
    expect_setequal(unique(matching$operation), c("fgrepl()", "fgsub()", "fcount()"))

    strings <- suppressMessages(
        run_benchmark_example(n = 5000, family = "strings", reps = 1, seed = 1)
    )
    expect_setequal(unique(strings$operation), c("ftrimws()", "fsubstr()", "fnchar()", "fchartr()"))

    dates <- suppressMessages(
        run_benchmark_example(n = 5000, family = "dates", reps = 1, seed = 1)
    )
    expect_setequal(unique(dates$operation), c("fas.Date()", "fas.POSIXct()"))

    all_families <- suppressMessages(
        run_benchmark_example(n = 5000, family = "all", reps = 1, seed = 1)
    )
    expect_true(all(
        c(matching$operation, strings$operation, dates$operation) %in% all_families$operation
    ))
    # every operation run has at least one baseline row
    expect_true(nrow(all_families) >= length(unique(all_families$operation)))
})

test_that("run_benchmark_example() returns the expected columns and non-negative timings", {
    tbl <- suppressMessages(
        run_benchmark_example(n = 2000, family = "matching", reps = 1, seed = 1)
    )
    expect_true(all(
        c("operation", "baseline", "base_s", "fast_s", "speedup",
          "speedup_label", "match", "expect_match", "match_symbol") %in% names(tbl)
    ))
    expect_true(all(tbl$base_s >= 0))
    expect_true(all(tbl$fast_s >= 0))
    expect_type(tbl$match, "logical")
})

test_that("run_benchmark_example() flags no unexpected mismatches at a fixed seed", {
    # A real correctness contract, not just a shape check: every comparison
    # this ships either genuinely agrees, or is a documented, expected
    # divergence. A failure here is a real regression to investigate, not a
    # flaky test to silence.
    tbl <- suppressMessages(
        run_benchmark_example(n = 3000, family = "all", reps = 1, seed = 1)
    )
    unexpected <- tbl[!tbl$match & tbl$expect_match, c("operation", "baseline")]
    expect_identical(nrow(unexpected), 0L, info = paste(capture.output(print(unexpected)), collapse = "\n"))
})

test_that("run_benchmark_example() skips similarity/phonetic gracefully with no comparison packages", {
    testthat::local_mocked_bindings(.bench_pkg = function(pkg) FALSE)
    similarity <- suppressMessages(run_benchmark_example(n = 500, family = "similarity", reps = 1, seed = 1))
    phonetic   <- suppressMessages(run_benchmark_example(n = 500, family = "phonetic", reps = 1, seed = 1))
    expect_null(similarity)
    expect_null(phonetic)
})

test_that("run_benchmark_example() rejects an invalid family", {
    expect_error(
        suppressMessages(run_benchmark_example(n = 100, family = "nope", reps = 1)),
        "should be one of"
    )
})

test_that("run_benchmark_example() never calls readline() when every argument is supplied", {
    testthat::local_mocked_bindings(
        readline = function(prompt = "") stop("readline() should not have been called"),
        .package = "base"
    )
    tbl <- suppressMessages(
        run_benchmark_example(n = 500, family = "matching", reps = 1, seed = 1)
    )
    expect_s3_class(tbl, "data.frame")
})
