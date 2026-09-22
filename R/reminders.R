# Interactive-only reminders that a faster fast.string equivalent exists for
# a base function the user just called. Implemented by attaching a small
# environment of pass-through wrappers to the search path (position 2, right
# after .GlobalEnv) from .onAttach() in zzz.R, so only code typed at the
# console or sourced into .GlobalEnv is affected — lookups from inside any
# package's own namespace (including this one) never consult the search
# path, so nothing here changes behaviour for code that calls these base
# functions from within a package.
#
# Every wrapper delegates to the real base:: function unchanged; the only
# effect is an optional once-per-session message. Turn it off with
# options(fast.string.reminders = FALSE).
#
# `speedup` is measured, not guessed: docs/06-benchmarks.qmd, n = 3,000,000
# strings, one run on the maintainer's machine (see @tbl-summary there and
# docs/_freeze/06-benchmarks/execute-results/html.json for the raw figures).
# grep/sub have no dedicated benchmark row — fgrep() and fsub() share fgrepl()
# and fgsub()'s matching/substitution engine, so they reuse those numbers.
# Re-measure and update here when that chapter is re-rendered.
.reminder_map <- list(
    grepl  = list(fast = "fgrepl",  speedup = "~9x",   note = "PCRE2 + parallel matching"),
    grep   = list(fast = "fgrep",   speedup = "~9x",   note = "PCRE2 + parallel matching"),
    sub    = list(fast = "fsub",    speedup = "~1.7x", note = "PCRE2 + parallel substitution"),
    gsub   = list(fast = "fgsub",   speedup = "~1.7x", note = "PCRE2 + parallel substitution"),
    trimws = list(fast = "ftrimws", speedup = "~4x",   note = "parallel trimming"),
    substr = list(fast = "fsubstr", speedup = "~1.2x", note = "parallel extraction"),
    nchar  = list(fast = "fnchar",  speedup = "~6x",   note = "parallel counting"),
    chartr = list(fast = "fchartr", speedup = "~4x",   note = "parallel byte-table translation")
)

# `is_interactive` is a parameter (not a bare interactive() call) so tests
# can drive both branches without depending on how they're run.
.remind <- function(base_fn, is_interactive = base::interactive()) {
    if (!isTRUE(is_interactive)) return(invisible())
    if (!isTRUE(getOption("fast.string.reminders", TRUE))) return(invisible())

    info <- .reminder_map[[base_fn]]
    cli::cli_inform(
        c("i" = "{.fn {base_fn}} called — {.fn {info$fast}} is a faster drop-in, {info$speedup} on 3M-row benchmarks ({info$note}); see {.url https://kylehaynes.github.io/fast.string/} for more information.",
          " " = "Silence with {.code options(fast.string.reminders = FALSE)}."),
        .frequency = "once",
        .frequency_id = paste0("fast.string::", base_fn)
    )
    invisible()
}

.reminder_grepl  <- function(...) { .remind("grepl");  base::grepl(...) }
.reminder_grep   <- function(...) { .remind("grep");   base::grep(...) }
.reminder_sub    <- function(...) { .remind("sub");    base::sub(...) }
.reminder_gsub   <- function(...) { .remind("gsub");   base::gsub(...) }
.reminder_trimws <- function(...) { .remind("trimws"); base::trimws(...) }
.reminder_substr <- function(...) { .remind("substr"); base::substr(...) }
.reminder_nchar  <- function(...) { .remind("nchar");  base::nchar(...) }
.reminder_chartr <- function(...) { .remind("chartr"); base::chartr(...) }

.reminders_search_name <- "fast.string:reminders"

.reminder_env <- function() {
    env <- new.env(parent = baseenv())
    env$grepl  <- .reminder_grepl
    env$grep   <- .reminder_grep
    env$sub    <- .reminder_sub
    env$gsub   <- .reminder_gsub
    env$trimws <- .reminder_trimws
    env$substr <- .reminder_substr
    env$nchar  <- .reminder_nchar
    env$chartr <- .reminder_chartr
    env
}

.attach_reminders <- function() {
    if (.reminders_search_name %in% base::search()) return(invisible())
    base::attach(.reminder_env(), name = .reminders_search_name, warn.conflicts = FALSE)
    invisible()
}

.detach_reminders <- function() {
    if (.reminders_search_name %in% base::search())
        base::detach(.reminders_search_name, character.only = TRUE)
    invisible()
}
