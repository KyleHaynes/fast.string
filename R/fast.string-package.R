#' fast.string: parallel string, date and fuzzy-matching functions
#'
#' Fast, multi-core alternatives to base R's string functions, plus the
#' building blocks of a record-linkage pipeline: fixed-format date and
#' timestamp parsing, phonetic blocking keys, string-similarity and
#' edit-distance measures, and memory-bounded fuzzy lookup.
#'
#' Regular expressions run on PCRE2 and fixed strings on a prepared literal
#' searcher, with work split across cores by Intel TBB (through
#' RcppParallel). Small inputs are processed serially in the calling thread;
#' larger ones are divided between threads.
#'
#' Loading the package prints the function index below as a tree. Silence it
#' with `options(fast.string.verbose = FALSE)` before [library()], or wrap the
#' call in [suppressPackageStartupMessages()].
#'
#' @eval .index_roxygen()
#'
#' @section Conventions:
#' * **Missing values** propagate: an `NA` input gives an `NA` result, and the
#'   comparison functions return `NA` when either side is `NA`.
#' * **Threads**: every parallel function takes `nthreads`. `NULL` (the
#'   default) uses the RcppParallel setting, see
#'   [RcppParallel::setThreadOptions()]; `1` forces serial execution.
#' * **Bytes or code points**: [jaro_winkler()] and the [edit_distance]
#'   functions compare encoded bytes by default (`use_bytes = TRUE`) for
#'   compatibility and speed; pass `use_bytes = FALSE` to compare UTF-8 code
#'   points. [fuzzy_match()] and [fuzzy_top_n()] default to code points.
#' * **PCRE-only syntax**: [fgrepl()], [fcount()], [fsub()], [fgsub()] and
#'   [gsub_all()] detect lookarounds, atomic groups and similar syntax when
#'   `perl = FALSE`, and pass the call to base R with `perl = TRUE` (with a
#'   message) so results stay correct.
#'
#' @examples
#' raw <- c("  Smith, John ", "SMITH,  JON", "Jones, Mary", NA)
#'
#' # Clean: trim, collapse repeated whitespace, unify case.
#' clean <- toupper(fgsub("[[:space:]]+", " ", ftrimws(raw)))
#' clean
#'
#' # Block on a phonetic key...
#' soundex(clean)
#'
#' # ...then score candidates against a reference table.
#' reference <- c("JONES, MARY", "SMITH, JOHN")
#' round(jaro_winkler_matrix(clean, reference), 2)
#' fuzzy_match(clean, reference, min_score = 0.85)
"_PACKAGE"
