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
#' In interactive sessions, calling a base function that has a fast.string
#' equivalent (`grepl()`, `grep()`, `sub()`, `gsub()`, `trimws()`, `substr()`,
#' `nchar()`, `chartr()`) prints a one-time-per-function reminder naming the
#' faster drop-in; the base function still runs unchanged either way. Turn
#' this off with `options(fast.string.reminders = FALSE)`.
#'
#' @eval .index_roxygen()
#'
#' @section Conventions:
#' * **Missing values** propagate: an `NA` input gives an `NA` result, and the
#'   comparison functions return `NA` when either side is `NA`.
#' * **Threads**: the matching, substitution, similarity and lookup
#'   functions take `nthreads`. `NULL` (the default) uses the RcppParallel
#'   setting, see [RcppParallel::setThreadOptions()]; `1` forces serial
#'   execution. The remaining parallel functions (phonetic codes, dates,
#'   [fnchar()], [fsubstr()], [fchartr()]) always use that setting.
#' * **Bytes or code points**: [jaro_winkler()] and the [edit_distance]
#'   functions compare encoded bytes by default (`use_bytes = TRUE`) for
#'   compatibility and speed; pass `use_bytes = FALSE` to compare UTF-8 code
#'   points. [fuzzy_match()] and [fuzzy_top_n()] default to code points.
#' * **Regular expressions**: [fgrepl()], [fcount()], [fsub()], [fgsub()]
#'   and [gsub_all()] run every pattern on PCRE2. With `perl = FALSE` they
#'   follow base R's default (TRE) regular expressions, except that the
#'   first matching alternative wins rather than the longest; patterns with
#'   PCRE-only syntax (lookarounds, atomic groups, ...) get `perl = TRUE`
#'   semantics. Text is matched by UTF-8 character, translating `latin1`
#'   input, unless `useBytes = TRUE`.
#' * **Phonetic codes** fold accented Latin letters to ASCII first, so
#'   accented and plain spellings of a name get the same code.
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
