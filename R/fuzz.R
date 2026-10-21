#' RapidFuzz string scorers
#'
#' Vectorised ports of the scorers in the Python
#' [RapidFuzz](https://github.com/rapidfuzz/RapidFuzz) package's `fuzz`
#' module, the maintained successor to fuzzywuzzy (thefuzz, fuzzywuzzy's own
#' successor, now runs on RapidFuzz too). Each compares `a[i]` with `b[i]`
#' and returns a similarity from 0 to 100. The arithmetic follows
#' rapidfuzz-cpp step by step, so scores agree with `rapidfuzz.fuzz` to
#' floating-point precision. Pairs are scored in parallel through
#' RcppParallel, with no Python dependency.
#'
#' * `fuzz_ratio()`: the normalised Indel similarity,
#'   `100 * (1 - d / (nchar(a) + nchar(b)))`, where `d` counts the
#'   insertions and deletions that turn one string into the other. This
#'   equals `200 * L / (nchar(a) + nchar(b))` for a longest common
#'   subsequence of length `L`.
#' * `fuzz_partial_ratio()`: the best `fuzz_ratio()` of the shorter string
#'   against a window of the longer one: every same-length window, plus
#'   the shorter windows at either end. The search is exhaustive, so the
#'   optimal alignment is always found; [fuzz_partial_ratio_alignment()]
#'   also reports where it is.
#' * `fuzz_token_sort_ratio()`: `fuzz_ratio()` after sorting each string's
#'   whitespace-separated words, so word order stops mattering.
#' * `fuzz_token_set_ratio()`: compares the words both strings share with
#'   the words only one of them has. Duplicate words are ignored, and the
#'   score is 100 when one string's words are a subset of the other's.
#' * `fuzz_token_ratio()`: the larger of `fuzz_token_sort_ratio()` and
#'   `fuzz_token_set_ratio()`, computed together.
#' * `fuzz_partial_token_sort_ratio()`, `fuzz_partial_token_set_ratio()` and
#'   `fuzz_partial_token_ratio()`: the same three with
#'   `fuzz_partial_ratio()` in place of `fuzz_ratio()`. The set versions
#'   score 100 as soon as the strings share a word.
#' * `fuzz_wratio()`: RapidFuzz's `WRatio`, a weighted best of the scorers
#'   above. It switches to the partial scorers once one string is 1.5
#'   times the length of the other.
#' * `fuzz_qratio()`: RapidFuzz's `QRatio`, which is `fuzz_ratio()` except
#'   that an empty string scores 0.
#'
#' Strings are compared by Unicode code point, as Python compares the
#' characters of a `str`. As in RapidFuzz, a no-break space (U+00A0) or
#' U+0085 only separates words in a string that also contains a character
#' above U+00FF. Python stores other strings with one byte per character,
#' and RapidFuzz then splits only at ASCII whitespace.
#'
#' Empty strings follow RapidFuzz. `fuzz_token_set_ratio()`,
#' `fuzz_partial_token_set_ratio()`, `fuzz_wratio()` and `fuzz_qratio()`
#' score 0 when either string is empty (for the set scorers, when either has
#' no words). The other scorers give two empty strings 100.
#'
#' @param a,b Equal-length character vectors.
#' @param full_process Logical (default `TRUE`). When `TRUE`, both strings
#'   first go through [fuzz_default_process()]: letters are lowercased,
#'   every other character except digits becomes a space, and the ends are
#'   trimmed. This matches passing `processor = utils.default_process` in
#'   RapidFuzz. Use `FALSE` to compare the strings as given, which is
#'   RapidFuzz's default. Only a literal `TRUE` turns processing on.
#' @param score_cutoff Number between 0 and 100 (default 0). Scores below
#'   it are returned as 0, as in RapidFuzz, and the scorers use it to stop
#'   early.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Numeric vector of scores in `[0, 100]`, `length(a)` long, and `NA`
#'   where `a[i]` or `b[i]` is `NA`. Scores are not rounded; fuzzywuzzy and
#'   thefuzz round them to whole numbers.
#' @seealso [fuzz_extract()], [fuzz_extract_one()] and [fuzz_cdist()] to
#'   search a table with these scorers, [jaro_winkler_tokens()] for a
#'   word-order-tolerant Jaro-Winkler score, and [edit_distance] for
#'   edit-based measures.
#' @examples
#' fuzz_ratio("this is a test", "this is a test!", full_process = FALSE)
#' fuzz_partial_ratio("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear")
#' fuzz_token_sort_ratio("fuzzy was a bear", "bear was a fuzzy")
#' fuzz_token_set_ratio("fuzzy was a bear", "fuzzy fuzzy bear was a bear")
#' fuzz_token_ratio("fuzzy was a bear", "a fuzzy bear")
#' fuzz_partial_token_set_ratio("Acme Pty Ltd", "ACME Corporation")
#' fuzz_wratio("New York Mets", "The New York Mets baseball club")
#' fuzz_qratio("", "")
#' @name fuzz
NULL

# Scorer names in the order of the RfScorer ids in src/rapidfuzz_core.h.
.fuzz_scorers <- c(
    "ratio", "partial_ratio", "token_sort_ratio", "token_set_ratio",
    "token_ratio", "partial_token_sort_ratio", "partial_token_set_ratio",
    "partial_token_ratio", "wratio", "qratio"
)

.fuzz_validate <- function(a, b) {
    if (!is.character(a) || !is.character(b))
        stop("`a` and `b` must be character vectors.")
    if (length(a) != length(b))
        stop("`a` and `b` must have the same length.")
}

.fuzz_cutoff <- function(score_cutoff) {
    if (!is.numeric(score_cutoff) || length(score_cutoff) != 1L ||
        is.na(score_cutoff) || score_cutoff < 0 || score_cutoff > 100)
        stop("`score_cutoff` must be a number between 0 and 100.")
    as.double(score_cutoff)
}

# The scorer id for the C++ side, from a name ("token_set_ratio",
# RapidFuzz's "WRatio", "fuzz_qratio", ...) or one of the fuzz_*() scorer
# functions themselves.
.fuzz_scorer_id <- function(scorer) {
    if (is.function(scorer)) {
        for (i in seq_along(.fuzz_scorers)) {
            candidate <- get(paste0("fuzz_", .fuzz_scorers[i]),
                             mode = "function")
            if (identical(scorer, candidate)) return(i - 1L)
        }
        stop("`scorer` must be one of the fuzz_*() scorers or its name.")
    }
    if (!is.character(scorer) || length(scorer) != 1L || is.na(scorer))
        stop("`scorer` must be one of the fuzz_*() scorers or its name.")
    id <- match(sub("^fuzz_", "", tolower(scorer)), .fuzz_scorers)
    if (is.na(id))
        stop("`scorer` must be one of: ",
             paste0("\"", .fuzz_scorers, "\"", collapse = ", "), ".")
    id - 1L
}

.fuzz_pairwise <- function(a, b, scorer, full_process, score_cutoff,
                           nthreads) {
    .fuzz_validate(a, b)
    fast_fuzz_score_impl(
        a, b, .fuzz_scorer_id(scorer), isTRUE(full_process),
        .fuzz_cutoff(score_cutoff), .as_nthreads(nthreads)
    )
}

#' @rdname fuzz
#' @export
fuzz_ratio <- function(a, b, full_process = TRUE, score_cutoff = 0,
                       nthreads = NULL) {
    .fuzz_pairwise(a, b, "ratio", full_process, score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_partial_ratio <- function(a, b, full_process = TRUE, score_cutoff = 0,
                               nthreads = NULL) {
    .fuzz_pairwise(a, b, "partial_ratio", full_process, score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_token_sort_ratio <- function(a, b, full_process = TRUE,
                                  score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, "token_sort_ratio", full_process, score_cutoff,
                   nthreads)
}

#' @rdname fuzz
#' @export
fuzz_token_set_ratio <- function(a, b, full_process = TRUE,
                                 score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, "token_set_ratio", full_process, score_cutoff,
                   nthreads)
}

#' @rdname fuzz
#' @export
fuzz_token_ratio <- function(a, b, full_process = TRUE, score_cutoff = 0,
                             nthreads = NULL) {
    .fuzz_pairwise(a, b, "token_ratio", full_process, score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_partial_token_sort_ratio <- function(a, b, full_process = TRUE,
                                          score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, "partial_token_sort_ratio", full_process,
                   score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_partial_token_set_ratio <- function(a, b, full_process = TRUE,
                                         score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, "partial_token_set_ratio", full_process,
                   score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_partial_token_ratio <- function(a, b, full_process = TRUE,
                                     score_cutoff = 0, nthreads = NULL) {
    .fuzz_pairwise(a, b, "partial_token_ratio", full_process, score_cutoff,
                   nthreads)
}

#' @rdname fuzz
#' @export
fuzz_wratio <- function(a, b, full_process = TRUE, score_cutoff = 0,
                        nthreads = NULL) {
    .fuzz_pairwise(a, b, "wratio", full_process, score_cutoff, nthreads)
}

#' @rdname fuzz
#' @export
fuzz_qratio <- function(a, b, full_process = TRUE, score_cutoff = 0,
                        nthreads = NULL) {
    .fuzz_pairwise(a, b, "qratio", full_process, score_cutoff, nthreads)
}

#' Where fuzz_partial_ratio() aligns two strings
#'
#' Port of RapidFuzz's `fuzz.partial_ratio_alignment()`. It returns the
#' [fuzz_partial_ratio()] score together with the slices of `a` and `b` that
#' produce it, so that
#' `fuzz_ratio(substr(a, src_start, src_end), substr(b, dest_start, dest_end), full_process = FALSE)`
#' gives the score back. When several alignments tie, the one RapidFuzz
#' reports is returned.
#'
#' Positions count characters of the strings as compared. With
#' `full_process = TRUE` these are the strings after
#' [fuzz_default_process()], whose positions can differ from the input's
#' (leading punctuation and spaces are trimmed).
#'
#' @inheritParams fuzz
#' @return A data frame with one row per pair and columns `score`,
#'   `src_start` and `src_end` (the slice of `a`), and `dest_start` and
#'   `dest_end` (the slice of `b`). Starts are 1-based and ends inclusive, as
#'   [substr()] takes them, so an empty slice has `end = start - 1`. A row is
#'   all `NA` when either input is `NA`. When the score is below
#'   `score_cutoff`, `score` is 0 and the positions are `NA` (RapidFuzz
#'   returns `None`).
#' @seealso [fuzz_partial_ratio()]
#' @examples
#' al <- fuzz_partial_ratio_alignment("a certain string", "cetain",
#'                                    full_process = FALSE)
#' al
#' substr("a certain string", al$src_start, al$src_end)
#' @export
fuzz_partial_ratio_alignment <- function(a, b, full_process = TRUE,
                                         score_cutoff = 0, nthreads = NULL) {
    .fuzz_validate(a, b)
    result <- fast_fuzz_alignment_impl(
        a, b, isTRUE(full_process), .fuzz_cutoff(score_cutoff),
        .as_nthreads(nthreads)
    )
    as.data.frame(result)
}

#' RapidFuzz's default string preprocessing
#'
#' Port of `rapidfuzz.utils.default_process()`, which the [fuzz] scorers
#' apply when `full_process = TRUE`. Every character that is not a letter
#' or digit becomes a space, letters are lowercased, and spaces are trimmed
#' from both ends. Letters and digits are judged by Unicode category, so
#' `_` becomes a space too, and runs of spaces inside the string are kept.
#' The character tables are RapidFuzz's own, which come from Python's
#' Unicode database and cover all of Unicode: accented and non-Latin
#' letters are kept and lowercased, not removed.
#'
#' @param x Character vector.
#' @inheritParams fuzz
#' @return A UTF-8 character vector the length of `x`, keeping its names and
#'   `NA`s.
#' @seealso [fuzz]
#' @examples
#' fuzz_default_process(c("Hello, World!", "  O'Brien_Smith  ", NA))
#' @export
fuzz_default_process <- function(x, nthreads = NULL) {
    if (!is.character(x)) stop("`x` must be a character vector.")
    .copy_names(fast_fuzz_default_process_impl(x, .as_nthreads(nthreads)), x)
}
