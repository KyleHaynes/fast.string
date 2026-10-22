#' LCS, Indel, prefix and postfix distances
#'
#' Ports of the remaining measures in RapidFuzz's `distance` module, scored
#' pair by pair in parallel across CPU cores via Intel TBB:
#'
#' * `lcs_seq_distance()`: `max(nchar(a), nchar(b))` minus the length of
#'   the longest common subsequence (LCS); `lcs_seq_similarity()` is the
#'   LCS length divided by the longer length. The LCS length itself is
#'   `max(nchar(a), nchar(b)) - lcs_seq_distance(a, b)`.
#' * `indel_distance()`: the number of insertions and deletions (no
#'   substitutions) that turn `a` into `b`, `nchar(a) + nchar(b) - 2 * LCS`;
#'   `indel_similarity()` is `1 - indel_distance / (nchar(a) + nchar(b))`,
#'   which is [fuzz_ratio()] on a 0-1 scale.
#' * `prefix_distance()` and `postfix_distance()`: the longer length minus
#'   the length of the common prefix (suffix); the similarities divide the
#'   common prefix (suffix) length by the longer length.
#'
#' Distances count characters (or bytes); similarities are in `[0, 1]`, and
#' two empty strings score `1`. The LCS uses RapidFuzz's bit-parallel
#' algorithm, so the results agree with `rapidfuzz.distance`'s
#' `LCSseq`, `Indel`, `Prefix` and `Postfix`.
#'
#' @param a,b Equal-length character vectors.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @param use_bytes Logical. Compare UTF-8 code points (`FALSE`, the
#'   default, as RapidFuzz compares the characters of a Python string) or
#'   bytes (`TRUE`).
#' @return Numeric vector the same length as `a`; `NA` where either input is
#'   `NA`.
#' @seealso [edit_distance] for Levenshtein and its relatives, and
#'   [edit_ops()] for the edits themselves.
#' @family edit distance functions
#' @examples
#' lcs_seq_distance("kitten", "sitting")
#' lcs_seq_similarity("kitten", "sitting")
#' indel_distance("kitten", "sitting")
#' indel_similarity("kitten", "sitting")
#' prefix_similarity(c("Johnson", "Johnston"), c("Johnston", "Jonson"))
#' postfix_distance("Thompson", "Simpson")
#' @name lcs_seq
NULL

.simple_metric <- function(a, b, metric, nthreads, use_bytes) {
    .editdist_validate(a, b, use_bytes)
    fast_simple_metric_impl(a, b, metric, use_bytes, .as_nthreads(nthreads))
}

#' @rdname lcs_seq
#' @export
lcs_seq_distance <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 0L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
lcs_seq_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 1L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
indel_distance <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 2L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
indel_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 3L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
prefix_distance <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 4L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
prefix_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 5L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
postfix_distance <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 6L, nthreads, use_bytes)

#' @rdname lcs_seq
#' @export
postfix_similarity <- function(a, b, nthreads = NULL, use_bytes = FALSE)
    .simple_metric(a, b, 7L, nthreads, use_bytes)

#' The edits that turn one string into another
#'
#' Lists the Levenshtein edits that turn each `a[i]` into `b[i]`, as
#' RapidFuzz's `Levenshtein.editops()` and `Levenshtein.opcodes()` do (and
#' Python's `difflib` opcodes), choosing the same alignment as RapidFuzz
#' when several are equally short.
#'
#' With `type = "editops"` each row is one edit: `"replace"` `a[a_pos]` with
#' `b[b_pos]`, `"delete"` `a[a_pos]`, or `"insert"` `b[b_pos]` before
#' `a[a_pos]` (`a_pos` is `nchar(a) + 1` when inserting at the end). With
#' `type = "opcodes"` each row is a block: `a_start:a_end` of `a` becomes
#' `b_start:b_end` of `b`, either unchanged (`"equal"`) or by one kind of
#' edit; empty ranges have `end = start - 1`, as with [substr()]. Positions
#' are 1-based characters (bytes with `use_bytes = TRUE`); RapidFuzz's are
#' 0-based.
#'
#' @param a,b Equal-length character vectors.
#' @param type `"editops"` (the default) or `"opcodes"`.
#' @param use_bytes Logical. Compare UTF-8 code points (`FALSE`, the
#'   default) or bytes (`TRUE`).
#' @return A data frame with one row per edit or block. `pair` is the index
#'   into `a` and `b` (pairs with an `NA` give no rows), `type` the edit,
#'   and then `a_pos` and `b_pos`, or `a_start`, `a_end`, `b_start` and
#'   `b_end`.
#' @seealso [levenshtein()] for the number of edits.
#' @family edit distance functions
#' @examples
#' edit_ops("kitten", "sitting")
#' edit_ops("kitten", "sitting", type = "opcodes")
#' @export
edit_ops <- function(a, b, type = c("editops", "opcodes"), use_bytes = FALSE) {
    .editdist_validate(a, b, use_bytes)
    type <- match.arg(type)
    res <- fast_edit_ops_impl(a, b, type == "opcodes", use_bytes)
    kinds <- c("equal", "replace", "insert", "delete")
    if (type == "editops") {
        return(data.frame(pair = res$pair, type = kinds[res$kind + 1L],
                          a_pos = as.integer(res$a1), b_pos = as.integer(res$b1),
                          stringsAsFactors = FALSE))
    }
    data.frame(pair = res$pair, type = kinds[res$kind + 1L],
               a_start = as.integer(res$a1), a_end = as.integer(res$a2),
               b_start = as.integer(res$b1), b_end = as.integer(res$b2),
               stringsAsFactors = FALSE)
}
