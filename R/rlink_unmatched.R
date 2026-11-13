#' Records that no pair reaches
#'
#' Returns the row numbers of the records that appear in none of a set of
#' pairs. Pass it the candidates and it finds the records the blocking never
#' gave a chance; pass it the links and it finds the residue, the records
#' still unlinked, which is what the next and looser run of [rLink()] is for.
#'
#' The pairs themselves cannot say how many records there were, because a
#' record that was never paired is in none of them. An [rLink()] result
#' records it, and whether the records were one file or two, and keeps both
#' through `[`, [order()] and [head()], so the links can be cut out of a
#' result and handed straight in. For any other pair table give `n`.
#'
#' @param pairs An [rLink()] result, usually subset to the pairs accepted as
#'   links, or any pair table with `index_1` and `index_2`, or `x_index` and
#'   `y_index`, columns.
#' @param side Which input to report on: `1` for `data_1`, `2` for `data_2`.
#'   `NULL` (the default) is `1`, or the one file when `pairs` is a
#'   deduplication, where a record in either column has been reached. For a
#'   deduplication that has lost its attributes give `side = 1:2`.
#' @param n Number of records in that input. Read from `pairs` when it is an
#'   [rLink()] result.
#' @return An integer vector of row numbers into the input, in ascending
#'   order, so `data_1[rLink_unmatched(links), ]` is the unlinked records.
#' @seealso [rLink()], whose `exclude` argument is the other half of a staged
#'   linkage, and [select_greedy()] to resolve scored pairs into links first.
#' @family record linkage pair functions
#' @examples
#' enrol <- data.frame(
#'     id      = 1:4,
#'     surname = c("SMITH", "JONES", "BROWN", "SMYTH"),
#'     given   = c("JOHN", "MARY", "ROB", "JON")
#' )
#' claims <- data.frame(
#'     id      = c("A", "B", "C"),
#'     surname = c("SMITH", "JONSE", "BROWNE"),
#'     given   = c("JON", "MARY", "ROBERT")
#' )
#'
#' # A strict first run: the surname must agree exactly
#' first <- rLink(enrol, claims, block_1 = list(surname),
#'                compare = list(gn = jaro_winkler(given)), pair_id = "id")
#' links <- first[gn > 0.8]
#'
#' # What it left behind on each side
#' rLink_unmatched(links)
#' rLink_unmatched(links, side = 2)
#'
#' # A looser second run over the residue alone. `pair_id` is built from the
#' # records, so the two results can be stacked and still say who is who.
#' rest_1 <- enrol[rLink_unmatched(links), ]
#' rest_2 <- claims[rLink_unmatched(links, side = 2), ]
#' rLink(rest_1, rest_2, block_1 = list(soundex(surname)),
#'       compare = list(gn = jaro_winkler(given)), pair_id = "id",
#'       keep = c("surname", "given"))
#'
#' # Deduplicating, a record on either side of a pair has been reached
#' dup <- rLink(enrol, block_1 = list(soundex(surname)))
#' rLink_unmatched(dup)
#' @export
rLink_unmatched <- function(pairs, side = NULL, n = NULL) {
    if (!is.data.frame(pairs))
        stop("`pairs` must be a data frame of pairs.", call. = FALSE)
    at <- .rlink_index_columns(names(pairs))
    if (is.null(at))
        stop(paste0("`pairs` must hold the pairs' two index columns: ",
                    "`index_1` and `index_2`, or `x_index` and `y_index`."),
             call. = FALSE)
    dedupe <- isTRUE(attr(pairs, "dedupe"))
    if (is.null(side)) side <- if (dedupe) 1:2 else 1L
    if (!is.numeric(side) || !length(side) || anyNA(side) ||
        !all(side %in% 1:2) || anyDuplicated(side))
        stop("`side` must be 1, 2, or 1:2 for a deduplication.",
             call. = FALSE)
    # Both columns of a deduplication index the one file, so reporting on
    # either is reporting on both.
    if (dedupe) side <- 1:2
    if (is.null(n)) {
        held <- attr(pairs, "n_records")
        if (is.null(held))
            stop(paste0("`n` is needed: `pairs` does not record how many ",
                        "records it was made from, as an `rLink()` result ",
                        "does."), call. = FALSE)
        n <- held[side[1L]]
    }
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 ||
        n != floor(n) || n > .Machine$integer.max)
        stop("`n` must be a single non-negative integer.", call. = FALSE)
    reached <- unlist(lapply(at[side], function(nm) pairs[[nm]]),
                      use.names = FALSE)
    if (!is.numeric(reached) || anyNA(reached) ||
        (length(reached) && (min(reached) < 1 || max(reached) > n)))
        stop(sprintf("`pairs` holds indices outside 1 to %s.",
                     format(n, big.mark = ",")), call. = FALSE)
    seen <- logical(n)
    seen[reached] <- TRUE
    which(!seen)
}
