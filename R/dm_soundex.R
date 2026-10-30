#' Daitch-Mokotoff Soundex
#'
#' The Daitch-Mokotoff Soundex ("Eastern European Soundex"), a six-digit
#' phonetic code built for the Slavic and Germanic spellings of Jewish
#' surnames that American Soundex handles badly. Where [soundex()] keeps
#' the first letter and codes only the next three consonants,
#' Daitch-Mokotoff codes letter *sequences* -- `"SCH"`, `"TSCH"`,
#' `"SZCZ"`, `"RZ"` -- to six digits, so `"Moskowitz"` and `"Moskovitz"`
#' both give `645740`, and `"Auerbach"` and `"Ohrbach"` meet as well.
#' Codes are built in parallel across CPU cores via Intel TBB.
#'
#' A spelling that could be pronounced more than one way gets **more than
#' one code**, which is the point of the algorithm: `"CH"` may be a 4
#' (as in `"church"`) or a 5 (as in `"loch"`), so `"Auerbach"` codes as
#' both `097400` and `097500`. To use the codes for blocking, treat each
#' one as a pass of its own: two records are candidates when any of their
#' codes agree. `simplify = TRUE` returns the codes as a matrix, which
#' [block_pairs()] takes as one blocking pass per column.
#'
#' Before coding, letters are lower-cased, the accented Latin letters the
#' rules list are folded (`"ß"` to `"s"`, `"é"` to `"e"`, `"ł"` to `"l"`),
#' and everything else that is not a letter of the algorithm's alphabet --
#' digits, punctuation, spaces, and letters of other scripts -- is dropped.
#' So `"Jackson-Jackson"`, `"Jackson Jackson"` and `"JacksonJackson"` all
#' code alike.
#'
#' @section Source:
#' The coding rules are Apache Commons Codec's
#' (`DaitchMokotoffSoundex`, Apache License 2.0), read by
#' `tools/gen_dm_table.py` into `src/dm_table.h`, and this function
#' reproduces that implementation's codes. The one difference is for a
#' value with no letter to code at all (`""`, `"123"`, `NA`): Commons
#' Codec gives it the code `000000`, while this function gives `NA`, as
#' [soundex()] and the other encoders here do. A `000000` code would
#' otherwise put every such value in one blocking block together.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]).
#' @param simplify Logical. Return a character matrix with one row per
#'   value and one column per code, padded with `NA`, instead of a list.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return A list of character vectors of six-digit codes, the same length
#'   as `x` and with `names(x)` kept; `NA_character_` for a value with no
#'   codable letter. With `simplify = TRUE`, a character matrix whose rows
#'   are named by `names(x)`.
#' @seealso [soundex()] and [refined_soundex()] for the American codes,
#'   [cologne()] for German, [double_metaphone()] for English with two
#'   codes, and [block_pairs()] to turn codes into candidate pairs.
#' @family phonetic codes
#' @examples
#' daitch_mokotoff(c("Moskowitz", "Moskovitz"))
#'
#' # One spelling, two pronunciations, two codes.
#' daitch_mokotoff(c("Auerbach", "Ohrbach"))
#'
#' # As blocking keys: one pass per column.
#' daitch_mokotoff(c("Peters", "Peterson", "Waszkiewicz"), simplify = TRUE)
#'
#' # Spellings that American Soundex separates and this code brings together.
#' x <- c("Szlamawicz", "Shlamovitz")
#' daitch_mokotoff(x)
#' soundex(x)
#' @export
daitch_mokotoff <- function(x, simplify = FALSE, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .check_flag(simplify, "simplify")
    result <- fast_daitch_mokotoff_impl(x, simplify, .as_nthreads(nthreads))
    if (simplify) {
        if (!is.null(names(x))) dimnames(result) <- list(names(x), NULL)
        return(result)
    }
    .copy_names(result, x)
}
