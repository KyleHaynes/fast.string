#' Fast whole-token dictionary replacement
#'
#' Splits each string into tokens at whitespace, replaces every token found
#' in `from` with the matching element of `to`, and joins the tokens again
#' with single spaces. It is the usual way to standardise names and
#' addresses -- expanding abbreviations (`"ST"` to `"STREET"`), mapping
#' variants to one spelling, or deleting titles and stop words (map them to
#' `""`) -- and does in one hashed pass per string what otherwise takes
#' `strsplit()`, `match()` and `paste()`, or one regular expression per
#' dictionary entry. Strings are processed in parallel across CPU cores via
#' Intel TBB.
#'
#' Only whole tokens are replaced: with `from = "ST"`, `"ST"` changes but
#' `"STAR"` and `"ST."` do not. Strip punctuation first (for example
#' `fgsub("[[:punct:]]+", " ", x)`) when it can be attached to the tokens.
#' Whitespace is Unicode's `White_Space` property, as in [fsquish()], and the
#' result is squished: leading, trailing and repeated whitespace is removed
#' even where nothing is replaced. A replacement may contain several words.
#' Replacements are not re-scanned, so a chain such as `A -> B`, `B -> C`
#' changes `"A"` to `"B"`.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param from Character vector of tokens to replace. Entries must be
#'   non-empty and contain no whitespace; when one appears more than once the
#'   first wins, as in [match()].
#' @param to Character vector the same length as `from` (or length 1, which
#'   is recycled): the replacements. `""` deletes the token.
#' @param ignore_case Logical. Match tokens regardless of the case of ASCII
#'   letters (as `ignore.case` in [fgrepl_any()]). The replacement is used as
#'   given.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [gsub_all()] to replace substrings rather than whole tokens,
#'   [fsquish()].
#' @family string utilities
#' @examples
#' addr <- c("12 SMITH ST", "3/45 NORTH RD", "  7   KING   ST  ", NA)
#' freplace_tokens(addr, c("ST", "RD"), c("STREET", "ROAD"))
#'
#' # Delete titles, case-insensitively
#' freplace_tokens(c("Dr John Smith", "MRS JANE DOE"), c("dr", "mrs", "mr"),
#'                 "", ignore_case = TRUE)
#' @export
freplace_tokens <- function(x, from, to, ignore_case = FALSE,
                            nthreads = NULL) {
    x <- .character_x(x)
    if (!is.character(from) || !is.character(to))
        stop("`from` and `to` must be character vectors.")
    if (length(to) == 1L) to <- rep(to, length(from))
    if (length(to) != length(from))
        stop("`to` must be the same length as `from`, or length 1.")
    if (anyNA(from) || anyNA(to))
        stop("`from` and `to` must not contain NA.")
    if (!is.logical(ignore_case) || length(ignore_case) != 1L ||
        is.na(ignore_case))
        stop("`ignore_case` must be TRUE or FALSE.")
    .copy_names(fast_replace_tokens_impl(
        x, from, to, ignore_case, .as_nthreads(nthreads)
    ), x)
}

#' Fast string padding
#'
#' Pads each string to at least `width` characters, as
#' `stringr::str_pad()` does -- for example zero-padding postcodes or
#' identifiers that lost their leading zeros (`"800"` to `"0800"`). Strings
#' already `width` characters or longer are returned unchanged. Runs in
#' parallel across CPU cores via Intel TBB.
#'
#' Width counts characters (Unicode code points), not display columns; for
#' strings marked `"bytes"` it counts bytes.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param width Non-negative integer: the minimum width of the result.
#' @param side `"left"` (the default, which right-aligns the text), `"right"`
#'   or `"both"`. With `"both"`, the left side gets the smaller half of an odd
#'   amount of padding, as in `stringr::str_pad()`.
#' @param pad A single character to pad with (default a space).
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [base::formatC()], [base::sprintf()] for numbers.
#' @family string utilities
#' @examples
#' fpad(c("800", "4000", "2", NA), 4, pad = "0")
#' fpad("abc", 7, side = "both", pad = "*")
#' @export
fpad <- function(x, width, side = c("left", "right", "both"), pad = " ",
                 nthreads = NULL) {
    x <- .character_x(x)
    side <- match.arg(side)
    if (!is.numeric(width) || length(width) != 1L || is.na(width) ||
        width < 0 || width != floor(width) || width > .Machine$integer.max)
        stop("`width` must be a single non-negative integer.")
    if (!is.character(pad) || length(pad) != 1L || is.na(pad) ||
        nchar(pad, type = "chars") != 1L)
        stop("`pad` must be a single character.")
    .copy_names(fast_pad_impl(
        x, as.integer(width), match(side, c("left", "right", "both")) - 1L,
        enc2utf8(pad), .as_nthreads(nthreads)
    ), x)
}

#' Character-class shape of strings, for profiling formats
#'
#' Replaces every upper-case letter with `"A"`, every lower-case letter with
#' `"a"` and every digit with `"9"`, keeping punctuation, symbols and
#' whitespace as they are, so `"QLD 4000"` becomes `"AAA 9999"` and
#' `"0412 345 678"` becomes `"9999 999 999"`. Tabulating the shapes of a
#' column shows at a glance which formats it holds and which values are
#' outliers -- the pattern profiling of data-quality tools. With
#' `collapse = TRUE` runs of the same class become one character
#' (`"Aa 9"`), which groups values by structure regardless of length.
#' Runs in parallel across CPU cores via Intel TBB.
#'
#' Classes follow Unicode's general categories: upper- and title-case
#' letters are `"A"`, lower-case letters and letters without case (such as
#' Chinese characters) are `"a"`, decimal digits in any script are `"9"`,
#' and combining marks are dropped, so an accented letter is one letter
#' whether or not it is decomposed. Strings marked `"bytes"` only have ASCII
#' letters and digits classified.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param collapse Logical. Collapse each run of `"A"`, `"a"` or `"9"` to one
#'   character.
#' @param ignore_case Logical. Map all letters to `"a"`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [valid_id()] to check identifiers that have check digits.
#' @family string utilities
#' @examples
#' phone <- c("0412 345 678", "(07) 3123 4567", "+61 412 345 678",
#'            "0412345678", "call me", NA)
#' string_shape(phone)
#' table(string_shape(phone, collapse = TRUE))
#'
#' string_shape(c("Smith", "SMITH", "O'Brien", "van der Berg"), collapse = TRUE)
#' @export
string_shape <- function(x, collapse = FALSE, ignore_case = FALSE,
                         nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(collapse, "collapse")
    .check_flag(ignore_case, "ignore_case")
    .copy_names(fast_string_shape_impl(
        x, collapse, ignore_case, .as_nthreads(nthreads)
    ), x)
}

#' Fast URL encoding and decoding
#'
#' `url_encode()` percent-encodes each string as [utils::URLencode()] does,
#' and `url_decode()` reverses it as [utils::URLdecode()] does, but over
#' whole vectors in parallel across CPU cores via Intel TBB rather than one
#' string at a time in R code.
#'
#' `url_encode()` encodes the UTF-8 bytes of every character other than the
#' unreserved `A-Z a-z 0-9 - . _ ~`; with `reserved = FALSE` (the default,
#' for whole URLs) the reserved characters `! # $ & ' ( ) * + , / : ; = ? @`
#' and the square brackets are kept as well, and with `reserved = TRUE` (for
#' one component, such as a query value) they are encoded too. Spaces become
#' `%20`. As in
#' `URLencode()`, a string that already contains a `%xx` escape is returned
#' unchanged unless `repeated = TRUE`.
#'
#' `url_decode()` turns each `%xx` escape back into its byte, and with
#' `plus = TRUE` also turns `+` into a space, as HTML form data and
#' `application/x-www-form-urlencoded` query strings use it. A `%` that does
#' not start a valid escape is kept as it is (`URLdecode()` turns it into
#' garbage). Decoded strings that are valid UTF-8 are marked as UTF-8; any
#' other bytes are returned in the native encoding, as `URLdecode()` returns
#' everything.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param reserved Logical. Encode the reserved characters too.
#' @param repeated Logical. Encode strings that already contain `%xx`
#'   escapes.
#' @param plus Logical. Decode `+` as a space.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [html_unescape()]
#' @family string utilities
#' @examples
#' q <- c("fish & chips", "caf\u00e9 /menu", "100%", NA)
#' enc <- url_encode(q, reserved = TRUE)
#' enc
#' url_decode(enc)
#'
#' # Query strings from web logs use '+' for spaces
#' url_decode("q=meat+pie%21&page=2", plus = TRUE)
#' @export
url_encode <- function(x, reserved = FALSE, repeated = FALSE,
                       nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(reserved, "reserved")
    .check_flag(repeated, "repeated")
    .copy_names(fast_url_encode_impl(
        x, reserved, repeated, .as_nthreads(nthreads)
    ), x)
}

#' @rdname url_encode
#' @export
url_decode <- function(x, plus = FALSE, nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(plus, "plus")
    .copy_names(fast_url_decode_impl(x, plus, .as_nthreads(nthreads)), x)
}

#' Fast HTML entity decoding and escaping
#'
#' Replaces HTML character references -- named ones such as `&amp;`,
#' `&eacute;` and `&nbsp;`, and numeric ones such as `&#233;` and `&#xE9;`
#' -- with the characters they stand for, exactly as Python's
#' `html.unescape()` does. Text scraped from web pages or exported from
#' content systems is often full of them, and base R has no decoder.
#' Strings are processed in parallel across CPU cores via Intel TBB.
#'
#' All 2,231 named references of the HTML Living Standard are known,
#' including the legacy ones that work without a trailing semicolon
#' (`&amp`, `&copy`), which are also recognised at the start of a longer
#' word (`"&copy2024"` gives the copyright sign and `2024`). Numeric
#' references follow the standard's rules for invalid numbers: `&#0;`,
#' surrogates and values
#' beyond U+10FFFF become U+FFFD, references to C1 control codes become the
#' Windows-1252 characters browsers show for them (`&#150;` is an en dash),
#' and a few other code points (most control characters, non-characters)
#' are dropped. Anything that is not a reference is left alone. The result
#' is UTF-8.
#'
#' `html_escape()` is the other direction, for putting a value R produced
#' into an HTML page: `&`, `<` and `>` become `&amp;`, `&lt;` and `&gt;`.
#' With `attribute = TRUE` the quotes and the line breaks are escaped as
#' well (`&quot;`, `&#39;`, `&#13;`, `&#10;`), which is what a value placed
#' inside an HTML attribute needs. It escapes exactly what
#' `htmltools::htmlEscape()` does, without the dependency, and in parallel.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param attribute Logical. Escape the quotes and the line breaks too, for
#'   a value that goes inside an HTML attribute.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#' @seealso [url_decode()], [ascii_fold()] to transliterate the result.
#' @family string utilities
#' @examples
#' html_unescape(c("Fish &amp; Chips", "caf&eacute; &#8211; open",
#'                 "&lt;b&gt;bold&lt;/b&gt;", "AT&T", NA))
#'
#' html_escape(c("Fish & Chips", "<b>bold</b>", NA))
#' html_escape("say \"hi\"", attribute = TRUE)
#'
#' # The two are inverses for the characters html_escape() writes.
#' html_unescape(html_escape("1 < 2 & 3 > 2"))
#' @export
html_unescape <- function(x, nthreads = NULL) {
    x <- .character_x(x)
    .copy_names(fast_html_unescape_impl(x, .as_nthreads(nthreads)), x)
}

#' @rdname html_unescape
#' @export
html_escape <- function(x, attribute = FALSE, nthreads = NULL) {
    x <- .character_x(x)
    .check_flag(attribute, "attribute")
    .copy_names(
        fast_html_escape_impl(x, attribute, .as_nthreads(nthreads)), x
    )
}

.check_flag <- function(value, name) {
    if (!is.logical(value) || length(value) != 1L || is.na(value))
        stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
    invisible(value)
}
