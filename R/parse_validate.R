#' Fast parsing of numbers from messy text
#'
#' Extracts the first number from each string and returns it as a double,
#' ignoring the text around it -- currency symbols, units, percent signs,
#' labels -- and the grouping marks inside it, as `readr::parse_number()`
#' does. It is the tool for spreadsheet exports and scraped values such as
#' `"$1,234.50"`, `"12.5%"`, `"approx. 3,000 kg"` or `"-$45"`. Strings are
#' parsed in parallel across CPU cores via Intel TBB.
#'
#' The number starts at the first digit, or at a decimal mark directly
#' followed by a digit. It continues through digits, single grouping marks
#' between digits, a decimal mark and its digits, and an exponent (`e` or
#' `E`, an optional sign and at least one digit). A minus sign (`-` or
#' U+2212) before the number makes it negative, even when spaces, currency
#' symbols or other punctuation stand between them (`"-$45"`, `"$ -45"`),
#' but not when it joins two words or codes (`"SKU-123"` is 123). Strings
#' with no number give `NA`, silently.
#'
#' The digits are converted with the C library's `strtod()`, so the result
#' is the correctly rounded double for the decimal number found. Results
#' agree with `readr::parse_number()` on ordinary input; they differ where
#' readr returns `NA` for a minus sign before a currency symbol (`"-$45"`)
#' or reads past a second decimal mark (`"1,234.5,6"` is `1234.5` here).
#' `Inf`, `NaN` and hexadecimal are not recognised.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param decimal_mark A single character: the decimal mark.
#' @param grouping_mark Character vector of single characters allowed
#'   between digit groups (`""` or `character(0)` for none). European
#'   formats typically use `decimal_mark = ","` with
#'   `grouping_mark = c(".", " ")`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Double vector the same length as `x`, with `names(x)` kept.
#' @seealso [fextract()] to pull out other patterns.
#' @family parsing, validation and hashing functions
#' @examples
#' fparse_number(c("$1,234.50", "12.5%", "approx. 3,000 kg", "-$45",
#'                 "SKU-123", "n/a", NA))
#'
#' # European formats
#' fparse_number(c("1.234,50 EUR", "12,5 %"), decimal_mark = ",",
#'               grouping_mark = c(".", " "))
#' @export
fparse_number <- function(x, decimal_mark = ".", grouping_mark = ",",
                          nthreads = NULL) {
    x <- .character_x(x)
    if (!is.character(decimal_mark) || length(decimal_mark) != 1L ||
        is.na(decimal_mark) || nchar(decimal_mark, type = "chars") != 1L)
        stop("`decimal_mark` must be a single character.")
    if (!is.character(grouping_mark) || anyNA(grouping_mark) ||
        any(nchar(grouping_mark, type = "chars") > 1L))
        stop("`grouping_mark` must be a character vector of single characters.")
    if (decimal_mark %in% grouping_mark)
        stop("`decimal_mark` cannot also be a grouping mark.")
    if (grepl("[0-9eE+-]", decimal_mark) ||
        any(grepl("[0-9eE+-]", grouping_mark)))
        stop("Marks cannot be digits, signs or 'e'.")
    .copy_names(fast_parse_number_impl(
        x, enc2utf8(decimal_mark), enc2utf8(grouping_mark),
        .as_nthreads(nthreads)
    ), x)
}

.id_types <- c("luhn", "abn", "acn", "tfn", "medicare", "isbn", "ean", "iban")

#' Fast check-digit validation of identifiers
#'
#' Checks whether each string is a well-formed identifier of the given type:
#' the right length, the right characters and a correct check digit. It
#' catches most typos and transpositions in keyed-in identifiers, so it is a
#' cheap data-quality rule and a way to tell real identifiers from junk
#' before matching on them. Strings are checked in parallel across CPU cores
#' via Intel TBB.
#'
#' Spaces, hyphens and other ASCII whitespace are ignored anywhere in the
#' string (and full stops too for IBANs), so `"51 824 753 556"` is checked
#' as `"51824753556"`. The types are:
#'
#' * `"luhn"`: any digit string passing the Luhn (mod 10) check -- payment
#'   card numbers, IMEIs and many others.
#' * `"abn"`: Australian Business Number (11 digits, mod 89).
#' * `"acn"`: Australian Company Number (9 digits, mod 10).
#' * `"tfn"`: Australian Tax File Number (9 digits, mod 11). The weights for
#'   the older 8-digit numbers are not published, so they give `NA`.
#' * `"medicare"`: Australian Medicare card number: 10 digits (or 11 with
#'   the individual reference number), the first from 2 to 6 and the ninth a
#'   check digit over the first eight.
#' * `"isbn"`: ISBN-10 (check character `0`-`9` or `X`; a 9-digit SBN counts
#'   as an ISBN-10 with a leading `0`) or ISBN-13 (an EAN-13 starting `978`
#'   or `979`).
#' * `"ean"`: EAN-8, UPC-A (12 digits), EAN-13 and GTIN-14 barcodes.
#' * `"iban"`: International Bank Account Number: the ISO 7064 mod 97-10
#'   check, plus the length and character classes of the country's BBAN in
#'   the SWIFT IBAN Registry. Country-specific national check digits inside
#'   the BBAN are not checked.
#'
#' Apart from Medicare, which it does not cover, the rules follow the
#' `python-stdnum` library (with `check_country = False` for IBANs), against
#' which they are tested. Passing the check does not mean the identifier was
#' ever issued.
#'
#' @param x Character vector. `NA` elements return `NA`.
#' @param type One of `"luhn"`, `"abn"`, `"acn"`, `"tfn"`, `"medicare"`,
#'   `"isbn"`, `"ean"` and `"iban"`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Logical vector the same length as `x`, with `names(x)` kept.
#' @seealso [string_shape()] to profile the formats in a column.
#' @family parsing, validation and hashing functions
#' @examples
#' valid_id(c("51 824 753 556", "51 824 753 557", NA), "abn")
#' valid_id(c("4111 1111 1111 1111", "4111 1111 1111 1112"), "luhn")
#' valid_id(c("978-0-306-40615-7", "0-306-40615-2", "0-306-40615-3"), "isbn")
#' valid_id("GB82 WEST 1234 5698 7654 32", "iban")
#' @export
valid_id <- function(x, type = c("luhn", "abn", "acn", "tfn", "medicare",
                                 "isbn", "ean", "iban"),
                     nthreads = NULL) {
    x <- .character_x(x)
    type <- match.arg(type)
    .copy_names(fast_valid_id_impl(
        x, match(type, .id_types) - 1L, .as_nthreads(nthreads)
    ), x)
}
