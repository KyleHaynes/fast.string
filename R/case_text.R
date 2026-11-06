#' Fast parallel case conversion
#'
#' Upper- and lower-case text in parallel across CPU cores via Intel TBB,
#' using the full, locale-independent Unicode case mappings. Case folding is
#' usually the first step of cleaning a name or address column for matching,
#' and [base::toupper()] and [base::tolower()] are single-threaded.
#'
#' The mappings are Unicode's own (`UnicodeData` plus `SpecialCasing`), the
#' same ones ICU applies in its root locale, so they can change the length
#' of a string: `"ß"` upper-cases to `"SS"`, `"ﬁ"` to `"FI"`, and `"İ"`
#' lower-cases to `"i"` plus a combining dot above. This is what
#' `stringi::stri_trans_toupper()` does and where both differ from
#' [base::toupper()], which maps one character to exactly one character and
#' so leaves `"ß"` alone.
#'
#' A capital sigma lower-cases to the final form `"ς"` at the end of a word
#' and to `"σ"` elsewhere (Unicode's `Final_Sigma` rule), so `"ΟΔΟΣ"` gives
#' `"οδος"`. No locale tailoring is applied: a dotless Turkish `"ı"`
#' upper-cases to `"I"`, not to `"İ"`, whatever the session's locale, which
#' makes the result reproducible across machines.
#'
#' Strings that are already in the requested case keep their existing
#' `CHARSXP`, so a column that is mostly upper-case already costs little to
#' upper-case again.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). `NA` elements return `NA`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#'   The result is UTF-8.
#' @seealso [ftitle_case()] for one capital per word, [ascii_fold()] to strip
#'   accents, and [fingerprint()] for a case-insensitive clustering key.
#' @family string utilities
#' @examples
#' fupper(c("Smith", "o'brien", "straße", NA))
#' flower(c("MACDONALD", "ΟΔΟΣ"))
#'
#' # Unlike toupper(), the Unicode mapping spells out the sharp s.
#' fupper("Straße")
#' toupper("Straße")
#' @export
fupper <- function(x, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_case_impl(x, TRUE, .as_nthreads(nthreads)), x)
}

#' @rdname fupper
#' @export
flower <- function(x, nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    .copy_names(fast_case_impl(x, FALSE, .as_nthreads(nthreads)), x)
}

# Nobiliary and patronymic particles that `style = "name"` keeps in lower
# case when they fall between two other words. Every entry is at least two
# letters, so a middle initial ("John E Smith") is never lower-cased.
.name_particles <- c(
    "af", "al", "av", "bin", "bint", "binte", "da", "das", "de", "del",
    "della", "den", "der", "des", "di", "do", "dos", "du", "el", "ibn",
    "la", "las", "le", "les", "los", "te", "ten", "ter", "van", "von", "zu"
)

#' Title case for names, in parallel
#'
#' Capitalises the first letter of every word and lower-cases the rest, in
#' parallel across CPU cores via Intel TBB. Base R has no title-case
#' function at all -- `paste0(toupper(substr(x, 1, 1)), tolower(substr(x,
#' 2, nchar(x))))` only handles the first word -- and the usual
#' alternatives get names wrong, which matters when the output is a name
#' column people will read or compare.
#'
#' A word is a run of letters, digits and combining marks. An apostrophe
#' inside a word belongs to it, so `"don't"` stays one word, while a hyphen
#' separates words, so `"mary-jane"` gives `"Mary-Jane"`. Everything
#' between words -- spaces, punctuation, anything else -- is copied through
#' unchanged. The first letter of each word takes Unicode's title-case
#' mapping, which differs from upper case for a few digraphs (`"ǳ"` titles
#' as `"ǲ"`, not `"Ǳ"`), and the rest of the word is lower-cased with the
#' same mappings [flower()] uses.
#'
#' @section Name rules:
#' With `style = "name"` (the default) three further rules apply, each of
#' them a convention of written names rather than of English:
#'
#' - A letter straight after `"Mc"` is capitalised (`"mcdonald"` gives
#'   `"McDonald"`), and so is one straight after a one-letter prefix and an
#'   apostrophe (`"o'brien"` gives `"O'Brien"`, `"d'arcy"` gives
#'   `"D'Arcy"`). `"Mac"` is deliberately left alone, because too many
#'   names begin with those letters without the prefix (`"Machado"`,
#'   `"Mackerras"`).
#' - A particle listed in `particles` stays in lower case when it falls
#'   between two other words, so `"vincent van gogh"` gives `"Vincent van
#'   Gogh"` while `"van gogh, vincent"` gives `"Van Gogh, Vincent"` --
#'   first and last words are always capitalised.
#' - A generational suffix in Roman numerals is upper-cased when it is not
#'   the first word: `"john smith iii"` gives `"John Smith III"`. Only
#'   `II`, `III`, `IV`, `VI`, `VII`, `VIII`, `IX`, `XI`, `XII` and `XIII`
#'   are treated this way. Single letters and the numerals that are also
#'   names or words (`DIX`, `MIX`, `LID`) are not, and a name that looks
#'   like a numeral is safe as the first word (`"ii naomasa"` gives `"Ii
#'   Naomasa"`) but not after it.
#'
#' `style = "simple"` turns all three off and capitalises every word.
#'
#' @param x Character vector (other vectors, such as factors, are converted
#'   with [as.character()]). `NA` elements return `NA`.
#' @param style `"name"` (the default) to apply the rules under
#'   **Name rules**, or `"simple"` to capitalise every word.
#' @param particles Character vector of particles to keep in lower case
#'   between words, or `NULL` (the default) for the built-in list: `"af"`,
#'   `"al"`, `"av"`, `"bin"`, `"bint"`, `"binte"`, `"da"`, `"das"`, `"de"`,
#'   `"del"`, `"della"`, `"den"`, `"der"`, `"des"`, `"di"`, `"do"`,
#'   `"dos"`, `"du"`, `"el"`, `"ibn"`, `"la"`, `"las"`, `"le"`, `"les"`,
#'   `"los"`, `"te"`, `"ten"`, `"ter"`, `"van"`, `"von"` and `"zu"`.
#'   Matching ignores case, and compares ASCII letters only, so an entry
#'   with accents never matches. Pass `character(0)` to keep the other name
#'   rules but capitalise every particle. Only used when
#'   `style = "name"`.
#' @param nthreads Positive integer per-call thread cap, or `NULL` to use the
#'   RcppParallel default. `1` forces serial execution.
#' @return Character vector the same length as `x`, with `names(x)` kept.
#'   The result is UTF-8.
#' @seealso [fupper()] and [flower()] for one case throughout, and
#'   [freplace_tokens()] to fix up whole words the rules cannot know about.
#' @family string utilities
#' @examples
#' ftitle_case(c("MARY-JANE O'BRIEN", "mcdonald", "van der berg",
#'               "vincent van gogh", "john smith iii", NA))
#'
#' # Without the name rules every word is simply capitalised.
#' ftitle_case("MARY-JANE O'BRIEN", style = "simple")
#'
#' # Dutch and Spanish conventions differ; supply your own particles.
#' ftitle_case("juan de la cruz", particles = c("de", "la"))
#' @export
ftitle_case <- function(x, style = c("name", "simple"), particles = NULL,
                        nthreads = NULL) {
    if (!is.character(x)) x <- as.character(x)
    style <- match.arg(style)
    if (identical(style, "simple") && !is.null(particles))
        stop("`particles` applies to `style = \"name\"`.")
    if (is.null(particles)) particles <- .name_particles
    if (!is.character(particles) || anyNA(particles))
        stop("`particles` must be a character vector without `NA`.")
    .copy_names(fast_title_case_impl(
        x, flower(enc2utf8(particles), nthreads = 1L),
        identical(style, "name"), .as_nthreads(nthreads)
    ), x)
}
