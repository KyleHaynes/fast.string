#' @useDynLib fast.string, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel setThreadOptions
NULL

.fast.string_functions <- list(
    fgrepl              = "grepl() equivalent (PCRE2 regex / RE2 fixed-string match)",
    fgrep               = "grep() equivalent, returns matching indices or values",
    fsub                = "sub() equivalent, first-match substitution",
    fgsub               = "gsub() equivalent, global substitution",
    gsub_all            = "multi-pattern substitution in a single pass",
    ftrimws             = "trimws() equivalent",
    fsubstr             = "substr() equivalent",
    fnchar              = "nchar() equivalent",
    fchartr             = "chartr() equivalent",
    format_date         = "format a Date/integer-days vector as strings",
    format_date_parts   = "assemble year/month/day fields into a date string",
    date_parts          = "decompose a Date vector into year/month/day columns",
    "fas.Date"          = "fast fixed-format date parsing",
    jaro_winkler        = "pairwise Jaro-Winkler string similarity",
    jaro_winkler_matrix = "all-pairs Jaro-Winkler similarity matrix",
    soundex             = "Soundex phonetic code",
    nysiis              = "NYSIIS phonetic code"
)

.onAttach <- function(libname, pkgname) {
    if (!isTRUE(getOption("fast.string.verbose", TRUE))) return(invisible())
    bullets <- vapply(
        names(.fast.string_functions),
        function(nm) sprintf("  * %-20s %s", paste0(nm, "()"), .fast.string_functions[[nm]]),
        character(1L)
    )
    packageStartupMessage(paste(c(
        "fast.string: parallel string/date/phonetic functions (PCRE2 + RE2 + RcppParallel)",
        bullets,
        "Suppress this message with options(fast.string.verbose = FALSE)."
    ), collapse = "\n"))
}

# Detects PCRE-specific syntax not present in the default TRE engine:
# lookaheads, lookbehinds, atomic groups, possessive quantifiers, named
# backreferences, and recursive constructs. When detected with perl = FALSE,
# we delegate to base::grepl(perl = TRUE) so behavior is always correct.
.has_pcre_only_syntax <- function(pattern) {
    base::grepl(
        paste0(
            "\\(\\?[=!]",       # (?= lookahead  (?! negative lookahead
            "|\\(\\?<[=!]",     # (?<= lookbehind  (?<! negative lookbehind
            "|\\(\\?>",         # (?> atomic group
            "|[*+?]\\+",        # *+  ++  ?+  possessive quantifiers
            "|\\(\\?P[=<]",     # (?P=  (?P<  named backref / group
            "|\\\\k[<']",       # \k<name>  \k'name'  named backreference
            "|\\(\\?R\\)",      # (?R) full-pattern recursion
            "|\\(\\?[0-9]",     # (?1) (?2) … numbered group recursion
            "|\\(\\?&"          # (?&name) named group recursion
        ),
        pattern, perl = TRUE
    )
}
