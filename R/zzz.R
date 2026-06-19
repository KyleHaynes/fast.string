#' @useDynLib fast.string, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel setThreadOptions
NULL

.fgrepl_env <- new.env(parent = emptyenv())
.fgrepl_env$mask_msg_shown <- FALSE

.show_mask_msg_once <- function() {
    if (!.fgrepl_env$mask_msg_shown) {
        cli::cli_inform(c(
            "i" = "{.pkg fgrepl}: {.fn grepl}, {.fn grep}, {.fn sub}, {.fn gsub}, {.fn trimws}, {.fn substr}, {.fn nchar}, and {.fn chartr} are masking {.pkg base} functions.",
            "i" = "Suppress with {.code options(fgrepl.verbose = FALSE)}."
        ))
        .fgrepl_env$mask_msg_shown <- TRUE
    }
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
