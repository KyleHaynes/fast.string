#' @useDynLib fast.string, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
NULL

.as_nthreads <- function(nthreads) {
    if (is.null(nthreads)) return(-1L)
    if (!is.numeric(nthreads) || length(nthreads) != 1L ||
        is.na(nthreads) || !is.finite(nthreads) ||
        nthreads < 1 || nthreads != floor(nthreads) ||
        nthreads > .Machine$integer.max) {
        stop("`nthreads` must be NULL or a positive integer.")
    }
    as.integer(nthreads)
}

.onAttach <- function(libname, pkgname) {
    if (!isTRUE(getOption("fast.string.verbose", TRUE))) return(invisible())
    version <- unname(getNamespaceVersion(pkgname))
    packageStartupMessage(paste(.index_banner(version), collapse = "\n"))
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
