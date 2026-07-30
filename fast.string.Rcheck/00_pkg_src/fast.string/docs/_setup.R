# Sourced at the top of every chapter (each .qmd renders in its own R
# session, so there's no shared state between chapters to rely on).
#
# Always install the current checkout. Rendering against whichever
# fast.string happens to be in the user's default library makes benchmark
# numbers and examples impossible to tie to the source being documented.
if ("fast.string" %in% loadedNamespaces()) {
    stop(
        "Render chapters in a fresh R session; fast.string is already loaded.",
        call. = FALSE
    )
}
if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("Rendering requires the 'remotes' package.", call. = FALSE)
}

.fast_string_checkout <- normalizePath("..", winslash = "/", mustWork = TRUE)
.fast_string_library <- tempfile("fast-string-doc-library-")
dir.create(.fast_string_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(.fast_string_library, .libPaths()))
remotes::install_local(
    .fast_string_checkout,
    lib = .fast_string_library,
    dependencies = FALSE,
    quiet = TRUE,
    upgrade = "never",
    force = TRUE
)
suppressPackageStartupMessages(
    library(fast.string, lib.loc = .fast_string_library)
)

.fast_string_loaded_library <- dirname(
    normalizePath(find.package("fast.string"), winslash = "/", mustWork = TRUE)
)
if (!identical(
    .fast_string_loaded_library,
    normalizePath(.fast_string_library, winslash = "/", mustWork = TRUE)
)) {
    stop("fast.string was not loaded from the isolated render library.")
}
options(fast.string.verbose = FALSE)
