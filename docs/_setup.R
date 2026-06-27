# Sourced at the top of every chapter (each .qmd renders in its own R
# session, so there's no shared state between chapters to rely on).
if (!requireNamespace("fast.string", quietly = TRUE)) {
    remotes::install_local("..", quiet = TRUE, upgrade = "never")
}
suppressPackageStartupMessages(library(fast.string))
options(fast.string.verbose = FALSE)
