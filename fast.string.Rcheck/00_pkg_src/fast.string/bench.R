# Core speed-regression harness for fast.string.
#
# Run from the repository root:
#   Rscript bench.R
#   Rscript bench.R --full --output=benchmark-results/core
#
# The default smoke profile verifies the harness with small corpora. The full
# profile exercises the complete 1/2/4/8/16 thread grid and is the profile to
# use for performance claims and acceptance gates.

arguments <- commandArgs(trailingOnly = TRUE)
full_profile <- "--full" %in% arguments
profile <- if (full_profile) "full" else "smoke"

output_argument <- grep("^--output=", arguments, value = TRUE)
output_directory <- if (length(output_argument)) {
    sub("^--output=", "", output_argument[[1L]])
} else {
    file.path(
        tempdir(),
        paste0("fast-string-benchmark-", format(Sys.time(), "%Y%m%dT%H%M%S"))
    )
}

checkout <- Sys.getenv("FAST_STRING_CHECKOUT", unset = ".")
source(file.path(checkout, "benchmark_helpers.R"), local = FALSE)

installation <- benchmark_install_checkout(checkout)
metadata <- benchmark_metadata(installation, profile)

set.seed(20260729)
if (full_profile) {
    short_n <- 100000L
    long_n <- 20000L
    pair_n <- 30000L
    matrix_side <- 128L
    thread_grid <- c(1L, 2L, 4L, 8L, 16L)
    warmups <- 1L
    iterations <- 5L
} else {
    short_n <- 5000L
    long_n <- 1000L
    pair_n <- 2000L
    matrix_side <- 64L
    thread_grid <- c(1L, 2L)
    warmups <- 1L
    iterations <- 3L
}

short_unique <- sprintf("row-%08d-value", seq_len(short_n))
short_repeated <- rep(
    c("alpha value", "beta value", "gamma value", "delta value"),
    length.out = short_n
)
long_unique <- paste0(
    sprintf("row-%08d-", seq_len(long_n)),
    strrep("abcdefghijklmnopqrstuvwxyz012345", 16L)
)
long_repeated <- rep(
    c(
        strrep("abcdefgh", 64L),
        strrep("ijklmnop", 64L),
        strrep("qrstuvwx", 64L),
        strrep("yz012345", 64L)
    ),
    length.out = long_n
)

inject_matches <- function(x, density, needle = "NEEDLE") {
    count <- max(1L, floor(length(x) * density))
    positions <- unique(
        pmin(
            length(x),
            pmax(1L, round(seq(1L, length(x), length.out = count)))
        )
    )
    x[positions] <- paste0(x[positions], needle)
    x
}

short_sparse <- inject_matches(short_unique, 0.01)
short_dense <- inject_matches(short_unique, 0.80)
long_sparse <- inject_matches(long_unique, 0.01)
long_dense <- inject_matches(long_unique, 0.80)

matrix_values <- sprintf("token-%04d", seq_len(matrix_side))
matrix_repeated <- rep(
    c("alpha beta", "alpha gamma", "delta beta", "delta gamma"),
    length.out = matrix_side
)

corpus_metadata <- function(
    kind,
    n_a,
    n_b = NA_integer_,
    string_bytes = NA_integer_,
    q = NA_integer_,
    match_density = NA_real_,
    unique_fraction = NA_real_
) {
    list(
        kind = kind,
        n_a = n_a,
        n_b = n_b,
        cells = if (is.na(n_b)) NA_real_ else as.double(n_a) * as.double(n_b),
        string_bytes = string_bytes,
        q = q,
        match_density = match_density,
        unique_fraction = unique_fraction
    )
}

specs <- list()
add_case <- function(id, kernel, category, corpus, make_fun, all_threads = FALSE) {
    specs[[length(specs) + 1L]] <<- list(
        id = id,
        kernel = kernel,
        category = category,
        corpus = corpus,
        make_fun = make_fun,
        all_threads = all_threads
    )
}

# Fixed searching: short/long, repeated/unique, and sparse/dense matches.
add_case(
    "fixed-short-unique-sparse",
    "fgrepl",
    "fixed search",
    corpus_metadata("short unique sparse", short_n, string_bytes = 18L,
                    match_density = 0.01, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", short_sparse, fixed = TRUE, nthreads = thread)
    },
    all_threads = TRUE
)
add_case(
    "fixed-short-unique-dense",
    "fgrepl",
    "fixed search",
    corpus_metadata("short unique dense", short_n, string_bytes = 18L,
                    match_density = 0.80, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", short_dense, fixed = TRUE, nthreads = thread)
    }
)
add_case(
    "fixed-short-repeated-no-match",
    "fgrepl",
    "fixed search",
    corpus_metadata("short repeated no-match", short_n, string_bytes = 11L,
                    match_density = 0, unique_fraction = 4 / short_n),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", short_repeated, fixed = TRUE, nthreads = thread)
    }
)
add_case(
    "fixed-long-unique-sparse",
    "fgrepl",
    "fixed search",
    corpus_metadata("long unique sparse", long_n, string_bytes = 525L,
                    match_density = 0.01, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", long_sparse, fixed = TRUE, nthreads = thread)
    }
)
add_case(
    "fixed-long-unique-dense",
    "fgrepl",
    "fixed search",
    corpus_metadata("long unique dense", long_n, string_bytes = 525L,
                    match_density = 0.80, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", long_dense, fixed = TRUE, nthreads = thread)
    }
)
add_case(
    "fixed-long-repeated-no-match",
    "fgrepl",
    "fixed search",
    corpus_metadata("long repeated no-match", long_n, string_bytes = 512L,
                    match_density = 0, unique_fraction = 4 / long_n),
    function(thread) function() {
        fast.string::fgrepl("NEEDLE", long_repeated, fixed = TRUE, nthreads = thread)
    }
)

# Sparse substitution and a representative regex scaling case.
add_case(
    "fixed-gsub-sparse",
    "fgsub",
    "substitution",
    corpus_metadata("short unique sparse", short_n, string_bytes = 18L,
                    match_density = 0.01, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgsub(
            "NEEDLE", "replacement", short_sparse,
            fixed = TRUE, nthreads = thread
        )
    },
    all_threads = TRUE
)
add_case(
    "regex-grepl-short",
    "fgrepl",
    "regex",
    corpus_metadata("short unique mixed", short_n, string_bytes = 18L,
                    match_density = 0.80, unique_fraction = 1),
    function(thread) function() {
        fast.string::fgrepl(
            "(NEEDLE|row-[0-9]{4}7)", short_dense,
            nthreads = thread
        )
    },
    all_threads = TRUE
)

# Linear column-major matrix dispatch, including pathological 1 x N / N x 1.
matrix_n <- max(4096L, matrix_side * matrix_side)
matrix_long <- sprintf("matrix-%06d", seq_len(matrix_n))
add_case(
    "jaro-matrix-1-by-n",
    "jaro_winkler_matrix",
    "matrix shape",
    corpus_metadata("1 x N", 1L, matrix_n, string_bytes = 13L,
                    unique_fraction = 1),
    function(thread) function() {
        fast.string::jaro_winkler_matrix(
            "matrix-000001", matrix_long, nthreads = thread
        )
    },
    all_threads = TRUE
)
add_case(
    "jaro-matrix-n-by-1",
    "jaro_winkler_matrix",
    "matrix shape",
    corpus_metadata("N x 1", matrix_n, 1L, string_bytes = 13L,
                    unique_fraction = 1),
    function(thread) function() {
        fast.string::jaro_winkler_matrix(
            matrix_long, "matrix-000001", nthreads = thread
        )
    }
)
add_case(
    "jaro-matrix-balanced",
    "jaro_winkler_matrix",
    "matrix shape",
    corpus_metadata(
        "balanced",
        matrix_side,
        matrix_side,
        string_bytes = 10L,
        unique_fraction = 1
    ),
    function(thread) function() {
        fast.string::jaro_winkler_matrix(
            matrix_values, rev(matrix_values), nthreads = thread
        )
    }
)

# Prepared q-gram cases: q boundaries and high reuse.
for (q_value in c(1L, 2L, 8L)) {
    local({
        q_case <- q_value
        add_case(
            paste0("qgram-matrix-q", q_case),
            "jaccard_matrix",
            "prepared q-grams",
            corpus_metadata(
                paste0("balanced repeated q=", q_case),
                matrix_side,
                matrix_side,
                string_bytes = 11L,
                q = q_case,
                unique_fraction = 4 / matrix_side
            ),
            function(thread) function() {
                fast.string::jaccard_matrix(
                    matrix_repeated,
                    rev(matrix_repeated),
                    q = q_case,
                    nthreads = thread
                )
            },
            all_threads = q_case == 2L
        )
    })
}

# Levenshtein's bit-vector boundaries and common-edge trimming.
for (boundary in c(63L, 64L, 65L, 127L, 128L)) {
    local({
        string_length <- boundary
        edge_length <- max(0L, string_length - 1L)
        common_a <- rep(
            paste0(strrep("a", edge_length), "x"),
            pair_n
        )
        common_b <- rep(
            paste0(strrep("a", edge_length), "y"),
            pair_n
        )
        no_edge_a <- rep(strrep("a", string_length), pair_n)
        no_edge_b <- rep(strrep("b", string_length), pair_n)

        add_case(
            paste0("levenshtein-common-edge-", string_length),
            "levenshtein",
            "edit-distance boundary",
            corpus_metadata(
                paste0("common edge length ", string_length),
                pair_n,
                string_bytes = string_length,
                unique_fraction = 1 / pair_n
            ),
            function(thread) function() {
                fast.string::levenshtein(
                    common_a, common_b, nthreads = thread
                )
            },
            all_threads = string_length == 64L
        )
        add_case(
            paste0("levenshtein-no-edge-", string_length),
            "levenshtein",
            "edit-distance boundary",
            corpus_metadata(
                paste0("no common edge length ", string_length),
                pair_n,
                string_bytes = string_length,
                unique_fraction = 1 / pair_n
            ),
            function(thread) function() {
                fast.string::levenshtein(
                    no_edge_a, no_edge_b, nthreads = thread
                )
            }
        )
    })
}

# Fused fuzzy normalisation and token matching.
fuzzy_a <- rep(
    c("  JOHN---SMITH  42 ", "Mary___Anne Jones", "alpha\tbeta"),
    length.out = pair_n
)
fuzzy_b <- rep(
    c("john smith 42", "MARY ANNE JONES", "beta alpha"),
    length.out = pair_n
)
add_case(
    "fuzz-ratio-full-process",
    "fuzz_ratio",
    "fuzzy normalisation",
    corpus_metadata("punctuation and whitespace", pair_n, string_bytes = 20L,
                    unique_fraction = 3 / pair_n),
    function(thread) function() {
        fast.string::fuzz_ratio(
            fuzzy_a, fuzzy_b, full_process = TRUE, nthreads = thread
        )
    },
    all_threads = TRUE
)
add_case(
    "jaro-token-direct-whitespace",
    "jaro_winkler_tokens",
    "token matching",
    corpus_metadata("direct ASCII whitespace", pair_n, string_bytes = 20L,
                    unique_fraction = 3 / pair_n),
    function(thread) function() {
        fast.string::jaro_winkler_tokens(
            fuzzy_a, fuzzy_b, strip = NULL, nthreads = thread
        )
    }
)

cat(
    sprintf(
        "fast.string %s benchmark (%s, %s, %d default threads)\n",
        metadata$package_version,
        metadata$commit,
        metadata$backend,
        metadata$default_threads
    )
)

results <- list()
for (spec in specs) {
    case_threads <- if (isTRUE(spec$all_threads)) {
        thread_grid
    } else {
        unique(c(1L, max(thread_grid)))
    }
    for (thread in case_threads) {
        cat(sprintf("  %-39s threads=%2d ... ", spec$id, thread))
        result <- benchmark_run_case(
            spec,
            thread = thread,
            warmups = warmups,
            iterations = iterations
        )
        results[[length(results) + 1L]] <- result
        cat(
            sprintf(
                "%.6fs median, %.1f MiB peak\n",
                result$median_seconds,
                result$peak_memory_bytes / 1024^2
            )
        )
    }
}

results <- do.call(rbind, results)
results <- cbind(
    metadata[rep(1L, nrow(results)), , drop = FALSE],
    results,
    row.names = NULL
)
paths <- benchmark_write_results(results, metadata, output_directory)

cat("\nBenchmark artifacts:\n")
cat("  results:  ", paths$results, "\n", sep = "")
cat("  metadata: ", paths$metadata, "\n", sep = "")
cat("  session:  ", paths$session, "\n", sep = "")
if (!length(output_argument)) {
    cat(
        "The default output directory is temporary; pass --output=PATH ",
        "to retain a run.\n",
        sep = ""
    )
}
