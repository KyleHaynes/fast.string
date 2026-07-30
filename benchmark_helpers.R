# Shared helpers for reproducible fast.string benchmark runs.
#
# These helpers deliberately live outside the package namespace: benchmark
# runs first install the working checkout into a fresh library, then load that
# exact installation. Dependencies may still be resolved from the user's
# existing library paths.

benchmark_install_checkout <- function(checkout = ".") {
    checkout <- normalizePath(checkout, winslash = "/", mustWork = TRUE)
    description <- file.path(checkout, "DESCRIPTION")
    if (!file.exists(description)) {
        stop("No DESCRIPTION found at checkout: ", checkout, call. = FALSE)
    }

    if ("fast.string" %in% loadedNamespaces()) {
        stop(
            "Run benchmarks in a fresh R session; fast.string is already loaded.",
            call. = FALSE
        )
    }
    if (!requireNamespace("remotes", quietly = TRUE)) {
        stop(
            "The benchmark harness needs the 'remotes' package to install the checkout.",
            call. = FALSE
        )
    }

    benchmark_library <- tempfile("fast-string-benchmark-library-")
    dir.create(benchmark_library, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(benchmark_library)) {
        stop("Could not create isolated benchmark library.", call. = FALSE)
    }

    original_libraries <- .libPaths()
    .libPaths(c(benchmark_library, original_libraries))
    remotes::install_local(
        checkout,
        lib = benchmark_library,
        dependencies = FALSE,
        upgrade = "never",
        force = TRUE,
        quiet = TRUE
    )
    suppressPackageStartupMessages(
        library("fast.string", character.only = TRUE, lib.loc = benchmark_library)
    )

    installed_library <- dirname(
        normalizePath(
            find.package("fast.string"),
            winslash = "/",
            mustWork = TRUE
        )
    )
    expected_library <- normalizePath(
        benchmark_library,
        winslash = "/",
        mustWork = TRUE
    )
    if (!identical(installed_library, expected_library)) {
        stop(
            "fast.string was not loaded from the isolated benchmark library.",
            call. = FALSE
        )
    }

    list(
        checkout = checkout,
        library = expected_library,
        original_libraries = original_libraries
    )
}

benchmark_git_value <- function(checkout, args) {
    value <- tryCatch(
        suppressWarnings(
            system2(
                "git",
                c("-C", shQuote(checkout), args),
                stdout = TRUE,
                stderr = FALSE
            )
        ),
        error = function(...) character()
    )
    if (!length(value)) NA_character_ else paste(value, collapse = "\n")
}

benchmark_compiler_info <- function() {
    r_executable <- file.path(R.home("bin"), "R")
    cxx14 <- tryCatch(
        system2(
            r_executable,
            c("CMD", "config", "CXX14"),
            stdout = TRUE,
            stderr = TRUE
        ),
        error = function(e) NA_character_
    )
    cxx14 <- paste(cxx14, collapse = " ")

    compiler <- if (!is.na(cxx14) && nzchar(cxx14)) {
        strsplit(trimws(cxx14), "[[:space:]]+")[[1L]][1L]
    } else {
        NA_character_
    }
    compiler <- gsub('^"|"$', "", compiler)
    compiler_version <- if (!is.na(compiler)) {
        tryCatch(
            paste(
                head(
                    system2(compiler, "--version", stdout = TRUE, stderr = TRUE),
                    1L
                ),
                collapse = " "
            ),
            error = function(e) NA_character_
        )
    } else {
        NA_character_
    }

    cxx14_flags <- tryCatch(
        paste(
            system2(
                r_executable,
                c("CMD", "config", "CXX14FLAGS"),
                stdout = TRUE,
                stderr = TRUE
            ),
            collapse = " "
        ),
        error = function(e) NA_character_
    )

    list(
        compiler_command = cxx14,
        compiler_version = compiler_version,
        compiler_flags = cxx14_flags
    )
}

benchmark_backend <- function() {
    link_flags <- tryCatch(
        paste(capture.output(RcppParallel::RcppParallelLibs()), collapse = " "),
        error = function(e) ""
    )
    if (grepl("(^|[[:space:]])-ltbb([[:space:]]|$)|tbb", link_flags)) {
        "TBB"
    } else {
        "TinyThread"
    }
}

benchmark_metadata <- function(installation, profile) {
    compiler <- benchmark_compiler_info()
    status <- benchmark_git_value(installation$checkout, "status --porcelain")
    commit <- benchmark_git_value(installation$checkout, "rev-parse HEAD")

    data.frame(
        run_id = paste0(
            format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
            "-",
            ifelse(is.na(commit), "unknown", substr(commit, 1L, 12L))
        ),
        timestamp_utc = format(
            Sys.time(),
            "%Y-%m-%dT%H:%M:%SZ",
            tz = "UTC"
        ),
        profile = profile,
        commit = commit,
        dirty = !is.na(status) && nzchar(status),
        package_version = as.character(utils::packageVersion("fast.string")),
        package_library = installation$library,
        r_version = R.version.string,
        compiler_command = compiler$compiler_command,
        compiler_version = compiler$compiler_version,
        compiler_flags = compiler$compiler_flags,
        backend = benchmark_backend(),
        os = paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
        architecture = R.version$arch,
        detected_cores = parallel::detectCores(logical = TRUE),
        default_threads = RcppParallel::defaultNumThreads(),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
}

benchmark_peak_memory <- function(fun) {
    if (requireNamespace("peakRAM", quietly = TRUE)) {
        measurement <- peakRAM::peakRAM(invisible(fun()))
        return(list(
            bytes = measurement[["Peak_RAM_Used_MiB"]][1L] * 1024^2,
            method = "peakRAM process high-water"
        ))
    }

    # Portable fallback. This captures R's heap high-water mark, not native
    # allocations, and is labelled accordingly in the output.
    invisible(gc(reset = TRUE))
    invisible(fun())
    high_water <- gc()
    max_used_mb <- suppressWarnings(
        as.numeric(high_water[, ncol(high_water)])
    )
    list(
        bytes = sum(max_used_mb, na.rm = TRUE) * 1024^2,
        method = "R gc() heap high-water (native memory excluded)"
    )
}

benchmark_warmed_median <- function(fun, warmups = 1L, iterations = 5L) {
    stopifnot(
        is.function(fun),
        length(warmups) == 1L,
        length(iterations) == 1L,
        warmups >= 1L,
        iterations >= 1L
    )

    for (i in seq_len(warmups)) {
        invisible(fun())
    }

    invisible(gc())
    if (requireNamespace("microbenchmark", quietly = TRUE)) {
        measurement <- suppressWarnings(
            microbenchmark::microbenchmark(
                invisible(fun()),
                times = iterations,
                unit = "ns",
                control = list(warmup = 0L)
            )
        )
        elapsed <- measurement$time / 1e9
    } else {
        warning(
            "Install 'microbenchmark' for sub-millisecond timing resolution.",
            call. = FALSE,
            immediate. = TRUE
        )
        elapsed <- numeric(iterations)
        for (i in seq_len(iterations)) {
            elapsed[i] <- unname(
                system.time(invisible(fun()))[["elapsed"]]
            )
        }
    }

    list(
        median_seconds = stats::median(elapsed),
        minimum_seconds = min(elapsed),
        maximum_seconds = max(elapsed),
        timings_seconds = paste(format(elapsed, digits = 9L), collapse = ";")
    )
}

benchmark_run_case <- function(spec, thread, warmups, iterations) {
    fun <- spec$make_fun(thread)
    timing <- benchmark_warmed_median(
        fun,
        warmups = warmups,
        iterations = iterations
    )
    memory <- benchmark_peak_memory(fun)

    corpus <- spec$corpus
    data.frame(
        case_id = spec$id,
        kernel = spec$kernel,
        category = spec$category,
        requested_threads = thread,
        corpus_kind = corpus$kind,
        n_a = corpus$n_a,
        n_b = corpus$n_b,
        cells = corpus$cells,
        string_bytes = corpus$string_bytes,
        q = corpus$q,
        match_density = corpus$match_density,
        unique_fraction = corpus$unique_fraction,
        warmups = warmups,
        iterations = iterations,
        median_seconds = timing$median_seconds,
        minimum_seconds = timing$minimum_seconds,
        maximum_seconds = timing$maximum_seconds,
        timings_seconds = timing$timings_seconds,
        peak_memory_bytes = memory$bytes,
        peak_memory_method = memory$method,
        stringsAsFactors = FALSE
    )
}

benchmark_write_results <- function(results, metadata, output_directory) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    output_directory <- normalizePath(
        output_directory,
        winslash = "/",
        mustWork = TRUE
    )

    metadata_path <- file.path(output_directory, "metadata.csv")
    results_path <- file.path(output_directory, "results.csv")
    session_path <- file.path(output_directory, "session-info.txt")

    utils::write.csv(metadata, metadata_path, row.names = FALSE, na = "")
    utils::write.csv(results, results_path, row.names = FALSE, na = "")
    writeLines(capture.output(utils::sessionInfo()), session_path)

    list(
        directory = output_directory,
        metadata = metadata_path,
        results = results_path,
        session = session_path
    )
}
