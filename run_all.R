#####

## RUN THE FULL PIPELINE
## POOLE'S CAVERN DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 03 OCTOBER 2026

#####

# Runs every analysis and plotting script in dependency order, starting
# from the six temperature records in measured/ as downloaded, and
# writes figures 6-11 to figs/.
#
# Each script runs in its own fresh R process (via Rscript), so no
# object, loaded package or option can leak from one script into the
# next. A script that relies on something it does not create or load
# itself will therefore fail here, as it would for anyone else.
#
# Run from the repository root (the folder containing this file):
#
#   Rscript run_all.R
#
# or open bcsc-dft-ijs.Rproj and source("run_all.R").
#
# Outputs besides the figures:
#
#   logs/<script>.log     Console output and warnings from each script.
#   logs/run_summary.csv  Status and run time of each script.
#   session_info.txt      R version, platform and every package version
#                         used by the pipeline.
#
# 08_spectrogram_analysis.R dominates the run time (c. 3 hours on a
# laptop with its default settings); every other script takes seconds.

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------

# TRUE deletes existing data/*.rds and figs/*.tif first, so the run
# really does start from the raw CSVs alone. Spectrograms saved in
# data/spectrogram_cache/ by an interrupted run are kept: script 08
# reuses them only if their input is identical to the recomputed one,
# so rerunning after an interruption resumes rather than restarts.
clean_start <- TRUE

# NULL runs every script. A character vector, e.g.
# c("06_ts_plot.R", "07_fft_plot.R"), reruns only those scripts against
# the existing data/*.rds; clean_start is then ignored.
run_only <- NULL

# Scripts in dependency order. The file names also sort in this order,
# but the list is kept explicit so the order is documented here.
scripts <- c(
    # Preprocessing (shared by every figure)
    "00_preprocess.R",
    "01_imputing.R",
    "02_kalman_filtering.R",
    "03_long_gaps.R",
    "04_day_night_split.R",
    "05_multiband_reconstruction.R",
    # Figure 6
    "06_ts_plot.R",
    # Figure 7
    "07_fft_analysis.R",
    "07_fft_plot.R",
    # Figure 8
    "08_spectrogram_analysis.R",
    "08_spectrogram_plot.R",
    # Figure 9
    "09_reconstruction_analysis.R",
    "09_reconstruction_plot.R",
    # Figure 10
    "10_segment_analysis.R",
    "10_segment_plot.R",
    # Figure 11
    "11_variance_analysis.R",
    "11_variance_plot.R"
)

log_dir <- "logs"
summary_file <- file.path(log_dir, "run_summary.csv")
session_file <- "session_info.txt"

# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

if (!dir.exists("measured") || !file.exists("analysis_functions.R")) {
    stop("Run this from the repository root (the folder containing measured/).")
}

missing_scripts <- scripts[!file.exists(scripts)]
if (length(missing_scripts) > 0) {
    stop("Missing scripts: ", paste(missing_scripts, collapse = ", "))
}

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

#' Run one script in a fresh R process.
#'
#' Console output and messages go to logs/<script>.log. Stops the whole
#' run if the script exits with an error, since every later script
#' depends on the ones before it.
#'
#' @param script File name of the script.
#' @return Run time in minutes.
run_script <- function(script) {
    rscript <- file.path(R.home("bin"), "Rscript")
    log_file <- file.path(log_dir, sub("\\.R$", ".log", script))

    message(format(Sys.time(), "%H:%M:%S"), "  ", script, " ...")
    start <- Sys.time()
    # options(warn = 1) prints each warning where it occurs; without it,
    # Rscript reports only "There were N warnings" and discards them.
    expr <- sprintf("options(warn = 1); source('%s')", script)
    status <- system2(rscript, c("-e", shQuote(expr)), stdout = log_file, stderr = log_file)
    minutes <- as.numeric(difftime(Sys.time(), start, units = "mins"))

    if (status != 0) {
        stop(script, " failed (exit status ", status, "); see ", log_file)
    }

    # Count real warnings, ignoring "package 'x' was built under R
    # version ..." notices, which only reflect how packages were built.
    log_lines <- readLines(log_file, warn = FALSE)
    warn_at <- grep("^Warning", log_lines)
    built_under <- grepl("was built under R version", log_lines[pmin(warn_at + 1, length(log_lines))]) |
        grepl("was built under R version", log_lines[warn_at])
    n_warn <- sum(!built_under)
    message(sprintf("          done in %.1f min%s", minutes,
                    if (n_warn > 0) sprintf(", %d warning(s) in log", n_warn) else ""))
    minutes
}

#' Packages used anywhere in the pipeline.
#'
#' Collects every package attached with library() or called with `::`
#' in the pipeline scripts and the two shared function files.
#'
#' @param files Script files to scan.
#' @return Sorted character vector of package names.
pipeline_packages <- function(files) {
    code <- unlist(lapply(files, readLines, warn = FALSE))
    code <- sub("#.*$", "", code)
    lib <- regmatches(code, regexpr("(?<=library\\()[A-Za-z0-9.]+", code, perl = TRUE))
    ns <- unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE)))
    sort(setdiff(unique(c(lib, ns)), c("base", "stats", "utils", "grDevices", "graphics", "methods")))
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------

dir.create(log_dir, showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("figs", showWarnings = FALSE)

if (!is.null(run_only)) {
    unknown <- setdiff(run_only, scripts)
    if (length(unknown) > 0) stop("Not pipeline scripts: ", paste(unknown, collapse = ", "))
    scripts <- scripts[scripts %in% run_only]
    clean_start <- FALSE
}

if (clean_start) {
    old <- c(list.files("data", pattern = "\\.rds$", full.names = TRUE),
             list.files("figs", pattern = "\\.tif$", full.names = TRUE))
    if (length(old) > 0) {
        message("Removing ", length(old), " existing output file(s) for a clean start.")
        file.remove(old)
    }
}

run_start <- Sys.time()
minutes <- setNames(rep(NA_real_, length(scripts)), scripts)
for (s in scripts) {
    minutes[s] <- run_script(s)
    write.csv(data.frame(script = scripts, minutes = round(minutes, 2)),
              summary_file, row.names = FALSE)
}

message(sprintf("\nAll %d scripts finished in %.1f min.", length(scripts),
                as.numeric(difftime(Sys.time(), run_start, units = "mins"))))

# ---------------------------------------------------------------------------
# Session information
# ---------------------------------------------------------------------------

# Written only after a full run. Attach every package the pipeline
# uses, so sessionInfo() records the exact versions that produced the
# figures.

if (!is.null(run_only)) {
    message("Partial run: ", session_file, " left unchanged.")
} else {
    pkgs <- pipeline_packages(c(scripts, "analysis_functions.R", "plot_helpers.R"))
    invisible(lapply(pkgs, function(p) {
        suppressPackageStartupMessages(library(p, character.only = TRUE))
    }))

    writeLines(c(
        paste("Pipeline run completed:", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
        paste("Packages used:", paste(pkgs, collapse = ", ")),
        "",
        capture.output(sessionInfo())
    ), session_file)

    message("Session information written to ", session_file, ".")
}
