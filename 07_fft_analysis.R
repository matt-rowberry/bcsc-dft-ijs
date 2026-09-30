#####

## FFT DATA PROCESSING FOR FIGURE 7
## TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 29 SEPTEMBER 2026

#####

# ---------------------------------------------------------------------------
# Packages and functions
# ---------------------------------------------------------------------------

library(dplyr)
library(purrr)

source("analysis_functions.R")

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/full_day_night_time_series.rds"
output_file <- "data/figure_07_bandpass.rds"
data <- readRDS(input_file)

fs_cpd <- 144

# ---------------------------------------------------------------------------
# Prepare the five access regimes
# ---------------------------------------------------------------------------

# The preprocessing pipeline produces:
#
#   full_period_1
#   full_period_2
#   full_period_3
#   full_period_4
#   full_period_5
#
# The original Figure 7 analysis used the short monitoring-point names
# expected by bandpass_by_daynight() and the plotting code.
#
# The current preprocessing file stores these measurements as:
#
#   ent_residual
#   ten_residual
#   sin_residual
#   brg_residual
#   mqs_residual
#   top_residual
#
# Therefore only the column names are changed here. The data themselves
# are unchanged.

#' Rename one access regime's residual columns to short site codes.
#'
#' @param x Data frame with `dttm` and `<site>_residual` columns.
#' @return `x` with columns `dttm, ent, ten, sin, brg, mqs, top`.
prepare_period <- function(x) {
    x %>%
        select(
            dttm,
            ent = ent_residual, ten = ten_residual, sin = sin_residual,
            brg = brg_residual, mqs = mqs_residual, top = top_residual
        )
}

df_list <- map(paste0("full_period_", 1:5), ~ prepare_period(data[[.x]]))
names(df_list) <- paste0("df", 1:5)

# ---------------------------------------------------------------------------
# Extract FFT series
# ---------------------------------------------------------------------------

#' Reshape one access regime's `bandpass_by_daynight()` output.
#'
#' Collects the per-signal `full`/`day`/`night` amplitude spectra into
#' one data frame per series type, with a shared frequency column.
#'
#' @param bandpass_out Output of `bandpass_by_daynight()`.
#' @return A named list (`full`, `day`, `night`), each a data frame
#'   with a `freqs` column and one amplitude column per signal.
extract_series <- function(bandpass_out) {
    signals <- names(bandpass_out)
    series_types <- c("full", "day", "night")

    map(series_types, function(series) {
        freqs <- bandpass_out[[1]][[series]]$freqs_cpd

        amplitudes <- map(signals, ~ bandpass_out[[.x]][[series]]$amplitude) %>%
            as.data.frame()
        colnames(amplitudes) <- signals

        cbind(freqs = freqs, amplitudes)
    }) %>%
        set_names(series_types)
}

# ---------------------------------------------------------------------------
# Process all five access regimes
# ---------------------------------------------------------------------------

#' Band-pass filter and reshape every access regime in `df_list`.
#'
#' @param df_list Named list of per-regime data frames (see
#'   `prepare_period()`).
#' @param fs_cpd,freq_min,freq_max Passed to `bandpass_by_daynight()`.
#' @return A named list, one `extract_series()` result per regime.
process_all_dfs <- function(df_list, fs_cpd, freq_min = 42, freq_max = 54) {
    map(
        df_list,
        ~ bandpass_by_daynight(
            .x, timestamp_col = "dttm", fs_cpd = fs_cpd,
            freq_min = freq_min, freq_max = freq_max
        )
    ) %>%
        map(extract_series)
}

all_bandpass_series <- process_all_dfs(df_list, fs_cpd)

# ---------------------------------------------------------------------------
# Normalise bandpass series
# ---------------------------------------------------------------------------

#' Normalise each monitoring point by one global maximum.
#'
#' Drops the `full` spectra (Figure 7 uses day and night only), then
#' divides each monitoring point's day and night amplitudes by that
#' point's maximum across all five access regimes and both day and
#' night.
#'
#' @param all_series Output of `process_all_dfs()`.
#' @param vars Monitoring-point columns to normalise.
#' @return `all_series`, restricted to `day`/`night` and normalised.
normalize_bandpass_series <- function(
    all_series,
    vars = c("ent", "ten", "sin", "brg", "mqs", "top")
) {
    # Remove the "full" entries. Figure 7 uses day and night spectra only.
    all_series <- map(all_series, ~ .x[names(.x) != "full"])

    # Calculate one global maximum for each monitoring point, across all
    # five access regimes and both day and night spectra.
    max_values <- map(vars, function(v) {
        max(
            map(all_series, function(df_item) c(df_item$day[[v]], df_item$night[[v]])) %>%
                unlist(),
            na.rm = TRUE
        )
    }) %>%
        set_names(vars)

    # Apply the global monitoring-point normalisation.
    map(all_series, function(df_item) {
        map(df_item, function(period_df) {
            period_df[vars] <- map_dfc(vars, ~ period_df[[.x]] / max_values[[.x]])
            period_df
        })
    })
}

df <- normalize_bandpass_series(all_bandpass_series)

# ---------------------------------------------------------------------------
# Save results for Figure 7
# ---------------------------------------------------------------------------

saveRDS(df, output_file)
