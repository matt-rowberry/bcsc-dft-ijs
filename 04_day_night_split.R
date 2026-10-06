#####

## ORGANISING DATAFRAMES FOR THE TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

source("analysis_functions.R")

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/kalman_residuals_full_imp.rds"
output_file <- "data/full_day_night_time_series.rds"

# ---------------------------------------------------------------------------
# Create aligned day, night and full-period datasets
# ---------------------------------------------------------------------------

# Day and night are defined by is_daytime() in analysis_functions.R,
# the same definition used for Figures 7, 9, 10 and 11:
#
#   Day:   09:30-17:20 UK local time
#   Night: 17:30-09:20 UK local time
#
# The timestamps are in UTC, so during British Summer Time day is
# 08:30-16:20 UTC.
#
# For the day and night datasets, observations outside the relevant
# interval are replaced with NA while the complete 10-minute time
# axis is retained. This preserves temporal alignment between the
# day and night series and is required for the downstream analyses.

#' Split a residual data frame into day, night and full-period sets.
#'
#' @param df Data frame with a timestamp column and a period column.
#' @param timestamp_col,period_col Column names.
#' @return A named list of data frames: `day_period_<n>`,
#'   `night_period_<n>` and `full_period_<n>` for each access regime
#'   `n`.
split_day_night_period <- function(
    df,
    timestamp_col = "dttm",
    period_col = "period"
) {
    df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]])

    is_day <- is_daytime(df[[timestamp_col]])
    is_night <- !is_day

    # Replace observations outside the selected interval with NA
    # while preserving the original column type.
    make_aligned <- function(df, mask) {
        cols_to_mask <- setdiff(names(df), c(timestamp_col, period_col))

        for (col in cols_to_mask) {
            # Assigning NA keeps the type of the column, so no
            # class-specific handling is needed.
            df[[col]][!mask] <- NA
        }

        df
    }

    df_day_aligned <- make_aligned(df, is_day)
    df_night_aligned <- make_aligned(df, is_night)

    # Split the aligned datasets by access regime.
    day_list <- split(df_day_aligned, df_day_aligned[[period_col]])
    night_list <- split(df_night_aligned, df_night_aligned[[period_col]])
    full_list <- split(df, df[[period_col]])

    # Name the resulting datasets.
    names(day_list) <- paste0("day_period_", names(day_list))
    names(night_list) <- paste0("night_period_", names(night_list))
    names(full_list) <- paste0("full_period_", names(full_list))

    c(day_list, night_list, full_list)
}

# ---------------------------------------------------------------------------
# Apply function and save results
# ---------------------------------------------------------------------------

result <- split_day_night_period(readRDS(input_file))
saveRDS(result, output_file)
