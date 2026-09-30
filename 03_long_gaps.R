#####

## PROCESSING LONG RUNS IN THE RESIDUAL DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 28 SEPTEMBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(lubridate)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/kalman_results_residuals.rds"
output_file <- "data/kalman_residuals_full_imp.rds"

# ---------------------------------------------------------------------------
# Read residual data and initialise first observations
# ---------------------------------------------------------------------------

df <- readRDS(input_file)

# The first recursive residual produced by KFAS is set to zero
# because it is affected by the initialisation of the state-space
# model and is not used as an empirical residual observation in the
# subsequent analysis.

df[1, 3:ncol(df)] <- 0

# ---------------------------------------------------------------------------
# Identify long runs of missing residuals
# ---------------------------------------------------------------------------

# Long runs of missing residuals are not interpolated. Instead, any
# calendar day containing a missing run is treated as unavailable for
# the subsequent analysis: all residual values within that day are
# set to zero. This prevents a long missing interval from being
# interpreted as a sequence of residual observations while retaining
# a complete regular time series for subsequent processing.
#
# Each residual series is processed independently, so the days set
# to zero can differ between monitoring points.

#' Zero out calendar days that contain a run of missing values.
#'
#' @param vec Numeric vector of residuals.
#' @param times POSIXct timestamps matching `vec`.
#' @return `vec` with every value on an affected calendar day set to
#'   zero.
replace_missing_days <- function(vec, times) {
    rle_na <- rle(is.na(vec))
    ends <- cumsum(rle_na$lengths)
    starts <- c(1, head(ends, -1) + 1)
    replace_zero <- rep(FALSE, length(vec))

    for (i in seq_along(rle_na$lengths)) {
        if (rle_na$values[i]) {
            # A run of missing residuals has been detected.
            start_day <- floor_date(times[starts[i]], unit = "day")
            end_day <- floor_date(times[ends[i]], unit = "day") + hours(23) + minutes(50)

            # Mark the complete calendar days containing the missing run.
            replace_zero <- replace_zero | (times >= start_day & times <= end_day)
        }
    }

    vec[replace_zero] <- 0
    vec
}

# ---------------------------------------------------------------------------
# Apply long-run processing to all residual series
# ---------------------------------------------------------------------------

residual_cols <- 3:ncol(df)
df[residual_cols] <- lapply(df[residual_cols], replace_missing_days, times = df$dttm)

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------

saveRDS(df, output_file)
