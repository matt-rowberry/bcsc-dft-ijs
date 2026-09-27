#####

## PROCESSING LONG RUNS IN THE RESIDUAL DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 25 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(lubridate)
library(writexl)
library(openxlsx)

#####

## 02. READ RESIDUAL DATA AND INITIALISE FIRST OBSERVATIONS

df <- read.xlsx(
    "data/kalman_results_residuals.xlsx"
)

# Convert Excel datetime values to POSIXct.

df$dttm <- as.POSIXct(
    (df$dttm - 25569) * 86400,
    origin = "1970-01-01",
    tz = "UTC"
)

options(digits = 16)

# The first recursive residual produced by KFAS is set to zero
# because it is affected by the initialisation of the state-space
# model and is not used as an empirical residual observation in
# the subsequent analysis.

df[1, 3:ncol(df)] <- 0

#####

## 03. IDENTIFY LONG RUNS OF MISSING RESIDUALS

# Long runs of missing residuals are not interpolated.
#
# Instead, any calendar day containing a missing run is treated
# as unavailable for the subsequent analysis. All residual values
# within that day are therefore set to zero.
#
# This prevents a long missing interval from being interpreted as
# a sequence of residual observations while retaining a complete
# regular time series for subsequent processing.

replace_missing_days <- function(
        vec,
        times
) {

    is_na <- is.na(vec)

    rle_na <- rle(is_na)

    ends <- cumsum(
        rle_na$lengths
    )

    starts <- c(
        1,
        head(ends, -1) + 1
    )

    replace_zero <- rep(
        FALSE,
        length(vec)
    )

    for (i in seq_along(rle_na$lengths)) {

        if (rle_na$values[i]) {

            # A run of missing residuals has been detected.

            start_idx <- starts[i]
            end_idx <- ends[i]

            start_day <- floor_date(
                times[start_idx],
                unit = "day"
            )

            end_day <- floor_date(
                times[end_idx],
                unit = "day"
            ) +
                hours(23) +
                minutes(50)

            # Mark the complete calendar days containing
            # the missing run.

            replace_zero <- replace_zero |
                (
                    times >= start_day &
                        times <= end_day
                )
        }
    }

    # Replace the affected observations with zero residuals.

    vec[replace_zero] <- 0

    vec
}

#####

## 04. APPLY LONG-RUN PROCESSING TO ALL RESIDUAL SERIES

residual_cols <- 3:ncol(df)

df[residual_cols] <- lapply(
    df[residual_cols],
    replace_missing_days,
    times = df$dttm
)

#####

## 05. SAVE RESULTS

write_xlsx(
    df,
    path = "data/kalman_residuals_full_imp.xlsx"
)
