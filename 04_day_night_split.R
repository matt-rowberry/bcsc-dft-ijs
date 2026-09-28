#####

## ORGANISING DATAFRAMES FOR THE TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 28 SEPTEMBER 2026

#####

## 01. LOAD DATA

df <- readRDS(
    "data/kalman_residuals_full_imp.rds"
)

#####

## 02. CREATE ALIGNED DAY, NIGHT AND FULL-PERIOD DATASETS

# Day and night intervals are defined using the access times
# applied throughout the study:
#
#   Day:   09:30–17:30
#   Night: 17:40–09:20
#
# The 10-minute interval from 09:20 to 09:30 and from 17:30
# to 17:40 is therefore excluded from both periods.
#
# For the day and night datasets, observations outside the
# relevant interval are replaced with NA while the complete
# 10-minute time axis is retained. This preserves temporal
# alignment between the day and night series and is required
# for the downstream analyses.

split_day_night_period <- function(
        df,
        timestamp_col = "dttm",
        period_col = "period"
) {

    df[[timestamp_col]] <- as.POSIXct(
        df[[timestamp_col]]
    )

    time_str <- format(
        df[[timestamp_col]],
        "%H:%M:%S"
    )

    is_night <- (
        time_str >= "17:40:00" |
            time_str <= "09:20:00"
    )

    is_day <- !is_night

    # Replace observations outside the selected interval with
    # NA while preserving the original column type.

    make_aligned <- function(df, mask) {

        df_masked <- df

        cols_to_mask <- setdiff(
            names(df),
            c(timestamp_col, period_col)
        )

        for (col in cols_to_mask) {

            # Assigning NA keeps the type of the column, so no
            # class-specific handling is needed.

            df_masked[[col]][!mask] <- NA
        }

        df_masked
    }

    df_day_aligned <- make_aligned(
        df,
        is_day
    )

    df_night_aligned <- make_aligned(
        df,
        is_night
    )

    # Split the aligned datasets by access regime.

    day_list <- split(
        df_day_aligned,
        df_day_aligned[[period_col]]
    )

    night_list <- split(
        df_night_aligned,
        df_night_aligned[[period_col]]
    )

    full_list <- split(
        df,
        df[[period_col]]
    )

    # Name the resulting datasets.

    names(day_list) <- paste0(
        "day_period_",
        names(day_list)
    )

    names(night_list) <- paste0(
        "night_period_",
        names(night_list)
    )

    names(full_list) <- paste0(
        "full_period_",
        names(full_list)
    )

    # Combine day, night and full-period datasets.

    all_lists <- c(
        day_list,
        night_list,
        full_list
    )

    return(all_lists)
}

#####

## 03. APPLY FUNCTION AND SAVE RESULTS

result <- split_day_night_period(df)

saveRDS(
    result,
    "data/full_day_night_time_series.rds"
)
