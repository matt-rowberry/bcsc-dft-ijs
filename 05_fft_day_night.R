#####

## FFT DATA PROCESSING FOR FIGURE 7
## TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 26 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(dplyr)
library(purrr)

#####

## 02. SET OPTIONS

Sys.setlocale(
    "LC_TIME",
    "English_United Kingdom"
)

options(digits = 16)

#####

## 03. LOAD BANDPASS FUNCTIONS

source("bandpass_functions.R")

#####

## 04. LOAD PREPROCESSED DATA

input_file <- "data/full_day_night_time_series.rds"

output_file <- "data/figure_07_bandpass.rds"

data <- readRDS(input_file)

#####

## 05. PREPARE THE FIVE ACCESS REGIMES

# The preprocessing pipeline produces:
#
#   full_period_1
#   full_period_2
#   full_period_3
#   full_period_4
#   full_period_5
#
# The original Figure 7 analysis used the short monitoring-point
# names expected by bandpass_fft_daynight() and the plotting code.
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
# Therefore only the column names are changed here. The data
# themselves are unchanged.

prepare_period <- function(x) {

    x %>%
        select(
            dttm,
            ent = ent_residual,
            ten = ten_residual,
            sin = sin_residual,
            brg = brg_residual,
            mqs = mqs_residual,
            top = top_residual
        )
}

df_list <- map(
    paste0("full_period_", 1:5),
    ~ prepare_period(data[[.x]])
)

names(df_list) <- paste0(
    "df",
    1:5
)

#####

## 06. SET SAMPLING FREQUENCY

fs_cpd <- 144

#####

## 07. EXTRACT FFT SERIES

extract_series <- function(bandpass_out) {

    signals <- names(bandpass_out)

    series_types <- c(
        "full",
        "day",
        "night"
    )

    map(
        series_types,
        function(series) {

            freqs <- bandpass_out[[1]][[series]]$freqs_cpd

            amplitudes <- map(
                signals,
                ~ bandpass_out[[.x]][[series]]$amplitude
            ) %>%
                as.data.frame()

            colnames(amplitudes) <- signals

            cbind(
                freqs = freqs,
                amplitudes
            )
        }
    ) %>%
        set_names(series_types)
}

#####

## 08. PROCESS ALL FIVE ACCESS REGIMES

process_all_dfs <- function(
        df_list,
        fs_cpd,
        freq_min = 42,
        freq_max = 54
) {

    map(
        df_list,
        ~ bandpass_fft_daynight(
            .x,
            timestamp_col = "dttm",
            fs_cpd = fs_cpd,
            freq_min = freq_min,
            freq_max = freq_max
        )
    ) %>%
        map(extract_series)
}

all_bandpass_series <- process_all_dfs(
    df_list,
    fs_cpd
)

#####

## 09. NORMALISE BANDPASS SERIES

normalize_bandpass_series <- function(
        all_series,
        vars = c(
            "ent",
            "ten",
            "sin",
            "brg",
            "mqs",
            "top"
        )
) {

    # Remove the "full" entries.
    # Figure 7 uses day and night spectra only.

    all_series <- map(
        all_series,
        ~ .x[names(.x) != "full"]
    )

    # Calculate one global maximum for each monitoring point.
    #
    # Each maximum is calculated across all five access regimes
    # and both day and night spectra.

    max_values <- map(
        vars,
        function(v) {

            max(
                map(
                    all_series,
                    function(df_item) {

                        c(
                            df_item$day[[v]],
                            df_item$night[[v]]
                        )
                    }
                ) %>%
                    unlist(),
                na.rm = TRUE
            )
        }
    ) %>%
        set_names(vars)

    # Apply the global monitoring-point normalisation.

    all_series_norm <- map(
        all_series,
        function(df_item) {

            map(
                df_item,
                function(period_df) {

                    period_df[vars] <- map_dfc(
                        vars,
                        ~ period_df[[.x]] /
                            max_values[[.x]]
                    )

                    period_df
                }
            )
        }
    )

    all_series_norm
}

#####

## 10. APPLY NORMALISATION

df <- normalize_bandpass_series(
    all_bandpass_series
)

#####

## 11. SAVE RESULTS FOR FIGURE 7

saveRDS(
    df,
    output_file
)

#####
