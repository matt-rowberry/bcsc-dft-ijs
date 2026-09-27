#####

## CALCULATING SPECTROGRAMS FOR POOLE'S CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 26 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(openxlsx)
library(lubridate)
library(dplyr)
library(phonTools)
library(seewave)

#####

options(digits = 16)
Sys.setlocale("LC_TIME", "English_United Kingdom")

#####

## 02. LOAD DATA

df <- read.xlsx(
    "data/kalman_residuals_full_imp.xlsx"
)

df$dttm <- as.POSIXct(
    (df$dttm - 25569) * 86400,
    origin = "1970-01-01",
    tz = "UTC"
)

df$dttm <- round_date(
    df$dttm,
    unit = "10 mins"
)

#####

## 03. DEFINE ACCESS REGIMES

# Access regimes are defined from the observation timestamp.

df$period <- case_when(
    df$dttm < ymd_hms(
        "2020-03-19 00:00:00",
        tz = "UTC"
    ) ~ 1,

    df$dttm < ymd_hms(
        "2020-07-20 00:00:00",
        tz = "UTC"
    ) ~ 2,

    df$dttm < ymd_hms(
        "2020-11-05 00:00:00",
        tz = "UTC"
    ) ~ 3,

    df$dttm < ymd_hms(
        "2021-05-18 00:00:00",
        tz = "UTC"
    ) ~ 4,

    TRUE ~ 5
)

#####

## 04. CREATE SPECTROGRAM INPUT DATAFRAME

# The six monitoring-point residual series are used as the
# input signals for the spectrogram analysis.

df_spec <- data.frame(
    period = df$period,
    ent = df$ent_residual,
    ten = df$ten_residual,
    sin = df$sin_residual,
    brg = df$brg_residual,
    mqs = df$mqs_residual,
    top = df$top_residual
)

# Missing values are replaced with zero before calculating
# the spectrograms, as in the original analysis.

df_spec[is.na(df_spec)] <- 0

#####

## 05. CONVERT SPECTROGRAM OUTPUT TO A DATAFRAME

spectro_to_df <- function(spec) {

    data.frame(
        time = rep(
            spec$time,
            each = length(spec$freq)
        ),

        frequency = rep(
            spec$freq,
            times = length(spec$time)
        ),

        amplitude = as.vector(
            spec$amp
        )
    )
}

#####

## 06. CALCULATE SPECTROGRAMS

sensor_names <- setdiff(
    names(df_spec),
    "period"
)

spectrogram_list <- lapply(
    sensor_names,
    function(sensor) {

        sound_obj <- makesound(
            df_spec[[sensor]],
            fs = 144000
        )

        spec <- spectro(
            sound_obj,
            wl    = 54,
            wn    = "bartlett",
            zp    = length(df_spec[[sensor]]) * 4,
            ovlp  = 0,
            dB    = "max0",
            flimd = c(45, 51),
            plot  = FALSE
        )

        spec_df <- spectro_to_df(
            spec
        )

        # Limit amplitudes to the range used in the original
        # spectrogram analysis.

        spec_df$amplitude <- pmin(
            pmax(
                spec_df$amplitude,
                -60
            ),
            0
        )

        spec_df$sensor <- sensor

        spec_df
    }
)

all_sensors_df <- bind_rows(
    spectrogram_list
)

#####

## 07. ASSIGN ACCESS REGIMES TO SPECTROGRAM TIME

# Spectrogram time is normalised within the complete record.
# The original analysis used these boundaries to assign the
# five access regimes.

all_sensors_df$period <- cut(
    all_sensors_df$time,

    breaks = c(
        0,
        0.155,
        0.278,
        0.386,
        0.580,
        0.731
    ),

    labels = 1:5,
    include.lowest = TRUE,
    right = TRUE
) %>%
    as.integer()

#####

## 08. SAVE RESULTS

saveRDS(
    all_sensors_df,
    "data/figure_08_spectrograms.rds"
)

#####
