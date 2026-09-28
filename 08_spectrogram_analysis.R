#####

## CALCULATING SPECTROGRAMS FOR POOLE'S CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 28 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(dplyr)
library(phonTools)
library(seewave)

#####

## 02. LOAD DATA

df <- readRDS(
    "data/kalman_residuals_full_imp.rds"
)

#####

## 03. CREATE SPECTROGRAM INPUT DATAFRAME

# The six monitoring-point residual series are used as the
# input signals for the spectrogram analysis.

df_spec <- data.frame(
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

## 04. CONVERT SPECTROGRAM OUTPUT TO A DATAFRAME

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

## 05. CALCULATE SPECTROGRAMS

# The residual series are treated as sound objects so that the
# spectrogram functions can be used. A sampling frequency of
# 144000 is used, i.e. 144 observations per day (144 cpd)
# multiplied by 1000. Frequencies are therefore reported in cycles
# per day (cpd), and one day of data lasts 0.001 s on the time
# axis of the sound object.

sensor_names <- names(df_spec)

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

## 06. ASSIGN ACCESS REGIMES TO SPECTROGRAM TIME

# The spectrogram time axis is measured in seconds of the sound
# object (see section 05), so it equals the number of days since
# the start of the record (2019-10-16) divided by 1000. The
# breakpoints below are the access-regime boundaries expressed on
# this axis:
#
#   0.000  start of record            (2019-10-16)
#   0.155  regime 1 -> 2, 155 days in (2020-03-19)
#   0.278  regime 2 -> 3, 278 days in (2020-07-20)
#   0.386  regime 3 -> 4, 386 days in (2020-11-05)
#   0.580  regime 4 -> 5, 580 days in (2021-05-18)
#   0.731  end of record, 731 days    (2021-10-15)

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

## 07. SAVE RESULTS

saveRDS(
    all_sensors_df,
    "data/figure_08_spectrograms.rds"
)

#####
