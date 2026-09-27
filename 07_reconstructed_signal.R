#####

## FIGURE 9: RECONSTRUCTED SIGNAL
## POOLE'S CAVERN
## DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 27 SEPTEMBER 2026

#####


# -------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------

library(openxlsx)
library(dplyr)
library(lubridate)
library(purrr)
library(tibble)


# -------------------------------------------------------------------------
# Input and output
# -------------------------------------------------------------------------

input_file <- "data/kalman_residuals_full_imp.xlsx"

output_full <- "data/kalman_filtered_multiband.rds"
output_segments <- "data/kalman_filtered_multiband_segments.rds"


# -------------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------------

df <- read.xlsx(input_file)

df$dttm <- as.POSIXct(
    (df$dttm - 25569) * 86400,
    origin = "1970-01-01",
    tz = "UTC"
)

period <- df$period

df <- df %>%
    select(-period)

options(digits = 16)

Sys.setlocale(
    "LC_TIME",
    "English_United Kingdom"
)


# -------------------------------------------------------------------------
# Sampling frequency
# -------------------------------------------------------------------------

fs_cpd <- 144


# -------------------------------------------------------------------------
# Band-pass processing
# -------------------------------------------------------------------------

source("bandpass_functions.R")

res <- process_day_night_multiband(
    df,
    timestamp_col = "dttm",
    fs_cpd = fs_cpd
)


# -------------------------------------------------------------------------
# Add access-regime identifier
# -------------------------------------------------------------------------

res$full_series$period <- period


# -------------------------------------------------------------------------
# Save processed data
# -------------------------------------------------------------------------

saveRDS(
    res$full_series,
    output_full
)

saveRDS(
    res$segment_summary,
    output_segments
)
