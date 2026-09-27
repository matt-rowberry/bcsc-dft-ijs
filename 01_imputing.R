#####

## IMPUTING SHORT RUNS OF MISSING VALUES IN TEMPERATURE RECORDS FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED ON 25 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(readr)
library(writexl)
library(dplyr)
library(lubridate)
library(imputeTS)
library(parallel)

#####

## 02. DEFINE ANALYSIS PERIOD

dttm <- seq(
    ymd_hms("2019-10-16 00:00:00"),
    ymd_hms("2021-10-15 23:50:00"),
    by = "10 mins"
)

#####

## 03. DEFINE ACCESS REGIMES

# Five access regimes were defined for the study:
#
# 1 = normal prepandemic operation
# 2 = first pandemic closure
# 3 = reopening between lockdowns
# 4 = second pandemic closure
# 5 = phased reopening
#
# Regime 5 includes two visitor-group limits: a maximum of six
# visitors per group from 18 May to 18 July 2021, followed by
# a maximum of twenty visitors from 19 July 2021. These intervals
# were treated as a single access regime for the analysis.

period <- case_when(
    dttm < ymd_hms("2020-03-19 00:00:00") ~ 1,
    dttm < ymd_hms("2020-07-20 00:00:00") ~ 2,
    dttm < ymd_hms("2020-11-05 00:00:00") ~ 3,
    dttm < ymd_hms("2021-05-18 00:00:00") ~ 4,
    TRUE ~ 5
)

#####

## 04. IMPORT QUALITY-CONTROLLED TEMPERATURE DATA

# Temperature observations were downloaded from the British Cave
# Research Association data archive:
#
# https://bcra.org.uk/data/all_readings/
#
# Prior to import, the downloaded observations were:
#   1. rounded to the nearest 10-minute interval; and
#   2. checked for and cleared of duplicate timestamps.
#
# The CSV files imported below therefore represent quality-controlled
# input data rather than the original downloaded observations.

csv_files <- list.files(
    "measured",
    pattern = "^\\d{2}_pooles_.*_2019_2021\\.csv$",
    full.names = TRUE
)

read_temp <- function(file) {
    df <- read_csv(file, show_col_types = FALSE)

    val <- df[[2]]
    name <- names(df)[2]

    setNames(data.frame(val), name)
}

df_list <- lapply(csv_files, read_temp)
df <- do.call(cbind.data.frame, df_list)

#####

## 05. IMPUTE SHORT RUNS OF MISSING DATA

# Missing runs of up to 36 observations (6 hours at 10-minute
# sampling) are imputed using a structural time-series model
# implemented through imputeTS::na_kalman().
#
# Longer missing runs are retained as missing.

ncores <- max(1, detectCores() - 1)

cl <- makeCluster(ncores)

clusterEvalQ(cl, library(imputeTS))

sr_imp_list <- parLapply(cl, df, function(col) {
    imputeTS::na_kalman(
        col,
        model = "StructTS",
        smooth = TRUE,
        maxgap = 36
    )
})

stopCluster(cl)

sr_imp <- as.data.frame(sr_imp_list)
names(sr_imp) <- names(df)

#####

## 06. ADD DATETIME AND ACCESS-REGIME COLUMNS

sr_imp <- sr_imp %>%
    mutate(
        dttm = dttm,
        period = period
    ) %>%
    select(dttm, period, everything())

#####

## 07. WRITE IMPUTED DATASET

write_xlsx(
    sr_imp,
    "data/all_sites_sr_imp.xlsx"
)
