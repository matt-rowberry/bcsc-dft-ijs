#####

## IMPUTING SHORT RUNS OF MISSING VALUES IN TEMPERATURE RECORDS FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED ON 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(imputeTS)
library(future)
library(future.apply)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/all_sites_10min.rds"
output_file <- "data/all_sites_sr_imp.rds"

# ---------------------------------------------------------------------------
# Analysis period
# ---------------------------------------------------------------------------

dttm <- seq(
    ymd_hms("2019-10-16 00:00:00"),
    ymd_hms("2021-10-15 23:50:00"),
    by = "10 mins"
)

# ---------------------------------------------------------------------------
# Access regimes
# ---------------------------------------------------------------------------

# Five access regimes were defined for the study:
#
# 1 = normal prepandemic operation
# 2 = first pandemic closure
# 3 = reopening between lockdowns
# 4 = second pandemic closure
# 5 = phased reopening
#
# Regime 5 includes two visitor-group limits: a maximum of six
# visitors per group from 18 May to 18 July 2021, followed by a
# maximum of twenty visitors from 19 July 2021. These intervals were
# treated as a single access regime for the analysis.

period <- case_when(
    dttm < ymd_hms("2020-03-19 00:00:00") ~ 1,
    dttm < ymd_hms("2020-07-20 00:00:00") ~ 2,
    dttm < ymd_hms("2020-11-05 00:00:00") ~ 3,
    dttm < ymd_hms("2021-05-18 00:00:00") ~ 4,
    TRUE ~ 5
)

# ---------------------------------------------------------------------------
# Import the regular 10-minute temperature series
# ---------------------------------------------------------------------------

# 00_preprocess.R converts the downloaded records into one 10-minute
# series per monitoring point, in UTC, on the same time axis as dttm
# above. The time axes are checked to match exactly, so that a series
# can never be shifted against the access regimes.

all_sites <- readRDS(input_file)

stopifnot(
    nrow(all_sites) == length(dttm),
    all(all_sites$dttm == dttm)
)

df <- all_sites[, setdiff(names(all_sites), "dttm")]

# ---------------------------------------------------------------------------
# Impute short runs of missing data
# ---------------------------------------------------------------------------

# Missing runs of up to 36 observations (6 hours at 10-minute
# sampling) are imputed using a structural time-series model
# implemented through imputeTS::na_kalman(). Longer missing runs are
# retained as missing.
#
# The six series are imputed independently, so the work is spread
# across worker processes with the future package. The number of
# workers affects only run time, not the results.

workers <- max(1, availableCores() - 1)
oplan <- plan(multisession, workers = workers)

sr_imp_list <- future_lapply(df, function(col) {
    na_kalman(col, model = "StructTS", smooth = TRUE, maxgap = 36)
})

plan(oplan)

sr_imp <- as.data.frame(sr_imp_list)
names(sr_imp) <- names(df)

# ---------------------------------------------------------------------------
# Add datetime and access-regime columns
# ---------------------------------------------------------------------------

sr_imp <- sr_imp %>%
    mutate(dttm = dttm, period = period) %>%
    select(dttm, period, everything())

# ---------------------------------------------------------------------------
# Write imputed dataset
# ---------------------------------------------------------------------------

saveRDS(sr_imp, output_file)
