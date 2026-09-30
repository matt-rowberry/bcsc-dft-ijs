#####

## IMPUTING SHORT RUNS OF MISSING VALUES IN TEMPERATURE RECORDS FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED ON 28 SEPTEMBER 2026

#####

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(imputeTS)
library(future)
library(future.apply)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

csv_dir <- "measured"
csv_pattern <- "^\\d{2}_pooles_.*_2019_2021\\.csv$"
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
# Import quality-controlled temperature data
# ---------------------------------------------------------------------------

# Temperature observations were downloaded from the British Cave
# Research Association data archive:
#
#   https://bcra.org.uk/data/all_readings/
#
# Prior to import, the downloaded observations were (1) rounded to
# the nearest 10-minute interval and (2) checked for and cleared of
# duplicate timestamps. The CSV files imported below therefore
# represent quality-controlled input data rather than the original
# downloaded observations.

#' Read one site's temperature column from a quality-controlled CSV.
#'
#' @param file Path to a CSV file with a timestamp column followed
#'   by a single temperature column.
#' @return A one-column data frame named after the temperature
#'   column.
read_temp <- function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    setNames(data.frame(df[[2]]), names(df)[2])
}

csv_files <- list.files(csv_dir, pattern = csv_pattern, full.names = TRUE)
df_list <- lapply(csv_files, read_temp)

# The six series are combined by position, not joined by timestamp.
# Each file must therefore already contain exactly one row per
# 10-minute interval of the analysis period, in chronological order
# (this is what the quality control described above provides). The
# row counts are checked here so that any misalignment fails loudly
# rather than silently shifting one series against the others.

stopifnot(
    all(vapply(df_list, nrow, integer(1)) == length(dttm))
)

df <- do.call(cbind.data.frame, df_list)

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
