#####

## PREPROCESSING THE DOWNLOADED TEMPERATURE RECORDS FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# Converts the six temperature records, as downloaded from the British
# Cave Research Association archive, into one regular 10-minute series
# per monitoring point on a common time axis:
#
#   https://bcra.org.uk/data/all_readings/
#
# Each downloaded reading carries the logger's own timestamp in UTC
# (ISO 8601, e.g. 2019-10-16T00:01:06Z), a few seconds to a few
# minutes off the 10-minute grid. The loggers do not change their
# clocks for British Summer Time, so all times here, and in every later
# script, are UTC.
#
# For each monitoring point:
#
#   1. each reading is assigned to the nearest 10-minute timestamp;
#   2. if two readings fall in the same 10-minute slot, the first is
#      kept (this happens twice in the whole dataset);
#   3. physically impossible readings caused by sensor faults (below
#      fault_threshold) are set to NA;
#   4. slots with no reading are left as NA.
#
# Gaps are filled later, in 01_imputing.R.

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(readr)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_dir <- "measured"
input_pattern <- "^\\d{2}_temp_[a-z]+_2019_2021\\.csv$"
output_file <- "data/all_sites_10min.rds"

# ---------------------------------------------------------------------------
# Settings
# ---------------------------------------------------------------------------

# Study period: 731 days of 144 ten-minute slots, from 2019-10-16 00:00
# to 2021-10-15 23:50 UTC.
study_start <- as.POSIXct("2019-10-16 00:00:00", tz = "UTC")
n_slots <- 731 * 144
slot_seconds <- 600

# Sensor faults appear as readings between -34 and -104 degC. Genuine
# readings never fall below about -3 degC (at the cave entrance in
# winter), so any reading below -20 degC is treated as a fault.
fault_threshold <- -20

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

#' Convert one downloaded record to a regular 10-minute series.
#'
#' @param file Path to a downloaded CSV with columns `dttm` (ISO 8601,
#'   UTC) and `temp` (degC).
#' @return Numeric vector of length `n_slots`, one value per 10-minute
#'   slot from `study_start`, with `NA` where there is no valid reading.
regularise_record <- function(file) {
    df <- read_csv(
        file,
        col_types = cols(dttm = col_datetime(), temp = col_double())
    )

    seconds <- as.numeric(difftime(df$dttm, study_start, units = "secs"))
    slot <- round(seconds / slot_seconds)

    # Every reading is less than 5 minutes from its slot, so "nearest"
    # is never ambiguous.
    stopifnot(all(abs(seconds - slot * slot_seconds) < slot_seconds / 2))

    in_period <- slot >= 0 & slot < n_slots
    first_in_slot <- !duplicated(slot)
    is_fault <- !is.na(df$temp) & df$temp < fault_threshold

    keep <- in_period & first_in_slot
    value <- ifelse(is_fault, NA_real_, df$temp)

    series <- rep(NA_real_, n_slots)
    series[slot[keep] + 1] <- value[keep]

    message(sprintf(
        "%s: %d readings, %d duplicate(s) dropped, %d fault(s) set to NA, %d of %d slots missing",
        basename(file), nrow(df), sum(in_period & !first_in_slot),
        sum(keep & is_fault), sum(is.na(series)), n_slots
    ))

    series
}

# ---------------------------------------------------------------------------
# Regularise all six records
# ---------------------------------------------------------------------------

files <- sort(list.files(input_dir, pattern = input_pattern, full.names = TRUE))
stopifnot(length(files) == 6)

# Monitoring-point codes from the file names, e.g. "ent" from
# "01_temp_ent_2019_2021.csv". The file-name prefix fixes the column
# order (MP1 to MP6).
site_codes <- sub("^\\d{2}_temp_([a-z]+)_.*$", "\\1", basename(files))

series <- lapply(files, regularise_record)
names(series) <- site_codes

all_sites <- data.frame(
    dttm = study_start + slot_seconds * (seq_len(n_slots) - 1),
    series
)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

saveRDS(all_sites, output_file)
