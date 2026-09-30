#####

## MULTIBAND RECONSTRUCTION FOR FIGURES 9, 10 AND 11
## POOLE'S CAVERN
## DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 29 SEPTEMBER 2026

#####

# Band-pass filters the six residual series within each continuous day
# or night segment and saves the two outputs used downstream:
#
#   data/kalman_filtered_multiband.rds
#       Reconstructed series (Figure 11).
#
#   data/kalman_filtered_multiband_segments.rds
#       Per-segment minimum and maximum (Figure 10).
#
# The band-pass computation is carried out once here; the Figure 9 data
# are derived from the first of these files in
# 09_reconstruction_analysis.R.

# ---------------------------------------------------------------------------
# Packages and functions
# ---------------------------------------------------------------------------

library(dplyr)
source("analysis_functions.R")

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/kalman_residuals_full_imp.rds"
output_full <- "data/kalman_filtered_multiband.rds"
output_segments <- "data/kalman_filtered_multiband_segments.rds"

fs_cpd <- 144

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

df <- readRDS(input_file)

# The access-regime identifier is set aside before filtering and
# restored afterwards.
period <- df$period
df <- df %>% select(-period)

# ---------------------------------------------------------------------------
# Band-pass processing
# ---------------------------------------------------------------------------

res <- reconstruct_multiband_segments(df, timestamp_col = "dttm", fs_cpd = fs_cpd)
res$full_series$period <- period

# ---------------------------------------------------------------------------
# Save processed data
# ---------------------------------------------------------------------------

saveRDS(res$full_series, output_full)
saveRDS(res$segment_summary, output_segments)
