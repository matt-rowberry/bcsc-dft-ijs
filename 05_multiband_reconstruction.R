#####

## MULTIBAND RECONSTRUCTION FOR FIGURES 9, 10 AND 11
## POOLE'S CAVERN DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 4 OCTOBER 2026

#####

# Band-pass filters the six residual series within each continuous day
# or night segment and saves the two outputs used downstream:
#
#   data/kalman_filtered_multiband.rds
#       Reconstructed series in degrees Celsius (Figure 9; RMS in
#       Figure 11).
#
#   data/kalman_filtered_multiband_segments.rds
#       Per-segment minimum and maximum, in degrees Celsius (Figure 10).
#
#   data/kalman_filtered_multiband_prewhitened.rds
#       Reconstructed series as the residuals themselves (variance
#       explained in Figure 11).
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
output_prewhitened <- "data/kalman_filtered_multiband_prewhitened.rds"
parameters_file <- "data/kalman_parameters.rds"

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

# The residuals are one-step-ahead prediction errors, which amplify the
# 45.5-50.5 cpd band relative to the temperature by a factor that
# differs between monitoring points (see NOTES.md). The band is
# therefore reconstructed twice:
#
#   recoloured  - the prediction-error filter undone using each
#                 monitoring point's steady-state Kalman gain from
#                 02_kalman_filtering.R, giving degrees Celsius
#   prewhitened - as the residuals themselves, so that its variance
#                 can be compared with the residual variance

params <- readRDS(parameters_file)
K <- setNames(params$K, paste0(params$site, "_residual"))
stopifnot(all(setdiff(names(df), "dttm") %in% names(K)))

res <- reconstruct_multiband_segments(df, timestamp_col = "dttm", fs_cpd = fs_cpd, K = K)
res$full_series$period <- period

res_pw <- reconstruct_multiband_segments(df, timestamp_col = "dttm", fs_cpd = fs_cpd)
res_pw$full_series$period <- period

# ---------------------------------------------------------------------------
# Save processed data
# ---------------------------------------------------------------------------

saveRDS(res$full_series, output_full)
saveRDS(res$segment_summary, output_segments)
saveRDS(res_pw$full_series, output_prewhitened)
