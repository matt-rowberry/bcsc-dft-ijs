#####

## FIGURE 9: RECONSTRUCTED SIGNAL POOLE'S CAVERN
## 45.5-50.5 cpd DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# The 45.5-50.5 cpd reconstruction is calculated once, for all of
# Figures 9, 10 and 11, in 05_multiband_reconstruction.R. This script
# selects and renames the columns needed for Figure 9.

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/kalman_filtered_multiband.rds"
output_file <- "data/reconstructed_45_5_50_5_cpd.rds"

# ---------------------------------------------------------------------------
# Retain only the 45.5-50.5 cpd signal
# ---------------------------------------------------------------------------

reconstructed_full <- readRDS(input_file) %>%
    select(
        timestamp, period, daynight,
        ent_residual_nb3, ten_residual_nb3, sin_residual_nb3,
        brg_residual_nb3, mqs_residual_nb3, top_residual_nb3
    ) %>%
    rename(dttm = timestamp)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

saveRDS(reconstructed_full, output_file)
