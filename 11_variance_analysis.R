#####

## FIGURE 11: RMS AMPLITUDE AND VARIANCE EXPLAINED
## DATA ANALYSIS
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# Prepares the data used by Figure 11, and writes the same values as a
# CSV table alongside the figure. Plotting is handled separately in
# 11_variance_plot.R.

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------

library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

ifft_file <- "data/kalman_filtered_multiband.rds"
residuals_file <- "data/kalman_residuals_full_imp.rds"
output_file <- "data/figure_11_heatmaps.rds"
table_file <- "figs/11_variance_values.csv"

ifft_cols <- c(
    "ent_residual_nb3", "ten_residual_nb3", "sin_residual_nb3",
    "brg_residual_nb3", "mqs_residual_nb3", "top_residual_nb3"
)

res_cols <- c(
    "ent_residual", "ten_residual", "sin_residual",
    "brg_residual", "mqs_residual", "top_residual"
)

# ---------------------------------------------------------------------------
# Load and align data
# ---------------------------------------------------------------------------

df_ifft <- readRDS(ifft_file)
df_res <- readRDS(residuals_file)

# The residual file contains dttm, period and the six residual series.
# Timestamp and day/night classification are aligned directly with
# df_ifft. Both objects are derived from the same residual file, so
# the rows correspond one-to-one; this is checked before aligning.

stopifnot(
    nrow(df_res) == nrow(df_ifft),
    all(df_res$dttm == df_ifft$timestamp),
    all(df_res$period == df_ifft$period)
)

df_res$timestamp <- df_ifft$timestamp
df_res$daynight <- df_ifft$daynight

# ---------------------------------------------------------------------------
# RMS summary
# ---------------------------------------------------------------------------

# Exact zeros in the reconstructed series mark days that were set to
# zero in 03_long_gaps.R (long runs of missing data). They are
# converted to NA here so that they are excluded from the summaries.

rms_summary <- df_ifft %>%
    pivot_longer(cols = all_of(ifft_cols), names_to = "monitoring_point", values_to = "ifft") %>%
    mutate(
        monitoring_point = sub("_residual_nb3$", "", monitoring_point),
        ifft = na_if(ifft, 0)
    ) %>%
    group_by(period, daynight, monitoring_point) %>%
    summarise(rms = sqrt(mean(ifft^2, na.rm = TRUE)), .groups = "drop") %>%
    complete(period, daynight, monitoring_point)

# ---------------------------------------------------------------------------
# Variance explained
# ---------------------------------------------------------------------------

ifft_long <- df_ifft %>%
    pivot_longer(cols = all_of(ifft_cols), names_to = "monitoring_point", values_to = "ifft") %>%
    mutate(
        monitoring_point = sub("_residual_nb3$", "", monitoring_point),
        ifft = na_if(ifft, 0)
    )

res_long <- df_res %>%
    pivot_longer(cols = all_of(res_cols), names_to = "monitoring_point", values_to = "residual") %>%
    mutate(monitoring_point = sub("_residual$", "", monitoring_point))

combined <- ifft_long %>%
    left_join(res_long, by = c("timestamp", "period", "daynight", "monitoring_point"))

variance_explained <- combined %>%
    filter(!is.na(ifft)) %>%
    group_by(period, daynight, monitoring_point) %>%
    summarise(
        residual_variance = var(residual),
        bandlimited_variance = var(ifft),
        variance_explained = 100 * bandlimited_variance / residual_variance,
        .groups = "drop"
    ) %>%
    complete(period, daynight, monitoring_point)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

figure_11 <- list(rms = rms_summary, variance_explained = variance_explained)
saveRDS(figure_11, output_file)

# The values plotted in Figure 11 as one table, for readers without R:
# one row per monitoring point, access regime and day/night period,
# with the residual and band-limited (45.5-50.5 cpd) variances from
# which the variance explained is calculated.
# Empty cells are combinations with no data (MP6 before August 2020;
# MP4 in regime 5).

mp_numbers <- c(ent = 1, ten = 2, sin = 3, brg = 4, mqs = 5, top = 6)

figure_11_table <- full_join(
    rms_summary, variance_explained,
    by = c("period", "daynight", "monitoring_point")
) %>%
    transmute(
        # site_code first: transmute() evaluates in order, and the next
        # line replaces monitoring_point.
        site_code = monitoring_point,
        monitoring_point = paste0("MP", mp_numbers[monitoring_point]),
        access_regime = period,
        day_night = daynight,
        rms_amplitude_degC = signif(rms, 6),
        residual_variance_degC2 = signif(residual_variance, 6),
        bandlimited_variance_degC2 = signif(bandlimited_variance, 6),
        variance_explained_pct = signif(variance_explained, 6)
    ) %>%
    mutate(across(where(is.double), ~ ifelse(is.nan(.x), NA, .x))) %>%
    relocate(monitoring_point) %>%
    arrange(monitoring_point, access_regime, day_night)

write.csv(figure_11_table, table_file, row.names = FALSE, na = "")
