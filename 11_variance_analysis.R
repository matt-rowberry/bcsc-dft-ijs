# Figure 11: RMS amplitude and variance explained
#
# Prepare the data used by Figure 11.
# Plotting is handled separately in 11_variance_plot.R.

library(dplyr)
library(tidyr)

# -------------------------------------------------------------------------
# DATA
# -------------------------------------------------------------------------

df_ifft <- readRDS(
    "data/kalman_filtered_multiband.rds"
)

df_res <- readRDS(
    "data/kalman_residuals_full_imp.rds"
)

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

# -------------------------------------------------------------------------
# MONITORING POINT COLUMNS
# -------------------------------------------------------------------------

ifft_cols <- c(
    "ent_residual_nb3",
    "ten_residual_nb3",
    "sin_residual_nb3",
    "brg_residual_nb3",
    "mqs_residual_nb3",
    "top_residual_nb3"
)

res_cols <- c(
    "ent_residual",
    "ten_residual",
    "sin_residual",
    "brg_residual",
    "mqs_residual",
    "top_residual"
)

# -------------------------------------------------------------------------
# RMS SUMMARY
# -------------------------------------------------------------------------

# Exact zeros in the reconstructed series mark days that were set to
# zero in 03_long_gaps.R (long runs of missing data). They are
# converted to NA here so that they are excluded from the summaries.

rms_summary <- df_ifft %>%
    pivot_longer(
        cols = all_of(ifft_cols),
        names_to = "monitoring_point",
        values_to = "ifft"
    ) %>%
    mutate(
        monitoring_point = sub(
            "_residual_nb3$",
            "",
            monitoring_point
        ),
        ifft = na_if(ifft, 0)
    ) %>%
    group_by(
        period,
        daynight,
        monitoring_point
    ) %>%
    summarise(
        rms = sqrt(mean(ifft^2, na.rm = TRUE)),
        .groups = "drop"
    ) %>%
    complete(
        period,
        daynight,
        monitoring_point
    )

# -------------------------------------------------------------------------
# VARIANCE EXPLAINED
# -------------------------------------------------------------------------

ifft_long <- df_ifft %>%
    pivot_longer(
        cols = all_of(ifft_cols),
        names_to = "monitoring_point",
        values_to = "ifft"
    ) %>%
    mutate(
        monitoring_point = sub(
            "_residual_nb3$",
            "",
            monitoring_point
        ),
        ifft = na_if(ifft, 0)
    )

res_long <- df_res %>%
    pivot_longer(
        cols = all_of(res_cols),
        names_to = "monitoring_point",
        values_to = "residual"
    ) %>%
    mutate(
        monitoring_point = sub(
            "_residual$",
            "",
            monitoring_point
        )
    )

combined <- ifft_long %>%
    left_join(
        res_long,
        by = c(
            "timestamp",
            "period",
            "daynight",
            "monitoring_point"
        )
    )

variance_explained <- combined %>%
    filter(!is.na(ifft)) %>%
    group_by(
        period,
        daynight,
        monitoring_point
    ) %>%
    summarise(
        variance_explained = 100 * var(ifft) / var(residual),
        .groups = "drop"
    ) %>%
    complete(
        period,
        daynight,
        monitoring_point
    )

# -------------------------------------------------------------------------
# SAVE
# -------------------------------------------------------------------------

figure_11 <- list(
    rms = rms_summary,
    variance_explained = variance_explained
)

saveRDS(
    figure_11,
    "data/figure_11_heatmaps.rds"
)
