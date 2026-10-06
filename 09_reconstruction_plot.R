#####

## FIGURE 9: RECONSTRUCTED SIGNAL POOLE'S CAVERN
## 45.5-50.5 cpd PLOTTING ONLY
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Packages and helpers
# ---------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

source("plot_helpers.R")
load_plot_fonts()

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/reconstructed_45_5_50_5_cpd.rds"
output_file <- "figs/09_ifft_plots.tif"
reconstructed_full <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Prepare plotting data
# ---------------------------------------------------------------------------

# Each series is given a time axis normalised to run from 0 to 1 within
# each access regime.

plot_data <- reconstructed_full %>%
    select(
        dttm, period,
        ent_residual_nb3, ten_residual_nb3, sin_residual_nb3,
        brg_residual_nb3, mqs_residual_nb3, top_residual_nb3
    ) %>%
    pivot_longer(
        cols = c(
            ent_residual_nb3, ten_residual_nb3, sin_residual_nb3,
            brg_residual_nb3, mqs_residual_nb3, top_residual_nb3
        ),
        names_to = "monitoring_point",
        values_to = "value"
    ) %>%
    mutate(monitoring_point = sub("_residual_nb3$", "", monitoring_point)) %>%
    group_by(monitoring_point, period) %>%
    mutate(
        t_num = as.numeric(dttm),
        time_norm = (t_num - min(t_num)) / (max(t_num) - min(t_num))
    ) %>%
    select(-t_num) %>%
    ungroup()

# ---------------------------------------------------------------------------
# Function for individual panels
# ---------------------------------------------------------------------------

#' Build one monitoring-point/access-regime reconstructed-signal panel.
#'
#' The y-axis settings for each monitoring point come from
#' `reconstruction_axes` (plot_helpers.R). Line and grid-line colour
#' encode the access regime (green for open, red for restricted);
#' there is no day/night split in this figure.
#'
#' @param data Long-format data frame (see `plot_data` above).
#' @param point Monitoring-point code (e.g. `"ent"`).
#' @param period_number Access-regime number (1-5).
#' @param show_y_axis,show_x_axis Whether to draw y-/x-axis text
#'   (first column and bottom row only).
#' @return A ggplot object.
make_panel <- function(
    data,
    point,
    period_number,
    show_y_axis = FALSE,
    show_x_axis = FALSE
) {
    axes <- reconstruction_axes[[point]]

    # Green for open-access regimes; red for restricted-access regimes.
    line_colour <- if (period_number %in% open_regimes) "#2CA02C" else "#B22222"
    grid_colour <- regime_grid_colour(period_number)

    p <- data %>% filter(monitoring_point == point, period == period_number)

    ggplot(p, aes(x = time_norm, y = value)) +
        geom_line(colour = line_colour, linewidth = 0.05) +
        scale_x_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.5, 1),
            labels = c("0", "0.5", "1"),
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            breaks = axes$breaks,
            labels = axes$labels,
            expand = c(mult = 0.005, add = 0)
        ) +
        coord_cartesian(ylim = axes$limit) +
        geom_hline(yintercept = 0, colour = "#333333", linewidth = 0.1) +
        labs(
            title = if (period_number == 1) monitoring_point_labels[[point]] else "",
            subtitle = "",
            x = "",
            y = ""
        ) +
        theme_ste() +
        theme(
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major.y = element_line(colour = grid_colour, linewidth = 0.3, linetype = "solid"),
            axis.text.x = if (show_x_axis) {
                element_text(size = 7, vjust = -0.5)
            } else {
                element_blank()
            },
            axis.text.y = if (show_y_axis) {
                element_text(angle = 90, size = 7, vjust = 1.5, hjust = 0.5)
            } else {
                element_blank()
            }
        )
}

# ---------------------------------------------------------------------------
# Create the 30 panels
# ---------------------------------------------------------------------------

points <- names(reconstruction_axes)
ifft_panels <- list()

for (i in seq_along(points)) {
    for (period in 1:5) {
        ifft_panels[[paste0("plot_", i, "_", period)]] <- make_panel(
            plot_data,
            point = points[i],
            period_number = period,
            show_y_axis = period == 1,
            show_x_axis = i == length(points)
        )
    }
}

# ---------------------------------------------------------------------------
# Assemble figure
# ---------------------------------------------------------------------------

ifft_plot <- assemble_figure(
    ifft_panels,
    ncol = 5,
    y_label = "r  e  c  o  n  s  t  r  u  c  t  e  d      s  i  g  n  a  l,     \u00B0  C",
    x_label = "n  o  r  m  a  l  i  s  e  d      t  i  m  e"
)

# ---------------------------------------------------------------------------
# Save figure
# ---------------------------------------------------------------------------

ggsave(
    filename = output_file,
    plot = ifft_plot,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600,
    compression = "lzw"
)
