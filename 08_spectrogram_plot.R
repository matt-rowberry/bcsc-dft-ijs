#####

## PLOTTING SPECTROGRAMS FOR POOLE'S CAVERN PLOTTING ONLY
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Packages and helpers
# ---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

source("plot_helpers.R")
load_plot_fonts()

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/figure_08_spectrograms.rds"
output_file <- "figs/08_spectrograms.tif"
all_sensors_df <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Define plot theme
# ---------------------------------------------------------------------------

# The spectrograms are heatmaps, so gridlines are removed and the
# access-regime colour is shown on the panel border instead. This
# theme is specific to Figure 8 (unlike theme_ste() in
# plot_helpers.R, shared by the line-plot figures) because raster
# panels need this different styling.

#' Base theme for the spectrogram panels.
#'
#' @param base_family Base font family.
#' @param base_size Base font size, in points.
#' @return A ggplot2 theme object.
theme_spec <- function(base_family = "Aptos", base_size = 7) {
    theme_classic(base_family = base_family, base_size = base_size) +
        theme(
            plot.background = element_rect(fill = "white", colour = NA),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(colour = "#d62728", fill = NA, linewidth = 0.5),
            plot.title = element_text(vjust = 0.2, hjust = 0, face = "bold", size = 8.5),
            plot.subtitle = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 7, vjust = -0.5),
            axis.text.y = element_text(angle = 90, size = 7, vjust = 1.5, hjust = 0.5),
            axis.ticks = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "none"
        )
}

# ---------------------------------------------------------------------------
# Define panel scales
# ---------------------------------------------------------------------------

#' X/Y scales for one spectrogram panel.
#'
#' @param df Panel data frame with `time` (normalised 0-1 axis).
#' @return A list of ggplot2 scale/coord components.
theme_scales <- function(df) {
    xmin <- min(df$time, na.rm = TRUE)
    xmax <- max(df$time, na.rm = TRUE)
    xmid <- (xmin + xmax) / 2

    list(
        scale_x_continuous(
            breaks = c(xmin, xmid, xmax),
            labels = c("0", "0.5", "1"),
            expand = c(mult = 0.0045, 0)
        ),
        coord_cartesian(xlim = c(xmin, xmax)),
        scale_y_continuous(
            limits = c(45, 51),
            breaks = c(46, 48, 50),
            labels = c("46", "48", "50"),
            expand = c(0, 0)
        )
    )
}

# ---------------------------------------------------------------------------
# Create individual spectrogram panel
# ---------------------------------------------------------------------------

#' Build one sensor/access-regime spectrogram panel.
#'
#' @param sensor Monitoring-point code (e.g. `"ent"`).
#' @param period Access-regime number (1-5).
#' @param show_x,show_y Whether to draw x-/y-axis text (bottom row and
#'   first column only).
#' @return A ggplot object.
make_panel <- function(sensor, period, show_x = FALSE, show_y = FALSE) {
    panel_df <- all_sensors_df %>%
        filter(sensor == !!sensor, .data$period == !!period)

    p <- ggplot(panel_df, aes(x = time, y = frequency, fill = amplitude)) +
        geom_raster(interpolate = TRUE) +
        scale_fill_viridis_c(option = "turbo", direction = -1) +
        theme_spec() +
        theme_scales(panel_df) +
        theme(
            axis.text.x = if (show_x) {
                element_text(size = 7, vjust = -0.5)
            } else {
                element_blank()
            },

            axis.text.y = if (show_y) {
                element_text(angle = 90, size = 7, vjust = 1.5, hjust = 0.5)
            } else {
                element_blank()
            },

            # Green border for open-access regimes; red border for
            # restricted-access regimes.
            panel.border = element_rect(
                colour = regime_grid_colour(period),
                fill = NA,
                linewidth = 0.5
            )
        )

    if (period == 1) p <- p + labs(title = monitoring_point_labels[[sensor]])
    p
}

# ---------------------------------------------------------------------------
# Create the 30 panels
# ---------------------------------------------------------------------------

plot_list <- list()
sensors <- names(monitoring_point_labels)

for (i in seq_along(sensors)) {
    for (period in 1:5) {
        plot_list[[paste0("plot_", i, "_", period)]] <- make_panel(
            sensor = sensors[i],
            period = period,
            show_x = i == length(sensors), # X-axis labels on bottom row
            show_y = period == 1           # Y-axis labels on first column
        )
    }
}

# ---------------------------------------------------------------------------
# Assemble figure
# ---------------------------------------------------------------------------

final_spectrograms <- assemble_figure(
    plot_list,
    ncol = 5,
    y_label = "f  r  e  q  u  e  n  c  i  e  s,     c  p  d",
    x_label = "n  o  r  m  a  l  i  s  e  d      t  i  m  e"
)

# ---------------------------------------------------------------------------
# Save figure
# ---------------------------------------------------------------------------

ggsave(
    filename = output_file,
    plot = final_spectrograms,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600,
    compression = "lzw"
)
