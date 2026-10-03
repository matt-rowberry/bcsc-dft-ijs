#####

## FIGURE 6: RESIDUAL TEMPERATURE TIME SERIES
## POOLE'S CAVERN PLOTTING ONLY
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 3 OCTOBER 2026

#####

# ---------------------------------------------------------------------------
# Packages and helpers
# ---------------------------------------------------------------------------

library(ggplot2)
source("plot_helpers.R")
load_plot_fonts()

# ---------------------------------------------------------------------------
# Input and output
# ---------------------------------------------------------------------------

input_file <- "data/full_day_night_time_series.rds"
output_file <- "figs/06_ts_plots.tif"
df <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Figure settings
# ---------------------------------------------------------------------------

night_colour <- "#FF7F0E"
day_colour <- "#2962FF"

# Y-axis limits and breaks for each monitoring point (chosen by eye to
# suit the size of the residuals at each point).

monitoring_points <- list(
    ent = list(limits = c(-2, 2), breaks = c(-2, 0, 2)),
    ten = list(limits = c(-0.4, 0.4), breaks = c(-0.4, 0, 0.4)),
    sin = list(limits = c(-0.2, 0.2), breaks = c(-0.2, 0, 0.2)),
    brg = list(limits = c(-0.2, 0.2), breaks = c(-0.2, 0, 0.2)),
    mqs = list(limits = c(-0.1, 0.1), breaks = c(-0.1, 0, 0.1)),
    top = list(limits = c(-0.1, 0.1), breaks = c(-0.1, 0, 0.1))
)

# ---------------------------------------------------------------------------
# X-axis helper
# ---------------------------------------------------------------------------

#' Datetime x-scale spanning the full range of `x`.
#'
#' @param x POSIXct vector; its range sets the scale limits.
#' @param add_days Padding added to each end, in days.
#' @param breaks Break positions; defaults to the start, midpoint and
#'   end of `x`'s range.
#' @param labels Break labels; `NULL` hides axis text.
#' @return A `ggplot2::scale_x_datetime()` object.
scale_x_auto <- function(x, add_days = 0.1, breaks = NULL, labels = NULL) {
    if (!inherits(x, "POSIXct")) stop("x must be POSIXct")

    lim <- range(x, na.rm = TRUE)
    if (is.null(breaks)) breaks <- lim[1] + diff(lim) * c(0, 0.5, 1)

    scale_x_datetime(
        limits = lim,
        breaks = breaks,
        labels = labels,
        expand = expansion(add = add_days * 24 * 60 * 60)
    )
}

# ---------------------------------------------------------------------------
# Create individual panels
# ---------------------------------------------------------------------------

#' Build one monitoring-point/access-regime panel.
#'
#' Draws the night-time series, day-time series and a zero reference
#' line for `point` within access regime `period`.
#'
#' @param point Monitoring-point code (e.g. `"ent"`).
#' @param period Access-regime number (1-5).
#' @param show_y,show_x Whether to draw y-/x-axis text (first column
#'   and bottom row only).
#' @return A ggplot object.
make_panel <- function(point, period, show_y = FALSE, show_x = FALSE) {
    settings <- monitoring_points[[point]]
    full_data <- df[[paste0("full_period_", period)]]
    night_data <- df[[paste0("night_period_", period)]]
    day_data <- df[[paste0("day_period_", period)]]
    residual_column <- paste0(point, "_residual")

    ggplot() +

        # Night-time observations
        geom_line(
            data = night_data,
            aes(x = dttm, y = .data[[residual_column]]),
            colour = night_colour,
            linewidth = 0.06
        ) +

        # Day-time observations
        geom_line(
            data = day_data,
            aes(x = dttm, y = .data[[residual_column]]),
            colour = day_colour,
            linewidth = 0.06,
            alpha = 0.8
        ) +

        # Zero reference line
        geom_hline(yintercept = 0, colour = "#333333", linewidth = 0.1) +

        # X axis
        scale_x_auto(
            full_data$dttm,
            labels = if (show_x) c("0", "0.5", "1") else NULL
        ) +

        # Y axis
        scale_y_continuous(
            breaks = settings$breaks,
            labels = if (show_y) settings$breaks else NULL,
            # Match the zero-expansion geometry used in Figure 8.
            # (Figure 7 uses a small multiplicative expansion,
            # mult = 0.001; Figures 9 and 10 use mult = 0.005.)
            expand = c(0, 0)
        ) +

        # Panel-specific y limits
        coord_cartesian(ylim = settings$limits) +

        # Monitoring-point title appears only in Access Regime 1.
        labs(title = if (period == 1) monitoring_point_labels[[point]] else NULL) +

        theme_ste() +

        theme(
            # X-axis labels only on bottom row
            axis.text.x = if (show_x) {
                element_text(size = 7, vjust = -0.5)
            } else {
                element_blank()
            },

            # Y-axis labels only in first column
            axis.text.y = if (show_y) {
                element_text(angle = 90, size = 7, vjust = 1.5, hjust = 0.5)
            } else {
                element_blank()
            },

            # Green for open-access regimes; red for restricted-access regimes.
            panel.grid.major.y = element_line(
                colour = regime_grid_colour(period),
                linewidth = 0.3,
                linetype = "solid"
            )
        )
}

# ---------------------------------------------------------------------------
# Generate all panels
# ---------------------------------------------------------------------------

points <- names(monitoring_points)
panels <- list()

for (i in seq_along(points)) {
    for (period in 1:5) {
        panel_name <- paste0("plot_", i, "_", period)
        panels[[panel_name]] <- make_panel(
            point = points[i],
            period = period,
            show_y = period == 1,        # Y-axis labels in first column
            show_x = i == length(points) # X-axis labels in bottom row
        )
    }
}

# ---------------------------------------------------------------------------
# Assemble figure
# ---------------------------------------------------------------------------

figure <- assemble_figure(
    panels,
    ncol = 5,
    y_label = "r  e  s  i  d  u  a  l      t  e  m  p  e  r  a  t  u  r  e",
    x_label = "n  o  r  m  a  l  i  s  e  d      t  i  m  e"
)

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

ggsave(
    filename = output_file,
    plot = figure,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600,
    compression = "lzw"
)
