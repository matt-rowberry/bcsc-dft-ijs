#####

## FFT PLOT FOR FIGURE 7
## TEMPERATURE DATA FROM POOLES CAVERN
## PLOTTING ONLY
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 29 SEPTEMBER 2026

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

input_file <- "data/figure_07_bandpass.rds"
output_file <- "figs/07_fft_plots.tif"
df <- readRDS(input_file)

# ---------------------------------------------------------------------------
# Create panel function
# ---------------------------------------------------------------------------

#' Build one monitoring-point/access-regime lollipop panel.
#'
#' Draws day- and night-time amplitude spectra as `geom_segment()`
#' "lollipops" for `point` within access regime `period`.
#'
#' @param point Monitoring-point code (e.g. `"ent"`).
#' @param period Access-regime number (1-5).
#' @param show_y,show_x Whether to draw y-/x-axis text (first column
#'   and bottom row only).
#' @return A ggplot object.
make_panel <- function(point, period, show_y = FALSE, show_x = FALSE) {
    day_data <- df[[paste0("df", period)]]$day
    night_data <- df[[paste0("df", period)]]$night
    grid_colour <- regime_grid_colour(period)

    y_labels <- if (show_y) c("0", "1") else c("", "")

    ggplot(day_data, aes(x = freqs, y = .data[[point]])) +

        geom_segment(
            aes(xend = freqs, y = 0, yend = .data[[point]]),
            colour = "#2962FF",
            linewidth = 0.2
        ) +

        geom_segment(
            data = night_data,
            aes(xend = freqs, y = 0, yend = .data[[point]]),
            colour = "#FF7F0E",
            linewidth = 0.2
        ) +

        geom_hline(yintercept = 0, colour = grid_colour, linewidth = 0.25) +

        scale_x_continuous(
            limits = c(42, 54),
            breaks = c(42, 48, 54),
            labels = c("42", "48", "54"),
            expand = c(0, 0.01)
        ) +

        scale_y_continuous(
            limits = c(0, 1),
            breaks = c(0, 1),
            labels = y_labels,
            expand = c(mult = 0.001, add = 0)
        ) +

        labs(
            title = if (period == 1) monitoring_point_labels[[point]] else "",
            subtitle = "",
            x = "",
            y = ""
        ) +

        theme_ste() +

        theme(
            panel.background = element_rect(fill = "white", colour = NA),

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

            panel.grid.major.y = element_line(
                colour = grid_colour,
                linewidth = 0.3,
                linetype = "solid"
            )
        )
}

# ---------------------------------------------------------------------------
# Create the 30 panels
# ---------------------------------------------------------------------------

plots <- list()
points <- names(monitoring_point_labels)

for (i in seq_along(points)) {
    for (period in 1:5) {
        plots[[paste0("plot_", i, "_", period)]] <- make_panel(
            point = points[i],
            period = period,
            show_y = period == 1,
            show_x = i == length(points)
        )
    }
}

# ---------------------------------------------------------------------------
# Assemble figure
# ---------------------------------------------------------------------------

final_plot_amplitude <- assemble_figure(
    plots,
    ncol = 5,
    y_label = "n  o  r  m  a  l  i  s  e  d     a  m  p  l  i  t  u  d  e",
    x_label = "f  r  e  q  u  e  n  c  i  e  s,     c  p  d"
)

# ---------------------------------------------------------------------------
# Save figure
# ---------------------------------------------------------------------------

ggsave(
    filename = output_file,
    plot = final_plot_amplitude,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600,
    compression = "lzw"
)
