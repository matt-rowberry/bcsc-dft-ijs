#####

## PLOTTING SEGMENT-LEVEL RECONSTRUCTED SIGNALS FOR FIGURE 10
## PLOTTING ONLY
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 28 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES AND HELPERS

library(dplyr)
library(ggplot2)

source("plot_helpers.R")

load_plot_fonts()

#####

## 02. LOAD PROCESSED DATA

figure_10 <- readRDS(
    "data/figure_10_segments.rds"
)

#####

## 03. X-AXIS HELPER

# In the bottom row of panels, the x-axis is labelled 0, 0.5 and 1
# across the range of segments in the access regime. The range is
# taken from the data for that regime.

segment_axis_scale <- function(segments) {

    rng <- range(segments)

    scale_x_continuous(
        limits = rng,
        breaks = c(
            rng[1],
            (rng[1] + rng[2]) / 2,
            rng[2]
        ),
        labels = c("0", "0.5", "1"),
        expand = c(0, 0.05)
    )
}

#####

## 04. CREATE PANEL FUNCTION

# The y-axis settings for each monitoring point come from
# reconstruction_axes (plot_helpers.R), as in Figure 9.

make_panel <- function(
        data,
        period_number,
        axes,
        title = "",
        show_x_axis = FALSE,
        show_y_axis = FALSE,
        x_scale = NULL
) {

    if (is.null(x_scale)) {
        x_scale <- scale_x_continuous(
            expand = c(0, 0.05)
        )
    }

    p <- ggplot(
        data %>% filter(period == period_number),
        aes(
            x = segment,
            ymin = ymin,
            ymax = ymax,
            colour = band_type
        )
    ) +
        geom_linerange(
            linewidth = 0.24
        ) +
        x_scale +
        scale_y_continuous(
            breaks = axes$breaks,
            labels = axes$labels,
            expand = c(
                mult = 0.005,
                add = 0
            )
        ) +
        coord_cartesian(
            ylim = axes$limit
        ) +
        scale_colour_manual(
            values = c(
                nb3_day = "#2962FF",
                nb3_night = "#FF7F0E"
            )
        ) +
        geom_hline(
            yintercept = 0,
            color = "#333333",
            linewidth = 0.1
        ) +
        labs(
            title = title,
            subtitle = "",
            x = "",
            y = ""
        ) +
        theme_ste() +
        theme(
            panel.background = element_rect(
                fill = "white",
                colour = NA
            ),
            panel.grid.major.y = element_line(
                colour = regime_grid_colour(period_number),
                linewidth = 0.3,
                linetype = "solid"
            )
        )

    if (!show_x_axis) {
        p <- p +
            theme(
                axis.text.x = element_blank()
            )
    }

    if (!show_y_axis) {
        p <- p +
            theme(
                axis.text.y = element_blank()
            )
    }

    p
}

#####

## 05. CREATE THE 18 PANELS

# Only the open-access regimes (1, 3 and 5) are shown.

periods <- c(1, 3, 5)

points <- names(reconstruction_axes)

panels <- list()

for (i in seq_along(points)) {

    point <- points[i]

    point_data <- figure_10 %>%
        filter(
            column == paste0(point, "_residual")
        )

    for (period in periods) {

        # The x-axis is labelled in the bottom row only.

        x_scale <- if (i == length(points)) {
            segment_axis_scale(
                point_data$segment[point_data$period == period]
            )
        } else {
            NULL
        }

        panels[[paste0(
            "plot_",
            i,
            "_",
            period
        )]] <- make_panel(
            point_data,
            period_number = period,
            axes = reconstruction_axes[[point]],
            title = if (period == 1) {
                monitoring_point_labels[[point]]
            } else {
                ""
            },
            show_x_axis = i == length(points),
            show_y_axis = period == 1,
            x_scale = x_scale
        )
    }
}

#####

## 06. ASSEMBLE FIGURE

# Three columns of panels, so the "Access regime" headings are
# centred differently from the five-column figures.

ifft_segment_plot_open <- assemble_figure(

    panels,

    ncol = 3,

    y_label = "r  e  c  o  n  s  t  r  u  c  t  e  d      s  i  g  n  a  l",

    x_label = "n  o  r  m  a  l  i  s  e  d      t  i  m  e",

    regime_labels = paste("Access regime", periods),

    regime_x = c(0.196, 0.516, 0.836)
)

#####

## 07. SAVE FIGURE

ggsave(
    filename = "figs/10_segment_plot.tif",
    plot = ifft_segment_plot_open,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600
)
