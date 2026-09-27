#####

## FIGURE 6: RESIDUAL TEMPERATURE TIME SERIES
## POOLE'S CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 26 SEPTEMBER 2026

#####


# -------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(cowplot)
library(extrafont)
font_import()
loadfonts(device = "win")


# -------------------------------------------------------------------------
# Input and output
# -------------------------------------------------------------------------

input_file <- "data/full_day_night_time_series.rds"
output_file <- "figs/06_ts_plots.tif"
df <- readRDS(input_file)

# -------------------------------------------------------------------------
# Figure settings
# -------------------------------------------------------------------------

night_colour <- "#FF7F0E"
day_colour <- "#2962FF"

monitoring_points <- list(
    ent = list(
        label = "Monitoring point 1",
        limits = c(-2, 2),
        breaks = c(-2, 0, 2)
    ),

    ten = list(
        label = "Monitoring point 2",
        limits = c(-0.4, 0.4),
        breaks = c(-0.4, 0, 0.4)
    ),

    sin = list(
        label = "Monitoring point 3",
        limits = c(-0.2, 0.2),
        breaks = c(-0.2, 0, 0.2)
    ),

    brg = list(
        label = "Monitoring point 4",
        limits = c(-0.2, 0.2),
        breaks = c(-0.2, 0, 0.2)
    ),

    mqs = list(
        label = "Monitoring point 5",
        limits = c(-0.1, 0.1),
        breaks = c(-0.1, 0, 0.1)
    ),

    top = list(
        label = "Monitoring point 6",
        limits = c(-0.1, 0.1),
        breaks = c(-0.1, 0, 0.1)
    )
)

# -------------------------------------------------------------------------
# Theme
# -------------------------------------------------------------------------

theme_ste <- function(
        base_family = "Aptos",
        base_size = 7
) {

    theme_classic(
        base_family = base_family,
        base_size = base_size
    ) +

        theme(

            plot.background =
                element_rect(
                    fill = "white",
                    colour = NA
                ),

            panel.background =
                element_rect(
                    fill = "white",
                    colour = NA
                ),

            plot.title =
                element_text(
                    vjust = 0.2,
                    hjust = 0,
                    face = "bold",
                    size = 8.5
                ),

            plot.subtitle =
                element_blank(),

            panel.grid.major.x =
                element_blank(),

            panel.grid.major.y =
                element_line(
                    colour = "#d62728",
                    linewidth = 0.3,
                    linetype = "solid"
                ),

            panel.grid.minor.x =
                element_blank(),

            panel.grid.minor.y =
                element_line(
                    colour = "#333333",
                    linewidth = 0.15,
                    linetype = "dotted"
                ),

            axis.line =
                element_blank(),

            axis.title.x =
                element_blank(),

            axis.title.y =
                element_blank(),

            axis.text.x =
                element_text(
                    size = 7,
                    vjust = -0.5
                ),

            axis.text.y =
                element_text(
                    angle = 90,
                    size = 7,
                    vjust = 1.5,
                    hjust = 0.5
                ),

            axis.ticks =
                element_blank(),

            plot.margin =
                margin(
                    0,
                    0,
                    0,
                    0
                ),

            legend.position =
                "none"
        )
}


# -------------------------------------------------------------------------
# X-axis helper
# -------------------------------------------------------------------------

scale_x_auto <- function(
        x,
        add_days = 0.1,
        breaks = NULL,
        labels = NULL
) {

    if (!inherits(x, "POSIXct")) {

        stop(
            "x must be POSIXct"
        )
    }

    lim <- range(
        x,
        na.rm = TRUE
    )

    if (is.null(breaks)) {

        breaks <-
            lim[1] +
            diff(lim) *
            c(
                0,
                0.5,
                1
            )
    }

    scale_x_datetime(

        limits = lim,

        breaks = breaks,

        labels = labels,

        expand =
            expansion(
                add =
                    add_days *
                    24 *
                    60 *
                    60
            )
    )
}


# -------------------------------------------------------------------------
# Create individual panels
# -------------------------------------------------------------------------

make_panel <- function(
        point,
        period,
        show_y = FALSE,
        show_x = FALSE,
        highlight = FALSE
) {

    settings <-
        monitoring_points[[point]]

    full_data <-
        df[[paste0(
            "full_period_",
            period
        )]]

    night_data <-
        df[[paste0(
            "night_period_",
            period
        )]]

    day_data <-
        df[[paste0(
            "day_period_",
            period
        )]]

    residual_column <-
        paste0(
            point,
            "_residual"
        )


    ggplot() +

        # Night-time observations
        geom_line(
            data = night_data,
            aes(
                x = dttm,
                y = .data[[residual_column]]
            ),
            colour = night_colour,
            linewidth = 0.06
        ) +

        # Day-time observations
        geom_line(
            data = day_data,
            aes(
                x = dttm,
                y = .data[[residual_column]]
            ),
            colour = day_colour,
            linewidth = 0.06,
            alpha = 0.8
        ) +

        # Zero reference line
        geom_hline(
            yintercept = 0,
            colour = "#333333",
            linewidth = 0.1
        ) +

        # X axis
        scale_x_auto(
            full_data$dttm,
            labels = if (show_x) {

                c(
                    "0",
                    "0.5",
                    "1"
                )

            } else {

                NULL
            }
        ) +

        # Y axis
        scale_y_continuous(

            breaks =
                settings$breaks,

            labels = if (show_y) {

                settings$breaks

            } else {

                NULL
            },

            # Match the zero-expansion geometry used
            # in Figures 7 and 8.
            expand =
                c(
                    0,
                    0
                )
        ) +

        # Panel-specific y limits
        coord_cartesian(
            ylim =
                settings$limits
        ) +

        # Monitoring-point title appears
        # only in Access Regime 1.
        labs(
            title = if (period == 1) {

                settings$label

            } else {

                NULL
            }
        ) +

        theme_ste() +

        theme(

            # X-axis labels only on bottom row
            axis.text.x =
                if (show_x) {

                    element_text(
                        size = 7,
                        vjust = -0.5
                    )

                } else {

                    element_blank()
                },

            # Y-axis labels only in first column
            axis.text.y =
                if (show_y) {

                    element_text(
                        angle = 90,
                        size = 7,
                        vjust = 1.5,
                        hjust = 0.5
                    )

                } else {

                    element_blank()
                },

            # Green for open-access regimes;
            # red for restricted-access regimes.
            panel.grid.major.y =
                if (highlight) {

                    element_line(
                        colour = "green4",
                        linewidth = 0.3,
                        linetype = "solid"
                    )

                } else {

                    element_line(
                        colour = "#d62728",
                        linewidth = 0.3,
                        linetype = "solid"
                    )
                }
        )
}


# -------------------------------------------------------------------------
# Generate all panels
# -------------------------------------------------------------------------

points <-
    names(
        monitoring_points
    )

panels <- list()


for (i in seq_along(points)) {

    point <-
        points[i]

    for (period in 1:5) {

        panel_name <-
            paste0(
                "plot_",
                i,
                "_",
                period
            )

        panels[[panel_name]] <-
            make_panel(

                point = point,

                period = period,

                # Y-axis labels in first column
                show_y =
                    period == 1,

                # X-axis labels in bottom row
                show_x =
                    i == length(points),

                # Green horizontal grid lines in
                # access regimes 1, 3 and 5
                highlight =
                    period %in% c(
                        1,
                        3,
                        5
                    )
            )
    }
}


# -------------------------------------------------------------------------
# Assemble figure
# -------------------------------------------------------------------------

figure <-
    wrap_plots(
        panels,
        ncol = 5
    ) +

    plot_layout() &

    theme(

        panel.spacing =
            unit(
                0,
                "mm"
            ),

        plot.margin =
            margin(
                1,
                1.25,
                1.5,
                2,
                "mm"
            )
    )


# -------------------------------------------------------------------------
# Add figure-wide labels
# -------------------------------------------------------------------------

figure <-
    ggdraw(

        xlim =
            c(
                -0.01,
                1
            ),

        ylim =
            c(
                -0.01,
                1.015
            )
    ) +

    draw_plot(

        figure,

        x = 0,

        y = 0,

        width = 1,

        height = 1
    ) +

    draw_label(

        "r  e  s  i  d  u  a  l      t  e  m  p  e  r  a  t  u  r  e",

        x = 0,

        y = 0.5,

        angle = 90,

        size = 9,

        fontfamily = "Aptos",

        fontface = "plain"
    ) +

    draw_label(

        "n  o  r  m  a  l  i  s  e  d      t  i  m  e",

        x = 0.515,

        y = 0,

        size = 9,

        fontfamily = "Aptos",

        fontface = "plain"
    ) +

    draw_label(

        "Access regime 1",

        x = 0.129,

        y = 1.005,

        size = 11,

        fontfamily = "Aptos",

        fontface = "bold"
    ) +

    draw_label(

        "Access regime 2",

        x = 0.3205,

        y = 1.005,

        size = 11,

        fontfamily = "Aptos",

        fontface = "bold"
    ) +

    draw_label(

        "Access regime 3",

        x = 0.5145,

        y = 1.005,

        size = 11,

        fontfamily = "Aptos",

        fontface = "bold"
    ) +

    draw_label(

        "Access regime 4",

        x = 0.707,

        y = 1.005,

        size = 11,

        fontfamily = "Aptos",

        fontface = "bold"
    ) +

    draw_label(

        "Access regime 5",

        x = 0.8995,

        y = 1.005,

        size = 11,

        fontfamily = "Aptos",

        fontface = "bold"
    )


# -------------------------------------------------------------------------
# Export
# -------------------------------------------------------------------------

ggsave(
    filename = output_file,
    plot = figure,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600
)

#####
