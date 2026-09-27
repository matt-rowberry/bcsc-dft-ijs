#####

## PLOTTING SPECTROGRAMS FOR POOLE'S CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 26 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(ggplot2)
library(viridis)
library(extrafont)
library(patchwork)
library(ggtext)
library(cowplot)

#####

options(digits = 16)
Sys.setlocale("LC_TIME", "English_United Kingdom")

#####

## 02. LOAD PROCESSED DATA

all_sensors_df <- readRDS(
    "data/figure_08_spectrograms.rds"
)

#####

## 03. DEFINE MONITORING-POINT LABELS

monitoring_points <- c(
    ent = "Monitoring point 1",
    ten = "Monitoring point 2",
    sin = "Monitoring point 3",
    brg = "Monitoring point 4",
    mqs = "Monitoring point 5",
    top = "Monitoring point 6"
)

#####

## 04. DEFINE PLOT THEME

theme_spec <- function(
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

            panel.border =
                element_rect(
                    colour = "#d62728",
                    fill = NA,
                    linewidth = 0.5
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
                element_blank(),

            panel.grid.minor.x =
                element_blank(),

            panel.grid.minor.y =
                element_blank(),

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

#####

## 05. DEFINE PANEL SCALES

theme_scales <- function(df) {

    xmin <- min(
        df$time,
        na.rm = TRUE
    )

    xmax <- max(
        df$time,
        na.rm = TRUE
    )

    xmid <- (
        xmin + xmax
    ) / 2

    list(

        scale_x_continuous(

            breaks = c(
                xmin,
                xmid,
                xmax
            ),

            labels = c(
                "0",
                "0.5",
                "1"
            ),

            expand = c(
                mult = 0.0045,
                0
            )
        ),

        coord_cartesian(
            xlim = c(
                xmin,
                xmax
            )
        ),

        scale_y_continuous(

            limits = c(
                45,
                51
            ),

            breaks = c(
                46,
                48,
                50
            ),

            labels = c(
                "46",
                "48",
                "50"
            ),

            expand = c(
                0,
                0
            )
        )
    )
}

#####

## 06. CREATE INDIVIDUAL SPECTROGRAM PANEL

make_panel <- function(
        sensor,
        period,
        show_x = FALSE,
        show_y = FALSE
) {

    panel_df <- all_sensors_df %>%
        filter(
            sensor == !!sensor,
            .data$period == !!period
        )

    p <- ggplot(
        panel_df,
        aes(
            x = time,
            y = frequency,
            fill = amplitude
        )
    ) +

        geom_raster(
            interpolate = TRUE
        ) +

        scale_fill_viridis_c(
            option = "turbo",
            direction = -1
        ) +

        theme_spec() +

        theme_scales(panel_df) +

        theme(

            axis.text.x =
                if (show_x) {

                    element_text(
                        size = 7,
                        vjust = -0.5
                    )

                } else {

                    element_blank()
                },

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
                }
        )

    if (period == 1) {

        p <- p +
            labs(
                title = monitoring_points[sensor]
            )
    }

    # Access regimes 1, 3 and 5 use the green border.
    # Access regimes 2 and 4 use the red border.

    border_colour <- if (
        period %in% c(1, 3, 5)
    ) {
        "green4"
    } else {
        "#d62728"
    }

    p <- p +
        theme(
            panel.border =
                element_rect(
                    colour = border_colour,
                    fill = NA,
                    linewidth = 0.5
                )
        )

    p
}

#####

## 07. CREATE THE 30 PANELS

plot_list <- list()

for (sensor in names(monitoring_points)) {

    for (period in 1:5) {

        # Y-axis labels on Access Regime 1 (first column)
        show_y <- period == 1

        # X-axis labels on Monitoring Point 6 (bottom row)
        show_x <- sensor == "top"

        panel_name <- paste0(
            "plot_",
            match(
                sensor,
                names(monitoring_points)
            ),
            "_",
            period
        )

        plot_list[[panel_name]] <- make_panel(
            sensor = sensor,
            period = period,
            show_x = show_x,
            show_y = show_y
        )
    }
}

#####

## 08. ARRANGE THE 30 PANELS

final_spectrograms <- wrap_plots(
    plot_list,
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

#####

## 09. ADD FIGURE-WIDE LABELS

final_spectrograms <-

    ggdraw(
        xlim = c(
            -0.01,
            1
        ),

        ylim = c(
            -0.01,
            1.015
        )
    ) +

    draw_plot(
        final_spectrograms,
        x = 0,
        y = 0,
        width = 1,
        height = 1
    ) +

    draw_label(
        "f  r  e  q  u  e  n  c  i  e  s,     c  p  d",
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

#####

## 10. SAVE FIGURE

ggsave(
    "figs/08_spectrograms.tif",
    final_spectrograms,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600,
    compression = "lzw"
)

#####
