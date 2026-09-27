#####

## FFT PLOT FOR FIGURE 7
## TEMPERATURE DATA FROM POOLES CAVERN
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 26 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(ggplot2)
library(purrr)
library(patchwork)
library(cowplot)

#####

## 02. SET OPTIONS

Sys.setlocale(
    "LC_TIME",
    "English_United Kingdom"
)

options(digits = 16)

#####

## 03. LOAD PROCESSED DATA

df <- readRDS(
    "data/figure_07_bandpass.rds"
)

#####

## 04. SET THEME

theme_ste <- function(
        base_family = "Aptos",
        base_size = 7
) {

    theme_classic(
        base_family = base_family,
        base_size = base_size
    ) +
        theme(
            plot.background = element_rect(
                fill = "white",
                colour = NA
            ),
            panel.background = element_rect(
                fill = "white",
                colour = NA
            ),
            plot.title = element_text(
                vjust = 0.2,
                hjust = 0,
                face = "bold",
                size = 8.5
            ),
            plot.subtitle = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(
                colour = "#d62728",
                linewidth = 0.3,
                linetype = "solid"
            ),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(
                colour = "#333333",
                linewidth = 0.15,
                linetype = "dotted"
            ),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(
                size = 7,
                vjust = -0.5
            ),
            axis.text.y = element_text(
                angle = 90,
                size = 7,
                vjust = 1.5,
                hjust = 0.5
            ),
            axis.ticks = element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "none"
        )
}

#####

## 05. SET MONITORING POINTS

monitoring_points <- c(
    ent = "Monitoring point 1",
    ten = "Monitoring point 2",
    sin = "Monitoring point 3",
    brg = "Monitoring point 4",
    mqs = "Monitoring point 5",
    top = "Monitoring point 6"
)

#####

## 06. CREATE PANEL FUNCTION

make_panel <- function(
        point,
        period,
        show_y = FALSE,
        show_x = FALSE
) {

    day_data <- df[[paste0("df", period)]]$day

    night_data <- df[[paste0("df", period)]]$night

    green_grid <- period %in% c(
        1,
        3,
        5
    )

    grid_colour <- if (green_grid) {
        "green4"
    } else {
        "#d62728"
    }

    y_labels <- if (show_y) {
        c("0", "1")
    } else {
        c("", "")
    }

    p <- ggplot(
        day_data,
        aes(
            x = freqs,
            y = .data[[point]]
        )
    ) +

        geom_segment(
            aes(
                xend = freqs,
                y = 0,
                yend = .data[[point]]
            ),
            colour = "#2962FF",
            linewidth = 0.2
        ) +

        geom_segment(
            data = night_data,
            aes(
                xend = freqs,
                y = 0,
                yend = .data[[point]]
            ),
            colour = "#FF7F0E",
            linewidth = 0.2
        ) +

        geom_hline(
            yintercept = 0,
            colour = grid_colour,
            linewidth = 0.25
        ) +

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
            expand = c(
                mult = 0.001,
                add = 0
            )
        ) +

        labs(
            title = if (period == 1) {
                monitoring_points[[point]]
            } else {
                ""
            },
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

            axis.text.x = if (show_x) {
                element_text(
                    size = 7,
                    vjust = -0.5
                )
            } else {
                element_blank()
            },

            axis.text.y = if (show_y) {
                element_text(
                    angle = 90,
                    size = 7,
                    vjust = 1.5,
                    hjust = 0.5
                )
            } else {
                element_blank()
            },

            panel.grid.major.y = element_line(
                colour = grid_colour,
                linewidth = 0.3,
                linetype = "solid"
            )
        )

    p
}

#####

## 07. CREATE THE 30 PANELS

plots <- list()

points <- names(monitoring_points)

for (i in seq_along(points)) {

    point <- points[i]

    for (period in 1:5) {

        plots[[paste0(
            "plot_",
            i,
            "_",
            period
        )]] <- make_panel(
            point = point,
            period = period,
            show_y = period == 1,
            show_x = i == length(points)
        )
    }
}

#####

## 08. COMBINE PANELS

final_plot_amplitude <- wrap_plots(
    plots,
    ncol = 5
) +
    plot_layout() &

    theme(
        panel.spacing = unit(
            0,
            "mm"
        ),
        plot.margin = margin(
            1,
            1.25,
            1.5,
            2,
            "mm"
        )
    )

#####

## 09. ADD FIGURE-WIDE LABELS

final_plot_amplitude <-

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
        final_plot_amplitude,
        x = 0,
        y = 0,
        width = 1,
        height = 1
    ) +

    draw_label(
        "n  o  r  m  a  l  i  s  e  d     a  m  p  l  i  t  u  d  e",
        x = 0,
        y = 0.5,
        angle = 90,
        size = 9,
        fontfamily = "Aptos",
        fontface = "plain"
    ) +

    draw_label(
        "f  r  e  q  u  e  n  c  i  e  s,     c  p  d",
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
    filename = "figs/07_fft_plots.tif",
    plot = final_plot_amplitude,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600
)

#####
