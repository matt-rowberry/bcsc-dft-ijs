#####

## PLOTTING SEGMENT-LEVEL RECONSTRUCTED SIGNALS FOR FIGURE 10
## CODE WRITTEN BY MATT ROWBERRY
## LAST MODIFIED 27 SEPTEMBER 2026

#####

## 01. LOAD LIBRARIES

library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)
library(cowplot)

#####

## 02. LOAD PROCESSED DATA

figure_10 <- readRDS(
    "data/figure_10_segments.rds"
)

#####

## 03. SET THEME

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

theme_set(theme_ste())

#####

## 04. SELECT MONITORING POINTS

plot_ent <- figure_10 %>%
    filter(column == "ent_residual")

plot_ten <- figure_10 %>%
    filter(column == "ten_residual")

plot_sin <- figure_10 %>%
    filter(column == "sin_residual")

plot_brg <- figure_10 %>%
    filter(column == "brg_residual")

plot_mqs <- figure_10 %>%
    filter(column == "mqs_residual")

plot_top <- figure_10 %>%
    filter(column == "top_residual")

#####

## 05. CREATE PANEL FUNCTION

make_panel <- function(
        data,
        period_number,
        y_limit,
        y_breaks,
        y_labels,
        title = "",
        show_x_axis = FALSE
) {

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
        scale_x_continuous(
            expand = c(0, 0.05)
        ) +
        scale_y_continuous(
            breaks = y_breaks,
            labels = y_labels,
            expand = c(
                mult = 0.005,
                add = 0
            )
        ) +
        coord_cartesian(
            ylim = y_limit
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
            )
        )

    if (!show_x_axis) {
        p <- p +
            theme(
                axis.text.x = element_blank()
            )
    }

    p
}

#####

## 06. MONITORING POINT 1

plot_1_1 <- make_panel(
    plot_ent,
    period_number = 1,
    y_limit = c(-1, 1),
    y_breaks = c(-1, 0, 1),
    y_labels = c("-1.00", "0", "1.00"),
    title = "Monitoring point 1"
) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_1_3 <- make_panel(
    plot_ent,
    period_number = 3,
    y_limit = c(-1, 1),
    y_breaks = c(-1, 0, 1),
    y_labels = c("-1", "0", "1")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_1_5 <- make_panel(
    plot_ent,
    period_number = 5,
    y_limit = c(-1, 1),
    y_breaks = c(-1, 0, 1),
    y_labels = c("-1", "0", "1")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 07. MONITORING POINT 2

plot_2_1 <- make_panel(
    plot_ten,
    period_number = 1,
    y_limit = c(-0.1, 0.1),
    y_breaks = c(-0.1, 0, 0.1),
    y_labels = c("-0.10", "0", "0.10"),
    title = "Monitoring point 2"
) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_2_3 <- make_panel(
    plot_ten,
    period_number = 3,
    y_limit = c(-0.1, 0.1),
    y_breaks = c(-0.1, 0, 0.1),
    y_labels = c("-0.1", "0", "0.1")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_2_5 <- make_panel(
    plot_ten,
    period_number = 5,
    y_limit = c(-0.1, 0.1),
    y_breaks = c(-0.1, 0, 0.1),
    y_labels = c("-0.1", "0", "0.1")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 08. MONITORING POINT 3

plot_3_1 <- make_panel(
    plot_sin,
    period_number = 1,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    title = "Monitoring point 3"
) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_3_3 <- make_panel(
    plot_sin,
    period_number = 3,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_3_5 <- make_panel(
    plot_sin,
    period_number = 5,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 09. MONITORING POINT 4

plot_4_1 <- make_panel(
    plot_brg,
    period_number = 1,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    title = "Monitoring point 4"
) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_4_3 <- make_panel(
    plot_brg,
    period_number = 3,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_4_5 <- make_panel(
    plot_brg,
    period_number = 5,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 10. MONITORING POINT 5

plot_5_1 <- make_panel(
    plot_mqs,
    period_number = 1,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    title = "Monitoring point 5"
) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_5_3 <- make_panel(
    plot_mqs,
    period_number = 3,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_5_5 <- make_panel(
    plot_mqs,
    period_number = 5,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05")
) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 11. MONITORING POINT 6

plot_6_1 <- make_panel(
    plot_top,
    period_number = 1,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    title = "Monitoring point 6",
    show_x_axis = TRUE
) +
    scale_x_continuous(
        limits = c(
            min(plot_top$segment[
                plot_top$segment >= 1 &
                    plot_top$segment <= 311
            ]),
            max(plot_top$segment[
                plot_top$segment >= 1 &
                    plot_top$segment <= 311
            ])
        ),
        breaks = c(
            min(plot_top$segment[
                plot_top$segment >= 1 &
                    plot_top$segment <= 311
            ]),
            (
                min(plot_top$segment[
                    plot_top$segment >= 1 &
                        plot_top$segment <= 311
                ]) +
                    max(plot_top$segment[
                        plot_top$segment >= 1 &
                            plot_top$segment <= 311
                    ])
            ) / 2,
            max(plot_top$segment[
                plot_top$segment >= 1 &
                    plot_top$segment <= 311
            ])
        ),
        labels = c("0", "0.5", "1"),
        expand = c(0, 0.05)
    ) +
    theme(
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_6_3 <- make_panel(
    plot_top,
    period_number = 3,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    show_x_axis = TRUE
) +
    scale_x_continuous(
        limits = c(
            min(plot_top$segment[
                plot_top$segment >= 557 &
                    plot_top$segment <= 773
            ]),
            max(plot_top$segment[
                plot_top$segment >= 557 &
                    plot_top$segment <= 773
            ])
        ),
        breaks = c(
            min(plot_top$segment[
                plot_top$segment >= 557 &
                    plot_top$segment <= 773
            ]),
            (
                min(plot_top$segment[
                    plot_top$segment >= 557 &
                        plot_top$segment <= 773
                ]) +
                    max(plot_top$segment[
                        plot_top$segment >= 557 &
                            plot_top$segment <= 773
                    ])
            ) / 2,
            max(plot_top$segment[
                plot_top$segment >= 557 &
                    plot_top$segment <= 773
            ])
        ),
        labels = c("0", "0.5", "1"),
        expand = c(0, 0.05)
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

plot_6_5 <- make_panel(
    plot_top,
    period_number = 5,
    y_limit = c(-0.05, 0.05),
    y_breaks = c(-0.05, 0, 0.05),
    y_labels = c("-0.05", "0", "0.05"),
    show_x_axis = TRUE
) +
    scale_x_continuous(
        limits = c(
            min(plot_top$segment[
                plot_top$segment >= 1161 &
                    plot_top$segment <= 1463
            ]),
            max(plot_top$segment[
                plot_top$segment >= 1161 &
                    plot_top$segment <= 1463
            ])
        ),
        breaks = c(
            min(plot_top$segment[
                plot_top$segment >= 1161 &
                    plot_top$segment <= 1463
            ]),
            (
                min(plot_top$segment[
                    plot_top$segment >= 1161 &
                        plot_top$segment <= 1463
                ]) +
                    max(plot_top$segment[
                        plot_top$segment >= 1161 &
                            plot_top$segment <= 1463
                    ])
            ) / 2,
            max(plot_top$segment[
                plot_top$segment >= 1161 &
                    plot_top$segment <= 1463
            ])
        ),
        labels = c("0", "0.5", "1"),
        expand = c(0, 0.05)
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(
            colour = "green4",
            linewidth = 0.3,
            linetype = "solid"
        )
    )

#####

## 12. COMBINE PANELS

ifft_segment_plot_open <- wrap_plots(
    plot_1_1, plot_1_3, plot_1_5,
    plot_2_1, plot_2_3, plot_2_5,
    plot_3_1, plot_3_3, plot_3_5,
    plot_4_1, plot_4_3, plot_4_5,
    plot_5_1, plot_5_3, plot_5_5,
    plot_6_1, plot_6_3, plot_6_5,
    ncol = 3
) +
    plot_layout() &
    theme(
        panel.spacing = unit(0, "mm"),
        plot.margin = margin(
            1,
            1.25,
            1.5,
            2,
            "mm"
        )
    )

#####

## 13. ADD OUTER LABELS

ifft_segment_plot_open <-
    ggdraw(
        xlim = c(-0.01, 1),
        ylim = c(-0.01, 1.015)
    ) +
    draw_plot(
        ifft_segment_plot_open,
        x = 0,
        y = 0,
        width = 1,
        height = 1
    ) +
    draw_label(
        "r  e  c  o  n  s  t  r  u  c  t  e  d      s  i  g  n  a  l",
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
        x = 0.196,
        y = 1.005,
        size = 11,
        fontfamily = "Aptos",
        fontface = "bold"
    ) +
    draw_label(
        "Access regime 3",
        x = 0.516,
        y = 1.005,
        size = 11,
        fontfamily = "Aptos",
        fontface = "bold"
    ) +
    draw_label(
        "Access regime 5",
        x = 0.836,
        y = 1.005,
        size = 11,
        fontfamily = "Aptos",
        fontface = "bold"
    )

#####

## 14. SAVE FIGURE

ggsave(
    filename = "figs/10_segment_plot.tif",
    plot = ifft_segment_plot_open,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600
)
