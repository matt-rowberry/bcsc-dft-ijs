#####

## POOLE'S CAVERN — FIGURE 9
## RECONSTRUCTED SIGNAL
## 45.5–50.5 cpd
## PLOTTING ONLY

#####

## 01. LOAD LIBRARIES

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)

#####

## 02. LOAD PROCESSED DATA

reconstructed_full <- readRDS(
    "data/reconstructed_45_5_50_5_cpd.rds"
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

#####

## 04. PREPARE PLOTTING DATA

plot_data <- reconstructed_full %>%
    select(
        dttm,
        period,
        ent_residual_nb3,
        ten_residual_nb3,
        sin_residual_nb3,
        brg_residual_nb3,
        mqs_residual_nb3,
        top_residual_nb3
    ) %>%
    pivot_longer(
        cols = c(
            ent_residual_nb3,
            ten_residual_nb3,
            sin_residual_nb3,
            brg_residual_nb3,
            mqs_residual_nb3,
            top_residual_nb3
        ),
        names_to = "monitoring_point",
        values_to = "value"
    ) %>%
    mutate(
        monitoring_point = case_when(
            monitoring_point == "ent_residual_nb3" ~ "ent",
            monitoring_point == "ten_residual_nb3" ~ "ten",
            monitoring_point == "sin_residual_nb3" ~ "sin",
            monitoring_point == "brg_residual_nb3" ~ "brg",
            monitoring_point == "mqs_residual_nb3" ~ "mqs",
            monitoring_point == "top_residual_nb3" ~ "top"
        )
    ) %>%
    group_by(
        monitoring_point,
        period
    ) %>%
    mutate(
        t_num = as.numeric(dttm),
        time_norm = (
            t_num - min(t_num)
        ) / (
            max(t_num) - min(t_num)
        )
    ) %>%
    select(-t_num) %>%
    ungroup()

#####

## 05. FUNCTION FOR INDIVIDUAL PANELS

make_panel <- function(
        data,
        point,
        period_number,
        y_limit,
        y_breaks,
        y_labels,
        line_colour,
        grid_colour,
        title = "",
        show_y_axis = TRUE,
        show_x_axis = FALSE
) {

    p <- data %>%
        filter(
            monitoring_point == point,
            period == period_number
        )

    ggplot(
        p,
        aes(
            x = time_norm,
            y = value
        )
    ) +
        geom_line(
            colour = line_colour,
            linewidth = 0.05
        ) +
        scale_x_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.5, 1),
            labels = c("0", "0.5", "1"),
            expand = c(0, 0)
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
        geom_hline(
            yintercept = 0,
            colour = "#333333",
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
                colour = grid_colour,
                linewidth = 0.3,
                linetype = "solid"
            ),
            axis.text.x = if (show_x_axis) {
                element_text(
                    size = 7,
                    vjust = -0.5
                )
            } else {
                element_blank()
            },
            axis.text.y = if (show_y_axis) {
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
}

#####

## 06. MONITORING POINT 1 — ENTRANCE

plot_1_1 <- make_panel(
    plot_data,
    "ent",
    1,
    c(-1, 1),
    c(-1, 0, 1),
    c("-1.00", "0", "1.00"),
    "#2CA02C",
    "green4",
    "Monitoring point 1",
    TRUE,
    FALSE
)

plot_1_2 <- make_panel(
    plot_data,
    "ent",
    2,
    c(-1, 1),
    c(-1, 0, 1),
    c("-1", "0", "1"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_1_3 <- make_panel(
    plot_data,
    "ent",
    3,
    c(-1, 1),
    c(-1, 0, 1),
    c("-1", "0", "1"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

plot_1_4 <- make_panel(
    plot_data,
    "ent",
    4,
    c(-1, 1),
    c(-1, 0, 1),
    c("-1", "0", "1"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_1_5 <- make_panel(
    plot_data,
    "ent",
    5,
    c(-1, 1),
    c(-1, 0, 1),
    c("-1", "0", "1"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

#####

## 07. MONITORING POINT 2 — TEN

plot_2_1 <- make_panel(
    plot_data,
    "ten",
    1,
    c(-0.1, 0.1),
    c(-0.1, 0, 0.1),
    c("-0.10", "0", "0.10"),
    "#2CA02C",
    "green4",
    "Monitoring point 2",
    TRUE,
    FALSE
)

plot_2_2 <- make_panel(
    plot_data,
    "ten",
    2,
    c(-0.1, 0.1),
    c(-0.1, 0, 0.1),
    c("-0.1", "0", "0.1"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_2_3 <- make_panel(
    plot_data,
    "ten",
    3,
    c(-0.1, 0.1),
    c(-0.1, 0, 0.1),
    c("-0.1", "0", "0.1"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

plot_2_4 <- make_panel(
    plot_data,
    "ten",
    4,
    c(-0.1, 0.1),
    c(-0.1, 0, 0.1),
    c("-0.1", "0", "0.1"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_2_5 <- make_panel(
    plot_data,
    "ten",
    5,
    c(-0.1, 0.1),
    c(-0.1, 0, 0.1),
    c("-0.1", "0", "0.1"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

#####

## 08. MONITORING POINT 3 — SINK

plot_3_1 <- make_panel(
    plot_data,
    "sin",
    1,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "Monitoring point 3",
    TRUE,
    FALSE
)

plot_3_2 <- make_panel(
    plot_data,
    "sin",
    2,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_3_3 <- make_panel(
    plot_data,
    "sin",
    3,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

plot_3_4 <- make_panel(
    plot_data,
    "sin",
    4,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_3_5 <- make_panel(
    plot_data,
    "sin",
    5,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

#####

## 09. MONITORING POINT 4 — BRIDGE

plot_4_1 <- make_panel(
    plot_data,
    "brg",
    1,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "Monitoring point 4",
    TRUE,
    FALSE
)

plot_4_2 <- make_panel(
    plot_data,
    "brg",
    2,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_4_3 <- make_panel(
    plot_data,
    "brg",
    3,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

plot_4_4 <- make_panel(
    plot_data,
    "brg",
    4,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_4_5 <- make_panel(
    plot_data,
    "brg",
    5,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

#####

## 10. MONITORING POINT 5 — MARY QUEEN OF SCOTS

plot_5_1 <- make_panel(
    plot_data,
    "mqs",
    1,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "Monitoring point 5",
    TRUE,
    FALSE
)

plot_5_2 <- make_panel(
    plot_data,
    "mqs",
    2,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_5_3 <- make_panel(
    plot_data,
    "mqs",
    3,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

plot_5_4 <- make_panel(
    plot_data,
    "mqs",
    4,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    FALSE
)

plot_5_5 <- make_panel(
    plot_data,
    "mqs",
    5,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    FALSE
)

#####

## 11. MONITORING POINT 6 — TOP

plot_6_1 <- make_panel(
    plot_data,
    "top",
    1,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "Monitoring point 6",
    TRUE,
    TRUE
)

plot_6_2 <- make_panel(
    plot_data,
    "top",
    2,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    TRUE
)

plot_6_3 <- make_panel(
    plot_data,
    "top",
    3,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    TRUE
)

plot_6_4 <- make_panel(
    plot_data,
    "top",
    4,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#B22222",
    "#d62728",
    "",
    FALSE,
    TRUE
)

plot_6_5 <- make_panel(
    plot_data,
    "top",
    5,
    c(-0.05, 0.05),
    c(-0.05, 0, 0.05),
    c("-0.05", "0", "0.05"),
    "#2CA02C",
    "green4",
    "",
    FALSE,
    TRUE
)

#####

## 12. ASSEMBLE FIGURE

ifft_panels <- wrap_plots(
    plot_1_1, plot_1_2, plot_1_3, plot_1_4, plot_1_5,
    plot_2_1, plot_2_2, plot_2_3, plot_2_4, plot_2_5,
    plot_3_1, plot_3_2, plot_3_3, plot_3_4, plot_3_5,
    plot_4_1, plot_4_2, plot_4_3, plot_4_4, plot_4_5,
    plot_5_1, plot_5_2, plot_5_3, plot_5_4, plot_5_5,
    plot_6_1, plot_6_2, plot_6_3, plot_6_4, plot_6_5,
    ncol = 5
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

## 13. ADD FIGURE-WIDE LABELS

ifft_plot <-
    ggdraw(
        xlim = c(-0.01, 1),
        ylim = c(-0.01, 1.015)
    ) +

    draw_plot(
        ifft_panels,
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

## 14. SAVE FIGURE

ggsave(
    filename = "figs/09_ifft_plots.tif",
    plot = ifft_plot,
    width = 174,
    height = 200.1,
    units = "mm",
    dpi = 600
)

#####
