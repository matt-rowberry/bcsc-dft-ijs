# Figure 11: RMS amplitude and variance explained
#
# Read processed data and create Figure 11.

library(dplyr)
library(ggplot2)
library(patchwork)

source("plot_helpers.R")

load_plot_fonts()

# -------------------------------------------------------------------------
# DATA
# -------------------------------------------------------------------------

figure_11 <- readRDS(
    "data/figure_11_heatmaps.rds"
)

rms_summary <- figure_11$rms

variance_explained <- figure_11$variance_explained

# -------------------------------------------------------------------------
# MONITORING POINT LABELS
# -------------------------------------------------------------------------

# Listed in this order so that monitoring point 1 appears at the
# top of each heatmap.

mp_levels <- c(
    "top",
    "mqs",
    "brg",
    "sin",
    "ten",
    "ent"
)

mp_labels <- c(
    "MP6",
    "MP5",
    "MP4",
    "MP3",
    "MP2",
    "MP1"
)

# -------------------------------------------------------------------------
# HELPER
# -------------------------------------------------------------------------

format_mp <- function(df) {

    df %>%
        mutate(
            monitoring_point = factor(
                monitoring_point,
                levels = mp_levels,
                labels = mp_labels
            ),
            period = factor(period),
            daynight = factor(
                daynight,
                levels = c("day", "night"),
                labels = c("Day", "Night")
            )
        )
}

# -------------------------------------------------------------------------
# THEME
# -------------------------------------------------------------------------

theme_ste_heatmap <- function(
        base_family = "Aptos",
        base_size = 8
) {

    theme_classic(
        base_family = base_family,
        base_size = base_size
    ) +
        theme(
            panel.background = element_rect(
                fill = "white",
                colour = NA
            ),
            panel.border = element_rect(
                colour = "red",
                fill = NA,
                linewidth = 0.6
            ),
            plot.background = element_rect(
                fill = "white",
                colour = NA
            ),
            panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_text(
                size = 9,
                margin = margin(t = 5)
            ),
            axis.title.y = element_text(
                size = 9,
                margin = margin(r = 12)
            ),
            axis.text.x = element_text(
                size = 8,
                margin = margin(t = 3)
            ),
            axis.text.y = element_text(
                size = 8,
                margin = margin(r = 3)
            ),
            strip.background = element_blank(),
            strip.text = element_text(size = 9),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 9),
            plot.margin = margin(
                0,
                0,
                0.25,
                0.25,
                "mm"
            )
        )
}

# The heatmaps do not add this theme themselves; it is applied to
# every plot in the session.

theme_set(theme_ste_heatmap())

# -------------------------------------------------------------------------
# FORMAT DATA
# -------------------------------------------------------------------------

rms_plot <- format_mp(
    rms_summary
)

variance_explained_plot <- format_mp(
    variance_explained
) %>%
    mutate(
        condition = paste0(
            "P",
            period,
            " ",
            daynight
        )
    )

# -------------------------------------------------------------------------
# RMS HEATMAP
# -------------------------------------------------------------------------

rms_heatmap <- ggplot(
    rms_plot,
    aes(
        x = period,
        y = monitoring_point,
        fill = rms
    )
) +
    geom_tile(
        colour = "white",
        linewidth = 0.4
    ) +
    facet_wrap(
        ~daynight,
        nrow = 1
    ) +
    scale_x_discrete(
        expand = c(0, 0)
    ) +
    scale_y_discrete(
        expand = c(0, 0)
    ) +
    scale_fill_viridis_c(
        option = "inferno",
        trans = "log10",
        limits = c(0.0003, 0.3),
        name = "RMS\namplitude",
        na.value = "#999999",
        guide = guide_colourbar(
            ticks.colour = NA,
            frame.colour = "black",
            frame.linewidth = 0.25
        )
    ) +
    labs(
        x = "A    c    c    e    s    s          r    e    g    i    m    e",
        y = "M o n i t o r i n g     p o i n t"
    ) +
    theme(
        panel.spacing.x = unit(4, "mm"),
        panel.spacing.y = unit(0, "mm")
    )

# -------------------------------------------------------------------------
# VARIANCE HEATMAP
# -------------------------------------------------------------------------

variance_heatmap <-
    ggplot(
        variance_explained_plot,
        aes(
            x = condition,
            y = monitoring_point,
            fill = variance_explained
        )
    ) +
    geom_tile(
        colour = "white",
        linewidth = 0.4
    ) +
    scale_x_discrete(
        expand = c(0, 0)
    ) +
    scale_y_discrete(
        expand = c(0, 0)
    ) +
    scale_fill_viridis_c(
        option = "inferno",
        name = "Variance\nexplained %",
        na.value = "#999999",
        guide = guide_colourbar(
            ticks.colour = NA,
            frame.colour = "black",
            frame.linewidth = 0.25
        )
    ) +
    labs(
        x = "A    c    c    e    s    s          r    e    g    i    m    e          a    n    d          d    i    e    l          p    h    a    s    e",
        y = "M o n i t o r i n g     p o i n t"
    ) +
    theme(
        axis.text.x = element_text(
            angle = 45,
            hjust = 1
        )
    )

# -------------------------------------------------------------------------
# COMBINED PLOT
# -------------------------------------------------------------------------

final_plot_amplitude <-
    wrap_plots(
        rms_heatmap,
        plot_spacer(),
        variance_heatmap,
        ncol = 1,
        heights = c(1, 0.08, 1)
    )

# -------------------------------------------------------------------------
# SAVE
# -------------------------------------------------------------------------

ggsave(
    filename = "figs/11_variance.tif",
    plot = final_plot_amplitude,
    width = 174,
    height = 116,
    units = "mm",
    dpi = 600
)
