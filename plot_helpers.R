#####

## PLOTTING HELPERS
## Poole's Cavern temperature data
## LAST MODIFIED 29 SEPTEMBER 2026

#####

# Shared by the figure-plotting scripts 06_ts_plot.R to 11_variance_plot.R.
# Each script loads this file with source("plot_helpers.R"). Values are
# unchanged from the originals; only comments and formatting have been
# made consistent with the rest of the repository.
#
# This file assumes ggplot2, patchwork and cowplot are already loaded by
# the calling script (functions here call them with `::`, except
# theme_ste(), which builds on ggplot2::theme_classic()).

# ---------------------------------------------------------------------------
# Fonts
# ---------------------------------------------------------------------------

#' Register the Aptos font on Windows.
#'
#' The figures use the Aptos font. On Windows it is registered with
#' `extrafont`; `font_import()` is slow, so it only runs if Aptos is not
#' already registered. On other platforms this is a no-op and ggplot2
#' falls back to its default font if Aptos is unavailable.
#'
#' @return `NULL`, invisibly.
load_plot_fonts <- function() {
    if (.Platform$OS.type != "windows") return(invisible(NULL))

    if (!requireNamespace("extrafont", quietly = TRUE)) {
        warning("Package 'extrafont' is not installed; fonts not loaded.")
        return(invisible(NULL))
    }

    registered <- tryCatch(extrafont::fonts(), error = function(e) character(0))
    if (!"Aptos" %in% registered) extrafont::font_import(prompt = FALSE)

    extrafont::loadfonts(device = "win", quiet = TRUE)
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# Access regimes and monitoring points
# ---------------------------------------------------------------------------

# Access regimes 1, 3 and 5 are the open-access regimes and are
# highlighted in green; regimes 2 and 4 are the restricted-access
# regimes and are highlighted in red.

open_regimes <- c(1, 3, 5)

#' Grid-line colour for an access regime.
#'
#' @param period Access-regime number (1-5).
#' @return `"green4"` for an open regime, `"#d62728"` otherwise.
regime_grid_colour <- function(period) {
    if (period %in% open_regimes) "green4" else "#d62728"
}

monitoring_point_labels <- c(
    ent = "Monitoring point 1",
    ten = "Monitoring point 2",
    sin = "Monitoring point 3",
    brg = "Monitoring point 4",
    mqs = "Monitoring point 5",
    top = "Monitoring point 6"
)

# Y-axis settings for the reconstructed 45.5-50.5 cpd signal. Shared by
# Figure 9 (full time series) and Figure 10 (segment ranges), which show
# the same quantity on the same scale. The labels are those shown in the
# first column of panels (access regime 1).

reconstruction_axes <- list(
    ent = list(limit = c(-1, 1), breaks = c(-1, 0, 1), labels = c("-1.00", "0", "1.00")),
    ten = list(limit = c(-0.1, 0.1), breaks = c(-0.1, 0, 0.1), labels = c("-0.10", "0", "0.10")),
    sin = list(limit = c(-0.05, 0.05), breaks = c(-0.05, 0, 0.05), labels = c("-0.05", "0", "0.05")),
    brg = list(limit = c(-0.05, 0.05), breaks = c(-0.05, 0, 0.05), labels = c("-0.05", "0", "0.05")),
    mqs = list(limit = c(-0.05, 0.05), breaks = c(-0.05, 0, 0.05), labels = c("-0.05", "0", "0.05")),
    top = list(limit = c(-0.05, 0.05), breaks = c(-0.05, 0, 0.05), labels = c("-0.05", "0", "0.05"))
)

# ---------------------------------------------------------------------------
# Line-plot theme
# ---------------------------------------------------------------------------

#' Shared line-plot theme.
#'
#' Used by Figures 6, 7, 9 and 10. (Figures 8 and 11 have their own
#' themes, defined in their own scripts, because they show heatmaps.)
#'
#' @param base_family Base font family.
#' @param base_size Base font size, in points.
#' @return A ggplot2 theme object.
theme_ste <- function(base_family = "Aptos", base_size = 7) {
    ggplot2::theme_classic(base_family = base_family, base_size = base_size) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "white", colour = NA),
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            plot.title = ggplot2::element_text(vjust = 0.2, hjust = 0, face = "bold", size = 8.5),
            plot.subtitle = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(colour = "#d62728", linewidth = 0.3, linetype = "solid"),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_line(colour = "#333333", linewidth = 0.15, linetype = "dotted"),
            axis.line = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 7, vjust = -0.5),
            axis.text.y = ggplot2::element_text(angle = 90, size = 7, vjust = 1.5, hjust = 0.5),
            axis.ticks = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(0, 0, 0, 0),
            legend.position = "none"
        )
}

# ---------------------------------------------------------------------------
# Figure assembly
# ---------------------------------------------------------------------------

#' Assemble a multi-panel figure with figure-wide labels.
#'
#' Combines `panels` (filled row by row) into a grid with
#' `patchwork::wrap_plots()`, then overlays the figure-wide axis labels
#' and "Access regime" headings with `cowplot`. The default heading
#' positions are those for five columns of panels; Figure 10, which
#' shows regimes 1, 3 and 5 only, supplies its own `regime_labels` and
#' `regime_x`.
#'
#' @param panels List of ggplot objects, filled row by row.
#' @param ncol Number of columns to arrange `panels` into.
#' @param y_label,x_label Figure-wide axis label text.
#' @param regime_labels Character vector of "Access regime" headings.
#' @param regime_x X positions (0-1, figure-relative) for
#'   `regime_labels`.
#' @return A cowplot/ggplot object ready for `ggsave()`.
assemble_figure <- function(
        panels,
        ncol,
        y_label,
        x_label,
        regime_labels = paste("Access regime", 1:5),
        regime_x = c(0.129, 0.3205, 0.5145, 0.707, 0.8995)
) {
    figure <- patchwork::wrap_plots(panels, ncol = ncol) +
        patchwork::plot_layout() &
        ggplot2::theme(
            panel.spacing = ggplot2::unit(0, "mm"),
            plot.margin = ggplot2::margin(1, 1.25, 1.5, 2, "mm")
        )

    figure <- cowplot::ggdraw(xlim = c(-0.01, 1), ylim = c(-0.01, 1.015)) +
        cowplot::draw_plot(figure, x = 0, y = 0, width = 1, height = 1) +
        cowplot::draw_label(y_label, x = 0, y = 0.5, angle = 90, size = 9, fontfamily = "Aptos", fontface = "plain") +
        cowplot::draw_label(x_label, x = 0.515, y = 0, size = 9, fontfamily = "Aptos", fontface = "plain")

    for (i in seq_along(regime_labels)) {
        figure <- figure +
            cowplot::draw_label(
                regime_labels[i], x = regime_x[i], y = 1.005,
                size = 11, fontfamily = "Aptos", fontface = "bold"
            )
    }

    figure
}
