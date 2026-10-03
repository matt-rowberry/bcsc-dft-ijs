#####

## PLOTTING HELPERS
## POOLES'S CAVERN TEMPERATURE DATA
## LAST MODIFIED 3 OCTOBER 2026

#####

# Shared by the figure-plotting scripts 06_ts_plot.R to 11_variance_plot.R.
# Each script loads this file with source("plot_helpers.R").
#
# Functions here call ggplot2, patchwork and grid with `::`, so the
# calling script needs only ggplot2 attached.

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
#' Arranges `panels` (filled row by row) in a grid, with an "Access
#' regime" heading above each column and figure-wide axis labels to
#' the left of and below the grid. The headings and labels are laid
#' out by patchwork in rows and columns of fixed size (in mm), aligned
#' with the panel areas, so they are never clipped at the edge of the
#' figure. Because patchwork builds the layout when the figure is
#' drawn, text is measured on the output device itself, in the font
#' that is actually used.
#'
#' @param panels List of ggplot objects, filled row by row.
#' @param ncol Number of columns to arrange `panels` into.
#' @param y_label,x_label Figure-wide axis label text.
#' @param regime_labels "Access regime" headings, one per column.
#' @param label_size,heading_size Font sizes (pt) of the axis labels
#'   and the headings.
#' @param label_space,heading_space Space (mm) reserved for the axis
#'   labels and the headings, about the height of the text itself.
#'   Text is not clipped to this space, so slightly larger text only
#'   moves closer to its neighbours.
#' @param outer_margin White margin (mm) around the whole figure.
#' @param panel_margin Margin (mm) around each panel: top, right,
#'   bottom, left. Neighbouring margins add up, so the gap between two
#'   columns of panels is the right plus the left margin.
#' @return A patchwork object ready for `ggsave()`.
assemble_figure <- function(
        panels,
        ncol,
        y_label,
        x_label,
        regime_labels = paste("Access regime", 1:5),
        label_size = 9,
        heading_size = 11,
        label_space = 3,
        heading_space = 4,
        outer_margin = 0.5,
        panel_margin = c(1.25, 1.5, 1.5, 2)
) {
    nrow <- length(panels) %/% ncol
    stopifnot(
        length(panels) == nrow * ncol,
        length(panel_margin) == 4,
        length(regime_labels) == ncol
    )

    # Spacing around each panel. Tick labels at the ends of an axis
    # extend beyond the panel edge, so the margins keep the end labels
    # of neighbouring panels clearly apart.
    panels <- lapply(panels, function(p) {
        p + ggplot2::theme(
            panel.spacing = ggplot2::unit(0, "mm"),
            plot.margin = ggplot2::margin(
                panel_margin[1], panel_margin[2], panel_margin[3], panel_margin[4], "mm"
            )
        )
    })

    # A text-only cell. wrap_elements(panel = ...) places the text in
    # the panel area of its cell, so it is centred on the panels it
    # sits above or beside rather than on the whole cell.
    text_cell <- function(label, size, face = "plain", rot = 0) {
        grob <- grid::textGrob(
            label,
            rot = rot,
            gp = grid::gpar(fontfamily = "Aptos", fontsize = size, fontface = face)
        )
        patchwork::wrap_elements(panel = grob, clip = FALSE) +
            ggplot2::theme(
                plot.margin = ggplot2::margin(0, 0, 0, 0),
                plot.background = ggplot2::element_blank()
            )
    }

    headings <- lapply(regime_labels, text_cell, size = heading_size, face = "bold")
    y_cell <- text_cell(y_label, size = label_size, rot = 90)
    x_cell <- text_cell(x_label, size = label_size)

    # Layout: row 1 holds the headings, column 1 the y label and the
    # last row the x label; the panels fill the rest. Cells are listed
    # in the same order as the plots passed to wrap_plots() below.
    design <- c(
        do.call(c, lapply(seq_len(ncol), function(j) patchwork::area(1, j + 1))),
        patchwork::area(2, 1, nrow + 1, 1),
        do.call(c, lapply(seq_len(nrow * ncol), function(k) {
            patchwork::area((k - 1) %/% ncol + 2, (k - 1) %% ncol + 2)
        })),
        patchwork::area(nrow + 2, 2, nrow + 2, ncol + 1)
    )

    patchwork::wrap_plots(
        c(headings, list(y_cell), panels, list(x_cell)),
        design = design,
        widths = grid::unit(c(label_space, rep(1, ncol)), c("mm", rep("null", ncol))),
        heights = grid::unit(c(heading_space, rep(1, nrow), label_space),
                             c("mm", rep("null", nrow), "mm"))
    ) +
        patchwork::plot_annotation(
            theme = ggplot2::theme(
                plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                plot.margin = ggplot2::margin(outer_margin, outer_margin, outer_margin, outer_margin, "mm")
            )
        )
}
