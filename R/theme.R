# Telescope ggplot theme + palette helpers -------------------------------
#
# `theme_telescope()` wraps `ggplot2::theme_bw()` with defaults tuned for
# scientific readability:
#   * light major panel grid (minor grid lines suppressed)
#   * bottom legend with bold title and larger key glyphs
#   * title/subtitle color driven by the `telescope.title_color` option so
#     branding stays configurable rather than hard-coded
#   * left-aligned title/subtitle to match the FASOM reporting style
#
# Palette helpers `scale_(color|fill)_telescope_d()` and
# `scale_(color|fill)_telescope_c()` provide colorblind-friendly defaults:
#   * discrete: Okabe-Ito for <=8 levels; RColorBrewer "Paired" for 9-12;
#     recycle Paired with a warning above 12
#   * continuous: viridis (option D) by default
#
# FASOM-specific `scale_(color|fill)_fasom_scenario()` map the ozone-scenario
# names to the palette used across the reporting materials.

# --- Theme -------------------------------------------------------------------

#' Scientific-readability ggplot theme
#'
#' Wraps [ggplot2::theme_bw()] with a light background grid and legend
#' defaults tuned for legibility. Intended as the default theme for all
#' telescope plots.
#'
#' @param base_size Base font size in points.
#' @param grid Which grid lines to draw: "XY" (both, default), "X", "Y",
#'   or "none". Only major grid lines are drawn; minor gridlines are always
#'   suppressed.
#' @return A ggplot2 theme object.
#' @export
theme_telescope <- function(base_size = 11, grid = c("XY", "X", "Y", "none")) {
  grid <- match.arg(grid)
  title_color <- getOption("telescope.title_color", "#007BA7")

  base <- ggplot2::theme_bw(base_size = base_size)

  gridded <- base +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("XY", "X")) ggplot2::element_line(color = "grey88", linewidth = 0.3)
                           else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("XY", "Y")) ggplot2::element_line(color = "grey88", linewidth = 0.3)
                           else ggplot2::element_blank()
    )

  gridded +
    ggplot2::theme(
      # Legend: bottom, bold title, larger key glyphs for legibility
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold"),
      legend.text      = ggplot2::element_text(size = base_size),
      legend.key.size  = ggplot2::unit(1.1, "lines"),
      legend.key.width = ggplot2::unit(1.4, "lines"),
      legend.spacing.x = ggplot2::unit(0.4, "lines"),
      legend.box.spacing = ggplot2::unit(0.4, "lines"),

      # Title / subtitle: configurable brand color, left-aligned to match
      # the FASOM reporting style. Caption follows the ggplot convention
      # (neutral grey, plain weight).
      plot.title     = ggplot2::element_text(size = base_size + 3,
                                             color = title_color,
                                             face = "bold", hjust = 0),
      plot.subtitle  = ggplot2::element_text(size = base_size + 1,
                                             color = title_color, hjust = 0),
      plot.caption   = ggplot2::element_text(size = base_size - 2,
                                             color = "grey50"),

      # Facets: high-contrast strip so labels read cleanly
      strip.background = ggplot2::element_rect(fill = "grey92", colour = "grey70"),
      strip.text       = ggplot2::element_text(size = ggplot2::rel(0.85), face = "bold"),

      # Axis: slightly larger axis titles vs body text
      axis.title = ggplot2::element_text(size = base_size + 1),
      axis.text  = ggplot2::element_text(size = base_size - 1)
    )
}

# --- Discrete palette --------------------------------------------------------

# Okabe-Ito colorblind-friendly palette (8 colors + black).
# Reference: Okabe & Ito (2008), https://jfly.uni-koeln.de/color/
.telescope_okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

# Return a discrete palette of `n` colors, warning on high cardinality.
# ggthemes::few_pal("Medium") only supplies 8 colors and returns NA past
# that, so the 9-12 tier uses RColorBrewer "Paired" (12-color qualitative).
telescope_palette_d <- function(n) {
  if (n <= length(.telescope_okabe_ito)) {
    return(.telescope_okabe_ito[seq_len(n)])
  }
  if (n <= 12 && requireNamespace("RColorBrewer", quietly = TRUE)) {
    return(RColorBrewer::brewer.pal(12, "Paired")[seq_len(n)])
  }
  warning(
    "telescope_palette_d(): ", n, " colors requested; palette exhausts ",
    "readable distinct hues at ~12. Consider lumping small categories or ",
    "faceting instead of coloring.",
    call. = FALSE
  )
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    return(rep_len(RColorBrewer::brewer.pal(12, "Paired"), n))
  }
  grDevices::hcl.colors(n, palette = "Set 3")
}

#' Discrete color scale for telescope plots (colorblind-friendly)
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @export
scale_color_telescope_d <- function(...) {
  ggplot2::discrete_scale("colour", "telescope",
                          palette = telescope_palette_d, ...)
}

#' Discrete fill scale for telescope plots (colorblind-friendly)
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @export
scale_fill_telescope_d <- function(...) {
  ggplot2::discrete_scale("fill", "telescope",
                          palette = telescope_palette_d, ...)
}

# --- Continuous palette ------------------------------------------------------

#' Continuous color scale for telescope plots (viridis)
#' @param option Viridis option letter (default "D").
#' @param ... Passed through to [ggplot2::scale_color_viridis_c()].
#' @export
scale_color_telescope_c <- function(option = "D", ...) {
  ggplot2::scale_color_viridis_c(option = option, ...)
}

#' Continuous fill scale for telescope plots (viridis)
#' @param option Viridis option letter (default "D").
#' @param ... Passed through to [ggplot2::scale_fill_viridis_c()].
#' @export
scale_fill_telescope_c <- function(option = "D", ...) {
  ggplot2::scale_fill_viridis_c(option = option, ...)
}

# --- FASOM ozone-scenario palette -------------------------------------------

# Canonical ozone-scenario colors used across the FASOM reporting materials.
.fasom_scenario_colors <- c(
  "BASE"    = "#808080",
  "15ppm"   = "#2166AC",
  "11ppm"   = "#4DAC26",
  "7ppm"    = "#D73027",
  "75ppb"   = "#F4A582",
  "70ppb"   = "#D6604D",
  "65ppb"   = "#FDAE61"
)

.fasom_scenario_labels <- c(
  "BASE"    = "Current Conditions (W126_CC)",
  "15ppm"   = "15 ppm-hrs",
  "11ppm"   = "11 ppm-hrs",
  "7ppm"    = "7 ppm-hrs",
  "75ppb"   = "75 ppb",
  "70ppb"   = "70 ppb",
  "65ppb"   = "65 ppb"
)

# Resolve which scenarios to include and in what order; unknown names pass
# through unchanged so callers can extend the palette without editing this file.
.fasom_scenario_resolve <- function(scens) {
  if (is.null(scens)) scens <- names(.fasom_scenario_colors)
  known <- intersect(scens, names(.fasom_scenario_colors))
  values <- .fasom_scenario_colors[known]
  labels <- .fasom_scenario_labels[known]
  list(values = values, labels = labels)
}

#' FASOM ozone-scenario color scale
#'
#' Discrete color scale mapping ozone-scenario codes (`BASE`, `15ppm`,
#' `75ppb`, ...) to the palette used across the FASOM reporting materials.
#'
#' @param scens Optional character vector of scenarios to include (in order).
#'   `NULL` (default) uses all known scenarios.
#' @param name Legend title (default `"Ozone Scenario"`).
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#' @export
scale_color_fasom_scenario <- function(scens = NULL, name = "Ozone Scenario", ...) {
  r <- .fasom_scenario_resolve(scens)
  ggplot2::scale_colour_manual(values = r$values, labels = r$labels,
                               name = name, ...)
}

#' FASOM ozone-scenario fill scale
#'
#' Discrete fill scale companion to [scale_color_fasom_scenario()].
#'
#' @inheritParams scale_color_fasom_scenario
#' @export
scale_fill_fasom_scenario <- function(scens = NULL, name = "Ozone Scenario", ...) {
  r <- .fasom_scenario_resolve(scens)
  ggplot2::scale_fill_manual(values = r$values, labels = r$labels,
                             name = name, ...)
}
