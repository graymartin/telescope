
# Setup -------------------------------------------------------------------

# Dispatch table: figtype -> geom-building function. Register additional
# chart types by adding to `.PLOT_BUILDERS`. Each builder receives
# `(plot, df)` and returns a ggplot object.
.PLOT_BUILDERS <- list(
  timeseries  = function(plot, df, params) plotting_line(plot, df,
                                                          y_col  = params$y_col,
                                                          labels = isTRUE(params$labels),
                                                          points = isTRUE(params$points),
                                                          hline0 = isTRUE(params$hline0)),
  bar         = function(plot, df, params) plotting_bar(plot, df,
                                                         y_col  = params$y_col,
                                                         labels = isTRUE(params$labels)),
  stacked_bar = function(plot, df, params) plotting_stacked_bar(plot, df,
                                                                 x_col  = params$x_col,
                                                                 y_col  = params$y_col,
                                                                 labels = isTRUE(params$labels)),
  MACC        = function(plot, df, params) plotting_macc(plot, df)
)

# Set of figtypes that use `fill` (stacked/dodged bar) rather than `color`.
.FILL_FIGTYPES <- c("bar", "stacked_bar")


# Functions ---------------------------------------------------------------
## Core plotting orchestration ----

#' Build a telescope plot from figure-builder parameters
#'
#' Orchestrator that stages preparation, geom construction, and final
#' styling. Called by both the Shiny UI (via `batching()`) and the
#' RMarkdown reports.
#'
#' @param df Input data frame (already loaded via `read_dataset()`).
#' @param figtype One of the keys in `.PLOT_BUILDERS`
#'   (`timeseries`, `bar`, `stacked_bar`, `MACC`).
#' @param fb_title_name,fb_figure_no Figure metadata (strings).
#' @param fb_x,fb_y Column names to map to x / y aesthetics.
#' @param fb_color Column name driving color/fill grouping.
#' @param fb_regions,fb_models,fb_years,fb_scenarios,fb_variable Filter
#'   selections (character vectors; NULL means "no filter").
#' @param fb_facet1,fb_facet2 Optional facet column names ("" = none).
#' @param fb_options Comma-separated option string
#'   (e.g. "Aggregate variables, Show plot details").
#' @param fb_x_title,fb_y_title,fb_x_units,fb_y_units Optional axis
#'   customization.
#' @param fb_linetype Optional column name mapped to the linetype aesthetic
#'   (typically `"model"` for multi-run comparisons). Silently ignored by
#'   bar figtypes.
#' @param fb_palette One of `"telescope"` (default; Okabe-Ito discrete /
#'   viridis continuous) or `"fasom_scenario"` (fixed ozone-scenario
#'   palette from the FASOM reporting materials).
#' @param fb_pct_change If `TRUE`, applies [pct_from_base()] to `df` before
#'   plotting; the y column is replaced by pct-change vs. `fb_pct_baseline`
#'   and rows with `scenario == fb_pct_baseline` are dropped. When `TRUE`
#'   and `fb_y_title` is not supplied, the y-axis label becomes
#'   `"% change vs. <baseline>"`.
#' @param fb_pct_baseline Scenario value used as the denominator when
#'   `fb_pct_change = TRUE`. Default `"BASE"`.
#' @return A ggplot (or `cowplot::plot_grid` for MACC).
#' @export
plotting <- function(df,
                     figtype,
                     fb_title_name,
                     fb_figure_no,
                     fb_x,
                     fb_y,
                     fb_color,
                     fb_regions,
                     fb_models,
                     fb_years,
                     fb_scenarios,
                     fb_variable,
                     fb_facet1 = "",
                     fb_facet2 = "",
                     fb_options = "",
                     fb_x_title = NULL,
                     fb_y_title = NULL,
                     fb_x_units = NULL,
                     fb_y_units = NULL,
                     fb_subtitle = NULL,
                     fb_caption = NULL,
                     fb_facet_scales = "free_y",
                     fb_facet_ncol = NULL,
                     fb_points = FALSE,
                     fb_hline0 = FALSE,
                     fb_labels = FALSE,
                     fb_linetype = NULL,
                     fb_palette = c("telescope", "fasom_scenario"),
                     fb_pct_change = FALSE,
                     fb_pct_baseline = "BASE",
                     fb_preprocessors = NULL,
                     fb_x_breaks = NULL) {
  fb_palette <- match.arg(fb_palette)
  # Empty strings from Shiny inputs / persisted CSVs should behave as "unset".
  if (!is.null(fb_linetype) && !nzchar(fb_linetype)) fb_linetype <- NULL

  # Normalize preprocessor list: accept NULL, empty, character vector, or a
  # single comma-joined string (persisted form from var_to_figdf()).
  if (!is.null(fb_preprocessors)) {
    if (length(fb_preprocessors) == 1L && is.character(fb_preprocessors) &&
        grepl(",", fb_preprocessors, fixed = TRUE)) {
      fb_preprocessors <- trimws(strsplit(fb_preprocessors, ",", fixed = TRUE)[[1]])
    }
    fb_preprocessors <- fb_preprocessors[nzchar(fb_preprocessors)]
    if (!length(fb_preprocessors)) fb_preprocessors <- NULL
  }

  # When pct_change is on, the baseline scenario must survive the scenario
  # filter or pct_from_base() has nothing to divide by. Inject it silently;
  # pct_from_base drops baseline rows before returning.
  if (isTRUE(fb_pct_change) && !is.null(fb_scenarios) &&
      length(fb_scenarios) > 0 && nzchar(fb_pct_baseline) &&
      !fb_pct_baseline %in% fb_scenarios) {
    fb_scenarios <- c(fb_scenarios, fb_pct_baseline)
  }

  params <- list(
    figtype     = figtype,
    title       = fb_title_name,
    figure      = fb_figure_no,
    x_col       = fb_x,
    y_col       = fb_y,
    color_col   = fb_color,
    regions     = fb_regions,
    models      = fb_models,
    years       = fb_years,
    scenarios   = fb_scenarios,
    variable    = fb_variable,
    facet1      = fb_facet1,
    facet2      = fb_facet2,
    options     = strsplit(fb_options, split = ", ")[[1]],
    x_title     = fb_x_title,
    y_title     = fb_y_title,
    x_units     = fb_x_units,
    y_units     = fb_y_units,
    subtitle    = fb_subtitle,
    caption     = fb_caption,
    facet_scales = fb_facet_scales,
    facet_ncol  = fb_facet_ncol,
    points      = isTRUE(fb_points),
    hline0      = isTRUE(fb_hline0),
    labels      = isTRUE(fb_labels),
    linetype_col  = fb_linetype,
    palette       = fb_palette,
    pct_change    = isTRUE(fb_pct_change),
    pct_baseline  = fb_pct_baseline,
    preprocessors = fb_preprocessors,
    x_breaks      = fb_x_breaks
  )

  prep <- plot_prepare(df, params)
  plot <- plot_build(prep$df, prep$params)
  plot <- plot_finalize(plot, prep$df, prep$params)
  return(plot)
}

# Stage 1: filter the data, resolve facet layout, and derive axis labels.
plot_prepare <- function(df, params) {
  # Preprocessors run first so filters can key off derived columns.
  if (length(params$preprocessors)) {
    df <- apply_preprocessors(df, params$preprocessors)
  }

  df <- plotting_filter(
    df,
    reg_f       = params$regions,
    mod_f       = params$models,
    yrs_f       = params$years,
    sce_f       = params$scenarios,
    var_f       = params$variable,
    y_col       = params$y_col,
    options_list = params$options
  )

  # % change vs. baseline. Applied after filtering (so the baseline row is
  # present in the frame) but before facet/label resolution (so y_lab picks
  # up the pct override when the caller hasn't set fb_y_title).
  if (isTRUE(params$pct_change) && !identical(params$figtype, "MACC")) {
    join_cols <- unique(c(
      params$x_col,
      params$color_col,
      params$linetype_col,
      if (nzchar(params$facet1)) params$facet1,
      if (nzchar(params$facet2)) params$facet2
    ))
    join_cols <- join_cols[!is.null(join_cols) & nzchar(join_cols)]
    df <- pct_from_base(df, join_cols = join_cols,
                        baseline = params$pct_baseline,
                        value_col = params$y_col)
    if (is.null(params$y_title)) {
      params$y_title <- paste0("% change vs. ", params$pct_baseline)
    }
  }

  # Facet resolution: fold facet1/facet2 into a normalized ordering so
  # a single facet always lands in `facet1_col`.
  if (params$facet1 == "" && params$facet2 == "") {
    facet_n <- 0
    facet1_col <- ""
    facet2_col <- ""
  } else if (params$facet1 == "") {
    facet_n <- 1
    facet1_col <- params$facet2
    facet2_col <- ""
  } else {
    facet_n <- if (params$facet2 == "") 1 else 2
    facet1_col <- params$facet1
    facet2_col <- params$facet2
  }
  params$facet_n    <- facet_n
  params$facet1_col <- facet1_col
  params$facet2_col <- facet2_col

  # Pre-aggregate: sum y_col within each aesthetic group so that lines and
  # bars don't zig-zag or stack silently when the input has multiple rows per
  # (x, color, facet1, facet2) combination. Idempotent when the input is
  # already unique per group. Skipped for MACC (which has custom semantics)
  # and when the y column is non-numeric.
  # df <- plot_preaggregate(df, params)

  # Axis labels. Suppress the x label for year time-series by default to
  # match the FASOM reporting style; callers can still force a label by
  # passing `fb_x_title`.
  if (!is.null(params$x_title)) {
    x_lab <- params$x_title
  } else if (identical(params$x_col, "year")) {
    x_lab <- NULL
  } else {
    x_lab <- str_to_title(params$x_col)
  }
  y_lab <- if (!is.null(params$y_title)) params$y_title else str_to_title(params$y_col)
  if (!is.null(x_lab) && !is.null(params$x_units)) x_lab <- paste0(x_lab, " (", params$x_units, ")")
  if (!is.null(params$y_units)) y_lab <- paste0(y_lab, " (", params$y_units, ")")
  params$x_lab <- x_lab
  params$y_lab <- y_lab

  list(df = df, params = params)
}

# Sum y_col within each aesthetic group (x, color, facet1, facet2) so lines
# and bars stay well-formed when the caller hands in data with residual
# per-region / per-scenario / per-variable duplicates. Idempotent for already
# unique inputs.
plot_preaggregate <- function(df, params) {
  if (identical(params$figtype, "MACC")) return(df)
  y_col <- params$y_col
  if (!y_col %in% names(df) || !is.numeric(df[[y_col]])) return(df)

  group_cols <- unique(c(
    params$x_col,
    params$color_col,
    params$linetype_col,
    if (nzchar(params$facet1_col)) params$facet1_col,
    if (nzchar(params$facet2_col)) params$facet2_col
  ))
  group_cols <- group_cols[nzchar(group_cols) & group_cols %in% names(df)]
  if (length(group_cols) == 0) return(df)

  # Fast path: skip when already unique per group.
  if (!anyDuplicated(df[group_cols])) return(df)

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(!!sym(y_col) := sum(!!sym(y_col), na.rm = TRUE),
                     .groups = "drop")
}
plot_build <- function(df, params) {
  color_col <- params$color_col
  linetype_col <- params$linetype_col
  palette <- if (is.null(params$palette)) "telescope" else params$palette

  if (params$figtype %in% .FILL_FIGTYPES) {
    plot <- plotting_base(df, params$x_col, params$y_col, color_col,
                          fill_col = color_col,
                          linetype_col = linetype_col,
                          palette = palette)
  } else {
    plot <- plotting_base(df, params$x_col, params$y_col, color_col,
                          color_col = color_col,
                          linetype_col = linetype_col,
                          palette = palette)
  }

  builder <- .PLOT_BUILDERS[[params$figtype]]
  if (is.null(builder)) {
    stop("Unknown figtype: '", params$figtype, "'. Registered: ",
         paste(names(.PLOT_BUILDERS), collapse = ", "))
  }
  builder(plot, df, params)
}

# Stage 3: labels, facets, theme, MACC composition, and other options.
plot_finalize <- function(plot, df, params) {
  # Optional "Show plot details" overlay.
  if ("Show plot details" %in% params$options) {
    details_list <- list(
      Regions   = paste(params$regions,   collapse = ", "),
      Models    = paste(params$models,    collapse = ", "),
      Years     = paste(params$years,     collapse = ", "),
      Scenarios = paste(params$scenarios, collapse = ", "),
      Variables = paste(params$variable,  collapse = ", ")
    )
    plot <- plotting_details(plot, details_list)
  }

  if (params$facet_n > 0) {
    facet_scale <- if (is.null(params$facet_scales)) "free" else params$facet_scales
    plot <- plotting_facet(plot, params$facet1_col, params$facet2_col,
                           scale = facet_scale, ncol = params$facet_ncol)
  }

  # Labels + theme. MACC has fixed axis titles.
  if (params$figtype == "MACC") {
    plot <- plotting_style(plot,
                           x_lab = "Mitigation (million tCO2e)",
                           y_lab = "Price ($/tCO2e)")
  } else {
    plot <- plotting_style(plot,
                           x_lab    = params$x_lab,
                           y_lab    = params$y_lab,
                           title    = params$title,
                           subtitle = params$subtitle,
                           caption  = params$caption)
  }

  # Default axis scales for non-MACC plots: strip commas from year labels
  # and add thousands separators to y-axis values.
  if (params$figtype != "MACC") {
    # Parse comma-separated numeric breaks from fb_x_breaks (e.g. "2020,2030,2050").
    x_breaks_num <- NULL
    if (!is.null(params$x_breaks) && length(params$x_breaks) &&
        any(nzchar(as.character(params$x_breaks)))) {
      raw <- as.character(params$x_breaks)
      if (length(raw) == 1L) raw <- strsplit(raw, "[,\\s]+", perl = TRUE)[[1]]
      raw <- raw[nzchar(raw)]
      x_breaks_num <- suppressWarnings(as.numeric(raw))
      if (any(is.na(x_breaks_num))) {
        warning("fb_x_breaks: ignoring non-numeric entries: ",
                paste(raw[is.na(x_breaks_num)], collapse = ", "))
        x_breaks_num <- x_breaks_num[!is.na(x_breaks_num)]
      }
      if (!length(x_breaks_num)) x_breaks_num <- NULL
    }
    if (identical(params$x_col, "year")) {
      plot <- plot + scale_x_continuous(
        breaks = if (!is.null(x_breaks_num)) x_breaks_num else waiver(),
        labels = scales::label_number(accuracy = 1, big.mark = "")
      )
    } else if (!is.null(x_breaks_num) && is.numeric(df[[params$x_col]])) {
      plot <- plot + scale_x_continuous(breaks = x_breaks_num)
    }
    if (is.numeric(df[[params$y_col]])) {
      plot <- plot + scale_y_continuous(labels = scales::label_comma())
    }
  }

  if ("Start y axis at 0" %in% params$options) {
    plot <- plot + expand_limits(y = 0)
  }

  # MACC needs a companion bar underneath the main panel.
  if (params$figtype == "MACC") {
    bar  <- plotting_macc_bar(df)
    plot <- plot + theme(legend.position = "none")
    plot <- cowplot::plot_grid(
      plotlist    = c(plot, bar),
      nrow        = 2,
      rel_heights = c(15, 1),
      align       = "v",
      axis        = "tb"
    )
  }

  plot
}

## Filter operators ----

# Include the value when the filter list is NULL (no filter given), else
# restrict to values in the list. Prefer this over the deprecated %annull%.
`%in_or_all%` <- function(x, filter) {
  if (is.null(filter)) TRUE else x %in% filter
}

# Exclude everything when the filter list is NULL, else restrict to values
# in the list. Prefer this over the deprecated %cancel%.
`%in_or_none%` <- function(x, filter) {
  if (is.null(filter)) FALSE else x %in% filter
}

# Back-compat aliases (deprecated; retained for one release).
`%annull%` <- `%in_or_all%`
`%cancel%` <- `%in_or_none%`

plotting_filter <- function(df, reg_f, mod_f, yrs_f, sce_f, var_f, y_col, options_list) {
  # NOTE: previous implementation switched semantics when nrow(df) > 5e5
  # (NULL filter meant "exclude all" instead of "include all"). That was a
  # silent behavior change with no obvious performance benefit. Semantics are
  # now consistent: NULL means "no restriction".
  # Apply each canonical filter only when its column exists so datasets that
  # are missing e.g. `region` (national-only files) still render.
  filter_if_present <- function(df, col, filter) {
    if (!col %in% names(df)) return(df)
    dplyr::filter(df, .data[[col]] %in_or_all% filter)
  }
  df <- df %>%
    filter_if_present("region",   reg_f) %>%
    filter_if_present("model",    mod_f) %>%
    filter_if_present("scenario", sce_f) %>%
    filter_if_present("variable", var_f)

  if ("year" %in% names(df) && !is.null(yrs_f) && nzchar(yrs_f)) {
    df <- df %>%
      dplyr::filter(
        (year >= as.numeric(str_sub(yrs_f, 1, 4))) &
        (year <= as.numeric(str_sub(yrs_f, 6, 9)))
      )
  }

  if ("Aggregate variables" %in% options_list && "variable" %in% names(df)) {
    df <-
      df %>%
      group_by(across(c(-variable, -!!sym(y_col)))) %>%
      summarize(!!sym(y_col) := sum(!!sym(y_col), na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      mutate(variable = "Aggregated")
  }

  if (y_col == "p") {
    df <- arrange(df, p)
  }

  return(df)
}

plotting_base <- function(df, x_col, y_col, group_col,
                          color_col = NULL, fill_col = NULL,
                          linetype_col = NULL,
                          palette = c("telescope", "fasom_scenario")) {
  palette <- match.arg(palette)
  # scale_*_fasom_scenario() only makes sense when the mapped column is
  # named `scenario`; on anything else its manual values would silently
  # miss and ggplot warns. Fall back to telescope palette with a message.
  fasom_col <- function(col) identical(palette, "fasom_scenario") &&
                             !is.null(col) && identical(col, "scenario")
  base <- ggplot(data = df, aes(x = !!sym(x_col), y = !!sym(y_col), group = !!sym(group_col)))

  # Attach color/fill aesthetic. If the mapped column is discrete
  # (character/factor), also apply the colorblind-friendly telescope palette;
  # numeric mappings are left to the default continuous scale so users can
  # customize via viridis / scale_color_telescope_c() downstream.
  if (!is.null(color_col)) {
    base <- base + aes(color = !!sym(color_col))
    if (color_col %in% names(df) &&
        (is.character(df[[color_col]]) || is.factor(df[[color_col]]))) {
      base <- base + if (fasom_col(color_col)) {
        scale_color_fasom_scenario()
      } else {
        if (identical(palette, "fasom_scenario")) {
          message("plotting_base(): fasom_scenario palette requested but ",
                  "color column is '", color_col, "', not 'scenario'; ",
                  "falling back to telescope palette.")
        }
        scale_color_telescope_d()
      }
    }
  }
  if (!is.null(fill_col)) {
    base <- base + aes(fill = !!sym(fill_col))
    if (fill_col %in% names(df) &&
        (is.character(df[[fill_col]]) || is.factor(df[[fill_col]]))) {
      base <- base + if (fasom_col(fill_col)) {
        scale_fill_fasom_scenario()
      } else {
        scale_fill_telescope_d()
      }
    }
  }

  # Optional linetype aesthetic (e.g., model as run distinction across
  # otherwise identical color/x/facet groups). Include in `group` too so
  # ggplot doesn't collapse lines across linetype categories.
  if (!is.null(linetype_col) && nzchar(linetype_col) &&
      linetype_col %in% names(df)) {
    base <- base + aes(linetype = !!sym(linetype_col),
                       group = interaction(!!sym(group_col), !!sym(linetype_col)))
  }

  return(base)
}

plotting_line <- function(plot, df, label_lines = NULL, points = FALSE, hline0 = FALSE,
                          y_col = NULL, labels = FALSE) {
  line <- plot

  if (isTRUE(hline0)) {
    line <- line +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.45)
  }

  line <- line +
    geom_line(data = df, linewidth = 0.8, lineend = "round")

  if (isTRUE(points)) {
    line <- line + geom_point(data = df, size = 2.2)
  }

  if (isTRUE(labels) && !is.null(y_col) && y_col %in% names(df)) {
    line <- line + geom_label(
      data = df,
      aes(label = .fmt_label(!!sym(y_col))),
      fill = ggplot2::alpha("white", 0.7),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      size = 3,
      show.legend = FALSE
    )
  }

  if (!is.null(label_lines)) {
    line <- line +
      geom_textline(alpha = 1, text_only = TRUE, aes(label = !!sym(label_lines)), hjust = 1, text_smoothing = 50, vjust = -0.75, size = 3, show.legend = FALSE, remove_long = TRUE)
  }

  return(line)
}

plotting_bar <- function(plot, df, y_col = NULL, labels = FALSE) {
  bar <- plot +
    geom_col(data = df, position = position_dodge(width = 0.7),
             width = 0.7, color = "black", linewidth = 0.2)

  if (isTRUE(labels) && !is.null(y_col) && y_col %in% names(df)) {
    bar <- bar + geom_text(
      data = df,
      aes(label = .fmt_label(!!sym(y_col))),
      position = position_dodge(width = 0.7),
      vjust = -0.3, size = 3, color = "black", show.legend = FALSE
    )
  }
  return(bar)
}

plotting_stacked_bar <- function(plot, df, x_col = NULL, y_col = NULL, labels = FALSE) {
  bar <- plot +
    geom_col(data = df, position = "stack", color = "white", linewidth = 0.2)

  if (isTRUE(labels) && !is.null(y_col) && y_col %in% names(df) &&
      !is.null(x_col) && x_col %in% names(df)) {
    # Suppress labels for segments contributing < 5% of column total to
    # reduce visual collision between adjacent segment labels.
    label_df <- df %>%
      dplyr::group_by(!!sym(x_col)) %>%
      dplyr::mutate(.share = abs(!!sym(y_col)) /
                             sum(abs(!!sym(y_col)), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.lbl = ifelse(is.finite(.share) & .share >= 0.05,
                                  .fmt_label(!!sym(y_col)), NA_character_))

    bar <- bar + geom_text(
      data = label_df,
      aes(label = .lbl),
      position = position_stack(vjust = 0.5),
      size = 3, color = "black", show.legend = FALSE, na.rm = TRUE
    )
  }
  return(bar)
}

# 3-significant-figure numeric label with comma grouping; used by fb_labels.
.fmt_label <- function(x) {
  vapply(
    signif(x, 3),
    function(v) format(v, big.mark = ",", scientific = FALSE,
                       trim = TRUE, drop0trailing = TRUE),
    character(1)
  )
}

plotting_facet <- function(plot, f1, f2, scale, ncol = NULL) {
  if (f2  == "") {
    facet <- plot +
      facet_wrap(as.formula(paste("~", f1)), scales = scale, ncol = ncol)
  } else {
    facet <- plot +
      facet_grid(as.formula(paste(f1, "~", f2)), scales = scale)
  }

  return(facet)
}

plotting_details <- function(plot, details_list) {
  text_prep <- paste0(names(details_list), ": ", unlist(details_list), collapse = "\n")
  
  details <- plot +
    annotate("text", x = -Inf, y = Inf, label = text_prep, hjust = 0, vjust = 1.1)
  
  return(details)
}

plotting_style <- function(plot, x_lab = "", y_lab = "", ...) {
  # Delegate all visual defaults to `theme_telescope()` so styling stays in
  # one place and can be swapped by callers overriding the theme later.
  plot +
    labs(x = x_lab, y = y_lab, ...) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),
           fill  = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_telescope()
}

## Batch plotting ----
# Coerce persisted string values back to the types plotting() expects.
# CSVs written by var_to_figdf() store everything as character.
.batching_coerce <- function(s) {
  bool_fields <- c("points", "hline0", "labels", "pct_change")
  num_fields  <- c("facet_ncol")
  # Drop NA-valued entries so plotting() falls back on its defaults.
  s <- s[!vapply(s, function(v) length(v) == 1L && is.na(v), logical(1))]
  for (k in intersect(names(s), bool_fields)) {
    s[[k]] <- isTRUE(as.logical(s[[k]]))
  }
  for (k in intersect(names(s), num_fields)) {
    n <- suppressWarnings(as.numeric(s[[k]]))
    s[[k]] <- if (length(n) == 1L && is.finite(n)) n else NULL
  }
  s
}

batching <- function(var_list, format_list = c("png")) {
  plot_list <- list()
  
  for (s_name in names(var_list)) {
    s <- var_list[[s_name]]
    
    # Store figure metadata not used in plotting()
    analysis <- s$analysis
    source <- s$source
    # Store variables that have no prefix
    df <- dataset_access(strsplit(s$dataset, split = ", ")[[1]])
    figtype <- s$figtype
    # Remove stored variables from list
    s[c("analysis", "source", "dataset", "figtype")] <- NULL

    s <- .batching_coerce(s)

    # Add prefix to remaining list elements
    names(s) <- paste0("fb_", names(s))
    # Add stored variables back to list
    s$df <- df
    s$figtype <- figtype
    
    plot <- do.call(plotting, s)
    plot_list[[s_name]] <- plot
    
    if ("png" %in% format_list) {
      png_dir <- system.file("output", "png", package = "telescope")
      dir.create(paste0(png_dir, "/", analysis), showWarnings = FALSE)
      png_analysis_dir <- system.file("output", "png", analysis, package = "telescope")
      filename <- paste0(png_analysis_dir, "/", tools::file_path_sans_ext(source), ".png")
      save_png(plot, filename)
    }
  }
  
  return(plot_list)
}

## Plot saving ----
save_png <- function(plot, filename) {
  device <- function(..., width, height) {grDevices::png(..., width = 10, height = 7, res = 300, units = "in")}
  ggsave(plot, filename = filename, device = device)
}
