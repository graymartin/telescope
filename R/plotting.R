
# Setup -------------------------------------------------------------------


# Functions ---------------------------------------------------------------
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
                     fb_facet2 = "") {
  # Figure details (string)
  title <- fb_title_name
  figure <- fb_figure_no

  # Column names (string)
  x_col <- fb_x
  y_col <- fb_y
  group_col <- fb_color
  color_col <- fb_color

  # Filters (string lists)
  reg_f <- fb_regions
  mod_f <- fb_models
  yrs_f <- fb_years # Passed modified single string
  sce_f <- fb_scenarios
  var_f <- fb_variable

  df <- plotting_filter(df, reg_f = reg_f, mod_f = mod_f, yrs_f = yrs_f, sce_f = sce_f, var_f = var_f)

  # Facet variables (string)
  if ((fb_facet1 == "") & (fb_facet2 == "")) {
    f_n <- 0
  } else if (fb_facet1 == "") {
    f_n <- 1
    f1_col <- fb_facet2
    f2_col <- ""
  } else {
    f_n <- 2
    f1_col <- fb_facet1
    f2_col <- fb_facet2
  }

  # Create plot base
  plot <- plotting_base(df, x_col, y_col, group_col, color_col)

  # Plot type
  if (figtype == "timeseries") {
    plot <- plotting_line(plot, df)
  }

  if (f_n > 0) {
    plot <- plotting_facet(plot, f1_col, f2_col)
  }

  # Style plot
  plot <- plotting_style(plot, x_lab = "", y_lab = "")

  return(plot)
}

`%annull%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return (e1 %in% e2)
  }
}

plotting_filter <- function(df, reg_f, mod_f, yrs_f, sce_f, var_f) {
  df <-
    df %>%
    dplyr::filter((region %annull% reg_f) & (model %annull% mod_f) & (scenario %annull% sce_f) & (variable %annull% var_f)) %>%
    dplyr::filter((year >= as.numeric(str_sub(yrs_f, 1, 4))) & (year <= as.numeric(str_sub(yrs_f, 6, 9))))

  return(df)
}

plotting_base <- function(df, x_col, y_col, group_col, color_col) {
  base <- ggplot(data = df, aes(x = !!sym(x_col), y = !!sym(y_col), group = !!sym(group_col), color = !!sym(color_col))) +
    theme_light(base_size = 10)

  return(base)
}

plotting_line <- function(plot, df, label_lines = NULL) {
  line <- plot +
    geom_line(data = df, size = 0.8, lineend = "round")

  if (!is.null(label_lines)) {
    line <- line +
      geom_textline(alpha = 1, text_only = TRUE, aes(label = !!sym(label_lines)), hjust = 1, text_smoothing = 50, vjust = -0.75, size = 3, show.legend = FALSE, remove_long = TRUE)
  }

  return(line)
}

plotting_facet <- function(plot, f1, f2 = "", scale = "free_x") {
  if(f2  == "") {
    facet <- plot +
      facet_wrap(as.formula(paste("~", f1)))
  } else {
    facet <- plot +
      facet_grid(as.formula(paste(f1, "~", f2)), scales = scale)
  }

  return(facet)
}

plotting_style <- function(plot, x_lab = "", y_lab = "", ...) {
  styled <- plot +
    labs(x = x_lab, y = y_lab) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.line.y = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(
        size = 14,
        colour = "#007BA7",
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        colour = "#007BA7",
        hjust = 0.5
      ),
      plot.caption = element_text(
        size = 10,
        colour = "#007BA7",
        face = "bold"
      )
    )

  return(styled)
}
