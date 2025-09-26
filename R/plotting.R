
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
                     fb_facet2 = "",
                     fb_options = "") {
  
  # Figure details (string)
  title <- fb_title_name
  figure <- fb_figure_no
  options <- strsplit(fb_options, split = ", ")[[1]]

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

  df <- plotting_filter(df, 
                        reg_f = reg_f, 
                        mod_f = mod_f, 
                        yrs_f = yrs_f, 
                        sce_f = sce_f, 
                        var_f = var_f,
                        y_col = y_col,
                        options_list = options)

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
  } else if (figtype == "MACC") {
    plot <- plotting_macc(plot, df)
  }
  
  if ("Show plot details" %in% options) {
    details_list <- list(
      "Regions" = paste(fb_regions, collapse = ", "),
      "Models" = paste(fb_models, collapse = ", "),
      "Years" = paste(fb_years, collapse = ", "),
      "Scenarios" = paste(fb_scenarios, collapse = ", "),
      "Variables" = paste(fb_variable, collapse = ", ")
    )
    
    plot <- plotting_details(plot, details_list)
  }

  if (f_n > 0) {
    if ("Fix vertical facet scale" %in% options) {
      plot <- plotting_facet(plot, f1_col, f2_col, scale = "free_x")
    } else {
      plot <- plotting_facet(plot, f1_col, f2_col, scale = "free")
    }
  }

  # Style plot
  if (figtype == "timeseries") {
    plot <- plotting_style(plot, x_lab = "", y_lab = "")
  } else if (figtype == "MACC") {
    plot <- plotting_style(plot, x_lab = "Mitigation (million tCO2e)", y_lab = "Price ($/tCO2e)")
  }
  
  if ("Start y axis at 0" %in% options) {
    plot <- plot + expand_limits(y = 0)
  }
  
  if (figtype == "MACC") {
    bar <- plotting_macc_bar(df)
    plot <- plot + theme(legend.position = "none")
    plot <- cowplot::plot_grid(plotlist = c(plot, bar), nrow = 2, rel_heights = c(15, 1), align = "v", axis = "tb")
  }

  return(plot)
}

`%annull%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return (e1 %in% e2)
  }
}

`%cancel%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(FALSE)
  } else {
    return (e1 %in% e2)
  }
}

plotting_filter <- function(df, reg_f, mod_f, yrs_f, sce_f, var_f, y_col, options_list) {
  
  if (nrow(df) > 5e5) {
    df <-
      df %>%
      dplyr::filter((region %cancel% reg_f) & (model %cancel% mod_f) & (scenario %cancel% sce_f) & (variable %cancel% var_f)) %>%
      dplyr::filter((year >= as.numeric(str_sub(yrs_f, 1, 4))) & (year <= as.numeric(str_sub(yrs_f, 6, 9))))
  } else {
    df <-
      df %>%
      dplyr::filter((region %annull% reg_f) & (model %annull% mod_f) & (scenario %annull% sce_f) & (variable %cancel% var_f)) %>%
      dplyr::filter((year >= as.numeric(str_sub(yrs_f, 1, 4))) & (year <= as.numeric(str_sub(yrs_f, 6, 9))))
  }

  if ("Aggregate variables" %in% options_list) {
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

plotting_macc <- function(plot, df) {
  p_min <- min(df$p, na.rm = TRUE)
  p_max <- max(df$p, na.rm = TRUE)
  baseline <- unique(df$baseline)[[1]]
  tech_feasi <- max(df$Q, na.rm = TRUE)
  
  line <- plot +
    geom_line(data = df, size = 0.8, lineend = "round") +
    xlim(0, baseline) +
    ylim(p_min - 1000, p_max) +
    geom_vline(xintercept = baseline, linetype = "dashed", color = "#00ba38") +
    geom_label(aes(x = baseline, y = 0, label = scales::number(baseline, accuracy = 0.1)), fill = "#00ba38", colour = "white", size = 2.9, fontface="bold") +
    geom_vline(xintercept = tech_feasi, linetype = "dashed", color = "#619cff") +
    geom_label(aes(x = tech_feasi, y = 0, label = scales::number(tech_feasi, accuracy = 0.1)), fill = "#619cff", colour = "white", size = 2.9, fontface="bold")
  
  return(line)
}

plotting_macc_bar <- function(df) {
  p_min <- min(df$p, na.rm = TRUE)
  p_max <- max(df$p, na.rm = TRUE) 
  
  baseline <- unique(df$baseline)[[1]]
  tech_feasi <- max(df$Q, na.rm = TRUE)
  abt_le_zero <- max(df[df$p <= 0, ]$Q, na.rm = TRUE)
  left_nudge <- -0.03*baseline
  
  b_f <- 1.0
  t_f <- tech_feasi/baseline
  a_f <- abt_le_zero/baseline
  
  bar_data <- data.frame(
    name = c("b", "t", "a"),
    label = c(b_f - (t_f + a_f), t_f - a_f, a_f),
    length = c(baseline, tech_feasi, abt_le_zero)
  )
  
  bar <- ggplot(data = bar_data) +
    geom_col(aes(x = length, y = "", fill = name), position = "identity") +
    guides(fill = "none") +
    geom_label(aes(x = length, y = "", fill = name, label = scales::percent(label)), position = position_nudge(x = left_nudge), colour = "white", size = 2.9, fontface="bold") +
    guides(fill = "none") +
    theme_void()
  
  return(bar)
}

plotting_facet <- function(plot, f1, f2, scale) {
  if(f2  == "") {
    facet <- plot +
      facet_wrap(as.formula(paste("~", f1)))
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
