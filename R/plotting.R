
# Setup -------------------------------------------------------------------


# Functions ---------------------------------------------------------------
## Core plotting functions ----
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
                     fb_y_units = NULL) {
  
  # Figure details (string)
  title <- fb_title_name
  figure <- fb_figure_no
  options <- strsplit(fb_options, split = ", ")[[1]]

  # Column names (string)
  x_col <- fb_x
  y_col <- fb_y
  group_col <- fb_color
  color_col <- fb_color

  # Axis labels (use custom title + optional units, else fall back to column name)
  x_lab <- if (!is.null(fb_x_title)) fb_x_title else str_to_title(x_col)
  y_lab <- if (!is.null(fb_y_title)) fb_y_title else str_to_title(y_col)
  if (!is.null(fb_x_units)) x_lab <- paste0(x_lab, " (", fb_x_units, ")")
  if (!is.null(fb_y_units)) y_lab <- paste0(y_lab, " (", fb_y_units, ")")

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
  if (figtype %in% c("bar", "stacked_bar")) {
    plot <- plotting_base(df, x_col, y_col, group_col, fill_col = fb_color)
  } else {
    plot <- plotting_base(df, x_col, y_col, group_col, color_col = fb_color)
  }

  # Plot type
  if (figtype == "timeseries") {
    plot <- plotting_line(plot, df)
  } else if (figtype == "bar") {
    plot <- plotting_bar(plot, df)
  } else if (figtype == "stacked_bar") {
    plot <- plotting_stacked_bar(plot, df)
  } else if (figtype == "MACC") {
    plot <- plotting_macc(plot, df)
  }
  
  details_list <- list(
    "Regions" = paste(fb_regions, collapse = ", "),
    "Models" = paste(fb_models, collapse = ", "),
    "Years" = paste(fb_years, collapse = ", "),
    "Scenarios" = paste(fb_scenarios, collapse = ", "),
    "Variables" = paste(fb_variable, collapse = ", ")
  )
  
  if ("Show plot details" %in% options) {
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
    plot <- plotting_style(plot,
                           x_lab = x_lab,
                           y_lab = y_lab,
                           title = title)
  } else if (figtype %in% c("bar", "stacked_bar")) {
    plot <- plotting_style(plot,
                           x_lab = x_lab,
                           y_lab = y_lab,
                           title = title)
  } else if (figtype == "MACC") {
    plot <- plotting_style(plot, x_lab = "Mitigation (million tCO2e)", y_lab = "Price ($/tCO2e)")
  }
  
  if ("Start y axis at 0" %in% options) {
    plot <- plot + expand_limits(y = 0)
  }
  
  if (figtype == "MACC") {    bar <- plotting_macc_bar(df)
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
      dplyr::filter((region %annull% reg_f) & (model %annull% mod_f) & (scenario %annull% sce_f) & (variable %annull% var_f)) %>%
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

plotting_base <- function(df, x_col, y_col, group_col, color_col = NULL, fill_col = NULL) {
  base <- ggplot(data = df, aes(x = !!sym(x_col), y = !!sym(y_col), group = !!sym(group_col))) +
    theme_light(base_size = 10)

  if (!is.null(color_col)) base <- base + aes(color = !!sym(color_col))
  if (!is.null(fill_col))  base <- base + aes(fill  = !!sym(fill_col))

  return(base)
}

plotting_line <- function(plot, df, label_lines = NULL) {
  line <- plot +
    geom_line(data = df, linewidth = 0.8, lineend = "round")

  if (!is.null(label_lines)) {
    line <- line +
      geom_textline(alpha = 1, text_only = TRUE, aes(label = !!sym(label_lines)), hjust = 1, text_smoothing = 50, vjust = -0.75, size = 3, show.legend = FALSE, remove_long = TRUE)
  }

  return(line)
}

plotting_bar <- function(plot, df) {
  bar <- plot +
    geom_col(data = df, position = "dodge", width = 0.7, color = "black", linewidth = 0.2)
  return(bar)
}

plotting_stacked_bar <- function(plot, df) {
  bar <- plot +
    geom_col(data = df, position = "stack", color = "white", linewidth = 0.2)
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
    labs(x = x_lab, y = y_lab, ...) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),
           fill  = guide_legend(nrow = 2, byrow = TRUE)) +
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
        size = 8,
        colour = "#007BA7",
        face = "bold"
      )
    )

  return(styled)
}

## Batch plotting ----
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
