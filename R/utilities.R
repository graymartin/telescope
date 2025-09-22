# Static config -----------------------------------------------------------
load_file_config <- function(filename) {
  file <- system.file("config", filename, package = "telescope")
  return(read.csv(file))
}

## Parameter-specific config loading ----
config_figure_type <- function(filename = "figure_type.csv") {
  config_base <- load_file_config(filename)

  for (i in 1:nrow(config_base)) {
    parent <- config_base[i, "inherits"][[1]]
    if (parent != "") {
      parent_row <- config_base[config_base["figure_type"] == parent, ]
      config_base[i, "required"] <- paste0(config_base[i, "required"], ", ", parent_row[1, "required"])
      config_base[i, "optional"] <- paste0(config_base[i, "optional"], ", ", parent_row[1, "optional"])
    }
  }

  config_required <-
    config_base %>%
    filter(figure_type != "base") %>%
    select(-any_of(c("optional", "inherits"))) %>%
    as.data.frame() %>%
    mutate(variable = as.list(strsplit(required, ", "))) %>%
    select(-required) %>%
    unnest(variable) %>%
    mutate(required = TRUE)

  config_optional <-
    config_base %>%
    filter(figure_type != "base") %>%
    select(-any_of(c("required", "inherits"))) %>%
    as.data.frame() %>%
    mutate(variable = as.list(strsplit(optional, ", "))) %>%
    select(-optional) %>%
    unnest(variable) %>%
    mutate(required = FALSE)

  config_figure_type <-
    bind_rows(config_required, config_optional)

  return(config_figure_type)
}

list_figure_type <- function() {
  return(unique(config_figure_type()$figure_type))
}

config_axes <- function(filename = "axes.csv") {
  config_base <- load_file_config(filename)

  config_axes <-
    config_base %>%
    mutate(options = as.list(strsplit(options, ", "))) %>%
    unnest(options)

  return(config_axes)
}

# Dynamic config ----------------------------------------------------------
config_variable <- function(df = getOption("telescope.default_dataframe", default = FALSE)) {
  return(as.list(unique(df$variable)))
}

config_model <- function(df = getOption("telescope.default_dataframe", default = FALSE)) {
  return(as.list(unique(df$model)))
}

config_scenario <- function(df = getOption("telescope.default_dataframe", default = FALSE)) {
  return(as.list(unique(df$scenario)))
}

config_region <- function(df = getOption("telescope.default_dataframe", default = FALSE)) {
  return(as.list(unique(df$region)))
}

# Figure mappings ---------------------------------------------------------
var_to_fig <- function(figtype,
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
  l_fig <- list(
    "figtype" = figtype,
    "title_name" = fb_title_name,
    "figure_no" = fb_figure_no,
    "x" = fb_x,
    "y" = fb_y,
    "color" = fb_color,
    "regions" = fb_regions,
    "models" = fb_models,
    "years" = fb_years,
    "scenarios" = fb_scenarios,
    "variable" = fb_variable,
    "facet1" = fb_facet1,
    "facet2" = fb_facet2
  )

  df_fig <-
    tibble::enframe(l_fig) %>%
    tidyr::pivot_wider() %>%
    unnest(fb_regions) %>%
    unnest(fb_models) %>%
    unnest(fb_scenarios) %>%
    unnest(fb_variable) %>%
    as.data.frame()

  return(df_fig)
}

fig_to_var <- function(df, figure_no) {

}
