# Static config -----------------------------------------------------------
load_file_config <- function(filename) {
  file <- system.file("config", filename, package = "telescope")
  return(read_csv(file, col_types = cols(.default = col_character()), na = character()))
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

config_analysis <- function() {
  dir <- system.file("output", "figure", package = "telescope")
  return(list.dirs(dir, full.names = FALSE, recursive = FALSE))
}

check_dataset <- function() {
  dir <- system.file("input", "dataset", package = "telescope")
  files <- list.files(dir, full.names = FALSE, recursive = TRUE)
  return(files)
}

config_dataset <- function() {
  files <- check_dataset()
  
  for (f in files) {
    if (!grepl("/", f)) {
      files[files == f] <- paste0("Other/", f)
    }
  }
  
  df <- 
    as.data.frame(purrr::list_transpose(strsplit(files, "/")), 
                  col.names = c("folder", "file"))
  
  l_files <- list()
  for (fl in unique(df$folder)) {
    df_f <- 
      dplyr::filter(df, folder == fl)
    
    l_f <- list()
    for (r in 1:nrow(df_f)) {
      filename <- df_f[[r, "file"]]
      if (fl != "Other") {
        l_f[[filename]] <- paste0(fl, "/", filename)
      } else {
        l_f[[filename]] <- filename
      }
    }
    
    l_files[[fl]] <- l_f
  }
  
  return(l_files)
}

# Figure mappings ---------------------------------------------------------
var_to_figdf <- function(dataset,
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
  l_fig <- list(
    "dataset" = dataset,
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
    "facet2" = fb_facet2,
    "options" = fb_options
  )

  df_fig <-
    tibble::enframe(l_fig) %>%
    tidyr::pivot_wider() %>%
    unnest(regions) %>%
    unnest(models) %>%
    unnest(scenarios) %>%
    unnest(variable) %>%
    as.data.frame()
  
  df_fig <- apply(df_fig, 2, as.character)
  
  df_fig <- as.data.frame(df_fig)

  return(df_fig)
}

load_file_map <- function(analysis, filename) {
  file <- system.file("output", "figure", analysis, filename, package = "telescope")
  return(read_csv(file, col_types = cols(.default = col_character()), na = character()))
}

analysis_to_figdf <- function(analysis = "default") {
  dir <- system.file("output", "figure", analysis, package = "telescope")
  analysis_files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  analysis_data <- list()
  for (filename in analysis_files) {
    analysis_filemap <- load_file_map(analysis = analysis, filename = filename)
    analysis_data[[filename]] <- analysis_filemap
  }
  
  figdf <- 
    bind_rows(analysis_data, .id = "source") %>% 
    mutate(analysis = analysis) %>% 
    relocate(analysis)
  
  file <- system.file("output", "intermediate", package = "telescope")
  saveRDS(figdf, file = paste0(file, "/", "fs_set_data.rds"))
  
  return(figdf)
}

# Input data manipulation -------------------------------------------------
# Read MACC data aggregated to USREP regions
read_agg_macc <- function(filename = "sub_data_v2.csv") {
  file <- system.file("data-raw", "MACC", filename, package = "telescope")
  data <- read_csv(file)
  
  data <-
    data %>% 
    mutate(model = ghg) %>% 
    mutate(scenario = as.character(year)) %>% 
    mutate(unit = ghg) %>% 
    mutate(year = year) %>% 
    mutate(variable = paste0(usrep_sector)) %>% 
    mutate(region = region) %>% 
    select(-any_of(c("usrep_sector", "ghg")))
  
  return(data)
}

