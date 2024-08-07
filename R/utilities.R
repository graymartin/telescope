# Setup -------------------------------------------------------------------

# UI ----------------------------------------------------------------------
category_select_list <- function() {
  category_list <- read.csv("./config/category_list_shiny.csv")
  
  category_list <-
    category_list %>% 
    mutate(level1 = sector) %>% 
    group_by(sector) %>% 
    fill(source, .direction = "down") %>% 
    fill(source_name, .direction = "down") %>% 
    mutate(level2 = source) %>% 
    mutate(level2_name = case_when(is.na(source_name) ~ source,
                                   TRUE ~ source_name)) %>% 
    mutate(level3 = category) %>% 
    mutate(level3_name = case_when(source_total_flag == TRUE ~ paste(level2_name, "(Total)"),
                                   !is.na(subsource_name) ~ subsource_name,
                                   !is.na(subsource) ~ subsource,
                                   !is.na(category) ~ category,
                                   !is.na(source) ~ source,
                                   TRUE ~ source_name)) %>% 
    mutate(level3_name = gsub("Wastewater and Human Sewage (Total)", "Wastewater (Total)", level3_name, fixed = TRUE))
  
  sectors <- unique(category_list$level1)
  sector_list <- list()
  for (sector in sectors) {
    sector_rows <- category_list[category_list$level1 == sector,]
    
    subsectors <- unique(sector_rows$level2_name)
    subsector_list <- list()
    for(subsector in subsectors) {
      subsector_rows <- sector_rows[sector_rows$level2_name == subsector,]
      
      for (ri in 1:nrow(subsector_rows)) {
        sector_row <- subsector_rows[ri, ]
        subsector_list[[sector_row$level3_name]] <- sector_row$level3
      }
    }
    sector_list[[sector]] <- subsector_list
  }
  return(sector_list)
}

country_select_list <- function() {
  country_list <- read.csv("./config/country_list_shiny.csv")
  country_list <-
    split(country_list$country_code, country_list$unfccc_country_name)
  
  return(country_list)
}

dataset_select_list <- function() {
  prequel <- "./data/"
  input_datasets_raw <- list.files(path = prequel, pattern = "*.csv", recursive = FALSE)

  input_datasets <- list()
  for (dataset in input_datasets_raw) {
    input_datasets[[dataset]] <- paste0(prequel, dataset)
  }
  
  composite_datasets <- list(
    "Emissions (2024)" = "./data/RGER 2024/composite_categories_2024.csv",
    "Emissions (2023)" = "./data/RGER 2023/composite_categories_2023.csv",
    "Emissions (2019)" = "./data/RGER 2019/GER2019_composite_summary.csv"
  )
  
  unfccc_raw_datasets <- list(
    "UNFCCC (2024)" = "./data/RGER 2024/UNFCCC_data_proc_2024.csv",
    "UNFCCC (2023)" = "./data/RGER 2023/UNFCCC_data_proc_2023.csv"
  )
  
  unfccc_int_datasets <- list(
    "Interpolated UNFCCC (2024)" = "./data/RGER 2024/UNFCCC_interpolated_2024.csv",
    "Interpolated UNFCCC (2023)" = "./data/RGER 2023/UNFCCC_interpolated_2023.csv"
  )

  return(c(composite_datasets, unfccc_raw_datasets, unfccc_int_datasets, input_datasets))
}

style_plot <- function(baseplot, xaxis="", yaxis="", fill="", ...) {
  
  styled <-
    baseplot +
    labs(x = xaxis, y = yaxis, fill = fill) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 14, colour = "#007BA7", face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, colour = "#007BA7", hjust = 0.5),
          plot.caption = element_text(size = 10, colour = "#007BA7", face = "bold"))
  styled
}