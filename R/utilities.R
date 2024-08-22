# Setup -------------------------------------------------------------------

# UI ----------------------------------------------------------------------
category_select_list <- function() {
  catlist <- readxl::read_excel("./config/category_list_shiny.xlsx")

  fixlist <-
    catlist %>%
    mutate(display_name = case_when(flag == 2 ~ paste0(category_display, " (Subsector total)"),
                                    TRUE ~ category_display))

  sectors <- unique(dplyr::filter(fixlist, flag == 1)$display_name)
  outlist <- list()
  for (sector in sectors) {
    total_rows <-
      fixlist[(fixlist$ipcc_sector == sector) & (fixlist$flag == 1),]

    sector_rows <-
      fixlist[(fixlist$ipcc_sector == sector) & (fixlist$flag != 1),] %>%
      arrange(`2`, flag)

    all_rows <-
      bind_rows(total_rows, sector_rows) %>%
      pull(category, display_name)

    outlist[[sector]] <- all_rows
  }

  return(outlist)
}

dataset_select_list <- function() {
  prequel <- "./data/"
  input_datasets_raw <- list.files(path = prequel, pattern = "*.csv", recursive = FALSE)

  input_datasets <- list()
  for (dataset in input_datasets_raw) {
    input_datasets[[dataset]] <- paste0(prequel, dataset)
  }

  emissions_datasets <- list(
    "Emissions Data (2024)" = "./data/2024NCBR/usproj_emissions_2024.csv",
    "Emissions Data (2022)" = "./data/2022NCBR/usproj_emissions_2022.csv",
    "Emissions Data (2021)" = "./data/2021NCBR/usproj_emissions.csv"
  )
  
  ghgi_datasets <- list(
    "GHGI (2024)" = "./data/2024 GHGI/2024 GHGI emissions data.csv",
    "GHGI (2022)" = "./data/2022 GHGI/2022 GHGI emissions data.csv",
    "GHGI (2021)" = "./data/2021 GHGI/2021 GHGI emissions data.csv"
  )

  return(c(emissions_datasets, ghgi_datasets, input_datasets))
}

default_ghg_gwp_list <- readr::read_csv("./config/ghg-gwp-list.csv",
                                        show_col_types = FALSE)

MMTCO2e_equiv_units <- c("MMTCO2e", "MMT CO2e",
                         "Mt CO2e",  "MtCO2e",
                         "Tg CO2e", "TgCO2e")

kt_equiv_units <- c("kt",
                    "kilotonne", "kilotonnes",
                    "Gigagram", "Gigagrams", "Gg")

gwp_convert_to_MMTCO2e <- function(x,
                                   to_unit = "MMTCO2e",
                                   gwp = "GWP_100_AR5",
                                   ghg_gwp_list = default_ghg_gwp_list,
                                   strict = TRUE) {
  
  stopifnot(is.data.frame(x))
  stopifnot(all(c("gas", "unit", "value") %in% names(x)))
  if(strict) stopifnot(all(x$unit %in% c(MMTCO2e_equiv_units, kt_equiv_units)))
  
  gwp_factor <- tibble(
    ghg = x$gas
  ) %>%
    left_join(ghg_gwp_list, by = "ghg") %>%
    pull(!!sym(gwp))
  
  conv_factor <- case_when(
    x$unit %in% MMTCO2e_equiv_units ~ 1,
    
    x$unit %in% kt_equiv_units &
      x$gas %in% c("CO2", "CH4", "N2O", "SF6", "NF3", "HFC-23") ~
      gwp_factor * 10^-3,
    
    TRUE ~ NA_real_
  )
  
  new_unit <- case_when(
    x$unit %in% MMTCO2e_equiv_units ~ MMTCO2e_equiv_units[[1]],
    x$gas %in% c("CO2", "CH4", "N2O", "SF6", "NF3", "HFC-23") ~ MMTCO2e_equiv_units[[1]],
    x$gas %in% c("HFCs", "PFCs", "Mix", "Total") &
      x$unit %in% kt_equiv_units ~ kt_equiv_units[[1]],
    TRUE ~ NA_character_
  )
  
  res_data <- tibble(
    gas = x$gas,
    unit = new_unit,
    value = x$value * conv_factor
  )

  x %>%
    mutate(res_data)
}

style_plot <- function(baseplot, xaxis="", yaxis="", fill="", ...) {
  
  styled <-
    baseplot +
    labs(x = xaxis, y = yaxis, fill = fill) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
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