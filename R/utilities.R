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
  input_datasets_raw <- list.files(path = prequel, pattern = "*.csv", recursive = TRUE)

  input_datasets <- list()
  for (dataset in input_datasets_raw) {
    input_datasets[[dataset]] <- paste0(prequel, dataset)
  }

  # TODO: Change to project-level configuration
  # emissions_datasets <- list(
  #   "Emissions Data (2024)" = "../../output/2024NCBR/usproj_emissions_2024.csv",
  #   "Emissions Data (2022)" = "../../output/2022NCBR/usproj_emissions_2022.csv",
  #   "Emissions Data (2021)" = "../../output/2021NCBR/usproj_emissions.csv"
  # )
  # 
  # ghgi_datasets <- list(
  #   "GHGI (2024)" = "../../data-raw/GHGI/2024 GHGI/2024 GHGI emissions data.csv",
  #   #"GHGI (2023)" = "../../data-raw/GHGI/2023 GHGI/2023 GHGI emissions data.csv",
  #   "GHGI (2022)" = "../../data-raw/GHGI/2022 GHGI/2022 GHGI emissions data.csv",
  #   "GHGI (2021)" = "../../data-raw/GHGI/2021 GHGI/2021 GHGI emissions data.csv"
  # )

  #return(c(emissions_datasets, ghgi_datasets, input_datasets))
  return(input_datasets)
}
