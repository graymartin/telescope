# Setup -------------------------------------------------------------------
last_historic_year <- 2022 # TODO: Change to project-level configuration

# Column checklists
country_list <- c()
category_list <- c()
gas_list <- c("N2O", "HFCs", "CH4", "PFCs", "SF6", "NF3")
unit_list <- c("MMTCO2e")
year_list <- c(seq(1990, 2050))

default_check_list <- c("country" = country_list,
               "category" = category_list,
               "gas" = gas_list,
               "unit" = unit_list,
               "year" = year_list)

# Functions ---------------------------------------------------------------
quality_df <- function(comp_df, check_list = default_check_list) {
  value_checks <- list()
  
  for (datacol in setdiff(colnames(comp_df), c("value"))) {
    id_vals <- unique(check_list[datacol])
    comp_vals <- unique(comp_df[[datacol]])
    
    if (length(id_vals) == 0) {
      id_vals = comp_vals
    }
    
    missingvals <- setdiff(id_vals, comp_vals)
    matchingvals <- intersect(id_vals, comp_vals)
    additionalvals <- setdiff(comp_vals, id_vals)
    
    value_checks[[datacol]] <- list("Check values" = id_vals,
                                    "Unique values in DF" = comp_vals,
                                    "Values missing from check" = missingvals,
                                    "Matching values" = matchingvals,
                                    "Additional values in DF" = additionalvals)
  }
  
  output_list <- list("Column values" = value_checks)
  
  return(output_list)
}
