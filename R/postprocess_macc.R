category_read <- function() {
  category_list <- 
    tar_read(category_list) 
  
  return(category_list)
}

sector_tree <- function() {
  sector_frame <- 
    category_read() %>% 
    dplyr::filter(category == source) %>% 
    select(sector, category)
  
  return(sector_frame)
}

source_tree <- function() {
  ngsys_list <- as.data.frame(cbind("category" = c("NGExplorationProduction", "NGTSD"), 
                                    "source" = c("NGSys", "NGSys")))
  
  petsys_list <- as.data.frame(cbind("category" = c("PetExplorationProduction", "PetRefining"), 
                                     "source" = c("PetSys", "PetSys")))
  
  ngo_list <- as.data.frame(cbind("category" = c("NGSys", "PetSys"), 
                                  "source" = c("NGO", "NGO")))
  
  wastewater_list <- as.data.frame(cbind("category" = c("WastewaterInd", "WastewaterDom"), 
                                         "source" = c("Wastewater", "Wastewater")))
  
  source_frame <- 
    category_read() %>% 
    dplyr::filter((category != source) & (subsource != "")) %>% 
    select(source, category) %>% 
    filter(!(source == "NGO")) %>% 
    bind_rows(ngsys_list) %>% 
    bind_rows(petsys_list) %>% 
    bind_rows(ngo_list) %>% 
    bind_rows(wastewater_list)%>% 
    group_by(source) %>% 
    mutate(source_cats = length(unique(category)))
  
  return(source_frame)
}

check_source_subsource <- function(df) {
  source_list <- source_tree()
  
  sources <- 
    df %>% 
    dplyr::filter(category %in% source_list$source) %>% 
    group_by(country, category, gas, year) %>% 
    summarize(source_total = sum(value, na.rm = TRUE))
  
  subsources <- 
    df %>% 
    dplyr::filter(category %in% source_list$category) %>% 
    left_join(source_list, by = join_by(category)) %>% 
    group_by(country, source, gas, year) %>% 
    summarize(subsource_total = sum(value, na.rm = TRUE),
              source_cats_included = length(unique(category)),
              source_cats_total = max(source_cats)) %>%  
    rename("category" = "source") %>% 
    select(-source_cats_included, -source_cats_total)
  
  tolerance <- 0.01
  
  comb <- 
    sources %>% 
    full_join(subsources, by = join_by(country, category, gas, year)) %>% 
    mutate(equal = case_when(source_total >= 0 ~ (((source_total >= subsource_total - tolerance*subsource_total) & (source_total <= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))),
                             source_total < 0 ~ (((source_total <= subsource_total - tolerance*subsource_total) & (source_total >= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))))) %>% 
    mutate(equal_none = (equal | ((subsource_total == 0) | is.na(subsource_total)))) %>% 
    mutate(greater = (source_total > subsource_total)) %>% 
    mutate(lesser = (source_total < subsource_total))
  
  return(comb)
}

global_source_subsource <- function(df, remove_missing_countries = TRUE) {
  source_list <- source_tree()
  
  if (remove_missing_countries) {
    df_checksum <- check_source_subsource(df)
    
    countries <- 
      df_checksum %>% 
      dplyr::filter((equal == FALSE) & (equal_none == TRUE)) %>%
      group_by(country, category, gas) %>% 
      summarize(count = n()) %>% 
      ungroup() %>% 
      select(country, category, gas) %>% 
      right_join(source_list, by = join_by(category == source), relationship = "many-to-many") %>% 
      pivot_longer(cols = c(category, category.y)) %>% 
      select(country, value, gas) %>% 
      rename("category" = "value") %>% 
      mutate(drop = TRUE) %>% 
      distinct()
    
    df <-
      df %>% 
      left_join(countries) %>%
      dplyr::filter(is.na(drop))
  }
  
  sources <- 
    df %>% 
    dplyr::filter(category %in% source_list$source) %>% 
    group_by(category, gas) %>% 
    summarize(source_total = sum(value, na.rm = TRUE))
  
  subsources <- 
    df %>% 
    dplyr::filter(category %in% source_list$category) %>% 
    left_join(source_list, by = join_by(category)) %>% 
    group_by(source, gas) %>% 
    summarize(subsource_total = sum(value, na.rm = TRUE),
              source_cats_included = length(unique(category)),
              source_cats_total = max(source_cats)) %>%  
    rename("category" = "source") %>% 
    select(-source_cats_included, -source_cats_total)
  
  tolerance <- 0.01
  
  comb <- 
    sources %>% 
    full_join(subsources, by = join_by(category, gas)) %>% 
    mutate(equal = case_when(source_total >= 0 ~ (((source_total >= subsource_total - tolerance*subsource_total) & (source_total <= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))),
                             source_total < 0 ~ (((source_total <= subsource_total - tolerance*subsource_total) & (source_total >= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))))) %>% 
    mutate(equal_none = (equal | ((subsource_total == 0) | is.na(subsource_total))))
  
  return(comb)
}

get_amendments <- function(df) {
  source_list <- source_tree()
  df_check <- check_source_subsource(df)
  
  out_df <- list()
  
  for (check_source in unique(df_check$category)) {
    print(paste0("Processing ", check_source))
    
    subsources <- 
      dplyr::filter(source_list, source == check_source) %>% 
      pull(category)
    
    df_source <- 
      dplyr::filter(df_check, category == check_source) %>% 
      dplyr::filter(!equal)
    
    print(df_source)
    
    df_source_group <-
      df_source %>% 
      group_by(country, category, gas) %>% 
      summarize(source_total = sum(source_total, na.rm = TRUE),
                subsource_total = sum(subsource_total, na.rm = TRUE)) %>% 
      ungroup()
    
    print(df_source_group)
    
    df_source_group_max <- 
      df_source_group %>% 
      slice_max(source_total, n = 1) %>% 
      pull(country)
    
    print(df_source_group_max)
    
    # Largest mismatched source for NGO is Russia, which seems to have country-reported source data but no subsource data
    source_led <- c("NGO", "Biomass", "Carbide", "CoalMining", "Landfills", "StatMob", "Wastewater", "PetSys", "NGSys", "ODSSubs", "Electronics", "NitricAdipic")
    
    # Largest mismatched source for electronics is China, which only has country-reported data for semiconductors
    subsource_led <- c()
    
    if (check_source %in% source_led) {
      # Modify subsource data to match source data
      df_long_source <- 
        df %>% 
        dplyr::filter(category == check_source) %>% 
        left_join(df_source, by = join_by(country, category, gas, year)) %>% 
        dplyr::filter(!is.na(source_total)) %>% 
        rename("source_category" = "category")
      
      df_long_subsource <-
        df %>% 
        dplyr::filter(category %in% subsources) %>% 
        mutate(source_category = check_source) %>% 
        left_join(df_source, by = join_by(country, source_category == category, gas, year)) %>% 
        dplyr::filter(!is.na(source_total)) %>% 
        group_by(country, source_category, gas, unit, year) %>% 
        mutate(subsource_combined_total = sum(value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(subsource_combined_total = replace_na(subsource_combined_total, 0)) 
      
      years_missing_subsource <- dplyr::filter(df_long_subsource, subsource_combined_total == 0)
      
      # TODO: Handle years with missing subsource data
      
      df_long_amended <-
        df_long_subsource %>% 
        dplyr::filter(subsource_combined_total != 0) %>% 
        mutate(contribution = value/subsource_combined_total) %>% 
        mutate(amended_value = contribution*source_total) %>% 
        select(country, category, gas, unit, year, value, amended_value)
      
      df_change <-
        df_long_amended %>% 
        group_by(country, category, gas, unit) %>% 
        summarize(amendment_change = sum(amended_value - value, na.rm = TRUE))
      
      df_long_amended <- 
        df_long_amended %>% 
        select(-value)
      
      out_df[[check_source]] <- list(df_long_amended, df_change, years_missing_subsource)
    } else if (check_source %in% subsource_led) {
      # Modify subsource data to match source data
      df_long_source <- 
        df %>% 
        dplyr::filter(category == check_source) %>% 
        left_join(df_source, by = join_by(country, category, gas, year)) %>% 
        dplyr::filter(!is.na(source_total)) %>% 
        mutate(subsource_total = replace_na(subsource_total, 0))
      
      years_missing_subsource <- dplyr::filter(df_long_source, (subsource_total == 0) & (source_total != 0))
      
      # TODO: Handle years with missing subsource data
      
      df_long_amended <-
        df_long_source %>% 
        dplyr::filter(subsource_total != 0) %>% 
        mutate(amended_value = subsource_total) %>% 
        select(country, category, gas, unit, year, value, amended_value)
      
      df_change <-
        df_long_amended %>% 
        group_by(country, category, gas, unit) %>% 
        summarize(amendment_change = sum(amended_value - value, na.rm = TRUE))
      
      df_long_amended <- 
        df_long_amended %>% 
        select(-value)
      
      out_df[[check_source]] <- list(df_long_amended, df_change, years_missing_subsource)
    }
  }
  
  return(out_df)
} 

postprocess <- function(df) {
  df_list <- get_amendments(df)
  
  amendments <- list()
  missing_years <- list()
  
  for (source in names(df_list)) {
    amendments[[source]] <- df_list[[source]][[1]]
    
    missing_years[[source]] <- df_list[[source]][[3]]
  }
  
  amendments <- 
    bind_rows(amendments)
  
  missing_years <- 
    bind_rows(missing_years) %>% 
    select(country, category, gas, unit, year) %>% 
    mutate(missing_value = TRUE)
  
  df_out <- 
    df %>% 
    mutate(unamended_value = value) %>% 
    left_join(amendments, by = join_by(country, category, gas, unit, year)) %>% 
    left_join(missing_years, by = join_by(country, category, gas, unit, year)) %>% 
    dplyr::filter(is.na(missing_value)) %>% 
    mutate(value = case_when(is.na(amended_value) ~ value,
                             !is.na(amended_value) ~ amended_value,
                             TRUE ~ value)) %>% 
    select(country, category, gas, unit, year, value, unamended_value)
  
  return(df_out)
}

if (FALSE) {
  source_list <- source_tree()
  
  input_df <- tar_read(composite_categories)
  write.csv(input_df, "../telescope/data/RGER 2024/composite_categories_2024.csv", row.names = FALSE)
  amended_df <- postprocess(input_df)
  amended_checksum <- check_source_subsource(amended_df)
  
  amended_ngsys <- postprocess(dplyr::select(amended_df, -unamended_value))
  amended_ngsys_checksum <- check_source_subsource(amended_ngsys)
  
  rger_usa <-
    amended_ngsys %>% 
    dplyr::filter(country == "USA") %>% 
    rename("rger_value" = "value")
  
  # TODO: Check categories in common
  btr_ger_cat_map <- readxl::read_excel("./data-raw/BTR/btr_ger_cat_map.xlsx", sheet = "cat_map")
  
  btr_usa <- 
    read.csv("./data-raw/BTR/usproj_emissions_2024.csv") %>% 
    rename("usproj_category" = "category") %>% 
    dplyr::filter(scenario %in% c("historical", "WM")) %>% 
    select(gas, year, value, unit, usproj_category) %>% 
    mutate(country = "USA") %>% 
    left_join(btr_ger_cat_map, by = join_by(usproj_category)) %>% 
    filter(!is.na(rger_category)) %>% 
    group_by(country, rger_category, gas, unit, year) %>% 
    summarize(value = sum(value, na.rm = TRUE)) %>% 
    rename("category" = "rger_category",
           "btr_value" = "value") 
  
  # TODO: We want historical data to line up with BTR
  usa_combined <- 
    rger_usa %>% 
    left_join(btr_usa, by = join_by(country, category, gas, unit, year))
  
  amended_btr <- 
    amended_ngsys %>% 
    rename("rger_value" = "value") %>% 
    left_join(btr_usa, by = join_by(country, category, gas, unit, year)) %>% 
    mutate(value = case_when(!is.na(btr_value) ~ btr_value,
                             TRUE ~ rger_value)) %>% 
    select(country, category, gas, unit, year, value) %>% 
    postprocess()
  
  amended_global <- global_source_subsource(amended_btr)
  amended_global_all <- global_source_subsource(amended_ngsys, remove_missing_countries = FALSE)
  
  write.csv(amended_btr, "../telescope/data/RGERQAQC/amended_composite.csv", row.names = FALSE)
}
