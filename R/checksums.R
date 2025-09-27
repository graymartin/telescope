


# Setup -------------------------------------------------------------------

# Functions ---------------------------------------------------------------
category_read <- function() {
  category_list <- read.csv("./config/category_list_shiny.csv")
  
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
  source_frame <- 
    category_read() %>% 
    dplyr::filter((category != source) & (subsource != "")) %>% 
    select(source, category) %>% 
    group_by(source) %>% 
    mutate(source_cats = length(unique(category)))
  
  return(source_frame)
}

check_source <- function(df) {
  source_list <- source_tree()
  
  sources <- 
    df %>% 
    dplyr::filter(category %in% source_list$source) %>% 
    group_by(country, category, gas) %>% 
    summarize(source_total = sum(value, na.rm = TRUE))
  
  subsources <- 
    df %>% 
    dplyr::filter(category %in% source_list$category) %>% 
    left_join(source_list) %>% 
    group_by(country, source, gas) %>% 
    summarize(subsource_total = sum(value, na.rm = TRUE),
              source_cats_included = length(unique(category)),
              source_cats_total = max(source_cats)) %>%  
    rename("category" = "source") %>% 
    select(-source_cats_included, -source_cats_total)
  
  tolerance <- 0.01
  
  comb <- 
    sources %>% 
    full_join(subsources) %>% 
    mutate(equal = case_when(source_total >= 0 ~ (((source_total >= subsource_total - tolerance*subsource_total) & (source_total <= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))),
                             source_total < 0 ~ (((source_total <= subsource_total - tolerance*subsource_total) & (source_total >= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))))) %>% 
    mutate(equal_none = (equal | ((subsource_total == 0) | is.na(subsource_total)))) %>% 
    mutate(greater = (source_total > subsource_total)) %>% 
    mutate(lesser = (source_total < subsource_total))
  
  comb_table_exact <- 
    as.data.frame(with(comb, table(category, gas, equal))) %>% 
    dplyr::filter(Freq > 0) %>% 
    mutate(equal = if_else(equal == TRUE, "exact_match", "not_exact")) %>% 
    pivot_wider(names_from = equal, values_from = Freq)
  
  comb_table_none <- 
    as.data.frame(with(comb, table(category, gas, equal_none))) %>% 
    dplyr::filter(Freq > 0) %>% 
    mutate(equal_none = if_else(equal_none == TRUE, "exact_or_missing", "mismatch")) %>% 
    pivot_wider(names_from = equal_none, values_from = Freq)
  
  comb_table <-
    full_join(comb_table_exact, comb_table_none) %>% 
    relocate(exact_match, .before = "not_exact") %>% 
    relocate(exact_or_missing, .before = "not_exact") %>% 
    mutate(exact_match = replace_na(exact_match, 0)) %>% 
    mutate(not_exact = replace_na(not_exact, 0)) %>% 
    mutate(mismatch = replace_na(mismatch, 0)) %>% 
    mutate(missing_subsource = exact_or_missing - exact_match) %>% 
    relocate(missing_subsource, .before = "not_exact") %>% 
    mutate(total = exact_match + not_exact) %>% 
    select(-not_exact, -exact_or_missing) %>% 
    arrange(category, gas)
  
  return(list(comb_table, comb))
}

# Checksums ---------------------------------------------------------------

if (FALSE) {
  dat_comp_list <- dataset_select_list()
  UNFCCC_interpolated <- readRDS("./data/RGERQAQC/UNFCCC_interpolated.rds")
  
  UNFCCC_interpolated_check <- check_source(UNFCCC_interpolated)
  UNFCCC_interpolated_table <- UNFCCC_interpolated_check[[1]]
  UNFCCC_interpolated_frame <- UNFCCC_interpolated_check[[2]]
  
  em_2024 <- 
    read_dataset("./data/RGER 2024/GER_composite_results.csv") %>% 
    select(-any_of(c("nonproportional_value")))
  
  em_2024_check <- check_source(em_2024)
  em_2024_table <- em_2024_check[[1]]
  em_2024_frame <- em_2024_check[[2]]
  
## Manual check ----
  df <- 
    em_2024
  
  source_list <- source_tree()
    
  sources <- 
    df %>% 
    dplyr::filter(category %in% source_list$source) %>% 
    group_by(country, category, gas, year) %>% 
    summarize(source_total = sum(value, na.rm = TRUE))
  
  subsources <- 
    df %>% 
    dplyr::filter(category %in% source_list$category) %>% 
    left_join(source_list) %>% 
    group_by(country, source, gas, year) %>% 
    summarize(subsource_total = sum(value, na.rm = TRUE),
              source_cats_included = length(unique(category)),
              source_cats_total = max(source_cats)) %>%  
    rename("category" = "source") %>% 
    select(-source_cats_included, -source_cats_total)
  
  tolerance <- 0.00001
  
  comb <- 
    sources %>% 
    full_join(subsources) %>% 
    mutate(equal = case_when(source_total >= 0 ~ (((source_total >= subsource_total - tolerance*subsource_total) & (source_total <= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))),
                             source_total < 0 ~ (((source_total <= subsource_total - tolerance*subsource_total) & (source_total >= subsource_total + tolerance*subsource_total)) | (is.na(source_total) & is.na(subsource_total))) & (!xor(is.na(source_total), is.na(subsource_total))))) %>% 
    mutate(equal_none = (equal | ((subsource_total == 0) | is.na(subsource_total)))) %>% 
    mutate(greater = (source_total > subsource_total)) %>% 
    mutate(lesser = (source_total < subsource_total))
  
  comb_table_exact <- 
    as.data.frame(with(comb, table(category, gas, equal))) %>% 
    dplyr::filter(Freq > 0) %>% 
    mutate(equal = if_else(equal == TRUE, "exact_match", "not_exact")) %>% 
    pivot_wider(names_from = equal, values_from = Freq)
  
  comb_table_none <- 
    as.data.frame(with(comb, table(category, gas, equal_none))) %>% 
    dplyr::filter(Freq > 0) %>% 
    mutate(equal_none = if_else(equal_none == TRUE, "exact_or_missing", "mismatch")) %>% 
    pivot_wider(names_from = equal_none, values_from = Freq)
  
  comb_table <-
    full_join(comb_table_exact, comb_table_none) %>% 
    relocate(exact_match, .before = "not_exact") %>% 
    relocate(exact_or_missing, .before = "not_exact") %>% 
    mutate(exact_match = replace_na(exact_match, 0)) %>% 
    mutate(not_exact = replace_na(not_exact, 0)) %>% 
    mutate(mismatch = replace_na(mismatch, 0)) %>% 
    mutate(missing_subsource = exact_or_missing - exact_match) %>% 
    relocate(missing_subsource, .before = "not_exact") %>% 
    mutate(total = exact_match + not_exact) %>% 
    select(-not_exact, -exact_or_missing) %>% 
    arrange(category, gas)
}

