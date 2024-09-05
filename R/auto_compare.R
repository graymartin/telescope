# Setup -------------------------------------------------------------------
last_historic_year <- 2022 # TODO: Change to project-level configuration

# Functions ---------------------------------------------------------------
compare_df <- function(prev_df, comp_df, scenarios = c("historical", "WM")) {
  # Filter to include relevant scenarios and drop scenario column
  if ("scenario" %in% colnames(prev_df)) {
    prev_df <-
      dplyr::filter(prev_df, scenario %in% scenarios) %>%
      select(-scenario)
  }

  if ("scenario" %in% colnames(comp_df)) {
    comp_df <-
      dplyr::filter(comp_df, scenario %in% scenarios) %>%
      select(-scenario)
  }

  # Rename value column in prev_df to prevvalue to prevent join matching
  if (!("prevvalue" %in% colnames(prev_df))) {
    prev_df <-
      prev_df %>%
      rename("prevvalue" = "value")
  }

  # Rows present in prev_df that are missing in comp_df
  missing_rows <-
    prev_df %>%
    anti_join(comp_df)

  # Rows present in prev_df that are present in comp_df
  matching_rows <-
    prev_df %>%
    inner_join(comp_df)

  # Rows missing in prev_df that are present in comp_df
  additional_rows <-
    comp_df %>%
    anti_join(prev_df)

  # Possible matches for missing_rows from additional_rows (ignoring scenarios)
  # possible_rows <-
  #   missing_rows %>%
  #   rename("prevscenario" = "scenario") %>%
  #   inner_join(additional_rows)

  value_checks <- list()

  for (datacol in setdiff(colnames(prev_df), c("prevvalue"))) {
    id_vals <- unique(prev_df[[datacol]])
    comp_vals <- unique(comp_df[[datacol]])
    missingvals <- setdiff(id_vals, comp_vals)
    matchingvals <- intersect(id_vals, comp_vals)
    additionalvals <- setdiff(comp_vals, id_vals)

    value_checks[[datacol]] <- list("Unique values (DF1)" = id_vals,
                                    "Unique values (DF2)" = comp_vals,
                                    "Values missing from DF2" = missingvals,
                                    "Matching values" = matchingvals,
                                    "Additional values in DF2" = additionalvals)
  }

  # Direct comparison between previous and current values
  difference_rows <-
    matching_rows %>%
    group_by(category) %>%
    dplyr::filter(if (any(c("CH4", "CO2", "N2O", "PFCs", "SF6", "HFCs", "NF3") %in% gas)) gas != "Total" else gas == "Total") %>%
    dplyr::ungroup() %>%
    mutate(difference = value - prevvalue) %>%
    mutate(abs_difference = abs(difference)) %>%
    mutate(percent_difference = difference/prevvalue) %>%
    mutate(abs_percent_difference = abs(percent_difference)) %>%
    mutate(fraction = (1 + percent_difference)) %>%
    group_by(country, category, gas, unit) %>%
    mutate(average_prevvalue = mean(prevvalue)) %>%
    mutate(average_value = mean(value)) %>%
    mutate(average_difference = mean(value) - mean(prevvalue)) %>%
    mutate(total_difference = sum(value) - sum(prevvalue)) %>%
    mutate(abs_total_difference = abs(total_difference)) %>%
    group_by(country, category, unit, year) %>%
    mutate(deviation = sum(value) - sum(prevvalue)) %>%
    mutate(percent_deviation = deviation/sum(prevvalue)) %>%
    mutate(abs_percent_deviation = abs(percent_deviation)) %>%
    group_by(country, category, unit) %>%
    mutate(scaled_deviation = scale(deviation)[, 1]) %>%
    mutate(sd_deviation = sd(scaled_deviation, na.rm = TRUE)) %>%
    mutate(ext_sd_deviation = sd(scaled_deviation[-which.max(scaled_deviation)], na.rm = TRUE)) %>%
    mutate(studentized_deviation = abs(scaled_deviation/sd_deviation)) %>%
    mutate(ext_studentized_deviation = abs(scaled_deviation/ext_sd_deviation)) %>%
    dplyr::ungroup() %>%
    mutate_at(vars(-group_cols()), function(x) ifelse(is.nan(x) | is.infinite(x), NA, x))
  
  # Direct comparison between previous and current values
  agg_difference_rows <-
    matching_rows %>%
    group_by(category) %>%
    dplyr::filter(if (any(c("CH4", "CO2", "N2O", "PFCs", "SF6", "HFCs", "NF3") %in% gas)) gas != "Total" else gas == "Total") %>%
    dplyr::ungroup() %>%
    group_by(category, gas, year, unit) %>% 
    summarize(value = sum(value, na.rm = TRUE),
              prevvalue = sum(prevvalue, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(difference = value - prevvalue) %>%
    mutate(abs_difference = abs(difference)) %>%
    mutate(percent_difference = difference/prevvalue) %>%
    mutate(abs_percent_difference = abs(percent_difference)) %>%
    mutate(fraction = (1 + percent_difference)) %>%
    group_by(category, gas, unit) %>%
    mutate(average_prevvalue = mean(prevvalue)) %>%
    mutate(average_value = mean(value)) %>%
    mutate(average_difference = mean(value) - mean(prevvalue)) %>%
    mutate(total_difference = sum(value) - sum(prevvalue)) %>%
    mutate(abs_total_difference = abs(total_difference)) %>%
    group_by(category, unit, year) %>%
    mutate(deviation = sum(value) - sum(prevvalue)) %>%
    mutate(percent_deviation = deviation/sum(prevvalue)) %>%
    mutate(abs_percent_deviation = abs(percent_deviation)) %>%
    group_by(category, unit) %>%
    mutate(scaled_deviation = scale(deviation)[, 1]) %>%
    mutate(sd_deviation = sd(scaled_deviation, na.rm = TRUE)) %>%
    mutate(ext_sd_deviation = sd(scaled_deviation[-which.max(scaled_deviation)], na.rm = TRUE)) %>%
    mutate(studentized_deviation = abs(scaled_deviation/sd_deviation)) %>%
    mutate(ext_studentized_deviation = abs(scaled_deviation/ext_sd_deviation)) %>%
    dplyr::ungroup() %>%
    mutate_at(vars(-group_cols()), function(x) ifelse(is.nan(x) | is.infinite(x), NA, x))

  trend_rows <-
    matching_rows %>%
    group_by(category, gas, year, unit) %>% 
    summarize(value = sum(value, na.rm = TRUE),
              prevvalue = sum(prevvalue, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(value = case_when(is.na(value) ~ 0,
                             TRUE ~ value))

  trends_and_spikes <- trends_df(trend_rows)

  output_list <- list("Missing rows" = missing_rows,
                      "Present rows" = matching_rows,
                      "Additional rows" = additional_rows,
                      # "Possible matches" = possible_rows,
                      "Difference summary" = difference_rows,
                      "Aggregated summary" = agg_difference_rows,
                      "Trends and spikes" = trends_and_spikes,
                      "Column values" = value_checks)

  return(output_list)
}

trends_df <- function(trend_df) {
  # Expects a dataframe with the following columns in any order:
  #   category, country, gas, year, unit, value
  # Change group_by variables if any other columns are included

  spikes <- find_spikes(trend_df)

  trend_rows <-
    trend_df %>%
    arrange(category, gas, year) %>%
    group_by(category, gas, unit) %>%
    # First-order difference
    mutate(lag_difference = value - lag(value, order_by = year)) %>%
    mutate(percent_lag_difference = lag_difference/lag(value, order_by = year)) %>%
    mutate(abs_lag_difference = abs(lag_difference)) %>%
    mutate(abs_percent_lag_difference = abs(percent_lag_difference)) %>%
    mutate(abs_difference_rank = min_rank(-abs_lag_difference)) %>%

    left_join(spikes)

  return(trend_rows)
}

find_spikes <- function(df, threshold = 3) {
  cat_list <- list()
  for (c in unique(df$category)) {
    cat_rows <-
      df %>%
      dplyr::filter(category == c) %>%
      arrange(year)

    gas_list <- list()
    for (g in unique(cat_rows$gas)) {
      gas_rows <-
        cat_rows %>%
        dplyr::filter(gas == g)

      vals <- pull(gas_rows, value)
      baseyear <- min(gas_rows$year)
      tsfull <- ts(vals, start = baseyear)

      fit <- forecast::auto.arima(tsfull, seasonal = FALSE)
      res <- residuals(fit)
      res_sd <- sd(res)

      res_df <-
        data.frame(category = c,
                   gas = g,
                   residual = as.matrix(res),
                   year = time(res)) %>%
        mutate(stdev = res_sd) %>%
        mutate(studentized_residual = abs(res/res_sd)) %>%
        mutate(studentized_residual = case_when((res_sd == 0) ~ 0,
                                                TRUE ~ studentized_residual)) %>%
        mutate(historical = case_when(year <= last_historic_year ~ TRUE, TRUE ~ FALSE)) %>%
        mutate(spike = (studentized_residual > threshold) & !historical)

      gas_list[[g]] <- res_df
    }

    gas_df <-
      gas_list %>%
      bind_rows()

    cat_list[[c]] <- gas_df
  }

  return(bind_rows(cat_list))
}

read_dataset <- function(filepath) {
  dataset <-
    read.csv(filepath)

  return(dataset)
}

write_comparison <- function(compare_dfs, filepath) {
  wb <- openxlsx::createWorkbook()

  for (sub_df in names(compare_dfs)) {
    if (sub_df == "Column values") {

      sub_list <- list()
      for (sub_sub_df in names(compare_dfs[[sub_df]])) {
        sub_list[[sub_sub_df]] <- pivot_wider(stack(compare_dfs$`Column values`[[sub_sub_df]]),
                                              names_from = ind, values_from = values, values_fn = list)
      }

      openxlsx::addWorksheet(wb, sub_df)
      openxlsx::writeDataTable(wb, sheet = sub_df, bind_rows(sub_list, .id = "Column"))

      next
    }

    openxlsx::addWorksheet(wb, sub_df)
    openxlsx::writeDataTable(wb, sheet = sub_df, compare_dfs[[sub_df]])
  }

  openxlsx::addWorksheet(wb, "Notes")

  max_diff_df <-
    compare_dfs$`Aggregated summary` %>%
    dplyr::filter(year > last_historic_year) %>%
    rename("Category" = "category") %>%
    group_by(Category) %>%
    dplyr::filter(if (any(c("CH4", "CO2", "N2O", "PFCs", "SF6", "HFCs", "NF3") %in% gas)) gas != "Total" else gas == "Total") %>%
    summarize(`Maximum studentized deviation` = max(ext_studentized_deviation, na.rm = TRUE),
              `Percent deviation` = abs_percent_deviation[which.max(ext_studentized_deviation)],
              `Deviation` = deviation[which.max(ext_studentized_deviation)],
              `Year` = year[which.max(ext_studentized_deviation)])

  summary_df <- data.frame("Category" = unique(c(compare_dfs$`Column values`$category$`Unique values (DF1)`,
                                                 compare_dfs$`Column values`$category$`Unique values (DF2)`)))

  summary_df <-
    summary_df %>%
    mutate(`Notes` = "") %>%
    left_join(max_diff_df)

  openxlsx::writeDataTable(wb, sheet = "Notes", summary_df)

  openxlsx::saveWorkbook(wb, filepath, overwrite = FALSE)
}

# Comparisons -------------------------------------------------------------
if (FALSE) {
  dat_comp_list <- dataset_select_list()

  em_2024 <- 
    read_dataset(dat_comp_list$`Emissions (2024)`) %>% 
    select(-any_of(c("nonproportional_value"))) %>% 
    mutate(value = case_when(category == "EPS" ~ value/1000,
                             TRUE ~ value)) # TODO: Fix this
    
  em_2022 <- 
    read_dataset(dat_comp_list$`Emissions (2019)`) %>% 
    select(-any_of(c("X")))
  
  comp_em_2019_2024 <- compare_df(em_2022, em_2024)
  write_comparison(comp_em_2019_2024, "./output/RGER/comp_em_2019_2024.xlsx")
}

