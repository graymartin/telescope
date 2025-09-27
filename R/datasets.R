# Input data manipulation -------------------------------------------------
## Sprint Tasks ----
### Variable Mapping ----
read_mapping <- function(sheet, filename = "USREP_Variable_Map.xlsx") {
  file <- system.file("data-raw", "VariableMapping", filename, package = "telescope")
  data <- read_excel(file, sheet = sheet)
  
  return(data)
}

read_t1_mapping <- function(sheet = "Task1", filename = "USREP_Variable_Map.xlsx") {
  return(read_mapping(sheet = sheet, filename = filename))
}

read_t2_mapping <- function(sheet = "Task2", filename = "USREP_Variable_Map.xlsx") {
  return(read_mapping(sheet = sheet, filename = filename))
}

read_t3_mapping <- function(sheet = "Task3", filename = "USREP_Variable_Map.xlsx") {
  return(read_mapping(sheet = sheet, filename = filename))
}

### Task 1 ----
read_t1_report <- function(filename = "report_Task1.xlsx") {
  file <- system.file("data-raw", "Task1", filename, package = "telescope")
  data_rpt <- 
    read_excel(file, sheet = "rpt") %>% 
    mutate(datasrc = "report_Task1.xlsx|rpt")
  
  data_rptr <- 
    read_excel(file, sheet = "rptr") %>% 
    mutate(datasrc = "report_Task1.xlsx|rptr")
  
  data <-
    bind_rows(data_rpt, data_rptr) %>% 
    left_join(read_t1_mapping()) %>% 
    filter(!is.na(var_map_full)) %>% 
    mutate(model = "Task 1 Report") %>% 
    mutate(scenario = scen) %>% 
    mutate(unit = unit) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(variable = paste0(var_map_full, " (", item,"|", label1, "|", label2, "|", str_to_lower(unit), ")")) %>% 
    mutate(region = region) %>% 
    select(-any_of(c("var_map_full", "label1", "label2", "scen", "item")))
  
  return(data)
}

if (getOption("telescope.reprocess_data")) {
  file_out <- system.file("input", "dataset", "Task1", package = "telescope")
  data <- read_t1_report()
  filename <- "task1_report.csv"
  write_csv(data, paste0(file_out, "/", filename))
}

### Task 2 ----
read_t2_report <- function(filename = "report_Task2.xlsx") {
  file <- system.file("data-raw", "Task2", filename, package = "telescope")
  data_rpt <- 
    read_excel(file, sheet = "rpt") %>% 
    mutate(datasrc = "report_Task2.xlsx|rpt")
  
  data_rptr <- 
    read_excel(file, sheet = "rptr") %>% 
    mutate(datasrc = "report_Task2.xlsx|rptr")
  
  data <-
    bind_rows(data_rpt, data_rptr) %>% 
    left_join(read_t2_mapping()) %>% 
    filter(!is.na(var_map_full)) %>% 
    mutate(model = "Task 2 Report") %>% 
    mutate(scenario = scen) %>% 
    mutate(unit = unit) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(variable = paste0(var_map_full, " (", item,"|", label1, "|", label2, "|", str_to_lower(unit), ")")) %>% 
    mutate(region = region) %>% 
    select(-any_of(c("var_map_full", "label1", "label2", "scen", "item")))
  
  return(data)
}

if (getOption("telescope.reprocess_data")) {
  file_out <- system.file("input", "dataset", "Task2", package = "telescope")
  data <- read_t2_report()
  filename <- "task2_report.csv"
  write_csv(data, paste0(file_out, "/", filename))
}

### Task 3 ----
read_t3_report <- function(filename = "report_Task3_new.xlsx") {
  file <- system.file("data-raw", "Task3", filename, package = "telescope")
  data_rpt <- 
    read_excel(file, sheet = "rpt") %>% 
    mutate(datasrc = "report_Task3_new.xlsx|rpt")
  
  data_rptr <- 
    read_excel(file, sheet = "rptr") %>% 
    mutate(datasrc = "report_Task3_new.xlsx|rptr")
  
  data <-
    bind_rows(data_rpt, data_rptr) %>% 
    left_join(read_t3_mapping()) %>% 
    filter(!is.na(var_map_full)) %>% 
    mutate(model = "Task 2 Report") %>% 
    mutate(scenario = scen) %>% 
    mutate(unit = unit) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(variable = paste0(var_map_full, " (", item,"|", label1, "|", label2, "|", str_to_lower(unit), ")")) %>% 
    mutate(region = region) %>% 
    select(-any_of(c("var_map_full", "label1", "label2", "scen", "item")))
  
  return(data)
}

if (getOption("telescope.reprocess_data")) {
  file_out <- system.file("input", "dataset", "Task3", package = "telescope")
  data <- read_t3_report()
  filename <- "task3_report.csv"
  write_csv(data, paste0(file_out, "/", filename))
}

## MACC ----
### Aggregated MACC ----
# Read MACC data aggregated to USREP regions and USREP sectors
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

if (getOption("telescope.reprocess_data")) {
  file_out <- system.file("input", "dataset", "MACC", package = "telescope")
  data <- read_agg_macc()
  filename <- "agg_macc.csv"
  write_csv(data, paste0(file_out, "/", filename))
}

### Detailed state-level MACC ----
read_state_macc <- function(filename = "MACC_STATE_04102025.csv") {
  file <- system.file("data-raw", "MACC", filename, package = "telescope")
  data <- read_csv(file)
  
  data <-
    data %>% 
    pivot_longer(all_of(c("q_ch4", "q_n2o", "q_fghg")), 
                 names_to = "ghg", 
                 values_to = "QGHG",
                 names_prefix = "q_") %>% 
    mutate(model = ghg) %>% 
    mutate(scenario = as.character(year)) %>% 
    mutate(unit = ghg) %>% 
    mutate(year = year) %>% 
    mutate(variable = paste(str_to_title(sector), 
                            str_to_title(source), 
                            tech_long,
                            sep = "|")) %>% 
    mutate(region = state) %>% 
    mutate(Q = q_total) %>% 
    select(-any_of(c("q_total", "ghg", "sector", "source", "tech", "tech_long", "state")))
  
  return(data)
}

if (getOption("telescope.reprocess_data")) {
  file_out <- system.file("input", "dataset", "MACC", package = "telescope")
  data <- read_state_macc()
  filename <- "state_macc.csv"
  write_csv(data, paste0(file_out, "/", filename))
}

### Detailed global MACC ----
read_global_macc <- function(filename = "MACC_04102025.csv") {
  file <- system.file("data-raw", "MACC", filename, package = "telescope")
  data <- read_csv(file)
  
  data <-
    data %>% 
    pivot_longer(all_of(c("q_ch4", "q_n2o", "q_fghg")), 
                 names_to = "ghg", 
                 values_to = "QGHG",
                 names_prefix = "q_") %>% 
    mutate(model = ghg) %>% 
    mutate(scenario = as.character(year)) %>% 
    mutate(unit = ghg) %>% 
    mutate(year = year) %>% 
    mutate(variable = paste(str_to_title(sector), 
                            str_to_title(source), 
                            tech_long,
                            sep = "|")) %>% 
    mutate(region = paste(country, state, sep = "|")) %>% 
    mutate(Q = q_total) %>% 
    select(-any_of(c("q_total", "country", "country_code", "ghg", "sector", "source", "tech", "tech_long", "state")))
  
  return(data)
}

if (getOption("telescope.reprocess_data")) {
logfr
}
