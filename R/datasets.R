# Setup -------------------------------------------------------------------
## Options ----
options(telescope.reprocess_data = FALSE)

# Input data manipulation -------------------------------------------------
## Tasks ----
### Mapping ----
read_mapping <- function(filename) {
  file <- system.file("data-raw", "VariableMapping", filename, package = "telescope")
  data <- readr::read_csv(file)
  
  return(data)
}

read_fasom_model_mapping <- function(filename = "FASOM_mod.csv") {
  return(read_mapping(filename = filename))
}

read_fasom_column_mapping <- function(filename = "FASOM_col.csv") {
  return(read_mapping(filename = filename))
}

### FASOM data----
read_fasom_data <- function(filename, entry) {
  # Resolve path: check inst/ first (installed package), then data-raw/ (development)
  # Not sure if this is necessary
  file <- system.file("data-raw", "FASOM", filename, package = "telescope")
  if (!nzchar(file)) {
    file <- file.path("data-raw", "FASOM", filename)
  }

  gdx_container <- gamstransfer::Container$new(file)

  sym <- gdx_container[entry]
  if (is.null(sym)) {
    stop("Symbol '", entry, "' not found in GDX file '", filename, "'.")
  }
  records <- sym$records
  if (is.null(records)) {
    stop("Symbol '", entry, "' in GDX file '", filename, "' has no records.")
  }

  data_gdx <-
    records %>%
    mutate(datasrc = paste0(filename, "|", entry)) %>%
    mutate(entry = entry)

  return(data_gdx)
}

### FASOM preprocessing ----
# Resolve the path to a GDX file, checking inst/ then data-raw/ (development).
fasom_gdx_path <- function(filename) {
  file <- system.file("data-raw", "FASOM", filename, package = "telescope")
  if (!nzchar(file)) file <- file.path("data-raw", "FASOM", filename)
  if (!file.exists(file)) stop("GDX file not found: ", filename)
  file
}

# Destination directory for processed FASOM CSVs.
# CSVs are written to per-model subdirectories:
#   input/dataset/FASOM/<gdx_basename>/<out_name>.csv
# (e.g. input/dataset/FASOM/V43t6_Ag/carbon_price.csv). The parent
# `input/dataset/FASOM/` is returned; per-model subdirs are created on demand.
fasom_dataset_dir <- function() {
  dir <- here::here("input", "dataset", "FASOM")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

# Main function: process all FASOM GDX entries to standard telescope CSV format.
# Saves one CSV per entry group to input/dataset/FASOM/<gdx_basename>/.
# Set force = TRUE to overwrite existing files.
process_fasom_gdx <- function(force = FALSE) {
  mod_map  <- read_fasom_model_mapping()   # FASOM_mod
  col_map  <- read_fasom_column_mapping()  # FASOM_col
  out_dir  <- fasom_dataset_dir()

  # Auto-discover GDX files
  gdx_dir   <- system.file("data-raw", "FASOM", package = "telescope")
  if (!nzchar(gdx_dir)) gdx_dir <- file.path("data-raw", "FASOM")
  gdx_files <- list.files(gdx_dir, pattern = "\\.gdx$", full.names = FALSE)

  # Warn about files not covered by FASOM_mod
  mapped_files <- unique(mod_map$file)
  unmapped     <- setdiff(gdx_files, mapped_files)
  if (length(unmapped) > 0) {
    warning("GDX files not in FASOM_mod (skipping): ", paste(unmapped, collapse = ", "))
  }

  # Process only included entries
  entries <- dplyr::filter(mod_map, include == TRUE)

  message("Processing FASOM GDX files...")

  for (i in seq_len(nrow(entries))) {
    row        <- entries[i, ]
    model_dir  <- row$model
    dest_dir   <- file.path(out_dir, model_dir)
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    dest       <- file.path(dest_dir, paste0(row$out_name, ".csv"))
    if (file.exists(dest) && !force) {
      message("  Skipping (exists): ", model_dir, "/", row$out_name)
      next
    }

    message("  Processing: ", row$entry, " -> ", model_dir, "/", row$out_name)
    raw <- tryCatch(
      read_fasom_data(row$file, row$entry),
      error = function(e) {
        warning("Skipping ", row$entry, " (", row$file, "): ", conditionMessage(e),
                call. = FALSE)
        NULL
      }
    )
    if (is.null(raw)) next

    # Keep only wildcard (*) or model_dir-specific rows, preferring model_dir when both exist.
    entry_col_map <- col_map %>%
      dplyr::filter(entry == row$entry, model %in% c("*", model_dir)) %>%
      dplyr::group_by(raw_col) %>%
      dplyr::arrange(dplyr::desc(model == model_dir), .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Identify unknown columns (not in mapping, and not auto-added by read_fasom_data)
    known_meta_cols <- c("datasrc", "entry")
    unknown_cols <- setdiff(names(raw), c(entry_col_map$raw_col, known_meta_cols))
    if (length(unknown_cols) > 0) {
      warning("Unknown columns in ", row$entry, " (dropping): ", paste(unknown_cols, collapse = ", "))
    }

    # Build rename vector from FASOM_col
    rename_vec <- setNames(entry_col_map$raw_col, entry_col_map$std_col)
    rename_vec <- rename_vec[rename_vec %in% names(raw)]  # only present columns
    df <- dplyr::rename(raw, !!!rename_vec)

    # Drop map_flag columns and unknown columns
    df <- dplyr::select(df, -dplyr::any_of(c("map_flag", unknown_cols)))

    # Add constants from FASOM_mod
    df <- dplyr::mutate(df, model = row$model)
    if (!is.na(row$unit)   && nzchar(row$unit))   df <- dplyr::mutate(df, unit   = row$unit)
    if (!is.na(row$region) && nzchar(row$region)) df <- dplyr::mutate(df, region = row$region)
    if (!is.na(row$const_variable) && nzchar(row$const_variable)) df <- dplyr::mutate(df, variable = row$const_variable)
    if (isTRUE(row$cast_year_int)) df <- dplyr::mutate(df, year = as.integer(as.character(year)))

    # Special handling for variable construction (ag_summary case)
    if (row$entry == "n_acompareAgSummary" && "category" %in% names(df) && "subcategory" %in% names(df)) {
      df <- dplyr::mutate(df, variable = paste(category, subcategory, sep = "|"))
    }

    # Normalize scenario baseline label — some GDX outputs spell it "Base",
    # others "BASE". Canonicalize to uppercase so downstream filters don't
    # silently drop rows. gamstransfer returns categorical columns as
    # factors; coerce to character first or `ifelse` will store integer
    # codes on the "no" branch.
    if ("scenario" %in% names(df)) {
      df <- dplyr::mutate(df,
        scenario = as.character(scenario),
        scenario = ifelse(tolower(scenario) == "base", "BASE", scenario))
    }

    readr::write_csv(df, dest)
  }

  message("FASOM GDX processing complete.")
  invisible(out_dir)
}
