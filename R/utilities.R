# Dataset access ----------------------------------------------------------

# Auto-select engine when file is large; readr for small.
.DATASET_FREAD_THRESHOLD_MB <- 40

# Read one or more processed dataset CSVs into a single tibble.
#
# Args:
#   path: One or more dataset identifiers. Two modes are supported:
#     1) Direct path mode: values include folders (e.g.
#        "FASOM/V43t6_Ag/carbon_price.csv"). Loaded as-is.
#     2) Filename mode: values are bare filenames (e.g. "carbon_price.csv").
#        Matching files are discovered under folders named in `model_filter`.
#   model_filter: Character vector of model/folder names used only for
#     filename mode (e.g. c("V43t6_Ag", "V43t6_For")). Ignored for direct
#     paths.
#   engine: "auto" (default) picks `data.table::fread()` when the file
#     exceeds ~40 MB, otherwise `readr::read_csv()`. Override with
#     "readr" or "fread" for reproducibility.
#   keep_source: When TRUE, retain the row-bind id column
#     `source_filename`. Default FALSE (dropped for a cleaner tibble).
#
# Returns: a tibble.
read_dataset <- function(path,
                         model_filter = NULL,
                         engine = c("auto", "readr", "fread"),
                         keep_source = FALSE,
                         tag_model_from_folder = FALSE) {
  engine <- match.arg(engine)
  base_dir <- system.file("input", "dataset", package = "telescope")

  if (!length(path)) {
    stop("`path` must contain at least one dataset path or filename.")
  }
  path <- as.character(path)

  all_files <- list.files(base_dir, full.names = FALSE, recursive = TRUE)
  all_files <- gsub("\\\\", "/", all_files)

  resolve_paths <- function(p) {
    p_norm <- gsub("\\\\", "/", p)

    # Case 1: direct path includes folder separators; model_filter is ignored.
    if (grepl("/", p_norm, fixed = TRUE)) {
      return(p_norm)
    }

    # Case 2: bare filename is resolved from model folders in model_filter.
    if (is.null(model_filter) || !length(model_filter)) {
      stop(
        "`model_filter` is required when `path` is a bare filename: ", p,
        ". Supply one or more model folders (e.g. 'V43t6_Ag')."
      )
    }

    model_filter_chr <- as.character(model_filter)
    matches <- all_files[basename(all_files) == p_norm]
    if (length(matches)) {
      parts <- strsplit(matches, "/", fixed = TRUE)
      has_model <- vapply(parts, function(x) any(x %in% model_filter_chr), logical(1))
      matches <- matches[has_model]
    }

    if (!length(matches)) {
      stop(
        "No dataset file found for filename '", p,
        "' under model_filter folders: ", paste(model_filter_chr, collapse = ", "), "."
      )
    }

    sort(unique(matches))
  }

  resolved <- unlist(lapply(path, resolve_paths), use.names = FALSE)

  read_one <- function(p) {
    full <- file.path(base_dir, p)
    if (!file.exists(full)) {
      stop("Dataset file not found: ", p, " (looked in ", base_dir, ")")
    }
    use_fread <- switch(engine,
      auto  = (file.info(full)$size / 1024^2) > .DATASET_FREAD_THRESHOLD_MB,
      fread = TRUE,
      readr = FALSE
    )
    if (use_fread) {
      tibble::as_tibble(data.table::fread(full))
    } else {
      readr::read_csv(full)
    }
  }

  parts <- lapply(resolved, read_one)
  names(parts) <- resolved

  out <- dplyr::bind_rows(parts, .id = "source_filename")

  # Populate `model` from the parent folder name so combining files from
  # different run folders (e.g. V43t6_Ag + V43t6b_Ag) yields a linetype-able
  # frame. Only fills where `model` is missing so existing values win.
  if (isTRUE(tag_model_from_folder)) {
    folder <- vapply(strsplit(out$source_filename, "/", fixed = TRUE),
                     function(x) if (length(x) > 1) x[[length(x) - 1L]]
                                 else NA_character_,
                     character(1))
    if (!"model" %in% names(out)) {
      out$model <- folder
    } else {
      needs <- is.na(out$model) | !nzchar(as.character(out$model))
      out$model[needs] <- folder[needs]
    }
  }

  if (!keep_source) {
    out <- dplyr::select(out, -dplyr::any_of("source_filename"))
  }

  out
}

# ---- Back-compat shims (retained; prefer read_dataset()) ----

dataset_access <- function(de_dataset) {
  read_dataset(de_dataset, keep_source = TRUE)
}

fasom_access <- function(de_dataset, model_filter = NULL) {
  read_dataset(de_dataset, model_filter = model_filter, engine = "readr")
}

fasom_access_fread <- function(de_dataset, model_filter = NULL) {
  read_dataset(de_dataset, model_filter = model_filter, engine = "fread")
}

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

# Preset selections -------------------------------------------------------

#' Load the preset registry (`config/preset.csv`)
#'
#' Cols: `name, label, dataset_pattern, column, values`. `values` is a
#' comma-separated list (or `*` meaning "all available").
#' @return A tibble.
#' @export
config_preset <- function(filename = "preset.csv") {
  file <- system.file("config", filename, package = "telescope")
  if (!nzchar(file) || !file.exists(file)) {
    return(tibble::tibble(name = character(), label = character(),
                          dataset_pattern = character(),
                          column = character(), values = character()))
  }
  readr::read_csv(file,
                  col_types = readr::cols(.default = readr::col_character()),
                  na = character())
}

#' Filter presets whose `dataset_pattern` matches the loaded dataset(s)
#' @export
presets_for <- function(dataset_path, registry = config_preset()) {
  if (!nrow(registry)) return(registry)
  if (!length(dataset_path)) return(registry[0, , drop = FALSE])
  keep <- vapply(registry$dataset_pattern, function(pat) {
    if (identical(pat, "*") || !nzchar(pat)) return(TRUE)
    any(vapply(dataset_path,
               function(p) isTRUE(grepl(pat, p, perl = TRUE)),
               logical(1)))
  }, logical(1))
  registry[keep, , drop = FALSE]
}

# Dynamic config ----------------------------------------------------------

# Extract unique values of `col` from `df`. When `df` is missing/invalid
# (e.g. the `telescope.default_dataframe` option is unset), returns an
# empty list rather than silently coercing FALSE through unique().
.config_unique <- function(df, col) {
  if (!is.data.frame(df) || !col %in% names(df)) return(list())
  as.list(unique(df[[col]]))
}

config_variable <- function(df = getOption("telescope.default_dataframe")) {
  .config_unique(df, "variable")
}

config_model <- function(df = getOption("telescope.default_dataframe")) {
  .config_unique(df, "model")
}

config_scenario <- function(df = getOption("telescope.default_dataframe")) {
  .config_unique(df, "scenario")
}

config_region <- function(df = getOption("telescope.default_dataframe")) {
  .config_unique(df, "region")
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

# Build the nested `choices` list consumed by shiny::selectInput.
# Structure: optgroup label ("family / display_name") -> filename -> path.
# Grouping is derived from the folder layout under `input/dataset/`:
#   * loose files at the root -> "Other"
#   * one-level folder (`foo/bar.csv`) -> group "foo"
#   * nested folder (`foo/bar/baz.csv`) -> group "foo / bar"
# The example_datasets folder is always sorted last.
config_dataset <- function() {
  files <- check_dataset()
  if (length(files) == 0) return(list())

  # Normalize separators to `/` for cross-platform matching.
  files <- gsub("\\\\", "/", files)

  assignment <- lapply(files, function(f) {
    parts <- strsplit(f, "/", fixed = TRUE)[[1]]
    if (length(parts) == 1) {
      list(family = "Other", display_name = "Other",
           file = parts[1], path = f)
    } else if (length(parts) == 2) {
      list(family = parts[1], display_name = parts[1],
           file = parts[2], path = f)
    } else {
      list(family = parts[1], display_name = parts[2],
           file = paste(parts[-c(1, 2)], collapse = "/"), path = f)
    }
  })

  df <- do.call(rbind, lapply(assignment, as.data.frame,
                              stringsAsFactors = FALSE))

  # Shiny selectInput() only supports one level of optgroup nesting, so
  # collapse family + display_name into a single label.
  df$group <- ifelse(df$family == df$display_name,
                     df$display_name,
                     paste(df$family, df$display_name, sep = " / "))

  # Sort: example_datasets last, everything else alphabetical by group.
  df$sort_key <- ifelse(df$family == "example_datasets", 2L, 1L)
  df <- df[order(df$sort_key, df$group, df$file), , drop = FALSE]

  out <- list()
  for (i in seq_len(nrow(df))) {
    g <- df$group[i]
    if (is.null(out[[g]])) out[[g]] <- character(0)
    entry <- df$path[i]
    names(entry) <- df$file[i]
    out[[g]] <- c(out[[g]], entry)
  }

  out
}

# Figure mappings ---------------------------------------------------------
# Convert single set of figure variables to DataFrame in figure mapping format
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
                         fb_options = "",
                         fb_x_title = "",
                         fb_y_title = "",
                         fb_x_units = "",
                         fb_y_units = "",
                         fb_subtitle = "",
                         fb_caption = "",
                         fb_facet_scales = "free_y",
                         fb_facet_ncol = "",
                         fb_points = FALSE,
                         fb_hline0 = FALSE,
                         fb_labels = FALSE,
                         fb_linetype = "",
                         fb_palette = "telescope",
                         fb_pct_change = FALSE,
                         fb_pct_baseline = "BASE",
                         fb_view = "",
                         fb_preprocessors = "",
                         fb_x_breaks = "") {
  l_fig <- list(
    "dataset"      = dataset,
    "figtype"      = figtype,
    "title_name"   = fb_title_name,
    "figure_no"    = fb_figure_no,
    "x"            = fb_x,
    "y"            = fb_y,
    "color"        = fb_color,
    "regions"      = fb_regions,
    "models"       = fb_models,
    "years"        = fb_years,
    "scenarios"    = fb_scenarios,
    "variable"     = fb_variable,
    "facet1"       = fb_facet1,
    "facet2"       = fb_facet2,
    "options"      = fb_options,
    "x_title"      = fb_x_title,
    "y_title"      = fb_y_title,
    "x_units"      = fb_x_units,
    "y_units"      = fb_y_units,
    "subtitle"     = fb_subtitle,
    "caption"      = fb_caption,
    "facet_scales" = fb_facet_scales,
    "facet_ncol"   = fb_facet_ncol,
    "points"       = as.character(isTRUE(fb_points)),
    "hline0"       = as.character(isTRUE(fb_hline0)),
    "labels"       = as.character(isTRUE(fb_labels)),
    "linetype"     = fb_linetype,
    "palette"      = fb_palette,
    "pct_change"   = as.character(isTRUE(fb_pct_change)),
    "pct_baseline" = fb_pct_baseline,
    "view"         = fb_view,
    "preprocessors" = if (length(fb_preprocessors))
                        paste(fb_preprocessors, collapse = ",") else "",
    "x_breaks"     = fb_x_breaks
  )

  df_fig <-
    tibble::enframe(l_fig) %>%
    tidyr::pivot_wider() %>%
    unnest(regions) %>%
    unnest(models) %>%
    unnest(scenarios) %>%
    unnest(variable) %>%
    as.data.frame()
  
  df_fig <- lapply(df_fig, as.character)
  
  df_fig <- as.data.frame(df_fig)

  return(df_fig)
}

# Convert DataFrame in figure mapping format to lists of variables
figdf_to_var <- function(df_fig) {
  split_list <- split(df_fig, list(df_fig$source))
  
  out_fig_vars <- list()
  for (fig_source in names(split_list)) {
    out_fig_vars[[fig_source]] <- lapply(split_list[[fig_source]], unique)
  }
  
  return(out_fig_vars)
}

# Load figure DataFrame in figure mapping format from disk
load_figure_file <- function(analysis, filename) {
  file <- system.file("output", "figure", analysis, filename, package = "telescope")
  return(read_csv(file, col_types = cols(.default = col_character()), na = character()))
}

# Combine set of figure files into DataFrame in figure mapping format
figures_to_figdf <- function(figure_dir = "default") {
  dir <- system.file("output", "figure", figure_dir, package = "telescope")
  analysis_files <- list.files(dir, full.names = FALSE, recursive = FALSE)
  analysis_data <- list()
  for (filename in analysis_files) {
    analysis_filemap <- load_figure_file(analysis = figure_dir, filename = filename)
    analysis_data[[filename]] <- analysis_filemap
  }
  
  figdf <- 
    bind_rows(analysis_data, .id = "source") %>% 
    mutate(analysis = figure_dir) %>% 
    relocate(analysis)
  
  file <- system.file("output", "intermediate", package = "telescope")
  saveRDS(figdf, file = paste0(file, "/", "fs_set_data.rds"))
  
  return(figdf)
}

# Load full figure set DataFrame in figure mapping format from disk
load_mapping_file <- function(filename) {
  file <- system.file("output", "mapping", filename, package = "telescope")
  return(read_csv(file, col_types = cols(.default = col_character()), na = character()))
}

# Wrapper for load_mapping_file (for consistency)
mapping_to_figdf <- function(filename = "set_default.csv") {
  figdf <- load_mapping_file(filename)
  return(figdf)
}

# Convert figure set to lists of variables
mapping_to_var <- function(filename = "set_default.csv") {
  figdf <- mapping_to_figdf(filename)
  return(figdf_to_var(figdf))
}

# Script export -----------------------------------------------------------

# Drop args whose value is one of the plotting() defaults so the emitted
# script only lists what the user actually customized. Keeps output compact.
.PLOTTING_DEFAULTS <- list(
  fb_facet1 = "",
  fb_facet2 = "",
  fb_options = "",
  fb_x_title = NULL,
  fb_y_title = NULL,
  fb_x_units = NULL,
  fb_y_units = NULL,
  fb_subtitle = NULL,
  fb_caption = NULL,
  fb_facet_scales = "free_y",
  fb_facet_ncol = NULL,
  fb_points = FALSE,
  fb_hline0 = FALSE,
  fb_labels = FALSE,
  fb_linetype = NULL,
  fb_palette = "telescope",
  fb_pct_change = FALSE,
  fb_pct_baseline = "BASE",
  fb_view = NULL,
  fb_preprocessors = NULL,
  fb_x_breaks = NULL
)

.is_default <- function(name, value) {
  if (!name %in% names(.PLOTTING_DEFAULTS)) return(FALSE)
  d <- .PLOTTING_DEFAULTS[[name]]
  if (is.null(d) && is.null(value)) return(TRUE)
  if (is.null(d) || is.null(value)) return(FALSE)
  if (length(d) != length(value)) return(FALSE)
  isTRUE(all.equal(d, value))
}

#' Build an unevaluated `plotting()` call from figure-builder parameters
#'
#' Produces a `call` object that, when evaluated with `df` bound, reproduces
#' the plot. Defaults are dropped for readability.
#'
#' @param df_expr An unevaluated R expression (typically `quote(df)` or the
#'   output of `bquote(read_dataset(.(path)))`). Passed as the `df` argument.
#' @param ... Named arguments matching `plotting()`'s `fb_*` params.
#' @return A `call` suitable for `deparse1()`.
#' @export
figure_to_call <- function(df_expr, figtype, ...) {
  args <- list(...)
  args <- args[!vapply(names(args),
                       function(n) .is_default(n, args[[n]]),
                       logical(1))]
  as.call(c(list(quote(telescope::plotting),
                 df = df_expr,
                 figtype = figtype),
            args))
}

#' Generate a standalone R script that reproduces a designed figure
#'
#' Emits a script that loads telescope, reads the dataset(s), and invokes
#' `plotting()` with the same parameters used in the Shiny app. This is the
#' "wrapped" export form; a raw ggplot form is planned for a later release.
#'
#' @param dataset Character vector of relative dataset paths (as returned by
#'   `read_dataset()`'s direct-path mode).
#' @param figtype Figure type key (e.g. `"timeseries"`).
#' @param fb_dynfilters Optional list of `list(col=, vals=)` specs. Each
#'   emits a `dplyr::filter(col %in% vals)` line between the data load and
#'   the `plotting()` call. When supplied together with preprocessors, the
#'   script emits preprocessors as a precursor too (so dynfilters can key
#'   off derived columns) and passes `fb_preprocessors = NULL` to
#'   `plotting()` to avoid double application.
#' @param ... Additional `fb_*` args forwarded to `figure_to_call()`.
#' @return A single character string containing the script.
#' @export
figure_to_script <- function(dataset, figtype, fb_dynfilters = NULL, ...) {
  args <- list(...)
  df_read_expr <- bquote(telescope::read_dataset(.(dataset)))

  header <- c(
    "# Generated by telescope::figure_to_script().",
    "# Reproduce a Shiny-designed figure. Edit at will.",
    "library(telescope)",
    ""
  )

  # Concatenate a multi-line deparse and prefix a single assignment to just
  # the first line (vectorized paste would corrupt line 2+).
  emit_assign <- function(expr, target = "df") {
    lines <- deparse(expr, width.cutoff = 500L)
    lines[1] <- paste0(target, " <- ", lines[1])
    lines
  }

  body <- emit_assign(df_read_expr)

  # Emit preprocessors as a precursor only when dynfilters need the
  # derived columns; otherwise let plotting() handle them internally.
  preproc <- args$fb_preprocessors
  has_preproc <- !is.null(preproc) &&
                 (is.character(preproc) && any(nzchar(preproc)))
  has_dyn <- length(fb_dynfilters) > 0
  if (has_preproc && has_dyn) {
    pp_expr <- bquote(telescope::apply_preprocessors(df, .(as.character(preproc))))
    body <- c(body, emit_assign(pp_expr))
    args$fb_preprocessors <- NULL
  }

  # Each dynfilter row -> one dplyr::filter() line.
  if (has_dyn) {
    for (spec in fb_dynfilters) {
      col <- spec$col; vals <- as.character(spec$vals)
      if (is.null(col) || !nzchar(col) || !length(vals)) next
      f_expr <- bquote(dplyr::filter(df, .data[[.(col)]] %in% .(vals)))
      body <- c(body, emit_assign(f_expr))
    }
  }

  # Build the plotting() call manually to keep `df` a symbol (do.call would
  # evaluate it and substitute stats::df).
  kept <- args[!vapply(names(args),
                       function(n) .is_default(n, args[[n]]),
                       logical(1))]
  call_expr <- as.call(c(list(quote(telescope::plotting),
                              df = quote(df),
                              figtype = figtype),
                         kept))
  call_lines <- emit_assign(call_expr, target = "plot")

  paste(c(header, body, "", call_lines, "", "print(plot)"), collapse = "\n")
}