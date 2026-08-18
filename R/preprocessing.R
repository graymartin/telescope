# Shared preprocessing helpers for telescope reports.
#
# Extracted from RMarkdown `helpers` chunks so preprocessing lives in one
# place and reports can `devtools::load_all()` them without duplicating
# CSV reads.

#' Load the GHG pool lookup (variable -> ghg_pool)
#'
#' Returns a named character vector suitable for `mutate(ghg_pool = GHG_POOL_MAP[variable])`.
#'
#' @return Named character vector: names are `variable` values, elements are
#'   `ghg_pool` labels.
#' @export
load_ghg_pool_map <- function() {
  file <- here::here("data-raw", "VariableMapping", "FASOM_submap_ghg_pool.csv")
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  stats::setNames(d$ghg_pool, d$variable)
}

#' Load commodity -> group lookup, split by group
#'
#' @return Named list of character vectors keyed by `group` (e.g. `Crops`,
#'   `Livestock`).
#' @export
load_commodity_groups <- function() {
  file <- here::here("data-raw", "VariableMapping", "FASOM_submap_commodity_group.csv")
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  split(d$commodity, d$group)
}

# Log-type prefixes used in FOR_Forest_To_Mill.
.SOFTWOOD_LOGS <- c("SW_SawLogs", "SW_PulpLogs", "SW_LogRes")
.HARDWOOD_LOGS <- c("HW_SawLogs", "HW_PulpLogs", "HW_LogRes")

#' Classify FASOM log types into softwood / hardwood
#'
#' @param log_type Character vector of log type codes.
#' @return Character vector of "Softwood" / "Hardwood" / NA_character_.
#' @export
classify_log_type <- function(log_type) {
  out <- rep(NA_character_, length(log_type))
  out[log_type %in% .SOFTWOOD_LOGS] <- "Softwood"
  out[log_type %in% .HARDWOOD_LOGS] <- "Hardwood"
  out
}

#' Categories to include in wheat supercategory
.WHEAT <- c("HardRedSpringWheat", "HardRedWinterWheat", "SoftRedWinterWheat", "SoftWhiteWheat", "DurumWheat")

#' Aggregate FASOM wheat subcrops into top-level wheat category
#' 
#'
aggregate_wheat <- function(df) {
  
}

#' Aggregate a long-format telescope frame over unspecified columns
#'
#' Convenience wrapper for the repeated
#' `group_by(...) |> summarise(sum, na.rm = TRUE) |> mutate(variable = "…")`
#' idiom in the figure code.
#'
#' @param df Data frame.
#' @param by Character vector of columns to group by.
#' @param value_col Column to sum (default `"value"`).
#' @param variable_label Optional label to write into `variable` after
#'   aggregation. `NULL` (default) leaves `variable` untouched.
#' @return A summarised tibble.
#' @export
aggregate_by <- function(df, by, value_col = "value", variable_label = NULL) {
  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(value_col),
                                   function(v) sum(v, na.rm = TRUE)),
                     .groups = "drop")
  if (!is.null(variable_label)) {
    out <- dplyr::mutate(out, variable = variable_label)
  }
  out
}

#' Convert a level series to % change vs. a baseline scenario
#'
#' Ported from `resource/app.R::pct_from_base()`. For each combination of
#' `join_cols`, finds the row whose `scenario_col == baseline` and returns
#' the other rows with `value_col` replaced by
#' `100 * (value - value_ref) / value_ref`. Emits warnings (not errors) for
#' the three common failure modes so callers can still see a plot when the
#' data is imperfect:
#'
#'   * missing baseline row (function returns an empty frame)
#'   * duplicate baseline rows per join key (uses first, warns on count)
#'   * zero / NA baseline denominators (yields Inf / NaN, warns on count)
#'
#' Usable from both the Shiny app (toggle "% change" view) and RMarkdown
#' chunks (call directly before `plotting()`).
#'
#' @param df Data frame.
#' @param join_cols Character vector of columns identifying a matched
#'   baseline row per scenario (e.g. `c("year", "commodity")`).
#' @param baseline Scenario value used as the denominator. Default
#'   `"BASE"`.
#' @param scenario_col Column holding scenario codes. Default `"scenario"`.
#' @param value_col Column to convert. Default `"value"`.
#' @return `df` with `value_col` replaced by pct-change and baseline rows
#'   filtered out. Non-baseline rows without a matching baseline yield
#'   `NA_real_`.
#' @export
pct_from_base <- function(df, join_cols,
                          baseline = "BASE",
                          scenario_col = "scenario",
                          value_col = "value") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!scenario_col %in% names(df)) {
    warning("pct_from_base(): scenario column '", scenario_col,
            "' not found; returning input unchanged.")
    return(df)
  }
  if (!value_col %in% names(df)) {
    warning("pct_from_base(): value column '", value_col,
            "' not found; returning input unchanged.")
    return(df)
  }

  join_cols <- intersect(join_cols, names(df))
  join_cols <- setdiff(join_cols, c(scenario_col, value_col))
  if (!length(join_cols)) {
    warning("pct_from_base(): no usable join columns after excluding ",
            "scenario/value; returning input unchanged.")
    return(df)
  }

  if (!baseline %in% df[[scenario_col]]) {
    warning("pct_from_base(): baseline '", baseline, "' not found in '",
            scenario_col, "'; returning empty frame.")
    return(df[0, , drop = FALSE])
  }

  base_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == baseline) %>%
    dplyr::select(dplyr::all_of(c(join_cols, value_col))) %>%
    dplyr::rename(.value_ref = dplyr::all_of(value_col))

  dup_check <- base_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.n != 1)
  if (nrow(dup_check) > 0) {
    warning("pct_from_base(): ", nrow(dup_check),
            " join key(s) have multiple baseline rows; the first is used.")
    base_df <- base_df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  bad_base <- base_df %>%
    dplyr::filter(is.na(.value_ref) | .value_ref == 0)
  if (nrow(bad_base) > 0) {
    warning("pct_from_base(): ", nrow(bad_base),
            " baseline value(s) are zero or NA \u2014 pct will be Inf/NaN.")
  }

  df %>%
    dplyr::filter(.data[[scenario_col]] != baseline) %>%
    dplyr::left_join(base_df, by = join_cols) %>%
    dplyr::mutate(!!dplyr::sym(value_col) :=
                    100 * (.data[[value_col]] - .value_ref) / .value_ref) %>%
    dplyr::select(-.value_ref)
}

# Preprocessor registry ---------------------------------------------------

# Names disallowed inside a preprocessor expression. Static AST scan; not a
# security boundary, just friction to catch accidents in a trusted CSV.
.PREPROCESSOR_BLOCKLIST <- c(
  "system", "system2", "shell", "Sys.setenv", "unlink", "file.remove",
  "file.rename", "download.file", "library", "require", "loadNamespace",
  "source", "eval", "evalq", "parse", "sys.call", "sys.function"
)

# Recursively scan an expression's call heads against the blocklist.
.scan_blocklist <- function(expr) {
  if (is.call(expr)) {
    head_name <- as.character(expr[[1]])
    if (length(head_name) == 1 && head_name %in% .PREPROCESSOR_BLOCKLIST) {
      stop("Preprocessor expression uses disallowed function: '", head_name, "'.")
    }
    for (i in seq_along(expr)) .scan_blocklist(expr[[i]])
  }
  invisible(TRUE)
}

#' Load the preprocessor registry (`config/preprocessor.csv`)
#'
#' @return A tibble with columns `name, description, applies_to, expr`.
#' @export
config_preprocessor <- function(filename = "preprocessor.csv") {
  file <- system.file("config", filename, package = "telescope")
  if (!nzchar(file) || !file.exists(file)) {
    return(tibble::tibble(name = character(), description = character(),
                          applies_to = character(), expr = character()))
  }
  readr::read_csv(file,
                  col_types = readr::cols(.default = readr::col_character()),
                  na = character())
}

#' Filter preprocessor registry entries that match a dataset path
#'
#' @param dataset_path Character vector of dataset relative paths (may be empty).
#' @param registry Optional pre-loaded registry (as returned by
#'   [config_preprocessor()]).
#' @return A tibble subset of `registry` whose `applies_to` regex matches any
#'   supplied dataset path (or matches `*`).
#' @export
preprocessors_for <- function(dataset_path, registry = config_preprocessor()) {
  if (!nrow(registry)) return(registry)
  if (!length(dataset_path)) return(registry[0, , drop = FALSE])
  keep <- vapply(registry$applies_to, function(pat) {
    if (identical(pat, "*") || !nzchar(pat)) return(TRUE)
    any(vapply(dataset_path,
               function(p) isTRUE(grepl(pat, p, perl = TRUE)),
               logical(1)))
  }, logical(1))
  registry[keep, , drop = FALSE]
}

#' Apply named preprocessors to a data frame
#'
#' Each preprocessor's `expr` column is parsed as a dplyr verb call (e.g.
#' `mutate(highlevel = str_extract(variable, "^[^_]+"))`) and piped onto `df`.
#' A static AST scan rejects a small blocklist of side-effecting names; the
#' CSV is treated as trusted input (allowlist enforcement would be more
#' restrictive but also more brittle for legitimate helpers).
#'
#' @param df Input data frame.
#' @param names Character vector of preprocessor names to apply (in order).
#' @param registry Optional pre-loaded registry.
#' @return The transformed data frame.
#' @export
apply_preprocessors <- function(df, names, registry = config_preprocessor()) {
  if (!length(names) || !nrow(registry)) return(df)
  keep <- registry$name %in% names
  if (!any(keep)) return(df)
  # Preserve caller-supplied order when possible.
  ordered <- registry[keep, , drop = FALSE]
  ordered <- ordered[match(intersect(names, ordered$name), ordered$name), ,
                     drop = FALSE]
  for (i in seq_len(nrow(ordered))) {
    expr_text <- ordered$expr[i]
    if (is.na(expr_text) || !nzchar(expr_text)) next
    parsed <- tryCatch(rlang::parse_expr(expr_text),
                       error = function(e) {
                         stop("Preprocessor '", ordered$name[i],
                              "' has invalid expression: ", conditionMessage(e))
                       })
    .scan_blocklist(parsed)
    df <- rlang::inject(df %>% !!parsed)
  }
  df
}
