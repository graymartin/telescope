

# Setup -------------------------------------------------------------------
c_figure_type <- config_figure_type()

# Local helpers -----------------------------------------------------------
# rlang::`%||%` isn't imported into the shiny app namespace, so define locally.
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
# Coerce "" / NULL to NULL so downstream plotting() falls back on its defaults
# instead of receiving an empty string.
.nz <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 1L && (is.na(x) || !nzchar(x))) return(NULL)
  x
}

# Server ------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  ## Input handling ----
  s_de_years <- reactive({
    years <- paste0(str_sub(input$de_years[[1]], 1, 4), "-", str_sub(input$de_years[[2]], 1, 4))
    
    return(years)
  })

  s_de_figure_no <- reactive({
    figure_no <- as.character(input$de_figure_no[[1]])

    return(figure_no)
  })

  ## Debugging  ----
  ### Required and optional figure parameters ----
  df_de_debug <- reactive({
    req(input$de_figure_type)
    base_table <- filter(c_figure_type, figure_type == input$de_figure_type)

    output_table <-
      base_table %>%
      mutate(selected = "")

    for (i in 1:nrow(output_table)) {
      var <- paste0("fb_", output_table[i, "variable"])
      output_table[i, "selected"] <- paste0(paste(as.character(input[[var]]), collapse = ", "), "")
    }

    return(output_table)

  })

  output$t_de_debug <- DT::renderDT(df_de_debug())
  
  ## Figure builder ----
  ### Figure builder rows ----
  df_rows <- reactive({
    if (is.null(input$de_models)) {
      de_models <- as.list(unique(df_de_data()$model))
    } else {
      de_models <- input$de_models
    }

    if (is.null(input$de_scenarios)) {
      de_scenarios <- as.list(unique(df_de_data()$scenario))
    } else {
      de_scenarios <- input$de_scenarios
    }

    if (is.null(input$de_regions)) {
      de_regions <- as.list(unique(df_de_data()$region))
    } else {
      de_regions <- input$de_regions
    }

    toggles <- input$de_styling_toggles
    is_pct  <- identical(input$de_view, "pct")

    df_r <- var_to_figdf(dataset = paste0(input$de_dataset, collapse = ", "),
                         figtype = input$de_figure_type,
                         fb_title_name = input$de_title_name,
                         fb_figure_no = s_de_figure_no(),
                         fb_x = input$de_x,
                         fb_y = input$de_y,
                         fb_color = input$de_color,
                         fb_regions = de_regions,
                         fb_models = de_models,
                         fb_years = s_de_years(),
                         fb_scenarios = de_scenarios,
                         fb_variable = input$de_variable,
                         fb_facet1 = input$de_facet1,
                         fb_facet2 = input$de_facet2,
                         fb_options = paste0(input$de_options, collapse = ", "),
                         fb_x_title = input$de_x_title,
                         fb_y_title = input$de_y_title,
                         fb_x_units = input$de_x_units,
                         fb_y_units = input$de_y_units,
                         fb_subtitle = input$de_subtitle,
                         fb_caption  = input$de_caption,
                         fb_facet_scales = input$de_facet_scales,
                         fb_facet_ncol   = if (is.null(input$de_facet_ncol) ||
                                               is.na(input$de_facet_ncol)) ""
                                           else as.character(input$de_facet_ncol),
                         fb_points = "points" %in% toggles,
                         fb_hline0 = "hline0" %in% toggles,
                         fb_labels = "labels" %in% toggles,
                         fb_linetype = input$de_linetype %||% "",
                         fb_palette  = input$de_palette,
                         fb_pct_change   = is_pct,
                         fb_pct_baseline = input$de_pct_baseline,
                         fb_preprocessors = input$de_preprocessors %||% character(0),
                         fb_x_breaks = input$de_x_breaks %||% "")

    file <- system.file("output", "intermediate", package = "telescope")
    saveRDS(df_r, file = paste0(file, "/", "de_figure_data.rds"))

    return(df_r)
  })

  output$t_de_rows <- DT::renderDT(df_rows())

  ### Figure name ----
  s_de_figure_filename <- reactive({
    req(input$de_figure_no)

    filename <- paste0("figure_", s_de_figure_no(), ".csv")
    
    return(filename)
  })
  
  s_de_figure_image_filename <- reactive({
    req(input$de_figure_no)
    
    filename <- paste0("figure_", s_de_figure_no(), ".png")
    
    return(filename)
  })

  observe({
    if (isTRUE(input$de_autotitle)) return()
    updateTextInput(
      inputId = "de_title_name",
      value = s_de_figure_no()
    )
  })

  ### Download figure file ----
  output$de_figure_download <- downloadHandler(
    filename = s_de_figure_image_filename(),
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = 7, res = 300, units = "in")
      ggsave(p_de_figure(), filename = file, device = device)
    })

  ### Export R script ----
  # Emit a `plotting()`-wrapped call so a colleague can source() the file
  # in a clean R session and reproduce the plot without opening the app.
  s_de_script <- reactive({
    req(input$de_dataset, input$de_figure_type, input$de_x, input$de_y)
    toggles <- input$de_styling_toggles
    is_pct  <- identical(input$de_view, "pct")
    figure_to_script(
      dataset = as.character(input$de_dataset),
      figtype = input$de_figure_type,
      fb_dynfilters = dynfilter_specs(),
      fb_title_name = input$de_title_name %||% "",
      fb_figure_no  = "",
      fb_x = input$de_x, fb_y = input$de_y,
      fb_color = input$de_color,
      fb_regions = input$de_regions,
      fb_models  = input$de_models,
      fb_years   = s_de_years(),
      fb_scenarios = input$de_scenarios,
      fb_variable  = input$de_variable,
      fb_facet1 = input$de_facet1 %||% "",
      fb_facet2 = input$de_facet2 %||% "",
      fb_options = paste0(input$de_options, collapse = ", "),
      fb_x_title  = .nz(input$de_x_title),
      fb_y_title  = .nz(input$de_y_title),
      fb_x_units  = .nz(input$de_x_units),
      fb_y_units  = .nz(input$de_y_units),
      fb_subtitle = .nz(input$de_subtitle),
      fb_caption  = .nz(input$de_caption),
      fb_facet_scales = input$de_facet_scales %||% "free_y",
      fb_facet_ncol = if (is.null(input$de_facet_ncol) ||
                          is.na(input$de_facet_ncol)) NULL
                      else as.integer(input$de_facet_ncol),
      fb_points = "points" %in% toggles,
      fb_hline0 = "hline0" %in% toggles,
      fb_labels = "labels" %in% toggles,
      fb_linetype = .nz(input$de_linetype),
      fb_palette  = input$de_palette %||% "telescope",
      fb_pct_change   = is_pct,
      fb_pct_baseline = input$de_pct_baseline %||% "BASE",
      fb_preprocessors = input$de_preprocessors,
      fb_x_breaks = .nz(input$de_x_breaks)
    )
  })

  output$de_script_download <- downloadHandler(
    filename = function() paste0("figure_", s_de_figure_no(), ".R"),
    content = function(file) writeLines(s_de_script(), file)
  )

  observeEvent(input$de_script_preview, {
    showModal(modalDialog(
      title = "R script",
      tags$pre(style = "white-space: pre-wrap; max-height: 60vh; overflow: auto;",
               s_de_script()),
      easyClose = TRUE, size = "l",
      footer = tagList(modalButton("Close"),
                       downloadButton("de_script_download", "Download"))
    ))
  })
  
  ### Save figure file ----
  observeEvent(input$de_figure_save, {
    file <- system.file("output", "figure", s_main_analysis(), package = "telescope")
    # TODO: Check for overwrite
    write.csv(df_rows(), paste0(file, "/", s_de_figure_filename()), row.names = FALSE)
    updateNumericInput(
      inputId = "de_figure_no",
      value = input$de_figure_no + 1
    )
  })
  
  ## Figure set ----
  ### Set selection ----
  s_main_analysis <- reactiveVal(value = "default")
  
  observeEvent({
    input$navbar_analysis_select
  }, {
    showModal(modalDialog(card(
      layout_columns(
        selectInput("modal_main_analysis", "Select existing analysis", choices = config_analysis(), selected = "default"),
        textInput("modal_main_new", "Create new analysis", placeholder = "New analysis")
      ),
      layout_columns(actionButton("modal_main_submit", "Load"), actionButton("modal_main_create", "Create"))
    ), easyClose = TRUE))
  })
  
  observeEvent({input$modal_main_create}, {
    s_main_analysis(input$modal_main_new)
    figure_dir <- system.file("output", "figure", package = "telescope")
    dir.create(paste0(figure_dir, "/", input$modal_main_new), showWarnings = FALSE)
    updateSelectInput(
      inputId = "modal_main_analysis",
      selected  = input$modal_main_new,
      choices = config_analysis()
    )
  })
  
  observeEvent({input$modal_main_submit}, {
    s_main_analysis(input$modal_main_analysis)
  })
  
  output$navbar_analysis_text <- renderText({s_main_analysis()})
  
  ### Show figures from analysis ----
  df_fs_data <- reactivePoll(3000, session, 
                             checkFunc = function () {
                               dir <- system.file("output", "figure", s_main_analysis(), package = "telescope")
                               return(list.files(dir, full.names = FALSE, recursive = FALSE))
                             }, 
                             valueFunc = function () {
                               return(figures_to_figdf(figure_dir = s_main_analysis()))
                             })
  
  output$t_fs_data <- DT::renderDT(
    datatable(df_fs_data(), selection = "single")
    )
  
  s_fs_data <- reactive({
    req(input$t_fs_data_rows_selected)
    return(df_fs_data()[[input$t_fs_data_rows_selected, "source"]])
  })
  
  output$s_fs_data <- renderText(s_fs_data())
  
  ### Save figures from analysis ----
  observeEvent({input$fs_images_save}, {
    batching(figdf_to_var(df_fs_data()), format_list = c("png"))
  })
  
  ## Data explorer ----
  ### Dataset combination ----
  df_de_datasets <- reactive({
    req(input$de_dataset)
    
    out_dataset <- dataset_access(input$de_dataset)
    
    return(out_dataset)
  })
  
  # Preprocessors are applied here (before filtering) so derived columns
  # like `highlevel` show up in the column pickers and preview tables.
  # plotting() is called with fb_preprocessors = NULL at runtime to avoid
  # double-application, but df_rows() still persists the selection so
  # batching() can replay from raw data.
  df_de_preprocessed <- reactive({
    df <- df_de_datasets()
    if (!length(input$de_preprocessors)) return(df)
    apply_preprocessors(df, input$de_preprocessors)
  })
  
  output$t_de_data <- DT::renderDT(df_de_datasets())
  
  ### Dataset reactive variables ----
  # Names of columns available in the currently loaded dataset(s). Feeds
  # every free-form column picker (x, y, color, linetype, facets). Reads
  # from the preprocessed frame so derived columns appear.
  de_columns <- reactive({
    df <- df_de_preprocessed()
    if (!is.data.frame(df) || !ncol(df)) return(character(0))
    names(df)
  })

  observeEvent({input$de_dataset}, {
    
    #### Models ----
    updateSelectInput(
      inputId = "de_models",
      choices = config_model(df = df_de_datasets())
    )
    
    #### Variable ----
    updateSelectInput(
      inputId = "de_variable",
      choices = config_variable(df = df_de_datasets())
    )
    
    #### Regions ----
    updateSelectInput(
      inputId = "de_regions",
      choices = config_region(df = df_de_datasets())
    )
    
    #### Scenarios ----
    updateSelectInput(
      inputId = "de_scenarios",
      choices = config_scenario(df = df_de_datasets())
    )

    #### Column pickers ----
    cols <- de_columns()
    facet_choices <- c("None" = "", cols)
    # Sensible defaults that preserve prior behavior when the columns exist.
    pick <- function(pref, cols) {
      hit <- intersect(pref, cols)
      if (length(hit)) hit[1] else if (length(cols)) cols[1] else NULL
    }
    updateSelectInput(session, "de_x",
                      choices = cols, selected = pick(c("year", "variable"), cols))
    updateSelectInput(session, "de_y",
                      choices = cols, selected = pick(c("value"), cols))
    updateSelectInput(session, "de_color",
                      choices = cols, selected = pick(c("scenario", "variable"), cols))
    updateSelectInput(session, "de_linetype",
                      choices = facet_choices, selected = "")
    updateSelectInput(session, "de_facet1",
                      choices = facet_choices, selected = "")
    updateSelectInput(session, "de_facet2",
                      choices = facet_choices, selected = "")

    #### Preprocessors ----
    reg <- preprocessors_for(dataset_path = input$de_dataset)
    choices <- if (nrow(reg)) stats::setNames(reg$name,
                                              paste0(reg$name, " \u2014 ",
                                                     reg$description))
               else character(0)
    updateCheckboxGroupInput(session, "de_preprocessors",
                             choices = choices, selected = character(0))

    #### Presets ----
    pre_reg <- presets_for(input$de_dataset)
    pre_choices <- if (nrow(pre_reg))
                     stats::setNames(pre_reg$name,
                                     paste0(pre_reg$label, " (",
                                            pre_reg$column, ")"))
                   else character(0)
    updateSelectInput(session, "de_preset",
                      choices = c("(none)" = "", pre_choices),
                      selected = "")
  })

  # Preset apply: look up the selected preset's column/values and push into
  # the matching column selector. `*` means "select every available choice".
  .PRESET_COL_INPUT <- c(variable = "de_variable",
                        scenario = "de_scenarios",
                        region   = "de_regions",
                        model    = "de_models")
  observeEvent(input$de_preset_apply, {
    req(input$de_preset, nzchar(input$de_preset))
    reg <- presets_for(input$de_dataset)
    row <- reg[reg$name == input$de_preset, , drop = FALSE]
    if (!nrow(row)) return()
    input_id <- .PRESET_COL_INPUT[[row$column[1]]]
    if (is.null(input_id)) {
      showNotification(paste0("Preset column '", row$column[1],
                              "' has no matching selector."),
                       type = "warning", duration = 4)
      return()
    }
    col <- row$column[1]
    df <- df_de_preprocessed()
    available <- if (is.data.frame(df) && col %in% names(df))
                    as.character(unique(df[[col]])) else character(0)

    values <- trimws(strsplit(row$values[1], ",", fixed = TRUE)[[1]])
    wildcard <- length(values) == 1L && identical(values, "*")
    if (wildcard) {
      matched <- available
      missing <- character(0)
    } else {
      matched <- intersect(values, available)
      missing <- setdiff(values, available)
    }
    updateSelectInput(session, input_id, selected = matched)

    # Build a compact toast: label, target column, and matched/missing counts.
    preview <- if (!length(matched)) "(nothing matched)"
               else if (length(matched) <= 5) paste(matched, collapse = ", ")
               else paste0(paste(matched[1:5], collapse = ", "),
                           ", + ", length(matched) - 5, " more")
    msg <- paste0(
      "Applied preset: ", row$label[1],
      " \u2192 ", col, " (", length(matched),
      if (wildcard) " total)" else paste0(" of ", length(values), " requested)"),
      "\n", preview
    )
    if (length(missing)) {
      msg <- paste0(msg, "\nNot found in data: ",
                    paste(missing, collapse = ", "))
    }
    showNotification(msg,
                     type = if (!length(matched)) "warning"
                            else if (length(missing)) "warning"
                            else "message",
                     duration = if (length(missing) || !length(matched)) 8 else 4)
  })

  # Warn (once per change) when the current color column has more distinct
  # values than the discrete palette can render.
  observeEvent(input$de_color, {
    df <- df_de_preprocessed()
    if (!is.data.frame(df) || !input$de_color %in% names(df)) return()
    n <- length(unique(df[[input$de_color]]))
    if (n > 12) {
      showNotification(
        paste0("Color column '", input$de_color, "' has ", n,
               " distinct values \u2014 the palette maxes out around 12. ",
               "Consider aggregating or faceting instead."),
        type = "warning", duration = 6
      )
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  ### Auto-populate title & labels ----
  # Track the last value each field was autofilled to; a subsequent autofill
  # only overwrites if the field is still that value (i.e. user hasn't edited).
  autofill_last <- reactiveValues()

  autofill_specs <- reactive({
    humanize <- function(x) {
      if (is.null(x) || !length(x)) return("")
      x <- as.character(x[[1]])
      if (is.na(x) || !nzchar(x)) return("")
      stringr::str_to_title(gsub("[_.]", " ", x))
    }
    ds <- input$de_dataset
    ds_base  <- if (length(ds)) tools::file_path_sans_ext(basename(ds[[1]])) else ""
    var_sel  <- input$de_variable
    scen_sel <- input$de_scenarios
    y_col    <- input$de_y

    title <- if (length(var_sel) == 1L && nzchar(var_sel)) var_sel
             else humanize(ds_base)
    y_title <- if (identical(y_col, "value") &&
                   length(var_sel) == 1L && nzchar(var_sel)) var_sel
               else humanize(y_col)

    # Pull units from the filtered frame when the column resolves to a single
    # non-empty value; suppress if validate() fails or values are mixed.
    y_units <- tryCatch({
      df <- df_de_data_bouncy()
      units_col <- intersect(c("units", "unit"), names(df))[1]
      if (is.data.frame(df) && !is.na(units_col)) {
        u <- unique(as.character(df[[units_col]]))
        u <- u[!is.na(u) & nzchar(u) & tolower(u) != "mixed"]
        if (length(u) == 1L) u else ""
      } else ""
    }, error = function(e) "")

    list(
      title_name = title,
      subtitle   = if (length(scen_sel) && length(scen_sel) <= 4L)
                     paste(scen_sel, collapse = ", ") else "",
      caption    = if (length(ds) && any(nzchar(ds)))
                     paste0("Source: ", paste(ds, collapse = ", ")) else "",
      x_title    = humanize(input$de_x),
      y_title    = y_title,
      y_units    = y_units
    )
  })

  observe({
    if (!isTRUE(input$de_autotitle)) return()
    specs <- autofill_specs()
    for (field in names(specs)) {
      input_id <- paste0("de_", field)
      current  <- input[[input_id]] %||% ""
      prev     <- isolate(autofill_last[[field]]) %||% ""
      if (!nzchar(current) || identical(current, prev)) {
        updateTextInput(session, input_id, value = specs[[field]])
        isolate(autofill_last[[field]] <- specs[[field]])
      }
    }
  })

  ### Custom column filters ----
  # Row IDs are stable integers; a monotonically-increasing counter avoids
  # ID reuse after row removal (Shiny caches removed inputs by ID).
  dynfilter_ids  <- reactiveVal(integer(0))
  dynfilter_next <- reactiveVal(1L)

  observeEvent(input$de_dynfilter_add, {
    new_id <- dynfilter_next()
    dynfilter_ids(c(dynfilter_ids(), new_id))
    dynfilter_next(new_id + 1L)
    # Local scope captures `new_id` in the observer closure; without it the
    # observer would see the latest counter value on every click.
    local({
      id <- new_id
      observeEvent(input[[paste0("de_dynfilter_rm_", id)]], {
        dynfilter_ids(setdiff(dynfilter_ids(), id))
      }, ignoreInit = TRUE)
    })
  })

  output$de_dynfilter_rows <- renderUI({
    ids <- dynfilter_ids()
    if (!length(ids)) return(NULL)
    rows <- lapply(ids, function(i) {
      col_id <- paste0("de_dynfilter_col_", i)
      val_id <- paste0("de_dynfilter_vals_", i)
      rm_id  <- paste0("de_dynfilter_rm_", i)
      div(
        style = "display: flex; gap: 6px; align-items: end; margin-bottom: 4px;",
        div(style = "flex: 1;",
            selectInput(col_id, NULL, choices = NULL)),
        div(style = "flex: 2;",
            selectInput(val_id, NULL, choices = NULL, multiple = TRUE)),
        div(actionButton(rm_id, "\u00d7",
                         class = "btn-danger btn-sm",
                         style = "padding: 4px 8px;"))
      )
    })
    tagList(rows)
  })

  # Column-choice population. `value` and `year` are excluded because they
  # have dedicated controls elsewhere (y-axis picker, year range).
  observe({
    ids <- dynfilter_ids()
    if (!length(ids)) return()
    cols <- setdiff(de_columns(),
                    c(input$de_y %||% "", "year", "value"))
    for (i in ids) {
      col_id <- paste0("de_dynfilter_col_", i)
      current <- isolate(input[[col_id]])
      updateSelectInput(session, col_id,
                        choices  = c("(select column)" = "", cols),
                        selected = if (!is.null(current) && current %in% cols) current else "")
    }
  })

  # Value-choice population, cascading off each row's column pick.
  observe({
    ids <- dynfilter_ids()
    if (!length(ids)) return()
    df <- df_de_preprocessed()
    for (i in ids) {
      col_id <- paste0("de_dynfilter_col_", i)
      val_id <- paste0("de_dynfilter_vals_", i)
      col <- input[[col_id]]
      if (!is.null(col) && nzchar(col) && col %in% names(df)) {
        vals <- as.character(sort(unique(df[[col]])))
        current <- isolate(input[[val_id]])
        updateSelectInput(session, val_id,
                          choices  = vals,
                          selected = intersect(current, vals))
      } else {
        updateSelectInput(session, val_id, choices = character(0), selected = character(0))
      }
    }
  })

  # Collapsed spec list consumed by df_de_data_bouncy. Rows with an empty
  # column or empty value list are silently dropped.
  dynfilter_specs <- reactive({
    ids <- dynfilter_ids()
    specs <- lapply(ids, function(i) {
      col  <- input[[paste0("de_dynfilter_col_", i)]]
      vals <- input[[paste0("de_dynfilter_vals_", i)]]
      if (is.null(col) || !nzchar(col) || is.null(vals) || !length(vals)) return(NULL)
      list(col = col, vals = as.character(vals))
    })
    specs[!vapply(specs, is.null, logical(1))]
  })

  ### Filtered data table ----
  df_de_data_bouncy <- reactive({
    validate(
      need(input$de_dataset, "Please select at least one dataset."),
      need(input$de_variable, "Please select at least one variable.")
    )
    
    full_data <- df_de_preprocessed()
    
    if (nrow(full_data) > 5e5) {
      if (input$de_figure_type == "MACC") {
        validate(
          need(input$de_regions, "Please select at least one region."),
          need(input$de_models, "Please select at least one GHG."),
          need(input$de_scenarios, "Please select at least one year.")
        )
      } else if (input$de_figure_type == "timeseries") {
        validate(
          need(input$de_regions, "Please select at least one region."),
          need(input$de_models, "Please select at least one model."),
          need(input$de_scenarios, "Please select at least one scenario.")
        )
      }
    }
    
    # In % change mode, the baseline scenario must survive the Shiny-side
    # pre-filter because the frame passed to plotting() is already filtered;
    # otherwise pct_from_base() finds no baseline row to divide by. Union
    # the baseline in silently — the user's scenario selection stays intact
    # in the UI, and pct_from_base drops the baseline before display.
    sce_filter <- input$de_scenarios
    if (identical(input$de_view, "pct") && !is.null(sce_filter) &&
        length(sce_filter) > 0 && nzchar(input$de_pct_baseline %||% "")) {
      sce_filter <- unique(c(sce_filter, input$de_pct_baseline))
    }

    df <- plotting_filter(
      full_data,
      reg_f = input$de_regions,
      mod_f = input$de_models,
      yrs_f = s_de_years(),
      sce_f = sce_filter,
      var_f = input$de_variable,
      y_col = input$de_y,
      options_list = input$de_options
    )

    # Custom column filters applied last so they can constrain any column,
    # including canonical ones the user may want to further narrow.
    for (spec in dynfilter_specs()) {
      if (spec$col %in% names(df)) {
        df <- dplyr::filter(df, .data[[spec$col]] %in% spec$vals)
      }
    }
    
    return(df)
  })
  
  df_de_data <- df_de_data_bouncy %>% throttle(100)
  
  output$t_de_filtered_data <- DT::renderDT(df_de_data())
  
  ### Data plot ----
  p_de_figure_bouncy <- reactive({
    req(input$de_dataset)
    req(input$de_variable)

    if ("Aggregate variables" %in% input$de_options) {
      de_variable <- c("Aggregated")
    } else {
      de_variable <- input$de_variable
    }

    toggles <- input$de_styling_toggles
    is_pct  <- identical(input$de_view, "pct")

    plot <- plotting(df = df_de_data(),
                     figtype = input$de_figure_type,
                     fb_title_name = input$de_title_name %||% "",
                     fb_figure_no = "",
                     fb_x = input$de_x,
                     fb_y = input$de_y,
                     fb_color = input$de_color,
                     fb_regions = input$de_regions,
                     fb_models = input$de_models,
                     fb_years = s_de_years(),
                     fb_scenarios = input$de_scenarios,
                     fb_variable = de_variable,
                     fb_facet1 = input$de_facet1,
                     fb_facet2 = input$de_facet2,
                     fb_options = paste0(input$de_options, collapse = ", "),
                     fb_x_title = .nz(input$de_x_title),
                     fb_y_title = .nz(input$de_y_title),
                     fb_x_units = .nz(input$de_x_units),
                     fb_y_units = .nz(input$de_y_units),
                     fb_subtitle = .nz(input$de_subtitle),
                     fb_caption  = .nz(input$de_caption),
                     fb_facet_scales = input$de_facet_scales %||% "free_y",
                     fb_facet_ncol   = if (is.null(input$de_facet_ncol) ||
                                           is.na(input$de_facet_ncol)) NULL
                                       else as.integer(input$de_facet_ncol),
                     fb_points = "points" %in% toggles,
                     fb_hline0 = "hline0" %in% toggles,
                     fb_labels = "labels" %in% toggles,
                     fb_linetype = .nz(input$de_linetype),
                     fb_palette  = input$de_palette %||% "telescope",
                     fb_pct_change   = is_pct,
                     fb_pct_baseline = input$de_pct_baseline %||% "BASE",
                     fb_preprocessors = NULL,
                     fb_x_breaks = .nz(input$de_x_breaks))

    return(plot)
  })
  
  p_de_figure <- p_de_figure_bouncy %>% debounce(100)
  
  output$p_de_figure <- renderPlot({
    p_de_figure()
  }, res = 130, height = 800)
  
  ## MACC ----
  ### UI update ----
  observeEvent({input$de_figure_type}, {
    if (input$de_figure_type == "MACC") {
      #### Models ----
      updateSelectInput(
        inputId = "de_models",
        label = "GHG"
      )
      
      #### Scenarios ----
      updateSelectInput(
        inputId = "de_scenarios",
        label = "Year"
      )
      
      #### Axes ----
      updateSelectInput(
        inputId = "de_x",
        choices = c("Q",
                    "QGHG"),
        selected = "Q"
      )
      
      updateSelectInput(
        inputId = "de_y",
        choices = c("p")
      )
    }
  })
  
  ## Figure set ----
  ### Figure set name ----
  s_fs_set_filename <- reactive({
    filename <- paste0("set_", s_main_analysis(), ".csv")
    
    return(filename)
  })
  
  observe({
    updateTextInput(
      inputId = "fs_set_filename",
      value = s_fs_set_filename()
    )
  })
  
  ### Save figure file ----
  observeEvent(input$fs_set_save, {
    file <- system.file("output", "mapping", package = "telescope")
    file_path <- paste0(file, "/", s_fs_set_filename())
    if (input$fs_overwrite) {
      write.csv(df_fs_data(), file_path, row.names = FALSE)
    } else if (file.exists(file_path)) {
      showModal( 
        modalDialog( 
          title = "Analysis set already exists", 
          easy_close = TRUE, 
          "To proceed, either change the output filename or allow overwriting." 
        ) 
      )
    } else {
      write.csv(df_fs_data(), file_path, row.names = FALSE)
    }
  })
  
})
