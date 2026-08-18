
# UI ----------------------------------------------------------------------
## Set viewer ----
fs_sidebar_options <- list(
  layout_columns(
    actionButton("navbar_analysis_select", "Select analysis directory"),
    actionButton("fs_set_save", "Save combined analysis set")
    ),
  textInput("fs_set_filename", "Filename"),
  checkboxInput("fs_overwrite", "Overwrite", TRUE),
  actionButton("fs_images_save", "Save plot images")
)
fs_sidebar <- sidebar(id = "fs_sidebar", fs_sidebar_options, width = "20%")
fs_figureset <- nav_panel("Saved figures", DT::DTOutput("t_fs_data"))
figureSetContent <- list(fs_figureset)

figureSet <- nav_panel("Set viewer", navset_card_pill(sidebar = fs_sidebar,
                                                              !!!figureSetContent,
                                                              full_screen = TRUE))

## Data explorer ----
de_sidebar_options <- list(
  layout_columns(
    selectInput(
      "de_dataset",
      "Datasets",
      choices = config_dataset(),
      multiple = TRUE
    ),
    
    selectInput(
      "de_figure_type",
      "Figure type",
      choices = list_figure_type(),
      multiple = FALSE)
  ),
  
  dateRangeInput(
    "de_years",
    "Years",
    startview = "decade",
    start = "2020-01-01",
    end = "2100-01-01",
    min = "1900-01-01",
    max = "2200-01-01",
    format = "yyyy"),
  
  layout_columns(
    selectInput(
      "de_variable",
      "Variables",
      # Populated from the loaded dataset by the server.
      choices = NULL,
      multiple = TRUE),
    
    selectInput(
      "de_models",
      "Models",
      choices = NULL,
      multiple = TRUE)
  ),
  
  layout_columns(
    selectInput(
      "de_regions",
      "Regions",
      choices = NULL,
      multiple = TRUE
    ),
    
    selectInput(
      "de_scenarios",
      "Scenarios",
      choices = NULL,
      multiple = TRUE
    )
  ),
  
  # Preset selections: pick a named filter set from config/preset.csv and
  # apply it to the corresponding column selector.
  layout_columns(
    selectInput("de_preset", "Preset", choices = character(0)),
    div(style = "margin-top: 25px;", actionButton("de_preset_apply", "Apply preset"))
  ),
  # Ad-hoc row-level filters on arbitrary columns. Each row is a
  # (column, values) pair; rendered dynamically by the server.
  tags$details(
    tags$summary("Custom column filters"),
    div(id = "de_dynfilter_container",
        style = "margin-top: 8px;",
        uiOutput("de_dynfilter_rows")),
    actionButton("de_dynfilter_add", "+ Add filter",
                 class = "btn-sm", style = "margin-top: 6px;")
  ),
  # x/y/facet/color/linetype pickers accept any column from the loaded
  # dataset (populated by the server), so derived columns like `highlevel`
  # are prototype-able without editing the app.
  layout_columns(
    selectInput("de_x", "Horizontal (x) axis", choices = NULL),
    selectInput("de_y", "Vertical (y) axis",   choices = NULL)
  ),
  
  # allowEmptyOption keeps the "None" (value "") entry selectable after the
  # user has picked a real column; without it selectize.js treats it as a
  # one-shot placeholder and hides it from the dropdown.
  layout_columns(
    selectizeInput("de_facet1", "Horizontal facet", choices = NULL,
                   options = list(allowEmptyOption = TRUE)),
    selectizeInput("de_facet2", "Vertical facet",   choices = NULL,
                   options = list(allowEmptyOption = TRUE))
  ),
  
  layout_columns(
    selectInput("de_color", "Color / group by", choices = NULL),
    selectizeInput("de_linetype", "Linetype by", choices = NULL,
                   options = list(allowEmptyOption = TRUE))
  ),

  checkboxGroupInput(
    "de_options",
    NULL,
    choices = list("Start y axis at 0" = "Start y axis at 0",
                   "Aggregate variables" = "Aggregate variables",
                   "Show plot details" = "Show plot details"),
    selected = list("Start y axis at 0", "Aggregate variables")
  ),

  # View mode: raw levels vs. % change relative to a baseline scenario.
  # The baseline value stays free-text so users on non-FASOM scenario
  # naming can adapt without editing the app.
  layout_columns(
    radioButtons(
      "de_view",
      "View",
      choices = c("Levels" = "levels", "% change vs. baseline" = "pct"),
      selected = "levels",
      inline = TRUE
    ),
    textInput("de_pct_baseline", "Baseline scenario", value = "BASE")
  ),

  # Styling block: everything that maps to a fb_* parameter on plotting()
  # beyond the required minimal set.
  accordion(
    open = FALSE,
    accordion_panel(
      "Styling",
      checkboxInput("de_autotitle",
                    "Auto-populate title & labels from data",
                    value = FALSE),
      layout_columns(
        textInput("de_x_title", "X-axis title"),
        textInput("de_y_title", "Y-axis title")
      ),
      layout_columns(
        textInput("de_x_units", "X-axis units"),
        textInput("de_y_units", "Y-axis units")
      ),
      textInput("de_subtitle", "Subtitle"),
      textInput("de_caption",  "Caption"),
      layout_columns(
        numericInput("de_facet_ncol", "Facet columns",
                     value = NA, min = 1, step = 1),
        selectInput("de_facet_scales", "Facet scales",
                    choices = c("free", "free_x", "free_y", "fixed"),
                    selected = "free_y")
      ),
      textInput("de_x_breaks",
                "X-axis breaks (comma-separated)",
                placeholder = "e.g. 2020, 2030, 2050"),
      radioButtons(
        "de_palette",
        "Color palette",
        choices = c("Telescope (default)" = "telescope",
                    "FASOM ozone scenario" = "fasom_scenario"),
        selected = "telescope",
        inline = TRUE
      ),
      checkboxGroupInput(
        "de_styling_toggles",
        NULL,
        choices = c("Show points"    = "points",
                    "Zero reference line" = "hline0",
                    "Data labels"    = "labels"),
        inline = TRUE
      ),
      # Preprocessors: registry-driven derived-column steps. Choices are
      # filtered per-dataset by the server.
      checkboxGroupInput("de_preprocessors", "Data preprocessors",
                         choices = character(0))
    )
  ),
  
  layout_columns(
    textInput("de_title_name", "Figure title"),
    numericInput("de_figure_no", "Figure number", value = 1)
  ),
  
  layout_columns(
    actionButton(
      "de_figure_save",
      "Save Mapping"),
    downloadButton(
      "de_figure_download",
      "Download Image")
  ),
  layout_columns(
    actionButton("de_script_preview", "Preview R script"),
    downloadButton("de_script_download", "Download R script")
  )
)

de_sidebar <- sidebar(id = "de_sidebar", de_sidebar_options, width = "30%")
de_figureset <- nav_panel("Plot", plotOutput("p_de_figure"))
de_debug <- nav_panel("Figure mapping", DT::DTOutput("t_de_rows"))
de_filtereddataview <- nav_panel("Filtered data", DT::DTOutput("t_de_filtered_data"))
de_dataview <- nav_panel("Full data", DT::DTOutput("t_de_data"))
dataExplorerContent <- list(de_figureset, de_debug, de_filtereddataview, de_dataview)

dataExplorer <- nav_panel("Data explorer", navset_card_pill(sidebar = de_sidebar,
                                                              !!!dataExplorerContent,
                                                              full_screen = TRUE))

## Main UI ----
ui <- page_fluid(
  tags$head(tags$style(HTML("
    .bslib-card, .tab-content, .tab-pane, .card-body {
      overflow: visible !important;
    }
    /* Restore independent scroll on the sidebar; the rule above lets
       selectize dropdowns escape card bounds but also disables the
       sidebar's own overflow. */
    .bslib-sidebar-layout > .sidebar,
    .bslib-sidebar-layout > .sidebar > .sidebar-content,
    .bslib-sidebar-layout > .sidebar .card-body {
      overflow-y: auto !important;
      max-height: calc(100vh - 180px);
    }
  "))),

  page_navbar(
    dataExplorer,
    figureSet,
    nav_spacer(),
    nav_item(textOutput("navbar_analysis_text")),
    nav_item(actionButton("navbar_analysis_select", "Select analysis directory")), 
    title = "Telescope",
    id = "page",
    fillable = TRUE
  )
)
