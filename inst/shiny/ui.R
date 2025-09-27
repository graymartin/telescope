
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
    start = "1900-01-01",
    end = "2200-01-01",
    min = "1900-01-01",
    max = "2200-01-01",
    format = "yyyy"),
  
  layout_columns(
    selectInput(
      "de_variable",
      "Variables",
      # TODO: Make dependent on de_dataset
      choices = NULL,
      multiple = TRUE),
    
    selectInput(
      "de_models",
      "Models",
      # TODO: Make dependent on de_dataset
      choices = NULL,
      multiple = TRUE)
  ),
  
  layout_columns(
    selectInput(
      "de_regions",
      "Regions",
      # TODO: Make dependent on de_dataset
      choices = NULL,
      multiple = TRUE
    ),
    
    selectInput(
      "de_scenarios",
      "Scenarios",
      # TODO: Make dependent on de_dataset
      choices = NULL,
      multiple = TRUE
    )
  ),
  
  layout_columns(
    # Constrain to different values
    selectInput("de_x", "Horizontal (x) axis",
                choices = dplyr::filter(config_axes(), axes == "x")$options),
    selectInput("de_y", "Vertical (y) axis",
                choices = dplyr::filter(config_axes(), axes == "y")$options)
  ),
  
  layout_columns(
    # Constrain to different values
    selectInput("de_facet1", "Horizontal facet",
                choices = c("None" = "", "scenario", "model", "region", "variable")),
    selectInput("de_facet2", "Vertical facet",
                choices = c("None" = "", "scenario", "model", "region", "variable"))
  ),
  
  layout_columns(
    radioButtons(
      "de_grouping",
      NULL,
      choices = list("Color by model" = "Color by model",
                     "Color by scenario" = "Color by scenario",
                     "Color by region" = "Color by region",
                     "Color by variable" = "Color by variable")
    ),
    
    checkboxGroupInput(
      "de_options",
      NULL,
      choices = list("Start y axis at 0" = "Start y axis at 0",
                     "Aggregate variables" = "Aggregate variables",
                     "Fix vertical facet scale" = "Fix vertical facet scale",
                     "Show plot details" = "Show plot details"),
      selected = list("Start y axis at 0", "Aggregate variables")
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
