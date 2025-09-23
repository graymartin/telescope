
# UI ----------------------------------------------------------------------
## Figure builder ----
fb_sidebar_options <- list(
  selectInput(
    "fb_figure_type",
    "Figure type",
    choices = list_figure_type(),
    multiple = FALSE
    ),

  dateRangeInput(
    "fb_years",
    "Years",
    startview = "decade",
    start = "2000-01-01",
    end = "2050-01-01",
    min = "1900-01-01",
    max = "2200-01-01",
    format = "yyyy"),

  selectInput("fb_variable", "Variable",
              choices = config_variable(),
              multiple = TRUE),
  
  layout_columns(
    selectInput("fb_models", "Model",
                choices = config_model(),
                multiple = TRUE),
    # Sort by model or filter to selected models
    selectInput("fb_scenarios", "Scenario",
                choices = config_scenario(),
                multiple = TRUE)
  ),
      
  layout_columns(
    selectInput("fb_regions", "Region",
                choices = config_region(),
                multiple = TRUE),
    # Sort by model or filter to selected models
    selectInput("fb_color", "Color group",
                choices = c("variable", "scenario", "model", "region"))
  ),
  
  layout_columns(
    # Constrain to different values
    selectInput("fb_x", "Horizontal (x) axis",
                choices = dplyr::filter(config_axes(), axes == "x")$options),
    selectInput("fb_y", "Vertical (y) axis",
                choices = dplyr::filter(config_axes(), axes == "y")$options)
  ),
  
  layout_columns(
    # Constrain to different values
    selectInput("fb_facet1", "Horizontal facet",
                choices = c("None" = "", "scenario", "model", "region")),
    selectInput("fb_facet2", "Vertical facet",
                choices = c("None" = "", "scenario", "model", "region"))
  ),

  layout_columns(
    textInput("fb_title_name", "Figure title"),
    # Autogenerate based on variable choice or from mapping
    numericInput("fb_figure_no", "Figure number", value = 1)
  ),
  
  layout_columns(
    actionButton(
      "fb_figure_save",
      "Save Mapping"),
    downloadButton(
      "fb_figure_download",
      "Download Mapping")
  )
)

# TODO: Add import and export functionality for figures and figure lists

fb_sidebar <- sidebar(id = "fb_sidebar", fb_sidebar_options, width = "30%")
fb_workbook <- nav_panel("Parameters", plotOutput("p_fb_figure", height = "600px"))
fb_output <- nav_panel("Filtered data", DT::DTOutput("t_fb_data"))
fb_data <- nav_panel("Output", DT::DTOutput("t_fb_rows"))
#fb_debug <- nav_panel("Debug", DT::DTOutput("t_fb_debug"))

figureBuilderContent <- list(fb_workbook, fb_output, fb_data)
figureBuilder <- nav_panel("Figure builder",
                           navset_card_pill(sidebar = fb_sidebar,
                                            !!!figureBuilderContent,
                                            full_screen = TRUE))

## Set viewer ----
fs_sidebar_options <- list(
  actionButton("navbar_analysis_select", "Select analysis"),
  textOutput("s_fs_data")
)
fs_sidebar <- sidebar(id = "fs_sidebar", fs_sidebar_options, width = "20%")
fs_figureset <- nav_panel("Saved figures", DT::DTOutput("t_fs_data"))
figureSetContent <- list(fs_figureset)

figureSet <- nav_panel("Set viewer", navset_card_pill(sidebar = fs_sidebar,
                                                              !!!figureSetContent,
                                                              full_screen = TRUE))

## Data explorer ----
de_sidebar_options <- list(
  dateRangeInput(
    "de_years",
    "Years",
    startview = "decade",
    start = "2000-01-01",
    end = "2050-01-01",
    min = "1900-01-01",
    max = "2200-01-01",
    format = "yyyy"),
  
  selectInput(
    "de_dataset",
    "Datasets",
    choices = config_dataset(),
    multiple = TRUE
  ),
  
  selectInput(
    "de_models",
    "Models",
    # TODO: Make dependent on de_dataset
    choices = NULL,
    multiple = TRUE
  ),
  
  selectInput(
    "de_variable",
    "Variables",
    # TODO: Make dependent on de_dataset
    choices = NULL,
    multiple = TRUE),
  
  selectInput(
    "de_regions",
    "Regions",
    # TODO: Make dependent on de_dataset
    choices = NULL,
    multiple = TRUE,
    selected = list("All regions")
  ),
  
  selectInput(
    "de_scenarios",
    "Scenarios",
    # TODO: Make dependent on de_dataset
    choices = NULL,
    multiple = TRUE,
    selected = list("All scenarios")
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
      choices = list("Group by model" = "Group by model",
                     "Group by scenario" = "Group by scenario",
                     "Group by region" = "Group by region",
                     "Group by variable" = "Group by variable")
    ),
    
    checkboxGroupInput(
      "de_options",
      NULL,
      choices = list("Start y axis at 0" = "Start y axis at 0",
                     "Aggregate variables" = "Aggregate variables"),
      selected = list("Start y axis at 0", "Aggregate variables")
    )
  )
)

de_sidebar <- sidebar(id = "de_sidebar", de_sidebar_options, width = "30%")
de_figureset <- nav_panel("Plot", plotOutput("p_de_figure"))
de_filtereddataview <- nav_panel("Filtered data", DT::DTOutput("t_de_filtered_data"))
de_dataview <- nav_panel("Full data", DT::DTOutput("t_de_data"))
dataExplorerContent <- list(de_figureset, de_filtereddataview, de_dataview)

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
    figureBuilder,
    figureSet,
    dataExplorer,
    nav_spacer(),
    nav_item(textOutput("navbar_analysis_text")),
    nav_item(actionButton("navbar_analysis_select", "Select analysis")), 
    title = "Telescope",
    id = "page",
    fillable = TRUE
  )
)
