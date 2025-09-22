
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

  card(
    #card_header("Data series"),
    layout_columns(col_widths = c(8, 4),
      selectInput("fb_variable", "Variable",
                  choices = config_variable(),
                  multiple = TRUE)
    )
  ),
  card(
    #card_header("Model selection"),
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
    )
  ),
  card(
    #card_header("Axes variables"),
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
    )
  ),

  card(
    #card_header("Figure details"),
    layout_columns(
      textInput("fb_title_name", "Figure title"),
      # Autogenerate based on variable choice or from mapping
      numericInput("fb_figure_no", "Figure number", value = 1)
    ),
    actionButton(
      "fb_figure_download",
      "Download Figure File"
    )
  )
)

# TODO: Add import and export functionality for figures and figure lists

fb_sidebar <- sidebar(id = "fb_sidebar", fb_sidebar_options, width = "30%")
fb_workbook <- nav_panel("Parameters", plotOutput("p_fb_figure", height = "600px"))
fb_output <- nav_panel("Data", DT::DTOutput("t_fb_data"))
fb_data <- nav_panel("Output", DT::DTOutput("t_fb_rows"))
#fb_debug <- nav_panel("Debug", DT::DTOutput("t_fb_debug"))

figureBuilderContent <- list(fb_workbook, fb_output, fb_data)
figureBuilder <- nav_panel("Figure builder",
                           navset_card_pill(sidebar = fb_sidebar,
                                            !!!figureBuilderContent,
                                            full_screen = TRUE))

## Data viewer ----
dataExplorer <- nav_panel("Figure explorer", "Content")

## Main UI ----
ui <- page_fluid(
  tags$head(tags$style(HTML("
    .bslib-card, .tab-content, .tab-pane, .card-body {
      overflow: visible !important;
    }
  "))),

  page_navbar(
    figureBuilder,
    dataExplorer,
    title = "Telescope",
    id = "page",
    fillable = TRUE
  )
)
