#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#source("utilities.R", local = TRUE)
cat_select_list <- category_select_list()
dat_select_list <- dataset_select_list()

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Validation Overview"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
              "year_range",
               "Years",
               startview = "decade",
               start = "2000-01-01",
               end = "2050-01-01",
               min = "1990-01-01",
               max = "2050-01-01",
               format = "yyyy"),

            checkboxGroupInput(
              "dataset",
              "Datasets",
              choices = dat_select_list
            ),

            checkboxGroupInput(
              "scenario",
              "Scenarios",
              choices = list("Historical" = "historical",
                             "WM" = "WM",
                             "WM-lowseq" = "WM-lowseq",
                             "WM-highseq" = "WM-highseq"),
              selected = c("historical", "WM")),

            selectInput(
              "category",
              "Categories",
              choices = cat_select_list,
              multiple = TRUE),

            # checkboxGroupInput(
            #   "showcategoryoptions",
            #   NULL,
            #   choices = list("Show all categories" = "show_all"),
            #   selected = list("Show all categories")
            # ),

            radioButtons(
              "groupoptions",
              NULL,
              choices = list("No grouping" = "",
                             "Group by category" = "Group by category",
                             "Break out gases" = "Break out gases",
                             "Aggregate categories" = "Aggregate categories")
            ),

            checkboxGroupInput(
              "options",
              "Options",
              choices = list("Start y axis at 0" = "Start y axis at 0",
                             #"Compare scenarios" = "Compare scenarios",
                             "Include CO2" = "Include CO2",
                             "Totals only" = "Totals only"),
              selected = list("Start y axis at 0", "Include CO2")
            ),

            textInput(
              "input_filename",
              NULL,
              placeholder = "Filename"
            ),

            downloadButton(
              "downloadplotbutton",
              "Download Plot"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          navbarPage(title = "Views", id = "Views",
                     tabPanel("Plot", plotOutput("sectorPlot", height = "600px")),
                     tabPanel("Table", dataTableOutput("sectorTable")),
                     navbarMenu("Summary",
                                tabPanel("Full dataset", dataTableOutput("fullSummaryTable")),
                                tabPanel("Wide dataset", DT::dataTableOutput("wideSummaryTable")))
                     )
        )
    )
))
