#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#source("utilities.R", local = TRUE)
cat_select_list <- category_select_list()
dat_select_list <- dataset_select_list()

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

    # Build table of data sources
    build_table <- reactive({
      req(input$category)
      req(input$dataset)

      # Iterate over datasets in list
      data_list <- list()
      for (d in input$dataset) {
        d_name <- names(dat_select_list)[dat_select_list == d]
        if (length(d_name) == 0) {
          d_name = d
        }

        dataset <-
          read.csv(d) %>%
          mutate(dataset_name = d_name)

        years_list <- c(1900:2500)
        years_list <- c(years_list, paste0("X", years_list), as.character(years_list))

        # Handle wide data by transposing to long format
        if (any(years_list %in% names(dataset))) {
          dataset <-
            pivot_longer(dataset, names_to = "year", cols = any_of(years_list)) %>%
            mutate(value = suppressWarnings(as.numeric(gsub(",", "", value)))) %>%
            mutate(year = as.numeric(str_sub(year, -4))) %>%
            mutate(type = "GHGI") # All wide data are flagged as historical/GHGI

        } else {
          dataset <-
            dataset %>%
            mutate(type = "Projections") # All long data are flagged as projections
        }

        # Add a scenario column if none exists in dataset
        if (!("scenario" %in% names(dataset))) {
          dataset$scenario <- "None"
        }

        # Handle different units in dataset (add manual exceptions as needed)
        if (("kt" %in% dataset$unit) | ("MT" %in% dataset$unit)) {
          dataset <-
            dataset %>%
            mutate(value = case_when(unit == "MT" ~ value * 1000,
                                     TRUE ~ value)) %>%
            mutate(unit = case_when(unit == "MT" ~ "kt",
                                    TRUE ~ unit)) %>%
            dplyr::filter(!is.na(unit)) %>%
            dplyr::filter(unit != "") %>%
            gwp_convert_to_MMTCO2e()
        }

        # Remove totals from categories with other gases or filter to totals only
        if (("Total" %in% dataset$gas) & !("Totals only" %in% input$options)) {
          dataset <-
            dataset %>%
            dplyr::filter(gas != "Total")

        } else if (("Total" %in% dataset$gas) & ("Totals only" %in% input$options)) {
          dataset <-
            dataset %>%
            dplyr::filter(gas == "Total")
        }

        # Allow filtering out CO2 from display
        if (("CO2" %in% dataset$gas) & !("Include CO2" %in% input$options)) {
          dataset <-
            dataset %>%
            dplyr::filter(gas != "CO2")
        }

        # Handle duplicated data rows in GHGI by taking only first row
        if ("table" %in% names(dataset)) {
          dataset <-
            dataset %>%
            group_by(gas, unit, category, year, .drop = FALSE) %>%
            slice_head(n = 1)
        }

        data_list[[d]] <- dataset
      }

      # Create main DataFrame from datasets in list
      data <-
        bind_rows(data_list, .id = "dataset_source") %>%
        dplyr::filter(category %in% input$category) %>%
        dplyr::filter((scenario %in% input$scenario) | (scenario == "None")) %>%
        dplyr::filter((year >= as.numeric(str_sub(input$year_range[[1]], 1, 4))) &
                 (year <= as.numeric(str_sub(input$year_range[[2]], 1, 4))))

      if ("Aggregate categories" %in% input$groupoptions) {
        data <-
          data %>%
          group_by(dataset_source, dataset_name, year, unit, gas, scenario, type) %>%
          summarize(value = sum(value, na.rm = TRUE),
                    category = "Aggregate",
                    .groups = "keep")
      }

      # Return data
      data
    })

    summarize_table <- reactive({
      if ("Break out gases" %in% input$groupoptions) {
        groupby_list <- c("dataset_name", "category", "dataset_source", "type", "year", "unit", "gas")
      } else {
        groupby_list <- c("dataset_name", "category", "dataset_source", "type", "year", "unit")
      }

      data <-
        build_table() %>%
        group_by_at(groupby_list) %>%
        summarize(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
        ungroup()

      data
    })

    sectorPlotInput <- reactive({
      data <- summarize_table()

      title <- paste0("Emissions Breakdown")
      subt <- "Historical and Projected"
      xaxis <- "Year"
      yaxis <- expression("MMT"~CO[2]*"e")
      color <- ""

      base <- ggplot(data, aes(x = year, y = value, group = interaction(category, dataset_name), color = interaction(dataset_name))) +
        geom_line(data = data, size = 0.8, lineend = "round", aes(linetype = interaction(type))) +
        geom_textline(alpha = 1, text_only = TRUE, aes(label = category), hjust = 1, text_smoothing = 50, vjust = -0.75, size = 3, show.legend = FALSE, remove_long = TRUE) +
        theme_light(base_size = 10)

      labelled <- style_plot(base, title, subtitle=subt, xaxis=xaxis, yaxis=yaxis) + theme(legend.text = element_text(size=10))

      if ("Start y axis at 0" %in% input$options) {
        labelled <-
          labelled +
          ylim(0, NA)
      }

      if ("Group by category" %in% input$groupoptions) {
        labelled <-
          labelled +
          facet_wrap(~category)
      } else if ("Break out gases" %in% input$groupoptions) {
        labelled <-
          labelled +
          facet_wrap(~gas)
      }

      labelled
    })

    output$sectorPlot <- renderPlot({
      sectorPlotInput()
      }, res = 130)

    output$sectorTable <- renderDataTable({
      data <- summarize_table()

      data
    }, options = list(pageLength = 10))

    output$fullSummaryTable <- renderDataTable({
      data <- build_table()

      data
    }, options = list(pageLength = 10))

    output$wideSummaryTable <- DT::renderDataTable({
      predata <-
        summarize_table() %>%
        pivot_wider(names_from = "year", values_from = "value")

      yearcols <- names(predata)[names(predata) %in%  as.character(c(1900:2100))]

      data <-
        predata %>%
        DT::datatable(., options = list(pageLength = 10,
                                        scrollX = TRUE,
                                        fixedColumns = list(leftColumns = 3)),
                      extensions='FixedColumns') %>%
        DT::formatRound(columns = yearcols, digits = 2)

      data
    })

    plotFilename <- reactive({
      req(input$category)

      filename <- paste0("plot_", stringr::str_to_lower(paste0(input$category, collapse = "_")), ".png")

      filename
    })

    observe({
      updateTextInput(
        inputId = "input_filename",
        value = plotFilename()
      )
    })

    output$downloadplotbutton <- downloadHandler(
      filename = reactive({input$input_filename}),
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = 10, height = 7, res = 300, units = "in")
        ggsave(sectorPlotInput(), filename = file, device = device)
    })

})
