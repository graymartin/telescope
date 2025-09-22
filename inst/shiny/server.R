

# Setup -------------------------------------------------------------------
df_input <- getOption("telescope.default_dataframe", default = FALSE)
c_figure_type <- config_figure_type()

# Server ------------------------------------------------------------------
server <- shinyServer(function(input, output) {
  ## Input handling ----
  fb_years_str <- reactive({
    years <- paste0(str_sub(input$fb_years[[1]], 1, 4), "-", str_sub(input$fb_years[[2]], 1, 4))

    return(years)
  })

  fb_figure_no_str <- reactive({
    figure_no <- as.character(input$fb_figure_no[[1]])

    return(figure_no)
  })

  ## Debugging  ----
  ### Required and optional figure parameters ----
  df_debug <- reactive({
    req(input$fb_figure_type)
    base_table <- filter(c_figure_type, figure_type == input$fb_figure_type)

    output_table <-
      base_table %>%
      mutate(selected = "")

    for (i in 1:nrow(output_table)) {
      var <- paste0("fb_", output_table[i, "variable"])
      output_table[i, "selected"] <- paste0(paste(as.character(input[[var]]), collapse = ", "), "")
    }

    return(output_table)

  })

  output$t_fb_debug <- DT::renderDT(df_debug())

  ## Figure data ----
  df_data <- reactive({
    df <- plotting_filter(
      df_input,
      reg_f = input$fb_regions,
      mod_f = input$fb_models,
      yrs_f = fb_years_str(),
      sce_f = input$fb_scenarios,
      var_f = input$fb_variable
    )

    return(df)
  })

  output$t_fb_data <- DT::renderDT(df_data())

  ## Figure builder plot ----
  p_fb_figure <- reactive({
    req(input$fb_figure_type)
    req(input$fb_variable)
    plot <- plotting(df = df_input,
                     figtype = input$fb_figure_type,
                     fb_title_name = input$fb_title_name,
                     fb_figure_no = fb_figure_no_str(),
                     fb_x = input$fb_x,
                     fb_y = input$fb_y,
                     fb_color = input$fb_color,
                     fb_regions = input$fb_regions,
                     fb_models = input$fb_models,
                     fb_years = fb_years_str(),
                     fb_scenarios = input$fb_scenarios,
                     fb_variable = input$fb_variable,
                     fb_facet1 = input$fb_facet1,
                     fb_facet2 = input$fb_facet2)

    return(plot)
  })

  output$p_fb_figure <- renderPlot({
    p_fb_figure()
  }, res = 130)

  ## Figure builder rows ----
  df_rows <- reactive({
    if (is.null(input$fb_models)) {
      fb_models <- as.list(unique(df_data()$model))
    } else {
      fb_models <- input$fb_models
    }

    if (is.null(input$fb_scenarios)) {
      fb_scenarios <- as.list(unique(df_data()$scenario))
    } else {
      fb_scenarios <- input$fb_scenarios
    }

    if (is.null(input$fb_regions)) {
      fb_regions <- as.list(unique(df_data()$region))
    } else {
      fb_regions <- input$fb_regions
    }

    df_r <- var_to_fig(figtype = input$fb_figure_type,
                       fb_title_name = input$fb_title_name,
                       fb_figure_no = fb_figure_no_str(),
                       fb_x = input$fb_x,
                       fb_y = input$fb_y,
                       fb_color = input$fb_color,
                       fb_regions = fb_regions,
                       fb_models = fb_models,
                       fb_years = fb_years_str(),
                       fb_scenarios = fb_scenarios,
                       fb_variable = input$fb_variable,
                       fb_facet1 = input$fb_facet1,
                       fb_facet2 = input$fb_facet2)

    file <- system.file("output", "intermediate", package = "telescope")
    saveRDS(df_r, file = paste0(file, "/", "df_r.rds"))

    return(df_r)
  })

  output$t_fb_rows <- DT::renderDT(df_rows())

  ## Figure name ----
  s_fb_figure_filename <- reactive({
    req(input$fb_figure_no)

    filename <- paste0("figure_", fb_figure_no_str(), ".csv")

    filename
  })

  observe({
    updateTextInput(
      inputId = "fb_title_name",
      value = s_fb_figure_filename()
    )
  })

  # ## Download figure file ----
  # output$fb_figure_download <- downloadHandler(
  #   filename = reactive({input$fb_title_name}),
  #   content = function(file) {write.csv(df_rows(), file)}
  #   )

  observeEvent(input$fb_figure_download, {
    file <- system.file("output", "figure", package = "telescope")
    write.csv(df_rows(), paste0(file, "/", s_fb_figure_filename()))
  })

})
