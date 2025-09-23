

# Setup -------------------------------------------------------------------
# TODO: Make this dynamic
df_input <- getOption("telescope.default_dataframe", default = FALSE)

c_figure_type <- config_figure_type()

# Server ------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  ## Input handling ----
  s_fb_years <- reactive({
    years <- paste0(str_sub(input$fb_years[[1]], 1, 4), "-", str_sub(input$fb_years[[2]], 1, 4))

    return(years)
  })
  
  s_de_years <- reactive({
    years <- paste0(str_sub(input$de_years[[1]], 1, 4), "-", str_sub(input$de_years[[2]], 1, 4))
    
    return(years)
  })

  s_fb_figure_no <- reactive({
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
  
  ## Figure builder ----
  ### Figure data ----
  df_fb_data <- reactive({
    df <- plotting_filter(
      df_input,
      reg_f = input$fb_regions,
      mod_f = input$fb_models,
      yrs_f = s_fb_years(),
      sce_f = input$fb_scenarios,
      var_f = input$fb_variable
    )

    return(df)
  })

  output$t_fb_data <- DT::renderDT(df_fb_data())

  ### Figure builder plot ----
  p_fb_figure <- reactive({
    req(input$fb_figure_type)
    req(input$fb_variable)
    plot <- plotting(df = df_input,
                     figtype = input$fb_figure_type,
                     fb_title_name = input$fb_title_name,
                     fb_figure_no = s_fb_figure_no(),
                     fb_x = input$fb_x,
                     fb_y = input$fb_y,
                     fb_color = input$fb_color,
                     fb_regions = input$fb_regions,
                     fb_models = input$fb_models,
                     fb_years = s_fb_years(),
                     fb_scenarios = input$fb_scenarios,
                     fb_variable = input$fb_variable,
                     fb_facet1 = input$fb_facet1,
                     fb_facet2 = input$fb_facet2)

    return(plot)
  })

  output$p_fb_figure <- renderPlot({
    p_fb_figure()
  }, res = 130)

  ### Figure builder rows ----
  df_rows <- reactive({
    if (is.null(input$fb_models)) {
      fb_models <- as.list(unique(df_fb_data()$model))
    } else {
      fb_models <- input$fb_models
    }

    if (is.null(input$fb_scenarios)) {
      fb_scenarios <- as.list(unique(df_fb_data()$scenario))
    } else {
      fb_scenarios <- input$fb_scenarios
    }

    if (is.null(input$fb_regions)) {
      fb_regions <- as.list(unique(df_fb_data()$region))
    } else {
      fb_regions <- input$fb_regions
    }

    df_r <- var_to_figdf(figtype = input$fb_figure_type,
                         fb_title_name = input$fb_title_name,
                         fb_figure_no = s_fb_figure_no(),
                         fb_x = input$fb_x,
                         fb_y = input$fb_y,
                         fb_color = input$fb_color,
                         fb_regions = fb_regions,
                         fb_models = fb_models,
                         fb_years = s_fb_years(),
                         fb_scenarios = fb_scenarios,
                         fb_variable = input$fb_variable,
                         fb_facet1 = input$fb_facet1,
                         fb_facet2 = input$fb_facet2)

    file <- system.file("output", "intermediate", package = "telescope")
    saveRDS(df_r, file = paste0(file, "/", "fb_figure_data.rds"))

    return(df_r)
  })

  output$t_fb_rows <- DT::renderDT(df_rows())

  ### Figure name ----
  s_fb_figure_filename <- reactive({
    req(input$fb_figure_no)

    filename <- paste0("figure_", s_fb_figure_no(), ".csv")
    
    return(filename)
  })

  observe({
    updateTextInput(
      inputId = "fb_title_name",
      value = s_fb_figure_no()
    )
  })

  ### Download figure file ----
  output$fb_figure_download <- downloadHandler(
    filename = s_fb_figure_filename(),
    content = function(file) {write.csv(df_rows(), file)}
    )
  
  ### Save figure file ----
  observeEvent(input$fb_figure_save, {
    file <- system.file("output", "figure", s_main_analysis(), package = "telescope")
    # TODO: Check for overwrite
    write.csv(df_rows(), paste0(file, "/", s_fb_figure_filename()), row.names = FALSE)
    updateNumericInput(
      inputId = "fb_figure_no",
      value = input$fb_figure_no + 1
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
                               return(analysis_to_figdf(analysis = s_main_analysis()))
                             })
  
  output$t_fs_data <- DT::renderDT(
    datatable(df_fs_data(), selection = "single")
    )
  
  s_fs_data <- reactive({
    req(input$t_fs_data_rows_selected)
    return(df_fs_data()[[input$t_fs_data_rows_selected, "source"]])
  })
  
  output$s_fs_data <- renderText(s_fs_data())
  
  ## Data explorer ----
  ### Dataset combination ----
  df_de_datasets <- reactive({
    req(input$de_dataset)
    
    raw_data_list <- list()
    for (s_dataset_selection in input$de_dataset) {
      s_dataset_name <- names(config_dataset())[config_dataset() == s_dataset_selection]
      
      dir <- system.file("input", "dataset", package = "telescope")
      s_filename <- paste0(dir, "/", s_dataset_selection)
      
      df_dataset <- read_csv(s_filename)
      
      raw_data_list[[s_dataset_selection]] <- df_dataset
    }
    
    out_dataset <- bind_rows(raw_data_list, .id = "source_filename")
    
    return(out_dataset)
  })
  
  output$t_de_data <- DT::renderDT(df_de_datasets())
  
  ### Dataset reactive variables ----
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
  })
  
  ### Filtered data table ----
  df_de_data <- reactive({
    req(input$de_dataset)
    
    df <- plotting_filter(
      df_de_datasets(),
      reg_f = input$de_regions,
      mod_f = input$de_models,
      yrs_f = s_de_years(),
      sce_f = input$de_scenarios,
      var_f = input$de_variable
    )
    
    if ("Aggregate variables" %in% input$de_options) {
      de_df <- 
        df %>% 
        dplyr::filter(variable %in% input$de_variable) %>% 
        group_by(across(c(-variable, -value))) %>% 
        summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
        ungroup() %>% 
        mutate(variable = "Aggregated")
    } else {
      de_df <- df
    }
    
    return(de_df)
  })
  
  output$t_de_filtered_data <- DT::renderDT(df_de_data())
  
  ### Data plot ----
  p_de_figure <- reactive({
    req(input$de_dataset)
    req(input$de_variable)
    
    if ("Group by model" %in% input$de_grouping) {
      de_color <- "model"
    } else if ("Group by scenario" %in% input$de_grouping) {
      de_color <- "scenario"
    } else if ("Group by region" %in% input$de_grouping) {
      de_color <- "region"
    } else if ("Group by variable" %in% input$de_grouping) {
      de_color <- "variable"
    } else {
      de_color <- "variable"
    }
    
    if ("Aggregate variables" %in% input$de_options) {
      de_variable <- "Aggregated"
    } else {
      de_variable <- input$de_variable
    }
    
    plot <- plotting(df = df_de_data(),
                     figtype = "timeseries",
                     fb_title_name = "",
                     fb_figure_no = "",
                     fb_x = "year",
                     fb_y = "value",
                     fb_color = de_color,
                     fb_regions = input$de_regions,
                     fb_models = input$de_models,
                     fb_years = s_de_years(),
                     fb_scenarios = input$de_scenarios,
                     fb_variable = de_variable,
                     fb_facet1 = input$de_facet1,
                     fb_facet2 = input$de_facet2)
    
    if ("Start y axis at 0" %in% input$de_options) {
      plot <- plot + expand_limits(y = 0)
    }
    
    return(plot)
  })
  
  output$p_de_figure <- renderPlot({
    p_de_figure()
  }, res = 130)
  
})
