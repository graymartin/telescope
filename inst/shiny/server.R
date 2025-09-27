

# Setup -------------------------------------------------------------------
c_figure_type <- config_figure_type()

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
    
    if ("Color by model" %in% input$de_grouping) {
      de_color <- "model"
    } else if ("Color by scenario" %in% input$de_grouping) {
      de_color <- "scenario"
    } else if ("Color by region" %in% input$de_grouping) {
      de_color <- "region"
    } else if ("Color by variable" %in% input$de_grouping) {
      de_color <- "variable"
    } else {
      de_color <- "variable"
    }

    df_r <- var_to_figdf(dataset = input$de_dataset,
                         figtype = input$de_figure_type,
                         fb_title_name = input$de_title_name,
                         fb_figure_no = s_de_figure_no(),
                         fb_x = input$de_x,
                         fb_y = input$de_y,
                         fb_color = de_color,
                         fb_regions = de_regions,
                         fb_models = de_models,
                         fb_years = s_de_years(),
                         fb_scenarios = de_scenarios,
                         fb_variable = input$de_variable,
                         fb_facet1 = input$de_facet1,
                         fb_facet2 = input$de_facet2,
                         fb_options = paste0(input$de_options, collapse = ", "))

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

  observe({
    updateTextInput(
      inputId = "de_title_name",
      value = s_de_figure_no()
    )
  })

  ### Download figure file ----
  output$de_figure_download <- downloadHandler(
    filename = s_de_figure_filename(),
    content = function(file) {write.csv(df_rows(), file)}
    )
  
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
  df_de_data_bouncy <- reactive({
    validate(
      need(input$de_dataset, "Please select at least one dataset."),
      need(input$de_variable, "Please select at least one variable.")
    )
    
    full_data <- df_de_datasets()
    
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
    
    df <- plotting_filter(
      full_data,
      reg_f = input$de_regions,
      mod_f = input$de_models,
      yrs_f = s_de_years(),
      sce_f = input$de_scenarios,
      var_f = input$de_variable,
      y_col = input$de_y,
      options_list = input$de_options
    )
    
    return(df)
  })
  
  df_de_data <- df_de_data_bouncy %>% throttle(100)
  
  output$t_de_filtered_data <- DT::renderDT(df_de_data())
  
  ### Data plot ----
  p_de_figure_bouncy <- reactive({
    req(input$de_dataset)
    req(input$de_variable)
    
    if ("Color by model" %in% input$de_grouping) {
      de_color <- "model"
    } else if ("Color by scenario" %in% input$de_grouping) {
      de_color <- "scenario"
    } else if ("Color by region" %in% input$de_grouping) {
      de_color <- "region"
    } else if ("Color by variable" %in% input$de_grouping) {
      de_color <- "variable"
    } else {
      de_color <- "variable"
    }
    
    if ("Aggregate variables" %in% input$de_options) {
      de_variable <- c("Aggregated")
    } else {
      de_variable <- input$de_variable
    }
    
    plot <- plotting(df = df_de_data(),
                     figtype = input$de_figure_type,
                     fb_title_name = "",
                     fb_figure_no = "",
                     fb_x = input$de_x,
                     fb_y = input$de_y,
                     fb_color = de_color,
                     fb_regions = input$de_regions,
                     fb_models = input$de_models,
                     fb_years = s_de_years(),
                     fb_scenarios = input$de_scenarios,
                     fb_variable = de_variable,
                     fb_facet1 = input$de_facet1,
                     fb_facet2 = input$de_facet2,
                     fb_options = paste0(input$de_options, collapse = ", "))
    
    return(plot)
  })
  
  p_de_figure <- p_de_figure_bouncy %>% debounce(100)
  
  output$p_de_figure <- renderPlot({
    p_de_figure()
  }, res = 130)
  
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
