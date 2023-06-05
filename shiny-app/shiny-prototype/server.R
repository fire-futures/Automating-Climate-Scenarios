server <- function(input, output) {
  
  # PRE BUILT TAB
  # select a scenario picker ----
  selected_scenario <- reactive({
    input$scenario_single_select
  })
  
  
  
  
  # BUILD OWN SCENARIO TAB
  
  #NON CLIMATE INPUTS 
  
  # select spatial extent picker ----
  ui_grid_cells <- reactive({
    input$spatial_extent_input
  })
  
  # select a model (GCM) picker ----
  ui_gcm <- reactive({
    input$model_single_select
  })
  
  # select an RCP assumption picker ----
  ui_rcp <- reactive({
    input$RCP_input
  })

  
  # select run type (season vs year) picker ----
  ui_segment_type <- reactive({
    input$season_vs_year_input
  })
  
  # select sampling grid cell picker ----
  ui_sample_cell <- reactive({
    input$sampling_grid_cell_input
  })
  
  # select a start date ----
  ui_start_date <- reactive({
    input$start_date_input
  })
  
  # select scenario duration ----
  selected_scenario_duration <- reactive({
    input$scenario_duration_slider_input
  })
  
  # select sampling window ----
  ui_sample_window <- reactive({
    input$sampling_window_slider_input
  })
  
  
  
  
  # CLIMATE CRITERIA TABLE CREATION 
  # Store the table data in a reactiveValues object
  data <- reactiveValues(
    original_data = data.frame(
      notes = c("Likely segment", "Likely segment", "Likely segment"),
      min_t_l = c(0.01, 0.01, 0.01),
      min_t_u = c(0.99, 0.99, 0.99),
      max_t_l = c(0.01, 0.01, 0.01),
      max_t_u = c(0.99, 0.99, 0.99),
      precip_l = c(0.01, 0.01, 0.01),
      precip_u = c(0.99, 0.99, 0.99),
      wind_l = c(0.01, 0.01, 0.01),
      wind_u = c(0.99, 0.99, 0.99),
      min_h_l = c(0.01, 0.01, 0.01),
      min_h_u = c(0.99, 0.99, 0.99),
      max_h_l = c(0.01, 0.01, 0.01),
      max_h_u = c(0.99, 0.99, 0.99)
    ),
    modified_data = NULL
  )
  
  # Render the table using rhandsontable
  output$myTable <- renderRHandsontable({
    num_rows <- input$scenario_duration_slider_input
    if (is.null(data$modified_data) || nrow(data$modified_data) != num_rows) {
      # Create a new table if modified_data is null or has a different number of rows
      data$modified_data <- data.frame(
        notes = rep("Likely segment", num_rows),
        min_t_l = rep(0.01, num_rows),
        min_t_u = rep(0.99, num_rows),
        max_t_l = rep(0.01, num_rows),
        max_t_u = rep(0.99, num_rows),
        precip_l = rep(0.01, num_rows),
        precip_u = rep(0.99, num_rows),
        wind_l = rep(0.01, num_rows),
        wind_u = rep(0.99, num_rows),
        min_h_l = rep(0.01, num_rows),
        min_h_u = rep(0.99, num_rows),
        max_h_l = rep(0.01, num_rows),
        max_h_u = rep(0.99, num_rows)
      )
    }
    
    rhandsontable(data$modified_data, width = "100%", height = "300px")
  })
  
  # Use observeEvent to update the modified_data in the reactiveValues object whenever myTable changes
  observeEvent(input$myTable, {
    data$modified_data <- hot_to_r(input$myTable)
  })
  
  # Create a reactive expression for the climate_criteria
  climate_criteria <- reactive({
    data$modified_data
  })
  
  # Now you can use this reactive expression within your renderText functions
  output$test <- renderText({
    paste("Climate Criteria:",
          ifelse(is.null(climate_criteria()), "None", toString(climate_criteria())),
          "Class:",
          class(climate_criteria()))
  })
  
  output$math_check <- renderText({
    mean_precip_l <- mean(climate_criteria()$precip_l, na.rm = TRUE)
    print(mean_precip_l)
  })
  
  
  
  
  
  
  
  
  
  
  
  

  # GETTING THE DATA FROM THE API ----
  centroids_of_interest <- reactive({
    find_grid_cell_centroid(ui_grid_cells = input$spatial_extent_input) #users specify grid cells of interest
  })
  
  
  #TESTING THE OUTPUT OF CENTROIDS OF INTEREST
  output$centroids_table <- renderTable({
    centroids_of_interest()
  })
  
  # Display the class of centroids_of_interest
  output$centroids_class <- renderText({
    class(centroids_of_interest())
  })
  
  
  # GET GRID CELL INFO
  grid_cell_data_list <- reactive({

    data_list <- list()  # Initialize an empty list to store the data frames

    for (i in 1:nrow(centroids_of_interest())) {
      data <- get_all_grid_cell_data(
        grid_cell_id = centroids_of_interest()[i, "grid_cell_id"],
        lat = centroids_of_interest()[i, "lat"],
        lon = centroids_of_interest()[i, "lon"],
        ui_gcm = input$model_single_select,
        ui_rcp = input$RCP_input
      )

      data_list[[i]] <- data  # Add the data frame to the list
    }

    return(data_list)  # Return the list of data frames
  })

  #TEST WHETHER GETTING GRID CELL INFO IS WORKING
  output$data_list_length <- renderText({
    length(grid_cell_data_list())
  })

  #TEST WHETHER GETTING GRID CELL INFO IS WORKING
  output$grid_cell_table_check <- renderTable({
    head(grid_cell_data_list()[[1]])
  })
  
  
  
  # # SCENARIO BUILDING ----
  # run_samples <- reactive({
  #   find_matches(
  #     base_data = grid_cell_data$frames[[input$sampling_grid_cell_input]],
  #     start_date = input$start_date_input,
  #     sample_window = input$sampling_window_slider_input,
  #     climate_criteria_table = ui_climate_criteria_table,
  #     segment_type = input$season_vs_year_input
  #   )
  # })
  
  
  # SCENARIO BUILDING ----
  
  # Create a reactive expression to store the grid cell data frames
  grid_cell_data <- reactive({
    grid_cell_data_list()
  })
  
  run_samples <- reactive({
    base_data <- grid_cell_data()
    
    if (!is.null(base_data)) {
      find_matches(
        base_data = base_data[[1]],
        start_date = input$start_date_input,
        sample_window = input$sampling_window_slider_input,
        climate_criteria_table = ui_climate_criteria_table,
        segment_type = input$season_vs_year_input
      )
    } else {
      NULL  # Return NULL if grid_cell_data is not available
    }
  })
  
  # TEST Display the number of samples
  output$sample_runs <- renderPrint({
    head(run_samples())
  })
  
  
  # Count the number of years at each index in the list
  number_matches <- reactive({
    samples <- run_samples()
    if (!is.null(samples)) {
      data.frame(Count = sapply(samples, length))
    } else {
      data.frame(Count = integer())  # Return an empty data frame if samples is NULL
    }
  })
  
  # TEST Display the number of samples
  output$number_matches_table <- renderTable({
    number_matches()
  })

   
  
  #RANDOMLY SELECT ----
  # Reactive expression to randomly select one element from each list
  run_randomly_selected <- reactive({
    randomly_select(run_samples = run_samples())
  })

  # Render the randomly selected samples (for testing)
  output$random_samples <- renderPrint({
    run_randomly_selected()
  })

  
  # STITCHING TOGETHER 
  time_series <- reactive({
    all_grid_cells <- grid_cell_data()  # assuming grid_cell_data() is a function that returns a list of data.frames
    output <- list()
    for (i in seq_along(all_grid_cells)) {
      output[[i]] <- cut_stitch_shiny(series_selection = run_randomly_selected(),
                                      all_grid_cells = all_grid_cells[[i]],
                                      start_date = input$start_date_input,
                                      segment_type = input$season_vs_year_input)
    }
    return(output)
  })
  
  # TEST THAT TIME SERIES WORKS 
  output$ts_table <- renderTable({
    head(time_series()[[1]])
  })
  
  


  # CLIMATE CRITERIA SECTION ----
  
  #precipitation data ----
  filtered_precipitation <- reactive({
    
    canESM2_time_series %>% filter(
      precip_1 >= input$precipitation_slider_input[1],
      precip_1 <= input$precipitation_slider_input[2])
    
  })
  
  #min temp data ----
  filtered_min_temp <- reactive({
    
    canESM2_time_series %>% filter(
      min_temp_1 >= input$min_temp_slider_input[1],
      min_temp_1 <= input$min_temp_slider_input[2])
    
  })
  
  #max temp data ----
  filtered_max_temp <- reactive({
    
    canESM2_time_series %>% filter(
      max_temp_1 >= input$max_temp_slider_input[1],
      max_temp_1 <= input$max_temp_slider_input[2])
    
  })
  
  #wind data ----
  filtered_wind <- reactive({
    
    canESM2_time_series %>% filter(
      wind_1 >= input$max_temp_slider_input[1],
      wind_1 <= input$max_temp_slider_input[2])
    
  })
  
  #minimum humidity data ----
  filtered_min_humidity <- reactive({
    
    canESM2_time_series %>% filter(
      min_humidity_1 >= input$min_humidity_slider_input[1],
      min_humidity_1 <= input$min_humidity_slider_input[2])
    
  })
  
  #maximum humidity data ----
  filtered_max_humidity <- reactive({
    
    canESM2_time_series %>% filter(
      max_humidity_1 >= input$max_humidity_slider_input[1],
      max_humidity_1 <= input$max_humidity_slider_input[2])
    
  })
  
  
  #OUTPUTS

  
  #Input values for function below to create a reactive histogram for each climate variable
  input_values <- reactive({
    list(
      total_precip = input$precipitation_slider_input,
      mean_min_temp = input$min_temp_slider_input,
      mean_max_temp = input$max_temp_slider_input,
      mean_wind = input$wind_slider_input,
      mean_min_humidity = input$min_humidity_slider_input,
      mean_max_humidity = input$max_humidity_slider_input
    )
  })
  
  
  
  #PLOTTING THE CLIMATE VARIABLE HISTOGRAMS ----
  #function to create histogram for a climate variable
  climate_plot <- function(climate_var) {
    
    #load the input values
    input_val <- input_values()
    
    #calculate quartiles for climate variable
    perc <- quantile(canESM2_rcp85_time_series_summary[[climate_var]], seq(from = 0, to = 1, by = 0.25))
    
    #round climate val to 1 decimal, include quartile separated by a return
    stack_label <- paste(round(perc, 1), names(perc), sep = "\n")
    
    #create the histogram
    ggplot(canESM2_rcp85_time_series_summary, aes(x = get(climate_var))) +
      geom_histogram(fill = "steelblue", color = "black") + 
      scale_x_continuous(breaks = perc,labels = stack_label) +
      geom_vline(xintercept = input_val[[climate_var]], color = "red3", lwd = 1) +
      labs(x = paste0(str_to_title(str_replace(climate_var, "_", " "))), y = "Count", 
           title = paste0("Histogram of ", str_to_title(str_replace(climate_var, "_", " ")))) +
      theme_minimal()
  }
  
  #render plot of histograms
  output$climate_histograms <- renderPlot({
    
    #save the plots to objects 
    min_temp <- climate_plot("mean_min_temp")
    max_temp <- climate_plot("mean_max_temp")
    precip <- climate_plot("total_precip")
    wind <- climate_plot("mean_wind")
    max_humidity <- climate_plot("mean_max_humidity")
    min_humidity <- climate_plot("mean_min_humidity")
    
    #arrange the plots
    grid.arrange(min_temp, 
                 max_temp, 
                 precip,
                 wind,
                 min_humidity,
                 max_humidity,
                 ncol = 2)
  })
  
  
  #PLOTTING THE SCENARIOS ----
  output$climate_variables_time_series <- renderPlot({
    canESM2_time_series %>%
      mutate(year = lubridate::year(time)) %>%
      group_by(year) %>%
      filter(year > 2020,
             year < 2045) %>%
      summarise(mean_wind = mean(wind_1),
                mean_max_temp = mean(max_temp_1),
                mean_min_temp = mean(min_temp_1),
                total_precip = sum(precip_1),
                mean_max_humidity = mean(max_humidity_1), 
                mean_min_humidity = mean(min_humidity_1)) %>%
      gather(key = "variable", value = "value", -year) %>%
      mutate(variable = factor(variable, levels = c("mean_min_temp", "mean_max_temp", "total_precip", "mean_wind", "mean_min_humidity", "mean_max_humidity"))) %>%
      ggplot(aes(x = as.Date(paste(year, "1", "1", sep="-")), y = value, group = variable, color = variable)) +
      geom_line() +
      labs(x = "Date", y = "", color = "", title = "") +
      facet_wrap(~ variable, nrow = 3, ncol = 2, scales = "free_y") +
      theme_minimal() +
      guides(color = FALSE)
  })
  
  
  #DOWNLOAD BUTTON ----
  output$download_button <- downloadHandler(
    filename = function() {
      "canESM2_time_series.csv"
    },
    content = function(file) {
      write.csv(canESM2_time_series, file, row.names = FALSE)
    }
  )
  
}
