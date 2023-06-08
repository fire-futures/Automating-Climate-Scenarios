server <- function(input, output) {
  
  # WELCOME TAB
  
  #display Cal-Adapt Map
  output$cal_adapt_map <- renderLeaflet({
    
    print("Rendering leaflet map.")
    
    caladapt_map()
  })
  
  
  # PRE BUILT TAB
  # select a scenario picker ----
  selected_scenario <- reactive({
    input$scenario_single_select
  })
  
  
  # BUILD OWN SCENARIO TAB
  
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
  
  # select segment type (season vs year) picker ----
  ui_segment_type <- reactive({
    input$season_vs_year_input
  })
  
  # select a start date ----
  ui_start_date <- reactive({
    as.Date(paste0(input$start_year_input, "-10-01"))
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
      min_temp_l = c(0, 0, 0),
      min_temp_u = c(1, 1, 1),
      max_temp_l = c(0, 0, 0),
      max_temp_u = c(1, 1, 1),
      precip_l = c(0, 0, 0),
      precip_u = c(1, 1, 1),
      wind_l = c(0, 0, 0),
      wind_u = c(1, 1, 1),
      min_humidity_l = c(0, 0, 0),
      min_humidity_u = c(1, 1, 1),
      max_humidity_l = c(0, 0, 0),
      max_humidity_u = c(1, 1, 1)
    ),
    modified_data = NULL
  )
  
  # Render the table using rhandsontable
  output$myTable <- renderRHandsontable({
    num_rows <- input$scenario_duration_slider_input #number of rows depends on duration
    if (is.null(data$modified_data) || nrow(data$modified_data) != num_rows) {
      # Create a new table if modified_data is null or has a different number of rows
      data$modified_data <- data.frame(
        notes = rep("Likely segment", num_rows),
        min_temp_l = rep(0, num_rows),
        min_temp_u = rep(1, num_rows),
        max_temp_l = rep(0, num_rows),
        max_temp_u = rep(1, num_rows),
        precip_l = rep(0, num_rows),
        precip_u = rep(1, num_rows),
        wind_l = rep(0, num_rows),
        wind_u = rep(1, num_rows),
        min_humidity_l = rep(0, num_rows),
        min_humidity_u = rep(1, num_rows),
        max_humidity_l = rep(0, num_rows),
        max_humidity_u = rep(1, num_rows)
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
  
  
  #GETTING GRIC CELL DATA TO CREATE SCENARIOS
  
  # Obtaining data from the API ----
  centroids_of_interest <- reactive({
    tryCatch( 
      {
        find_grid_cell_centroid(ui_grid_cells = input$spatial_extent_input) # users specify grid cells of interest
      },
      error = function(e) {
        # Custom error message
        return("Please specify your GCM, RCP assumption, and desired spatial extent to visualize the climate distributions")
      }
    )
  })
  
  # Testing output of centroids of interest
  output$centroids_table <- renderTable({
    centroids_of_interest()
  })
  
  # Display the class of centroids of interest
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
  
  

  
  # SCENARIO BUILDING ----
  
  # Create a reactive expression to store the grid cell data frames
  grid_cell_data <- reactive({
    grid_cell_data_list()
  })
  
  run_samples <- reactive({
    base_data <- grid_cell_data()

    if (!is.null(base_data)) {
      find_matches_shiny(
        base_data = base_data[[1]],
        start_date = ui_start_date(),
        sample_window = input$sampling_window_slider_input,
        climate_criteria_table = climate_criteria(),
        segment_type = input$season_vs_year_input
      )
    } else {
      NULL  # Return NULL if grid_cell_data is not available
    }
  })
  
  
  # Count the number of years at each index in the list
  number_matches <- reactive({
    tryCatch(
      {
        samples <- run_samples()
        if (!is.null(samples)) {
          data.frame(Count = sapply(samples, length))
        } else {
          # Check if grid_cell_data is NULL or empty
          if (is.null(grid_cell_data()) || nrow(grid_cell_data()) == 0) {
            data.frame(Count = "Please select grid cell(s)")
          } else {
            data.frame(Count = integer())  # Return an empty data frame if samples is NULL
          }
        }
      },
      error = function(e) {
        data.frame(Count = "Please select grid cell(s)")
      }
    )
  })
  
  # TEST Display the number of samples
  output$number_matches_table <- renderTable({
    if (is.character(number_matches()[["Count"]])) {
      tibble(Count = number_matches()[["Count"]])
    } else {
      number_matches()
    }
  })
  
  
  #RANDOMLY SELECT ----
  # Reactive expression to randomly select one element from each list
  run_randomly_selected <- reactive({
    randomly_select(run_samples = run_samples())
  })
  
  
  # STITCHING TOGETHER 
  time_series <- reactive({
    all_grid_cells <- grid_cell_data()  # assuming grid_cell_data() is a function that returns a list of data.frames
    output <- list()
    for (i in seq_along(all_grid_cells)) {
      output[[i]] <- cut_stitch_shiny(series_selection = run_randomly_selected(),
                                      all_grid_cells = all_grid_cells[[i]],
                                      start_date = ui_start_date(),
                                      segment_type = input$season_vs_year_input)
    }
    return(output)
  })
  
  
  #GET METADATA FOR SCENARIO
  metadata <- reactive({
    
    get_metadata_shiny(criteria_list = climate_criteria(),
                                 gcm = ui_gcm(), 
                                 rcp = ui_rcp(), 
                                 sample_cell = ui_grid_cells()[[1]], 
                                 spatial_extent = ui_grid_cells(), 
                                 start_date = ui_start_date(), 
                                 segment_type = ui_segment_type(), 
                                 duration = selected_scenario_duration(), 
                                 sample_window = ui_sample_window(), 
                                 number_of_matches = number_matches() 
  )
  }) #END GET METADATA
  
  
  #OUTPUTS ----
  
  # Wrap the climate_plot function inside a reactive expression
  plot_histograms <- reactive({
    climate_plot <- function(climate_var, data) {
      perc <- quantile(data[[climate_var]], seq(from = 0, to = 1, by = 0.25))
      stack_label <- paste(round(perc, 2), names(perc), sep = "\n")
      
      ggplot(data, aes(x = get(climate_var))) +
        geom_histogram(fill = "steelblue", color = "black", bins = 30) +
        scale_x_continuous(breaks = perc, labels = stack_label) +
        labs(
          x = "",
          y = "Count",
          title = paste0("Histogram of ", str_to_title(str_replace_all(climate_var, "_", " ")))
        ) +
        theme_minimal()
    }
    
    # Obtain the first data frame from the reactive dataset
    raw_data <- grid_cell_data_list()[[1]]
    
    # Summarize the data frame
    summarized_data <- raw_data %>%
      select(water_year, wind, min_humidity, max_humidity, min_temp, max_temp, precip) %>%
      group_by(water_year) %>%
      summarise(
        mean_min_temp = mean(min_temp),
        mean_max_temp = mean(max_temp),
        total_precip = sum(precip),
        wind_days_above_threshold = sum(wind >= wind_threshold),
        mean_min_humidity = mean(min_humidity),
        mean_max_humidity = mean(max_humidity)
      )
    
    # Call the climate_plot function with the desired climate variables and the summarized data frame
    min_temp <- climate_plot("mean_min_temp", summarized_data)
    max_temp <- climate_plot("mean_max_temp", summarized_data)
    precip <- climate_plot("total_precip", summarized_data)
    wind <- climate_plot("wind_days_above_threshold", summarized_data)
    min_humidity <- climate_plot("mean_min_humidity", summarized_data)
    max_humidity <- climate_plot("mean_max_humidity", summarized_data)
    
    # Return the plots as a list
    list(min_temp, max_temp, precip, wind, min_humidity, max_humidity)
  })
  
  # Render plot of histograms
  output$climate_histograms <- renderPlot({
    plots <- tryCatch(
      {
        plot_histograms()  # Get the plots from the reactive expression
      },
      error = function(e) {
        # Custom error message
        message <- "Please select grid cell(s) to specify your desired spatial extent"
        plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, main = message)
        return(NULL)
      }
    )
    
    if (!is.null(plots)) {
      # Arrange the plots
      grid.arrange(
        plots[[1]],
        plots[[2]],
        plots[[3]],
        plots[[4]],
        plots[[5]],
        plots[[6]],
        ncol = 3
      )
    }
  })
  
  
  
  #PLOTTING THE SCENARIOS ----
  sample_cell_time_series <- reactive({
    time_series()[[1]] %>%
      select(sequence_date, wind, min_humidity, max_humidity, min_temp, max_temp, precip) %>%
      rename(time = sequence_date) %>%
      mutate(time = as.Date(time, origin = "1970-01-01"))
  })
  
  output$climate_variables_time_series <- renderPlot({
    tryCatch({
      data <- sample_cell_time_series()  # make sure to call the reactive data frame with ()
      data %>%
        mutate(year = lubridate::year(time)) %>%
        group_by(year) %>%
        summarise(wind_days_over_threshold = sum(wind > wind_threshold),
                  mean_max_temp = mean(max_temp),
                  mean_min_temp = mean(min_temp),
                  total_precip = sum(precip),
                  mean_max_humidity = mean(max_humidity),
                  mean_min_humidity = mean(min_humidity)) %>%
        gather(key = "variable", value = "value", -year) %>%
        mutate(variable = factor(variable, levels = c("mean_min_temp", 
                                                      "mean_max_temp", 
                                                      "total_precip", 
                                                      "wind_days_over_threshold", 
                                                      "mean_min_humidity", 
                                                      "mean_max_humidity"))) %>%
        ggplot(aes(x = as.Date(paste(year, "1", "1", sep="-")), y = value, group = variable, color = variable)) +
        geom_line() +
        labs(x = "Date", y = "", color = "") +
        facet_wrap(~ variable, nrow = 2, ncol = 3, scales = "free_y", labeller = labeller(variable = c(
          mean_min_temp = "Time Series of Mean Minimum Temperature",
          mean_max_temp = "Time Series of Mean Maximum Temperature",
          total_precip = "Time Series of Total Precipitation",
          wind_days_over_threshold = "Time Series of Wind Days Above Threshold",
          mean_min_humidity = "Time Series of Mean Minimum Humidity",
          mean_max_humidity = "Time Series of Mean Maximum Humidity"
        ))) +
        theme_minimal() +
        theme(
          strip.text = element_text(size = 12, face = "bold")
        ) +
        guides(color = FALSE)
    }, error = function(e) {
      # Custom error message
      message <- "Please select grid cell(s) to specify your desired spatial extent"
      plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, main = message)
      return(NULL)
    })
  })
  

  
  
  
  # DOWNLOAD BUTTON SERVER SIDE ----
  output$download_button <- downloadHandler(
    filename = function() {
      "time_series.zip"
    },
    content = function(file) {
      target_dir <- paste0(ui_gcm(), "_", ui_rcp(), "_all_grid_cell_output")
      
      # Check if the target directory exists, create it if it doesn't
      if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE)
      }
      
      # Save each data frame as a separate CSV file in its corresponding grid cell folder
      for (i in seq_along(ui_grid_cells())) {
        grid_cell <- ui_grid_cells()[i]
        grid_dir <- file.path(target_dir, paste0("grid_cell_", grid_cell))
        
        # Check if the grid cell directory exists, create it if it doesn't
        if (!dir.exists(grid_dir)) {
          dir.create(grid_dir, recursive = TRUE)
        }
        
        
        file_name <- paste0("grid_cell_", grid_cell, "_", ui_gcm(), "_", ui_rcp(), "_output.csv")
        file_path <- file.path(grid_dir, file_name)
        write.csv(time_series()[[i]], file_path, row.names = FALSE)
        
        # Debugging statement to check if the CSV file is created
        cat("CSV file created:", file_path, "\n")
      }
      
      #metadata csv
      metadata_file_path <- file.path(target_dir, "metadata.csv")
      write.csv(metadata(), metadata_file_path, row.names = FALSE)
      
      # Code to generate ASCII files
      file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
      col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
      for (i in seq_along(file_type)) {
        for (j in seq_along(time_series())) {
          grid_cell <- ui_grid_cells()[j]
          grid_dir <- file.path(target_dir, paste0("grid_cell_", grid_cell))
          
          file_name <- file.path(grid_dir, paste0("grid_cell_", grid_cell, "_", ui_gcm(), "_", ui_rcp(), "_output", file_type[i]))
          start_date <- as.Date(ui_start_date())
          top_of_file <- sprintf("%04d %02d %02d 1", year(start_date), month(start_date), day(start_date))
          write.table(top_of_file, file = file_name, row.names = FALSE, col.names = FALSE, quote = FALSE)
          write.table(time_series()[[j]][[col_names[i]]], file = file_name, row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
        }
      }
      
      # Create the zip archive containing the individual grid cell directories
      zip_file <- file.path(target_dir, "time_series.zip")
      zip::zip(zip_file, files = list.files(target_dir, full.names = TRUE, recursive = TRUE))
      
      # Move the zip archive to the specified download location
      file.copy(zip_file, file)
      
      # Clean up: delete the temporary directory
      unlink(target_dir, recursive = TRUE)
    }
  )
  
  
  
}