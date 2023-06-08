#' Cut and Stitch Time Series Data
#' cut_stitch_ts()
#' 
#' This function cuts and stitches time series data based on the specified model selection and series selection.
#' The resulting data is saved as a CSV file and converted into individual time series files for each variable.
#'
#' @param model_selection: The model selection data frame
#' @param series_selection: A vector of selected series to cut and stitch; automatically set to be sample_grid_series created from the randomly_select.R function
#' @param start_date: The start date for the time series 
#' @param df_names: The name of the data frame containing each model selection to be looped through
#' @param root_folder: The root folder path where the output files will be saved
#' @param ui_gcm The user interface GCM selection
#' @param ui_rcp The user interface RCP selection
#'
#' @return Folder of time series files and a composite csv file
#'

# Testing the filter used - located in the for loops for both year and season
# grid_cell_46896[format(as.Date(grid_cell_46896$time, format="%Y/%m/%d"), "%m-%d") >= format(as.Date(ui_start_date, format="%Y/%m/%d"),"%m-%d"), ]

cut_stitch_ts <- function(model_selection = ui_sample_cell,      # loop through dataframes of each grid cell
                          series_selection = unlist(sample_grid_series),
                          start_date = ui_start_date,
                          df_names,
                          root_folder,
                          ui_gcm = ui_gcm,
                          ui_rcp = ui_rcp)
{
  model_selection <- get(df_names, envir = globalenv()) 
  
  
  # CREATE FOLDER -----
  grid_name_formatted <- paste0(df_names, "_output")      # Create File/Folder Naming Convention
  folder_path <- file.path(root_folder, grid_name_formatted)  # For new .csv and time series files with shared name
  dir.create(folder_path, showWarnings = FALSE)
  
  # CREATE BLANK DATA FRAME ----
  col_names <- colnames(model_selection)       
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names   # Empty data frame with column names
  
  # CHECK DATE YYYY-MM-DD ----
  start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
  combined_cut_model$time <- as.Date(combined_cut_model$time , format = "%Y/%m/%d", origin = "1970-01-01")
  
  # CUT YEAR OR SEASON -----
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in 1:length(series_selection)) {
      selected_year <- series_selection[i]
      new_cut <- model_selection[which(model_selection$water_year == selected_year), ]  # Search matching time
      new_cut <- new_cut[order(as.Date(new_cut$time, format = "%Y/%m/%d")),]  # Make sure the dates are in order
      
      # Filter the first sequence by the start date
      if (i == 1) { 
        formatted_dates <- format(as.Date(new_cut$time, format="%Y/%m/%d"), "%m-%d")
        start_date_month_day <- format(as.Date(start_date, format="%Y/%m/%d"), "%m-%d")
        start_index <- which(formatted_dates == start_date_month_day)[1]
        new_cut <- new_cut[start_index:nrow(new_cut), ]
        }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    } # END YEARS LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in 1:length(series_selection)) {
      season_year <- strsplit(series_selection[i], " ")
      year <- season_year[[1]][1]
      wet_dry <- season_year[[1]][2]
      new_cut <- model_selection[which(model_selection$season == wet_dry 
                                       & model_selection$water_year == year),]
      new_cut <- new_cut[order(as.Date(new_cut$time, format = "%Y/%m/%d")),]  # Make sure the dates are in ORDER
      
      # Filter the first sequence by the start date
      if (i == 1) {  
        formatted_dates <- format(as.Date(new_cut$time, format="%Y-%m-%d"), "%m-%d")
        start_date_month_day <- format(as.Date(start_date, format="%Y-%m-%d"), "%m-%d")
        start_index <- which(formatted_dates == start_date_month_day)[1]
        new_cut <- new_cut[start_index:nrow(new_cut), ]
      }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    }
  }
  
  # DEALING WITH UNORDERED LEAP YEAR -----
  combined_cut_model <- combined_cut_model[!(format(combined_cut_model$time, "%m-%d") == "02-29"), ] # Remove all February 29ths in the data frame

  combined_cut_model <- combined_cut_model %>% mutate(old_date = as.character(time)) # Add character of original date
  combined_cut_model <- combined_cut_model %>% select(-time) # Delete TIME column for next step to work
  
  leapyears <- seq(from = 1, to = nrow(combined_cut_model), by = 1460)   # sequence of 1460 rows (365 days * 4 years) to add a leap year row
  for(i in leapyears){    # For every 1460 days rows, add a row
    combined_cut_model <- combined_cut_model %>% 
      add_row(max_temp = mean(combined_cut_model$max_temp[i - 1], combined_cut_model$max_temp[i + 1]),
              precip = 0,
              max_humidity = mean(combined_cut_model$max_humidity[i-1], combined_cut_model$max_humidity[i+1]),
              min_humidity = mean(combined_cut_model$min_humidity[i-1], combined_cut_model$min_humidity[i+1]),
              wind = mean(combined_cut_model$wind[i-1], combined_cut_model$wind[i+1]),
              min_temp = mean(combined_cut_model$min_temp[i-1], combined_cut_model$min_temp[i+1]),
              water_year = combined_cut_model$water_year[i-1],
              season = combined_cut_model$season[i-1],
              .after = i)   # Set location of new row
  }
  
  # NEW DATE COLUMN -----
  num_rows <- nrow(combined_cut_model)
  dates <- seq(from = start_date, length.out = num_rows, by = "day")
  combined_cut_model$sequence_date <- dates
  
  # SAVING -----
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  write.csv(combined_cut_model,      # Save final data frame as a .csv
            file.path(folder_path, paste0(grid_name_formatted, ".csv")), 
            row.names = FALSE)

  
  # CONVERT TO TIME SERIES -----
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
  
  for (i in seq_along(file_type)) {
    # Construct file name with proper extension
    file_name <- file.path(folder_path, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%04d %02d %02d 1", year(start_date), month(start_date), day(start_date))
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write variable data to file, appending to top line
    write.table(combined_cut_model[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
  }
}

