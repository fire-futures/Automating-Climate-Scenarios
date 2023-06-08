# FUNCTION -----
cut_stitch_ts_vc <- function(model_selection = ui_sample_cell, # loop through dataframes of each grid cell #vc modified
                          series_selection = unlist(sample_grid_series),
                          start_date = ui_start_date,
                          df_names,
                          rcp = ui_rcp, #45, #vic added
                          gcm = ui_gcm) #"MIROC5")  #vic added
  # will need to pull out from grid cells to name) 
{
  # get list of all dataframes with data
  
  
  # CREATE FOLDER -----
  # Name files with model and grid number
  grid_name_formatted <- df_names
  
  # Name files with model and grid number
  #model_name <- deparse(substitute(model_selection)) # This is to pull the name out from the dataframe
  #grid_name_formatted <- paste0(str_to_title(model_name))
  # finds the gcm for each grid cell for the rcp and the gcm of the sample grid cell
  model_selection <- get(df_names, envir = globalenv()) # vic modified. may want to relabel sample_cell in the find_df func
  
  # For new .csv and time series files with shared name
  folder_name <- paste0(grid_name_formatted, "_time_series")
  dir.create(folder_name, showWarnings = FALSE)
  
  
  # CUT AND STITCH -----
  # Empty data frame with column names
  col_names <- colnames(model_selection)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Check if series_selection is by year or season_year
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- model_selection[which(model_selection$water_year == i), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      season_year <- strsplit(i, " ")
      year <- season_year[[1]][1]
      wet_dry <- season_year[[1]][2]
      new_cut <- model_selection[which(model_selection$season == wet_dry 
                                       & model_selection$water_year == year), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } # END IF
  
  
  # DEALING WITH UNORDERED LEAP YEAR -----
  # Remove all February 29ths in the dataframe
  combined_cut_model$time <- as.Date(combined_cut_model$time)
  combined_cut_model <- combined_cut_model[!(format(combined_cut_model$time, "%m-%d") == "02-29"), ]
  
  # Delete TIME column because years are in random order
  combined_cut_model <- combined_cut_model %>% select(-time)
  # Create sequence of every 1460 rows (4 years) to add a leap year row
  leapyears <- seq(from = 1, to = nrow(combined_cut_model), by = 1460)
  # For every 1460 days rows, add a row
  for(i in leapyears){
    combined_cut_model <- combined_cut_model %>% 
      add_row(max_temp = mean(combined_cut_model$max_temp[i - 1], combined_cut_model$max_temp[i + 1]),
              precip = 0,
              max_humidity = mean(combined_cut_model$max_humidity[i-1], combined_cut_model$max_humidity[i+1]),
              min_humidity = mean(combined_cut_model$min_humidity[i-1], combined_cut_model$min_humidity[i+1]),
              wind = mean(combined_cut_model$wind[i-1], combined_cut_model$wind[i+1]),
              min_temp = mean(combined_cut_model$min_temp[i-1], combined_cut_model$min_temp[i+1]),
              water_year = combined_cut_model$water_year[i-1],
              .after = i)   # Set location of new row
  }
  
  
  # Save final dataframe as a .csv
  write.csv(combined_cut_model, 
            file.path(folder_name, paste0(grid_name_formatted, ".csv")), 
            row.names = FALSE)
  
  # CONVERT TO TIME SERIES -----
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
  
  
  for (i in seq_along(file_type)) {
    # Construct file name with proper extension
    file_name <- file.path(folder_name, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%d %d %d 1", year(start_date), month(start_date), day(start_date))
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write variable data to file, appending to top line
    write.table(combined_cut_model[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
  }
}
