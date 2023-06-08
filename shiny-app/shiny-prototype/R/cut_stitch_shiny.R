cut_stitch_shiny <- function (series_selection,
                              all_grid_cells,
                              start_date,
                              segment_type)
{
  
  # CREATE BLANK DATA FRAME ----
  col_names <- colnames(all_grid_cells)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names   # Empty data frame with column names
  
  # CHECK DATE YYYY-MM-DD ----
  start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
  combined_cut_model$time <- as.Date(combined_cut_model$time , format = "%Y/%m/%d", origin = "1970-01-01")
  
  # CUT YEAR OR SEASON -----
  if(segment_type == "year") {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- all_grid_cells[which(all_grid_cells$water_year == i), ]  # Search matching time
      if (i == series_selection[1]) {
        # Filter the first sequence by the start date
        formatted_dates <- format(as.Date(new_cut$time, format="%Y/%m/%d"), "%m-%d")
        start_date_month_day <- format(as.Date(start_date, format="%Y/%m/%d"), "%m-%d")
        start_index <- which(formatted_dates == start_date_month_day)[1]
        new_cut <- new_cut[start_index:nrow(new_cut), ]
      }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    } # END YEARS LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      season_year <- strsplit(i, " ")
      year <- season_year[[1]][1]
      wet_dry <- season_year[[1]][2]
      new_cut <- all_grid_cells[which(all_grid_cells$season == wet_dry
                                      & all_grid_cells$water_year == year), ]
      if (i == series_selection[1]) {  # Filter the first sequence by the start date
        formatted_dates <- format(as.Date(new_cut$time, format="%Y/%m/%d"), "%m-%d")
        start_date_month_day <- format(as.Date(start_date, format="%Y/%m/%d"), "%m-%d")
        start_index <- which(formatted_dates == start_date_month_day)[1]
        new_cut <- new_cut[start_index:nrow(new_cut), ]
      }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    } # END SEASONS LOOP
    
  } # END IF
  
  
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
  
  return(combined_cut_model)
}