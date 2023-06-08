climate_criteria_df <- data.frame(
  wind_l = c(0, 0.01, 0.01, 0.01, 0.01, 0.01),
  wind_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.99),
  min_humidity_l = c(0, 0.01, 0.01, 0.01, 0.01, 0.80),
  min_humidity_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.99),
  max_humidity_l = c(0, 0.01, 0.01, 0.01, 0.01, 0.01),
  max_humidity_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.99),
  min_temp_l = c(0, 0.01, 0.01, 0.01, 0.01, 0.01),
  min_temp_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.99),
  max_temp_l = c(0, 0.01, 0.1, .35, 0.01, 0.01),
  max_temp_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.99),
  precip_l = c(0, 0.01, 0.01, 0.01, 0.01, 0.01),
  precip_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.75)
)

find_matches_shiny <- function(rcp = NULL,
                         gcm = NULL,
                         sample_cell = NULL,
                         base_data = NULL,
                         segment_type,
                         start_date,
                         climate_criteria_table,
                         sample_window) {
  
  df_list <- list() #initializing empty list
  
  season_list <- season_order(start_date, nrow(climate_criteria_table)) #creating a list to specify which season to find percentiles from 
  
  for (i in 1:length(climate_criteria_table)) {
    start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
    
    df <- find_sample_window(start_date, sample_window, base_data)
    
    if(segment_type == "year") {
      runs <- filter_df_shiny(df, climate_criteria_table[i,,drop = FALSE])
    }
    
    if(segment_type == "season") {
      runs <- filter_df_shiny(df, climate_criteria_table[i,,drop = FALSE], season_list[i,])
    }
    
    df_list[[i]] <- runs
    start_date <- start_date + lubridate::years(1)
  }
  
  
  # Returns the list of filtered years for each data frame that can be put into the stitches function
  return(df_list)
}


segment_samples <<-
  find_matches_shiny(
    base_data = ui_sample_cell, 
    start_date = ui_start_date,
    sample_window = ui_sample_window,
    climate_criteria_table = climate_criteria_df,
    segment_type = ui_segment_type,
  )

head(segment_samples)
