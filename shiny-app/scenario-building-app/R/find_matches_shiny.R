find_matches_shiny <- function(rcp = NULL,
                               gcm = NULL,
                               sample_cell = NULL,
                               base_data = NULL,
                               segment_type,
                               start_date,
                               climate_criteria_table,
                               sample_window) {
  df_list <- list() #initializing empty list
  season_list <- season_order_shiny(start_date, nrow(climate_criteria_table)) #creating a list to specify which season to find percentiles from
  for (i in 1:nrow(climate_criteria_table)) {
    start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
    df <- find_sample_window_shiny(start_date, sample_window, base_data)
    if(segment_type == "year") {
      runs <- filter_df_shiny(df, climate_criteria_table[i,,drop = FALSE])
    }
    if(segment_type == "season") {
      runs <- filter_df_shiny(df, climate_criteria_table[i,,drop = FALSE], season_list[i])
    }
    df_list[[i]] <- runs
    start_date <- start_date + lubridate::years(1)
  }
  # Returns the list of filtered years for each data frame that can be put into the stitches function
  return(df_list)
}