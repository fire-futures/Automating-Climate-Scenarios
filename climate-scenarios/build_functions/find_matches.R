#' Build Runs Function
#'
#' This function builds runs of filtered data frames based on provided parameters.
#'
#' @param rcp Integer specifying the Representative Concentration Pathway (RCP) scenario.
#' @param gcm Character specifying the General Circulation Model (GCM).
#' @param sample_cell Integer specifying the sample cell.
#' @param base_data Optional data frame to use as input. If provided, other arguments (rcp, gcm, sample_cell) will be ignored.
#' @param segment_type Character specifying the type of runs to build. Either "year" or "season".
#' @param start_date Character specifying the start date for building the runs in format "MM/DD/YYYY".
#' @param climate_criteria_table List of data frames containing climate variable information for filtering.
#' @param sample_window Function to generate sample window of data.
#'
#' @return A list of data frames that match their criteria, one for each run in the climate_criteria_table, ready to be used in the stitches function.
#'
#' @examples
#' find_matches(
#'   rcp = 45,
#'   gcm = "MIROC5",
#'   sample_cell = 2,
#'   start_date = '04/07/2023',
#'   sample_window = 10,
#'   duration = 10,
#'   climate_criteria_table = climate_criteria_table,
#'   segment_type = "season"
#' )
find_matches <- function(rcp = NULL,
                       gcm = NULL,
                       sample_cell = NULL,
                       base_data = NULL,
                       segment_type,
                       start_date,
                       climate_criteria_table,
                       sample_window) {
  
  df_list <- list() #initalizing empty list
  
  season_list <- season_order(start_date, length(climate_criteria_table)) #creating a lis to specify which season to find percentiles from 
  
  for (i in 1:length(climate_criteria_table)) {
    start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
    
    df <- find_sample_window(start_date, sample_window, base_data) #Calculating the correct sampling window for each run 
    
    if(segment_type == "year") {
      runs <- filter_df(df, climate_criteria_table[[i]])
    }
    
    if(segment_type == "season") {
      # Call filter_df at index i of the climate variables list
      runs <- filter_df(df, climate_criteria_table[[i]], season_list[[i]])
    }
    
    # Appends the output to the df_list
    df_list[[i]] <- runs
    start_date <- start_date + lubridate::years(1) # Add one year to start_date
  }
  
  # Returns the list of filtered years for each data frame that can be put into the stitches function
  return(df_list)
  
}
