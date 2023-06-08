#' Filter Data Frame Based on Climate Table and Season List
#'
#' This function filters a data frame based on a climate table and an optional season list.
#'
#' @param df Data frame to filter.
#' @param climate_criteria_table Climate table, which is a data frame specifying the climate variable, lower percentile,
#' upper percentile, and corresponding thresholds for filtering.
#' @param season_list Optional season list to filter the data frame by season. Default is NULL.
#' @return Filtered data frame or a concatenated string of water years and seasons.
#' @examples
#' filter_df(df, climate_criteria_table, season_list = c("winter", "spring"))
#' filter_df(df, climate_criteria_table)
#' @export
#' 
#' We will define wind stats as days that are above a certain threshold
wind_threshold <- 6.5 # we chose 15 mph or 6.5 m/s to dictate above average windy days 
 
filter_df <- function(df, climate_criteria_table, season_list = NULL) {
  
  if (!is.null(season_list)) {
    # Group data frame by water year and season, and calculate summary statistics
    df <- df %>% group_by(water_year, season) %>% summarise(wind = sum(wind >= wind_threshold),
                                                            max_temp = mean(max_temp),
                                                            min_temp = mean(min_temp),
                                                            precip = sum(precip),
                                                            min_humidity = mean(min_humidity),
                                                            max_humidity = mean(max_humidity))
    
    for (j in 1:nrow(climate_criteria_table)) {
      df <- df %>% filter(season %in% season_list) # Filter by season list
      
      climate_var <- climate_criteria_table[j, "variable"]
      lower <- climate_criteria_table[j, "lower"] 
      upper <- climate_criteria_table[j, "upper"]
      
      lower_perc <- quantile(df[[climate_var]], lower, na.rm = TRUE) # Calculate lower percentile
      
      upper_perc <- quantile(df[[climate_var]], upper, na.rm = TRUE) # Calculate upper percentile
      
      df <- df %>% filter(!is.na(!!sym(climate_var)), !!sym(climate_var) >= lower_perc, !!sym(climate_var) <= upper_perc) # Filter by thresholds
    }
    
    return(paste(df$water_year, df$season)) # Return concatenated string of water years and seasons
  } else {
    # Group data frame by water year and calculate summary statistics
    df <- df %>% group_by(wy = water_year) %>% 
      summarise(wind = sum(wind >= wind_threshold),
                max_temp = mean(max_temp),
                min_temp = mean(min_temp),
                precip = sum(precip),
                min_humidity = mean(min_humidity),
                max_humidity = mean(max_humidity))
    
    for (j in 1:nrow(climate_criteria_table)) {
      climate_var <- climate_criteria_table[j, "variable"]
      lower <- climate_criteria_table[j, "lower"] 
      upper <- climate_criteria_table[j, "upper"]
  
      lower_perc <- quantile(df[[climate_var]], lower, na.rm = TRUE) # Calculate lower percentile
      
      upper_perc <- quantile(df[[climate_var]], upper, na.rm = TRUE) # Calculate upper percentile
      
      df <- df %>% filter(!is.na(!!sym(climate_var)), !!sym(climate_var) >= lower_perc, !!sym(climate_var) <= upper_perc) # Filter by thresholds
    }
    
    return(df$wy) # Return water years
  }
}
