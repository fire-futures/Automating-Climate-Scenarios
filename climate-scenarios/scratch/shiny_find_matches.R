ui_sample_cell <- grid_cell_46662 # Choose your sample grid cell of interest. It should match one of the grid cells chosen in the spatial extent defined above. 

ui_sample_cell_metaData <- 'grid_cell_46662'# In quotes for your metadata file

ui_segment_type <- 'season' #year or season
#season is defined in find_matches.R currently: wet is months October 1st-March 31st and dry April 1st-September 30th

ui_start_date <- '2000/12/01' # YYYY/MM/DD


ui_sample_window <- 20 #Even number: split above and below start date

wind_threshold <- 6.5 # we chose 15 mph or 6.5 m/s to dictate above average windy days 

season_order_shiny <- function(start_date, n) {
  season <- ifelse(month(start_date) %in% c(10, 11, 12, 1, 2, 3), "wet", "dry")
  season_list <- vector("list", n)
  
  for (i in 1:n) {
    season_list[[i]] <- season
    season <- ifelse(season == "wet", "dry", "wet")
  }
  
  return(season_list)
} 


filter_df_shiny <- function(df, climate_criteria_table, season_list = NULL) {
  if (!is.null(season_list)) {
    df <- df %>% group_by(water_year, season) %>% summarise(wind = sum(wind >= wind_threshold),
                                                            max_temp = mean(max_temp),
                                                            min_temp = mean(min_temp),
                                                            precip = sum(precip),
                                                            min_humidity = mean(min_humidity),
                                                            max_humidity = mean(max_humidity))
    for (j in 1:nrow(climate_criteria_table)) {
      filtered_df <- df %>% filter(season %in% unlist(season_list))
      
      for (var in c("wind", "min_humidity", "max_humidity", "min_temp", "max_temp", "precip")) {
        lower <- climate_criteria_table[j, paste0(var, "_l")] 
        upper <- climate_criteria_table[j, paste0(var, "_u")]
        
        lower_val <- quantile(df[[var]], lower, na.rm = TRUE) 
        upper_val <- quantile(df[[var]], upper, na.rm = TRUE)
        
        if (!is.na(lower_val) & !is.na(upper_val)) {
          filtered_df <- filtered_df %>% filter(!is.na(!!sym(var)), !!sym(var) >= lower_val, !!sym(var) <= upper_val)
        }
      }
    }
    return(paste(filtered_df$water_year, filtered_df$season))
  } else {
    df <- df %>% group_by(wy = water_year) %>% 
      summarise(wind = sum(wind >= wind_threshold),
                max_temp = mean(max_temp),
                min_temp = mean(min_temp),
                precip = sum(precip),
                min_humidity = mean(min_humidity),
                max_humidity = mean(max_humidity))
    
    for (j in 1:nrow(climate_criteria_table)) {
      for (var in c("wind", "min_humidity", "max_humidity", "min_temp", "max_temp", "precip")) {
        lower <- climate_criteria_table[j, paste0(var, "_l")] 
        upper <- climate_criteria_table[j, paste0(var, "_u")]
        
        lower_val <- quantile(df[[var]], lower, na.rm = TRUE) 
        upper_val <- quantile(df[[var]], upper, na.rm = TRUE)
        
        if (!is.na(lower_val) & !is.na(upper_val)) {
          df <- df %>% filter(!is.na(!!sym(var)), !!sym(var) >= lower_val, !!sym(var) <= upper_val)
        }
      }
    }
    return(df$wy)
  }
}





season_order_shiny <- function(start_date, n) {
  season <- ifelse(month(start_date) %in% c(10, 11, 12, 1, 2, 3), "wet", "dry")
  season_list <- vector("list", n)
  
  for (i in 1:n) {
    season_list[[i]] <- season
    season <- ifelse(season == "wet", "dry", "wet")
  }
  
  return(season_list)
} 

season_test <- season_order_shiny('2002/07/01', nrow(climate_criteria))

find_sample_window_shiny <- function(start_date, sample_window, df) {
  start_date <- as.Date(start_date, format = "%Y/%m/%d")# Convert start_date to date format
  start <- (start_date - years(ceiling(sample_window/2))) # Calculate the first year in the sampling window
  end <- (start_date +  years(ceiling(sample_window/2))) # Calculate the last year in the sampling window
  
  # Check if sample_window argument is not NULL
  if (!is.null(sample_window)) {
    # Filter the data frame based on the start and end dates
    df <- df %>% filter(time >= start & time <= end)
  } else {
    df <- df
    # If sample_window is NULL, issue a warning and return the entire data frame
    warning("sample_window argument is NULL. Specify sampling window, or else we'll pull from all available years.")
  }
}



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
  
  for (i in 1:length(climate_criteria_table)) {
    start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
    
    df <- find_sample_window_shiny(start_date, sample_window, base_data)
    
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


climate_criteria <- data.frame(
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
  precip_u = c(1, 0.99, 0.99, 0.99, 0.99, 0.75),
  notes = c("likely", "likely", "likely", "likely","likely", "likely")
)



segment_samples <<-
  find_matches_shiny(
    base_data = ui_sample_cell, 
    start_date = ui_start_date,
    sample_window = ui_sample_window,
    climate_criteria_table = climate_criteria,
    segment_type = ui_segment_type,
  )

head(segment_samples)

filter_df_shiny(grid_cell_46662, climate_criteria[1,,drop = FALSE], season_test[1])
