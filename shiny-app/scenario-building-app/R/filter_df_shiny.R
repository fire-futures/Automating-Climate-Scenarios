wind_threshold <- 6.5 # we chose 15 mph or 6.5 m/s to dictate above average windy days

filter_df_shiny <- function(df, climate_criteria_table, season_list = NULL) {
  if (!is.null(season_list)) {
    df <- df %>% group_by(water_year, season) %>% summarise(wind = sum(wind >= wind_threshold),
                                                            max_temp = mean(max_temp),
                                                            min_temp = mean(min_temp),
                                                            precip = sum(precip),
                                                            min_humidity = mean(min_humidity),
                                                            max_humidity = mean(max_humidity))
    for (j in 1:nrow(climate_criteria_table)) {
      df <- df %>% filter(season %in% season_list)
      for (var in c("wind", "min_humidity", "max_humidity", "min_temp", "max_temp", "precip")) {
        lower <- climate_criteria_table[j, paste0(var, "_l")]
        upper <- climate_criteria_table[j, paste0(var, "_u")]
        lower_val <- quantile(df[[var]], lower, na.rm = TRUE) # Calculate the actual value from the percentile
        upper_val <- quantile(df[[var]], upper, na.rm = TRUE)
        if (!is.na(lower_val) & !is.na(upper_val)) {
          df <- df %>% filter(!is.na(!!sym(var)), !!sym(var) >= lower_val, !!sym(var) <= upper_val)
        }
      }
    }
    return(paste(df$water_year, df$season))
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