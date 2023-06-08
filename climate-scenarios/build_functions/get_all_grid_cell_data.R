# get all the data from this GCM and RCP, including historical
get_all_grid_cell_data <- function(grid_cell_id, lat, lon, ui_gcm, ui_rcp) {
  
  # Wind
  wind_slug <- paste0('wspeed_day_', ui_gcm , '_', 'rcp', ui_rcp)
  wind_slug_historical <- paste0('wspeed_day_', ui_gcm , '_historical')
  wind_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
    ca_slug(c(wind_slug, wind_slug_historical))
 
   wind_tbl <- wind_data %>% 
    ca_getvals_tbl(quiet = TRUE) %>% 
    select(dt, val) %>% 
    mutate(val = as.numeric(val)) %>% 
    rename(wind = val)
   
   # Min Relative Humidity
   minrh_slug <- paste0('relhumid-min_day_', ui_gcm , '_', 'rcp', ui_rcp)
   minrh_slug_historical <- paste0('relhumid-min_day_', ui_gcm , '_historical')
   minrh_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(minrh_slug, minrh_slug_historical))
   
   minrh_tbl <- minrh_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(min_humidity = val)
   
   # max relative humidity 
   maxrh_slug <- paste0('relhumid-max_day_', ui_gcm , '_', 'rcp', ui_rcp)
   maxrh_slug_historical <- paste0('relhumid-max_day_', ui_gcm , '_historical') 
   maxrh_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(maxrh_slug, maxrh_slug_historical))
   
   maxrh_tbl <- maxrh_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(max_humidity = val)
   
   # min temperature
   mintemp_slug <- paste0('tasmin_day_', ui_gcm , '_', 'rcp', ui_rcp)
   mintemp_slug_historical <- paste0('tasmin_day_', ui_gcm , '_historical')
   mintemp_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(mintemp_slug, mintemp_slug_historical))
   
   mintemp_tbl <- mintemp_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(min_temp = val)
   
   # max temperature
   maxtemp_slug <- paste0('tasmax_day_', ui_gcm , '_', 'rcp', ui_rcp)
   maxtemp_slug_historical <- paste0('tasmax_day_', ui_gcm , '_historical')
   maxtemp_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(maxtemp_slug, maxtemp_slug_historical))
   
   maxtemp_tbl <- maxtemp_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(max_temp = val)
   
   # precipitation 
   pr_slug <- paste0('pr_day_', ui_gcm , '_', 'rcp', ui_rcp)
   pr_slug_historical <- paste0('pr_day_', ui_gcm , '_historical')
   pr_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(pr_slug, pr_slug_historical))
   
   pr_tbl <- pr_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(precip = val)
   
  # join all climate variables together into one climate table 
   joined_tbl <- wind_tbl %>%
     inner_join(minrh_tbl, by = "dt") %>%
     inner_join(maxrh_tbl, by = "dt") %>%
     inner_join(mintemp_tbl, by = "dt") %>%
     inner_join(maxtemp_tbl, by = "dt") %>%
     inner_join(pr_tbl, by = "dt") %>% 
     mutate(dt = as_date(dt)) %>% 
     rename(time = dt) %>% 
     # add water year column
     mutate(water_year = ifelse(month(time) > 9, year(time) + 1, year(time))) %>% 
     # designate season given a month
     mutate(season = ifelse(lubridate::month(time) %in%  c(10, 11, 12, 1, 2, 3), "wet", "dry")) %>% 
     # changing units to RHESSys units
      # wind: in m/s so no conversion needed
      # relative humidity: % between 0 and 100 -> value between 0 and 1
     mutate(min_humidity = min_humidity/100) %>% 
     mutate(max_humidity = max_humidity/100) %>% 
      #.temperature: degrees K -> degrees C
     mutate(min_temp = min_temp - 273.15) %>% 
     mutate(max_temp = max_temp - 273.15) %>% 
      # precipitation: kg/m^2/s -> m. Equation used: multiply value by 86,400 (24 hours/day x 60 minutes/hour x 60 seconds/minute) to get daily values in mm. then divide by 1000 to get daily values in meters. Source: https://cal-adapt.org/help/glossary/
     mutate(precip = precip * 86400/1000)
   
   # save joined table for each grid cell in the global environment
   grid_cell_df_naming <- paste0("grid_cell_", grid_cell_id, "_", ui_gcm, "_", ui_rcp)
   assign(grid_cell_df_naming, joined_tbl, envir = .GlobalEnv)
  # temp in celcius, rain in meters  
}



