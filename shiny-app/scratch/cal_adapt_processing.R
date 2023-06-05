library(strex)
library(tidyverse)
library(lubridate)

#Cleaning Cal-Adapt Data
#Climate Variables: Min Temperature, Max Temperature, Max Humidity, Min Humidity, Relative Humidity, Wind Speed

#Finding all minimum temperature files
min_temp_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/min_temp_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(min_temp_files) #writing them into a list

#Function to read in all minimum temperature files and name them by their model name (after the 5th dash in the filepath
#and excluding .csv)
read_in_min <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6) #Creating the name for by subsetting the filepath after the 6th / 
  name <- str_before_last(name, '.csv') #Subsetting the name to remove the .csv at the end
  file <- read_csv(filepath, show_col_types = FALSE) %>% #read in the filepath supplied in the function call as a csv
    rename('min_1' = '0', 'min_2' = '1') %>% #rename the cal cadapt grids 
    mutate(min_1 = (min_1 - 273) * 9/5 + 32, #convert to farenhite
           min_2 = (min_2 - 273) * 9/5 + 32)
  assign(name, file,  envir = parent.frame()) #assign the name we initiated with the file we read in and put it in our global environment
} 

# Reading through list of file paths and applying the function defined above to put them in the global environment and clean them accordingly
for (i in min_temp_files) {
  read_in_min(i)
}
#####################################################################
#Finding all MAX TEMP files
#Same format as above
max_temp_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/max_temp_all", 
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
as.list(max_temp_files) #writing them into a list

read_in_max <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6) 
  name <- str_before_last(name, '.csv') 
  file <- read_csv(filepath, show_col_types = FALSE) %>%  
    rename('max_temp_1' = '0', 'max_temp_2' = '1') %>%  
    mutate(max_temp_1 = (max_temp_1 - 273) * 9/5 + 32, 
           max_temp_2 = (max_temp_2 - 273) * 9/5 + 32)
  assign(name, file,  envir = parent.frame()) 
} 


for (i in max_temp_files) {
  read_in_max(i)
}

##############################################################
#Finding all MINIMUM HUMIDITY files
#Same format as above
min_humidity_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/min_humidity_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(min_humidity_files) #writing them into a list

read_in_min_humidity <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('min_humidity_1' = '0', 'min_humidity_2' = '1') %>%
    mutate(min_humidity_1 = min_humidity_1/ 100, min_humidity_2 = min_humidity_2/100) 
  assign(name, file,  envir = parent.frame())
} 

for (i in min_humidity_files) {
  read_in_min_humidity(i)
}
########################################################################
max_humidity_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/max_humidity_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(max_humidity_files)

read_in_max_humidity <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('max_humidity_1' = '0', 'max_humidity_2' = '1') %>% 
    mutate(max_humidity_1 = max_humidity_1/100, max_humidity_2 = max_humidity_2/100)  
  assign(name, file,  envir = parent.frame())
} 

for (i in max_humidity_files) {
  read_in_max_humidity(i)
}
########################################################################
#Finding all PRECIP files
#Same format as above
precip_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/precip_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(precip_files)

read_in_precip <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('precip_1' = '0', 'precip_2' = '1') %>% 
    mutate(precip_1 = precip_1 * 86.4, precip_2 = precip_2 * 86.4) #Convert Precipitation from kg/m2/s to meters
  assign(name, file,  envir = parent.frame())
} 

for (i in precip_files) {
  read_in_precip(i)
}

########################################################################
#Finding all WIND files
#Same format as above
wind_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/wind_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(wind_files)

read_in_wind <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('wind_1' = '0', 'wind_2' = '1') 
  assign(name, file,  envir = parent.frame())
} 

for (i in wind_files) {
  read_in_wind(i)
}

########################################################
#Function to combine all climate variables so we have 12 data frames for the 4 riority models and their climate scenarios (rcp45, rcp85, historical)
model_scenario_join <- function(pattern_string) {
  pattern_vals <- grep(pattern_string, names(.GlobalEnv),value=TRUE)  #Searches for a specified pattern given in the function call 
  pattern_list <- do.call("list",mget(pattern_vals, inherits = TRUE)) #Creates a list from the retrieved objects.
  test <- pattern_list %>% 
    reduce(left_join, by = "time") #going over the pattern list an interatively joining by time variable
} 

#####################################################
#Joining all climate models with one of the three scenarios: rcp4, rcp85, historical
MIROC5_rcp45 <- model_scenario_join('MIROC5_rcp4') 
MIROC5_rcp85 <- model_scenario_join('MIROC5_rcp85') 
MIROC_historical <- model_scenario_join('MIROC5_historical') 


CanESM2_rcp85 <- model_scenario_join('CanESM2_rcp85')  
CanESM2_rcp45 <- model_scenario_join('CanESM2_rcp45') 
CanESM2_historical <- model_scenario_join('CanESM2_historical') 

HadGEM2ES_rcp85 <-  model_scenario_join('HadGEM2-ES_rcp85') 
HadGEM2ES_rcp45 <-  model_scenario_join('HadGEM2-ES_rcp45')
HadGEM2ES_historical <-  model_scenario_join('HadGEM2-ES_historical') 


CNRM_CM5_rcp85 <- model_scenario_join('CNRM-CM5_rcp85') 
CNRM_CM5_rcp45 <-  model_scenario_join('CNRM-CM5_rcp45')
CNRM_CM5_historical <- model_scenario_join('CNRM-CM5_historical') 



CanESM2_hist_rcp45 <- rbind(CanESM2_historical, CanESM2_rcp45) %>%
  select(time, wind_1, max_temp_1, min_1, precip_1, max_humidity_1, min_humidity_1) %>%#choosing to focus on just one grid cell here 
  rename(min_temp_1 = min_1) %>%
  mutate(time = lubridate::ymd(time))

write_csv(CanESM2_hist_rcp45, "shiny-prototype/data/canESM2_time_series.csv")
