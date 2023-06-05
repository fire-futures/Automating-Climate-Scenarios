# LOAD PACKAGES ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(markdown)
library(fresh)
library(gridExtra)
library(zip) #for creating zipfiles

#packages from workflow markdown doc
library(strex)
library(lubridate)
library(purrr)
library(here)
library(sf)
library(caladaptr)  # remotes::install_github("ucanr-igis/caladaptr")
library(ggrepel)
library(rhandsontable) #For creating interactive table 



# READ IN DATA ----

source("/capstone/firefutures/data_cleaning/packages.R")
source("/capstone/firefutures/data_cleaning/cal_adapt_4models_allscenarios.R")
#source("/capstone/firefutures/shiny-app/scratch/cut_stitch_shiny.R")


# Assign default values to allow workflow functions to source
ui_grid_cells <- NULL
ui_gcm <- NULL
ui_rcp <- NULL


# SOURCE IN FUNCTIONS ----

# Construct the full path to the build_functions directory
build_functions_dir <- file.path("~/fire_futures/climate-scenarios/build_functions")

# Get a list of all the R files in the build_functions directory
r_files <- list.files(build_functions_dir, pattern = "\\.R$", full.names = TRUE)

# Use lapply() to source each file in the directory
lapply(r_files, source)



# DATA MANIPULATION ---- 

canESM2_time_series <- CanESM2_rcp85 %>%
  select(time, wind_1, max_temp_1, min_temp_1, precip_1, max_humidity_1, min_humidity_1)

canESM2_rcp85_time_series_summary <- CanESM2_rcp85 %>%
  select(time, wind_1, max_temp_1, min_temp_1, precip_1, max_humidity_1, min_humidity_1) %>%
  group_by(lubridate::year(time)) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1))

# Loop through each column and add a percentile column
for (col in colnames(canESM2_rcp85_time_series_summary)[-1]) {
  percentile_col_name <- paste0(col, "_pctl")
  canESM2_rcp85_time_series_summary <- canESM2_rcp85_time_series_summary %>%
    mutate(!!percentile_col_name := ntile(!!sym(col), 100))
}

scenarios <- c("Probable_1_CanESM2", "Probable_2_CNRM_CM5", "Probable_3_HadGEM2ES", "Probable_4_MIROC5")

models <- c("CanESM2", "CNRM_CM5", "HadGEM2ES", "MIROC5")

RCP_assumptions <- c(45, 85)

run_type <- c("year", "season")

grid_list <- seq(48693, 48698)

sample_cell_grid_list <- paste0("grid_cell_", seq(48693, 48698))






# criteria list (to be removed once table is added) ----
criteria_list <- list(
  data.frame(
    segment_spec_id = '1', # may need for metadata clarity?
    notes = 'Likely segment',
    variable = c('wind', 'precip', 'max_temp'),
    lower = c(.1, .1, .1),
    upper = c(.99, .99, .99)
  ),
  data.frame(
    segment_spec_id = '2',
    notes = 'Average-Above Average Dry',
    variable = c('min_temp'),
    lower = c(.0),
    upper = c(1)
  ),
  data.frame(
    segment_spec_id = '3',
    notes = 'Average: wind, precip, max_temp',
    variable = c('wind', 'precip', 'max_temp'),
    lower = c(0, 0, 0),
    upper = c(1, 1, 1)
  ),
  data.frame(
    segment_spec_id = '4',
    notes = 'No specification',
    variable = c('wind'),
    lower = (0),
    upper = (1) #ALL RUNS IN SAMPLE WINDOW
  )
)

criteria_list <- repeat_segments(criteria_list, c(1,2,3,4), 4) #repeat 1-4 four times

ui_climate_criteria_table <- criteria_list




















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


