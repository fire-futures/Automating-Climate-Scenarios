#' Get Metadata
#'
#' This function generates metadata based on user input and saves it to a CSV file.
#'
#' @details The function requires additional information, such as `all_cells`, `season` definitions, `scenario_duration`, `available_samples`, `desired_samples`, `historical_extremes` inclusion, and `calibration` inclusion. These details still need to be implemented in the function.
#'
#' @return None (metadata is saved to a CSV file and stored in the global environment).
#' @import dplyr
#' @import tidyr
#' @export
get_metadata <- function(root_output_folder_name=root_output_folder_name) {
  
  # still need: all_cells, season definitions, scenario duration, available samples, desired_samples, historical_extremes included y/n, calibration included y/n
  
  # make a dataframe with the number of rows that we nee
  climate_vars_df <- do.call(rbind, criteria_list) 

  # add in all the columns we need and save metadata to the global environment
  metadata <<- climate_vars_df %>% 
    mutate(gcm_rcp = ifelse(
      ui_caladapt_ind == 'Y', paste0(ui_gcm, "_", ui_rcp), 'NA: user input data')) %>% 
    mutate(sample_cell = ui_sample_cell_metaData) %>% 
    mutate(all_cells = paste(ui_df_names, collapse = ", ")) %>%
    #mutate(dry_season_def = 'not functional') %>% # not yet functional
    #mutate(wet_season_def = 'not functional') %>% # not yet functional
    mutate(scenario_start_date = ui_start_date) %>% 
    mutate(segment_type = ui_segment_type) %>% 
    mutate(scenario_duration = ifelse(
      ui_segment_type == 'year', # if the user is building years
      length(criteria_list), # there are as many years as there are climate criteria
      round(length(criteria_list)/4,2))) %>% # otherwise the user is building by season so we divide by 4
    mutate(sample_window = ui_sample_window) %>%
    merge(number_matches)
   # mutate(historical_extremes_incl = 'N') %>% # not yet functional
    #mutate(calibration_included = 'N') # not yet functional

  # Define the file path for the CSV file
  csv_file_path <- file.path(root_output_folder_name, "metadata.csv")
  
  # Save the file in the specified folder
  write.csv(metadata, csv_file_path, row.names = FALSE)
  
  
}
