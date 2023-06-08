#' Extracts data from a data frame based on RCP, GCM, and sample cell
#' Adds Water Year Column
#'
#' This function extracts data from a data frame based on specified values of
#' RCP (Representative Concentration Pathway), GCM (Global Climate Model), and
#' sample cell.
#'
#' @param rcp Numeric value indicating the RCP (Representative Concentration Pathway) scenario. Allowed values are 45 or 85.
#' @param gcm Character value indicating the GCM (Global Climate Model) option. Allowed options are "MIROC5", "HadGEM2ES", "CanESM2", or "CNRM_CM5".
#' @param sample_cell Numeric value indicating the sample cell for which data needs to be extracted.
#'
#' @return A data frame containing the extracted data, with columns for time, the specified sample cell, and a water year column.
#' @export
#'
#' @examples
#' # Extract data for RCP 45, GCM "MIROC5", and sample cell 2
#' find_df_example <- find_df(45, "MIROC5", 2)
#'
#' # Extract data for RCP 85, GCM "CNRM_CM5", and sample cell 1
#' find_df_example <- find_df(85, "CNRM_CM5", 1)
#' 
#' 

find_df <- function(rcp, gcm, sample_cell) {
  
  # Define the valid GCM options
  valid_gcms <- c("MIROC5", "HadGEM2ES", "CanESM2", "CNRM_CM5")
  
  # Check if input gcm is valid. If not, return an error message.
  if (!gcm %in% valid_gcms) {
    stop("Invalid value for gcm. Allowed options are: ", paste(valid_gcms, collapse = ", "))
  }
  
  # Ensure that input rcp is numeric
  rcp <- as.numeric(rcp)
  
  # Check if input rcp is valid (either 45 or 85). If not, return an error message.
  if (!rcp %in% c(45, 85)) {
    stop("Invalid value for rcp. Only 45 or 85 are allowed.")
  }
  
  # Construct the name of the data frame to extract, based on the input GCM and RCP values
  df_name <- paste(gcm, paste0("rcp", rcp), sep = "_")
  
  # Extract the data frame from the global environment, using the constructed name
  df <- get(df_name, envir = globalenv())
  
  # Get the names of the columns in the data frame that correspond to the specified sample cell
  var_names <- grep(paste0("_", sample_cell, "$"), names(df), value = TRUE)
  
  # Select the time column and the columns corresponding to the specified sample cell,
  # and rename the columns to remove the sample cell suffix
  combined_df <- select(df, time, one_of(var_names)) %>%
    rename_all( ~ gsub(paste0("_", sample_cell, "$"), "", .))
  
  # Return the resulting data frame
  combined_df <- combined_df %>% mutate(water_year = ifelse(month(time) > 9, year(time) + 1, year(time)))
  return(combined_df) # adding water year column 
}