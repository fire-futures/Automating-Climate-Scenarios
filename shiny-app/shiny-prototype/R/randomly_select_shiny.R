#' Function to randomly select out of matching yearly options---------
#' randomly_select()
#'
#' @param run_samples : output from 
#' Randomly select one of each option
#' @return
#' @export Save as new list sample_grid_series
#'
#' @examples

randomly_select <- function(run_samples = run_samples) {
  
  # Create blank list for randomly selected elements
  sample_grid_series <- list()
  
  # Randomly select one element of each list
  for (i in seq_along(run_samples)) {
    if (length(run_samples[[i]]) == 1) {
      sample_grid_series[[i]] <- run_samples[[i]]
    } 
    else {      # if only one option, select it (added this to fix random error)
      sample_grid_series[[i]] <- base::sample(run_samples[[i]], size = 1, replace = TRUE)
    }
  }
  
  # Assign the resulting list to a variable in the global environment
  assign("sample_grid_series", sample_grid_series, envir = .GlobalEnv)
  
  # Return resulting list
  return(sample_grid_series)
  
}

