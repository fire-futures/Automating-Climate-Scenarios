#' Function to randomly select elements from list of matching yearly options
#' randomly_select()
#' This function randomly selects one element from each list in the input \code{segment_samples}.
#' If a list only contains one element, that element is selected.
#' The randomly selected elements are returned as a list \code{sample_grid_series}
#' 
#' @param segment_samples : A list of lists, each sublists contains elements from which one will be randomly selected.
#' Randomly select one of each option
#' 
#' @return A list containing on randomly selected element from each input sublist.
#' 

randomly_select <- function(segment_samples = segment_samples) {
  
  # Create blank list for randomly selected elements
  sample_grid_series <- list()
  
  # Randomly select one element of each list
  for (i in seq_along(segment_samples)) {
    if (length(segment_samples[[i]]) == 1) {
      sample_grid_series[[i]] <- segment_samples[[i]]
    } 
    else {      # if only one option, select it (added this to fix random error)
      sample_grid_series[[i]] <- base::sample(segment_samples[[i]], size = 1, replace = TRUE)
    }
  }
  
  # Assign the resulting list to a variable in the global environment
  assign("sample_grid_series", sample_grid_series, envir = .GlobalEnv)
  
  # Return resulting list
  return(sample_grid_series)
  
}

