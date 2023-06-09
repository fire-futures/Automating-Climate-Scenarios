#' sample_window Function
#'
#' Filter a data frame based on a specified sampling window.
#'
#' @param start_date A character string representing the start date in the format "%mm/%dd/%yyyy".
#' @param sample_window A numeric value specifying the number of years for the sampling window.
#' @param df A data frame to be filtered.
#'
#' @return A filtered data frame based on the specified sampling window.
#'
#' @details This function takes a start date, a sample window (in years), and a data frame as input.
#' It then filters the data frame based on the start date and the sample window, and returns the filtered data frame.
#' Half of the sampling window before the start date and half after. If there is an odd number entered all years are rounded up. 
#' If the sample_window argument is NULL, a warning is issued, and the entire data frame is returned.
#' 
#' @examples
#' # Example usage:
#' df <- data.frame(time = as.Date(c("01/01/2010", "01/01/2015", "01/01/2020"), format = "%m/%d/%Y"),
#'                  value = c(1, 2, 3))
#' start_date <- "01/01/2018"
#' sample_window <- 5
#' filtered_df <- sample_window(start_date, sample_window, df)
#'
#' @import lubridate
#' @import dplyr
#' @keywords data manipulation, filtering, sampling
#'
#' @export
find_sample_window <- function(start_date, sample_window, df) {
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