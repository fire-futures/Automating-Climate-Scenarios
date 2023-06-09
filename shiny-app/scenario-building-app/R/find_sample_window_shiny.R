find_sample_window_shiny <- function(start_date, sample_window, df) {
  start_date <- as.Date(start_date, format = "%Y/%m/%d")# Convert start_date to date format
  start <- (start_date - years(ceiling(sample_window/2))) # Calculate the first year in the sampling window
  end <- (start_date +  years(ceiling(sample_window/2))) # Calculate the last year in the sampling window
  # Check if sample_window argument is not NULL
  if (!is.null(sample_window)) {
    # Filter the data frame based on the start and end dates
    df <- df %>% filter(time >= start & time <= end)
    df <- df %>% filter(time != max(time))
  } else {
    df <- df
    # If sample_window is NULL, issue a warning and return the entire data frame
    warning("sample_window argument is NULL. Specify sampling window, or else we'll pull from all available years.")
  }
}