#' Plot Stitched Time Series by Segment
#'
#' This function generates a line plot of stitched time series data by segment.
#'
#' @param time_series A data frame containing the time series data.
#' @param upper_year The upper limit of the years to include in the plot
#' @param lower_year The lower limit of the years to include in the plot
#'
#' @return A ggplot object displaying the stitched time series plot.
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom ggplot2 geom_line
#' @export
segments_plots <- function(time_series, upper_year, lower_year) {
  
  plot <- time_series %>%
    mutate(month = lubridate::month(sequence_date)) %>%
    mutate(year = lubridate::year(sequence_date)) %>%
    filter(year < upper_year & year > lower_year) %>%
    group_by(year, month) %>%
    summarise(water_year = first(water_year),
              season = first(season),
              mean_max_temp = mean(max_temp),
              mean_min_temp = mean(min_temp)) %>%
    tidyr::gather(key = "variable", value = "value", -year, -month, -water_year, -season) %>%
    ggplot2::ggplot(aes(x = as.Date(paste(year, month, "1", sep = "-")), y = value, group = variable, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "", color = "", title = "Stitched Time Series by Segment") +
    ggplot2::facet_wrap(~year, scales = "free_x")
  
  # Update the key labels
  plot <- plot + ggplot2::scale_color_manual(values = c("mean_max_temp" = "red", "mean_min_temp" = "blue"),
                                             labels = c("mean_max_temp" = "Mean Monthly Max Temp", "mean_min_temp" = "Mean Monthly Min Temp"))
  
  return(plot)
}
