#' Yearly Precipitation Plot
#'
#' This function generates a line plot that represents the total yearly precipitation 
#' for each 'Water Year'. A 'Water Year' is defined as beginning in October and ending in September.
#'
#' @param time_series A data frame that contains the precipitation data. 
#'                    The data frame should contain the following variables: 
#'                    sequence_date (date) and precip (numeric).
#'
#' @return A ggplot object that represents the total yearly precipitation per 'Water Year'.
#'         The x-axis represents the 'Water Year' and the y-axis represents the total yearly precipitation. 
#'
precipitation_plot <- function(time_series){
  
  time_series %>% 
  mutate(new_water_year = ifelse(month(sequence_date) > 9, year(sequence_date) + 1, year(sequence_date))) %>%
  group_by(new_water_year) %>%
  summarise(total_yearly_precip = sum(precip)) %>%
  ggplot(aes(x = new_water_year, y = total_yearly_precip)) +
  geom_line() +
  labs(x = "Water Year", y = "Total Yearly Precipitation", title = "Yearly Precipitation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}