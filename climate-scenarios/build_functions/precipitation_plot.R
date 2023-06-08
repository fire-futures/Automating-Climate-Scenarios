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