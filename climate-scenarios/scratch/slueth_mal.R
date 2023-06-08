grid_cell_46662_time_series %>% 
  group_by(water_year, season) %>% 
  summarize(max_temp = mean(max_temp)) %>% 
  ggplot(aes(water_year, max_temp)) + 
  geom_line()


test <- grid_cell_46662 %>% 
  group_by(water_year, season) %>% 
  summarize(max_temp = mean(max_temp)) %>% 
  filter(water_year >= 1987 & water_year <= 2017)







 
test <- find_sample_window(ui_start_date, ui_sample_window, ui_sample_cell)





indexed <-
  grid_cell_46662 %>% group_by(water_year) %>% summarise(max_temp = mean(max_temp)) %>%
  filter(water_year >= 1985 & water_year <= 2015)







seg1 <- filter_df(indexed, ui_climate_criteria_table[[1]])



# test2 <- grid_cell_46662_time_series %>% 
#   group_by(water_year) %>% 
#   summarize(max_temp = mean(max_temp))

lower_perc <- quantile(test$max_temp, 0, na.rm = TRUE) # Calculate lower percentile
lower_perc


upper_perc <- quantile(test$max_temp, 0.05, na.rm = TRUE) # Calculate upper percentile
upper_perc


df <- test %>% filter(!is.na(!!sym('max_temp')), !!sym('max_temp') >= lower_perc, !!sym('max_temp') <= upper_perc)

percentiles <- quantile(test$max_temp, probs = c(0, .05))

# Print the percentiles
print(percentiles)

#25%      50%      75% 
#20.64186 22.83002 27.02554 

season <- season_order(ui_start_date, length(ui_climate_criteria_table))

filter_df(df = grid_cell_46662, climate_criteria_table = ui_climate_criteria_table[[1]])

grid_cell_46662_time_series %>%
  group_by(water_year) %>%
  summarise(wind_days_over_threshold = sum(wind > wind_threshold),
            mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp),
            total_precip = sum(precip),
            mean_max_humidity = mean(max_humidity),
            mean_min_humidity = mean(min_humidity)) %>%
  gather(key = "variable", value = "value", -water_year) %>%
  mutate(variable = factor(variable, levels = c("mean_min_temp", "mean_max_temp", "total_precip", "wind_days_over_threshold", "mean_min_humidity", "mean_max_humidity"))) %>%
  ggplot(aes(x = as.Date(paste(water_year, "1", "1", sep = "-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "") +
  facet_wrap(~ variable, nrow = 2, ncol = 3, scales = "free_y") +
  theme_minimal() +
  guides(color = "none")

grid_cell_46662_time_series %>%
  group_by(water_year) %>%
  summarize(mean_max_temp = mean(max_temp)) %>%
  ggplot(aes(x = water_year, y = mean_max_temp)) +
  geom_line() +
  labs(x = "Water Year", y = "Mean Maximum Temperature", title = "Mean Maximum Temperature per Year")

