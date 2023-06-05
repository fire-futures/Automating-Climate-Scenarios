library(tidyverse)

canESM2_time_series <- read_csv("shiny-prototype/data/canESM2_time_series.csv")

canESM2_summary_by_year <- canESM2_time_series %>%
  group_by(year) %>%
  summarize(mean_min_temp = mean(min_temp_1),
            mean_max_temp = mean(max_temp_1),
            total_precip = sum(precip_1),
            mean_wind = mean(wind_1),
            mean_min_humidity = mean(min_humidity_1),
            mean_max_humidity = mean(max_humidity_1))


#time series 
ggplot(canESM2_summary_by_year, aes(x = year, y = mean_min_temp)) +
  geom_point() +
  theme_minimal()

ggplot(canESM2_summary_by_year, aes(x = year, y = total_precip)) +
  geom_point() +
  theme_minimal()


#histogram distributions 
ggplot(canESM2_summary_by_year, aes(x = mean_min_temp)) +
  geom_histogram(bins = 15) +
  theme_minimal()

ggplot(canESM2_summary_by_year, aes(x = total_precip)) +
  geom_histogram(bins = 20) +
  theme_minimal()



