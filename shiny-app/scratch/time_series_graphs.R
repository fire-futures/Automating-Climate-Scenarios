source("/capstone/firefutures/data_cleaning/packages.R")
source("/capstone/firefutures/data_cleaning/cal_adapt_4models_allscenarios.R")

#first attempt at obtaining desired summary info
canESM2_time_series %>%
  mutate(month = lubridate::month(time)) %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year, month) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1)) %>%
  arrange(year) %>%
  filter(year > 2020)

# Time series graphs grouping by YEAR and MONTH
canESM2_time_series %>%
  mutate(month = lubridate::month(time)) %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year, month) %>%
  filter(year > 2014,
         year < 2016) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1)) %>%
  gather(key = "variable", value = "value", -year, -month) %>%
  ggplot(aes(x = as.Date(paste(year, month, "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "") +
  facet_wrap(~ variable, scales = "free_y")




canESM2_time_series %>%
  mutate(month = lubridate::month(time)) %>%
  mutate(year = lubridate::year(time)) %>%
  filter(month > 2 & month < 7) %>%
  group_by(year, month) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1)) %>%
  ggplot(aes(y = mean_max_temp)) +
  geom_boxplot() +
  facet_wrap(~month, ncol = 4)
  
  
  gather(key = "variable", value = "value", -year, -month) %>%
  ggplot(aes(x = as.Date(paste(year, month, "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "") +
  facet_wrap(~ variable, scales = "free_y")






# Time series graphs grouping by just YEAR
canESM2_time_series %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year) %>%
  filter(year > 2020,
         year < 2045) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1)) %>%
  gather(key = "variable", value = "value", -year) %>%
  mutate(variable = factor(variable, levels = c("mean_min_temp", "mean_max_temp", "total_precip", "mean_wind", "mean_min_humidity", "mean_max_humidity"))) %>%
  ggplot(aes(x = as.Date(paste(year, "1", "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "") +
  facet_wrap(~ variable, nrow = 3, ncol = 2, scales = "free_y") +
  theme_minimal() +
  guides(color = FALSE)




