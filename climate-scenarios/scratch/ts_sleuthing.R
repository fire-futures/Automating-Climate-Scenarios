library(tidyverse)

#grid_cell_23696_time_series <- read_csv("grid_cell_23696_time_series/grid_cell_23696_time_series.csv")

# Generate a sequence of dates with a length equal to the number of rows in df
#date_seq <- seq.Date(from = as.Date(ui_start_date, format = "%m/%d/%Y", origin = "1970-01-01"), by = "day", length.out = nrow(grid_cell_23696_time_series))

# Assign the sequence of dates to the 'date' column of the data frame
#grid_cell_23696_time_series$date <- date_seq

# Group the data by the year_season order. MONTHLY.
grid_cell_46896_time_series %>%
  mutate(month = lubridate::month(sequence_date)) %>%
  mutate(year = lubridate::year(sequence_date)) %>%
  filter(year < 2013 & year > 2000) %>%
  group_by(year, month) %>%
  summarise(water_year = first(water_year),
            season = first(season),
            mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp)) %>%
  gather(key = "variable", value = "value", -year, -month, -water_year, -season) %>%
  ggplot(aes(x = as.Date(paste(year, month, "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "CanESM RCP 8.5 Year Segment Scenario") +
  facet_wrap(~year, scales = "free_x")

# Group the data by the year_season order DAILY
grid_cell_23696_time_series %>%
  mutate(month = lubridate::month(sequence_date)) %>%
  mutate(year = lubridate::year(sequence_date)) %>%
  filter(year < 2013 & year > 2000) %>%
  gather(key = "variable", 
         value = "value", 
         -year, -month, -water_year, -season, -wind, -min_humidity, -max_humidity, -precip, -date) %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "CanESM RCP 8.5 Year Segment Scenario") +
  facet_wrap(~year, scales = "free_x")




## REPEATING THE ABOVE GRAPHS FOR THE PREDICTIONS FROM CAL-ADAPT (NOT CONSTRUCTED SCENARIOS)


# Group the data by the year_season order. MONTHLY.
grid_cell_23696 %>%
  mutate(month = lubridate::month(time)) %>%
  mutate(year = lubridate::year(time)) %>%
  filter(year < 2013 & year > 2000) %>%
  group_by(year, month) %>%
  summarise(water_year = first(water_year),
            season = first(season),
            mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp)) %>%
  gather(key = "variable", value = "value", -year, -month, -water_year, -season) %>%
  ggplot(aes(x = as.Date(paste(year, month, "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "CanESM RCP 8.5 CAL ADAPT") +
  facet_wrap(~year, scales = "free_x")


# Group the data by the year_season order DAILY
grid_cell_23696 %>%
  mutate(month = lubridate::month(time)) %>%
  mutate(year = lubridate::year(time)) %>%
  filter(year < 2013 & year > 2000) %>%
  gather(key = "variable", 
         value = "value", 
         -year, -month, -water_year, -season, -wind, -min_humidity, -max_humidity, -precip, -time) %>%
  ggplot(aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "CanESM RCP 8.5 CalAdapt") +
  facet_wrap(~year, scales = "free_x")
