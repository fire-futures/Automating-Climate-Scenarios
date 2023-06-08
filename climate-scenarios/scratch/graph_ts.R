library(tidyverse)

#grid_cell_23696 <- read_csv("grid_cell_23696_time_series/grid_cell_23696_time_series.csv")

# Generate a sequence of dates with a length equal to the number of rows in df
date_seq <- seq.Date(from = as.Date(ui_start_date, format = "%m/%d/%Y", origin = "1970-01-01"), by = "day", length.out = nrow(grid_cell_23696))

# Assign the sequence of dates to the 'date' column of the data frame
grid_cell_23696$date <- date_seq

# Group the data by the year_season order
grid_cell_23696 %>%
  mutate(month = lubridate::month(date)) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year < 2005 & year > 2002) %>%
  group_by(year, month) %>%
  summarise(water_year = first(water_year),
            season = first(season),
            mean_wind = mean(wind),
            mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp),
            total_precip = sum(precip),
            mean_max_humidity = mean(max_humidity),
            mean_min_humidity = mean(min_humidity)) %>%
  gather(key = "variable", value = "value", -year, -month, -water_year, -season) %>%
  ggplot(aes(x = as.Date(paste(year, month, "1", sep="-")), y = value, group = variable, color = variable)) +
  geom_line() +
  labs(x = "Date", y = "", color = "", title = "") +
  facet_wrap(~ variable, scales = "free_y")













# test <- grid_cell_23696 %>%
#   filter(water_year == 2002)
#
# # Create a new variable that combines the year and season information
# grid_cell_23696 <- grid_cell_23696 %>%
#   mutate(year_season = paste0(water_year, season))
#
# # Find the unique combinations of year and season and assign them an order
# year_season_order <- match(grid_cell_23696$year_season, unique(grid_cell_23696$year_season))
#
# # Add a new variable to the data frame with the year-season order
# grid_cell_23696 <- grid_cell_23696 %>%
#   mutate(year_season_order = year_season_order)
