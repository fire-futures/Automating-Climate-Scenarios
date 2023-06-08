library(knitr)
library(kableExtra)

time_series <- grid_cell_23696_time_series %>% 
  select(old_date, min_temp, max_temp, precip, min_humidity, max_humidity, wind) %>%
  mutate(old_date = round(old_date,3), min_temp = round(min_temp,3), max_temp = round(max_temp,3), precip = round(precip, 3), min_humidity = round(min_humidity,3), max_humidity = round(max_humidity,3), wind = round(wind,3)) %>% 
  rename(Date = old_date, "Min Temp (°C)" = min_temp, "Max Temp (°C)" = max_temp, Precipitation = precip, "Min Humidity" = min_humidity, "Max Humidity" = max_humidity, Wind = wind) %>% 
  filter(year(Date) == 2024)

table <- kable(time_series, format = "html", align = "c", caption = "Time Series Data")

finaltable <- table %>%
  kable_classic() %>%       
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                stripe_color = c("#E0E0E0", "white")) %>% 
  add_header_above(c(" " = 1, "Temperature (°C)" = 2, " " = 1, "Humidity" = 2, " " = 1))


table %>%
  kable_classic() %>%  # Apply a classic table theme
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                stripe_color = c("#E0E0E0", "white"),
                full_width = FALSE) %>%  
  row_spec(row = 0, bold = TRUE, color = "white", background = "#9E9E9E") %>%   # Header row formatting
  row_spec(row = c(1, nrow(time_series)), bold = TRUE)

print(finaltable)


