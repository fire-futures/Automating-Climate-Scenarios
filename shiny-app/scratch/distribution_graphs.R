source("/capstone/firefutures/data_cleaning/packages.R")
source("/capstone/firefutures/data_cleaning/cal_adapt_4models_allscenarios.R")

canESM2_rcp85_time_series_summary <- CanESM2_rcp85 %>%
  select(time, wind_1, max_temp_1, min_temp_1, precip_1, max_humidity_1, min_humidity_1) %>%
  group_by(lubridate::year(time)) %>%
  summarise(mean_wind = mean(wind_1),
            mean_max_temp = mean(max_temp_1),
            mean_min_temp = mean(min_temp_1),
            total_precip = sum(precip_1),
            mean_max_humidity = mean(max_humidity_1), 
            mean_min_humidity = mean(min_humidity_1))

# Loop through each column and add a percentile column
for (col in colnames(canESM2_rcp85_time_series_summary)[-1]) {
  percentile_col_name <- paste0(col, "_pctl")
  canESM2_rcp85_time_series_summary <- canESM2_rcp85_time_series_summary %>%
    mutate(!!percentile_col_name := ntile(!!sym(col), 100))
}

ggplot(data = canESM2_rcp85_time_series_summary, aes(x = total_precip)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "black") +
  geom_vline(xintercept = c(0.3, 1.2), color = "red3") +
  labs(x = "Total Precipitation (mm)", y = "Count", title = "Histogram of Total Precipitation") +
  theme_minimal()



perc <- quantile(canESM2_rcp85_time_series_summary$mean_max_temp, seq(from = 0, to = 1, by = 0.1))

l <- paste(round(perc, 1), names(perc), sep = "\n")

ggplot(canESM2_rcp85_time_series_summary, aes(x = mean_max_temp)) +
  geom_histogram(fill = "steelblue", color = "black") + 
  scale_x_continuous(breaks = perc,labels = l) +
  geom_vline(xintercept = c(64, 72), color = "red3", lwd = 1) +
  labs(x = "Average Maximum Temp °C", y = "Count", title = "Histogram of Average Maximum Temp") +
  theme_minimal()











# ggplot(data = canESM2_rcp85_time_series_summary, aes(x = total_precip)) +
#   geom_density(fill = "steelblue") +
#   theme_minimal() +
#   scale_x_continuous(sec.axis = sec_axis(~ rescale(., c(0, 100), percentiles = total_precip_pctl), name = "Percentiles")
#   )


## issues for when reactive.
## The values won't a perfect associated percentile. We'd need to calculate the percentile by adding that value to the data frame?

# #doesn't work
# ggplot(data = canESM2_rcp85_time_series_summary, aes(x = total_precip)) +
#   geom_histogram(bins = 25, fill = "steelblue", color = "black") +
#   labs(x = "Total Precipitation (mm)", y = "Count", title = "Histogram of Total Precipitation") +
#   theme_minimal() +
#   scale_x_continuous(sec.axis = sec_axis(~ ., breaks = canESM2_rcp85_time_series_summary$total_precip_pctl, labels = paste0(canESM2_rcp85_time_series_summary$total_precip_pctl, "%")))
# 
# 
# ggplot(data = canESM2_rcp85_time_series_summary, aes(x = total_precip)) +
#   geom_density(fill = "steelblue") +
#   theme_minimal() +
#   scale_x_continuous(
#     limits = c(0, 2.5),
#     breaks = seq(0, 2.5, by = 0.5),
#     sec.axis = sec_axis(
#       ~ rescale(., c(0, 1), c(0, 100), percentiles = quantile(canESM2_rcp85_time_series_summary$total_precip, probs = seq(0, 1, by = 0.1))),
#       name = "Percentiles",
#       breaks = quantile(canESM2_rcp85_time_series_summary$total_precip, probs = seq(0, 1, by = 0.1))
#     )
#   )

