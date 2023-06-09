season_order_shiny <- function(start_date, n) {
  season <- ifelse(month(start_date) %in% c(10, 11, 12, 1, 2, 3), "wet", "dry")
  season_list <- vector("list", n)
  for (i in 1:n) {
    season_list[[i]] <- season
    season <- ifelse(season == "wet", "dry", "wet")
  }
  return(season_list)
}