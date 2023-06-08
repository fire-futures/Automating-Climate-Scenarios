#' Generate a list of season orders based on start date
#'
#' This function generates a list of season orders based on a given start date. The season order is determined by the start date, with the season being either "wet" or "dry".
#'
#' @param start_date The starting date for generating the season order.
#' @param n The number of season orders to generate.
#'
#' @return A list of season orders.
#'
#' @examples
#' start_date <- as.Date("2023-01-01")
#' season_list <- season_order(start_date, 5)
#' season_list
#'
#' @export
season_order <- function(start_date, n) {
  season <- ifelse(month(start_date) %in% c(10, 11, 12, 1, 2, 3), "wet", "dry")
  season_list <- vector("list", n)
  
  for (i in 1:n) {
    season_list[[i]] <- season
    season <- ifelse(season == "wet", "dry", "wet")
  }
  
  return(season_list)
}