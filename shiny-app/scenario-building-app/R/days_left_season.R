# Season check function
season_check <- function(date) {
  date <- ymd(date)
  month_day <- day(date)
  year_day <- year(date)
  season <- ifelse(month(date) %in% c(11, 12, 1, 2, 3, 4), "wet", "dry")
  
  if (season == "wet") {
    # If the month is already November or later, the start of the next dry season is next year
    if (month(date) >= 11) {
      start_next_season <- ymd(paste(year_day + 1, "/05/01", sep=""))
    } else {
      start_next_season <- ymd(paste(year_day, "/05/01", sep=""))
    }
  } else { # dry season
    # If the month is already May or later, the start of the next wet season is later this year
    if (month(date) >= 5) {
      start_next_season <- ymd(paste(year_day, "/11/01", sep=""))
    } else { # If the month is before May, the start of the next wet season is this year
      start_next_season <- ymd(paste(year_day, "/11/01", sep=""))
    }
  }
  
  days_to_next_season <- as.integer(difftime(start_next_season, date, units = "days"))
  
  return(days_to_next_season = days_to_next_season)
}

season_check("2002/11/01")