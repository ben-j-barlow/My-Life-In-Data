library(tidyverse)
library(data.table)
library(packcircles)
library(ggplot2)

fill_missing_data <- function(audit) {
  # get (start, start + 1, ..., end) as sequence of dates
  start_date <- min(audit$date)
  end_date <- max(audit$date)
  all_dates <- as.Date(seq(start_date, end_date, by = "day"))
  
  # for each type (gym etc), input duration = 0 on all dates with no entry
  for (ty in unique(audit$type)) {
    audit_filtered_by_type <- audit %>% filter(type == ty)
    missing_dates <- all_dates[!all_dates %in% audit_filtered_by_type$date]
    to_append <- data.frame(
      date = missing_dates,
      type = rep(ty, length(missing_dates)),
      duration = rep(0, length(missing_dates))
    )
    audit <- rbind(audit, to_append)
  }
  
  # return full data: for each type and each date, there exists exactly 1 entry
  return(audit)
}

read_audit <- function() {
  path <- "/Users/benbarlow/dev/my-life-in-data/sport/data/audit.csv"
  audit <- read.table(path, header = TRUE)
  audit$date <- dmy(audit$date)
  
  return(audit)
}

clean_audit <- function(audit) {
  audit <- audit %>% filter(!type %in% c("upper", "pool"))
  audit$type <- replace(audit$type, audit$type == "individual", "pitch (individual)")
  audit$type <- replace(audit$type, audit$type == "team", "pitch (team)")
  audit$type <- replace(audit$type, audit$type == "pt", "gym (with PT)")
  return(audit)
}

calculate_years_skied <- function(history, weeks_skied = 25, days_per_week_skied = 5, hours_per_day_skied = 4, fball_matches_per_year = 25, avg_mins_per_match = 75) {
  lifetime_hours_ski <- weeks_skied * days_per_week_skied * hours_per_day_skied
  
  avg_hour_per_match <- avg_mins_per_match / 60
  years_played_football <- history %>%
    filter(sport == "Football") %>%
    pull(years_played)
  lifetime_hours_fball <- fball_matches_per_year * avg_hour_per_match * years_played_football
  
  years_skied <- as.integer(years_played_football * (lifetime_hours_ski / lifetime_hours_fball))
  return(years_skied)
}