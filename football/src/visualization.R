library(ggplot2)
library(lubridate)
library(tidyverse)


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
  path <- "/Users/benbarlow/dev/my-life-in-data/football/data/audit.csv"
  audit <- read.table(path, header = TRUE)
  audit$date <- dmy(audit$date)

  return(fill_missing_data(audit))
}


visualize <- function(audit, y_unit = "hr") {
  switch(y_unit,
    "hr" = {
      mult <<- 1 / 60
    },
    "min" = {
      mult <<- 1
    },
    default = {
      mult <<- 1
    }
  )

  audit <- audit %>%
    # mutate(week = floor_date(date, unit = "week")) %>%
    arrange(date) %>%
    group_by(type) %>%
    mutate(cumulative_duration = cumsum(duration))

  return(ggplot(audit, aes(x = date, y = cumulative_duration * mult, colour = type)) +
    geom_line() +
    xlab("Date") +
    ylab("Total duration / hours"))
}
