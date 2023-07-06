library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(stringr)

character_to_yyyy <- function(x) {
  x <- ymd(paste0(x, "-01-01"))
  return(year(x))
}

get_today_for_subtitle <- function() {
  return(paste0(day(today()), suffix(day(today())), " ", month(today(), label = TRUE), " ", year(today())))
}

suffix <- function(x) {
  if (x %in% c(11, 12, 13)) {
    return("th")
  } else {
    last_digit <- as.character(x %% 10)
    to_return <- switch(last_digit,
                        "1" = "st",
                        "2" = "nd",
                        "3" = "rd",
                        "th")
    return(to_return)
  }
}