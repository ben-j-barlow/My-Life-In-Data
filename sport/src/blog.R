visualize_activity <- function(audit, color_dict = get_colors()) {
  # not yet used
  
  # Create a new data frame with an 'overall' category
  duration_by_week_type <- audit %>%
    mutate(week = lubridate::floor_date(date, "week")) %>%
    group_by(week, type) %>%
    summarise(total_duration = sum(duration))
  
  ggplot(duration_by_week_type, aes(x = week, y = total_duration, fill = type)) +
    geom_col() +
    scale_fill_manual(values = color_dict) +
    facet_wrap(~ type, nrow = 2) +
    xlab("Week") +
    ylab("Total Duration")
  #scale_x_date(date_breaks = "2 week", date_labels = "%b %d", limits = c(as.Date("2023-01-23"), max(audit$date)))
  
  # Calculate the total duration by week and type
  ggplot(duration_by_week_type, aes(x = week, y = total_duration, fill = type)) +
    geom_col() +
    scale_fill_manual(values = color_dict) +
    xlab("Week") +
    ylab("Total Duration") +
    scale_x_date(date_breaks = "2 week", date_labels = "%b %d", limits = c(as.Date("2023-01-23"), max(audit$date))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
