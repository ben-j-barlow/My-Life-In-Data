library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)

audit <- fread("data/auditc.csv")
directory <- fread("data/podcast_directoryc.csv")
podcast <- merge(audit, directory, by.x = "podcast_id", by.y = "id")

#podcast <- merged %>%
#  mutate(week = week(date)) 

podcast$date <- dmy(podcast$date)

podcast_counts <- podcast %>%
  group_by(week = floor_date(date, unit = "week"), category) %>%
  summarise(count = n())

# Custom shape definitions (replace with your own shapes)
#box_shapes <- c("circle", "square", "triangle")

# Plotting the stacked bar chart
ggplot(podcast_counts, aes(x = week, y = count, fill = category)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%d-%b-%Y", date_breaks = "1 month") +
  labs(title = "Number of Podcasts Listened per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()

ggplot(podcast_counts, aes(x = week, y = count, fill = category)) +
  geom_tile(width = 1, height = 0.95) +
  scale_x_date(date_labels = "%d-%b-%Y", date_breaks = "1 month") +
  labs(title = "Number of Podcasts Listened per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()

ggplot(podcast_counts, aes(x = week, y = count, fill = category)) +
  geom_tile(stat = "identity") +
  scale_x_date(date_labels = "%d-%b-%Y", date_breaks = "1 month") +
  labs(title = "Number of Podcasts Listened per Week",
       x = "Week",
       y = "Count") +
  theme_minimal()
  #theme(panel.spacing.y = unit(0.5, "cm")) + 
