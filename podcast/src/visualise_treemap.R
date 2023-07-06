library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplotify)
library(treemapify)


year_to_display <- 2023

audit <- fread("data/auditc.csv")
directory <- fread("data/podcast_directoryc.csv")
podcast <- merge(audit, directory, by.x = "podcast_id", by.y = "id")

#podcast <- merged %>%
#  mutate(week = week(date)) 

podcast$date <- dmy(podcast$date)

podcast <- podcast %>%
  mutate(month = format(date, "%b"), year = format(date, "%Y")) %>%
  filter(year == year_to_display)

counts <- podcast %>%
  group_by(month, year, category) %>%
  summarise(count = n())

plot <- ggplot(counts, aes(area = count, fill = category, label = category)) +
  geom_treemap() +
  facet_wrap(~ month + year, ncol = 4) +
  theme(legend.position = "right")

plot



library(ggplot2)

library(ggplot2)
library(ggplot2)

# Create a sample dataset
data <- data.frame(
  Month = c("01-01-2023", "01-02-2023", "01-03-2023", "01-04-2023", "01-05-2023", "01-06-2023", "01-07-2023", "01-08-2023", "01-09-2023", "01-10-2023", "01-11-2023", "01-12-2023"),
  Event = c("Event A", "Event B", "Event C", "Event A", "Event B", "Event C", "Event A", "Event B", "Event C", "Event A", "Event B", "Event C"),
  Count = c(5, 8, 6, 4, 7, 9, 3, 6, 5, 2, 4, 6),
  Type = c("Type 1", "Type 2", "Type 1", "Type 2", "Type 1", "Type 2", "Type 1", "Type 2", "Type 1", "Type 2", "Type 1", "Type 2")
)

# Convert Month to factor
data$Month <- as.Date(data$Month)
data$Month <- factor(data$Month, levels = unique(data$Month))

# Plot the graph
ggplot(data, aes(x = Month, y = Count, fill = Type)) +
  geom_rect(aes(xmin = as.numeric(format(Month, "%m")), xmax = as.numeric(format(Month, "%m")) + 0.8, ymin = 0, ymax = Count), color = "black") +
  labs(x = "Month", y = "Count", title = "Count of Events by Month") +
  scale_fill_manual(values = c("Type 1" = "blue", "Type 2" = "red")) +
  theme_minimal()
