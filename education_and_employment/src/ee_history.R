library(ggplot2)
library(data.table)
library(ggrepel)


produce_ee_overview <- function() {
  my_data <- data.table(
    place = c(
      "Pre-education", "St James' First School", "St Michaels' Middle School",
      "Poole Grammar School", "University of Warwick",
      "Decision Scientist, Account Technologies", "University of Edinburgh"
    ),
    years = c(4, 5, 3, 6, 3, 2, 1),
    label = c("4 (4)", "5 (9)", "3 (12)", "6 (18)", "3 (21)", "2 (23)", "1 (24)")
  )
  my_data$place <- factor(my_data$place, levels = my_data$place)

  my_data[, percentage := years / sum(years) * 100]
  my_data[, cumu_years := cumsum(years)]

  p <- ggplot(my_data, aes(x = "", y = percentage, fill = place)) +
    geom_bar(stat = "identity", width = 1) +
    geom_label_repel(aes(label = label),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE,
               label.size = 0.5) +
    coord_polar("y", start = 0, direction = -1) +
    labs(title = "Years Spent in Education and Employment", fill = "Education/Employment") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5), # Center-align the plot title
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 10, l = 0),
      legend.direction = "horizontal",
      legend.spacing.y = unit(0.5, "cm"),
      legend.title.align = 0.5, # Align the legend title to the center
      legend.title = element_text(vjust = 0.5) # Vertically center the legend title
    ) +
    guides(fill = guide_legend(ncol = 2, nrow = 4, title.position = "top"))
  
  p
  return(p)
}