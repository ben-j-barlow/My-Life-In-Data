library(here)
library(ggplot2)

produce_credits_by_dept <- function(to_file = FALSE, file_name = "cred_by_dept") {
  my_data <- fread(file.path(here(), "education_and_employment/data/modules.csv"))
  
  department_mapping <- c("ST" = "Statistics", "CS" = "Comp Sci", "MA" = "Maths", "Diss" = "Dissertation", "IB" = "Business", "PH" = "Philosophy", "INF" = "Informatics")
  my_data$department_mapped <- factor(department_mapping[my_data$department], levels = unlist(department_mapping))
  
  p <- ggplot(my_data, aes(x = department_mapped, y = credits, fill = degree)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Higher Education Credits Studied (by Department and Degree)",
      x = "Department",
      y = "Credits",
      fill = "Degree"
    ) +
    theme_minimal()
  
  if (to_file)
    ggsave(filename = file.path(here(), paste0("education_and_employment/plot/", file_name,".png")), plot = p, bg = "white")
  return(p)
}
