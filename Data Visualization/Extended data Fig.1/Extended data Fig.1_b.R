
library(readxl)  
library(ggplot2)
library(dplyr)   
library(patchwork) 


file_path <- "\\Extended data Fig.1_b and c.xlsx"


data_raw <- read_excel(file_path, sheet = 1)

all_categories <- unique(data_raw$Category[!is.na(data_raw$Category)])
print(all_categories)

developer_colors <- c(
  "Healthcare Professionals Only" = "#c85e62",
  "Healthcare Professional-Led & Engineer-Supported" = "#f59c7c",
  "Engineer-Led & Healthcare Professional-Supported" = "#fded95"
)

plot_pie <- function(data, title){
  plot_data <- data %>%
    group_by(Developer) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = count / sum(count) * 100,
           label = paste0(count, "\n(", round(percentage, 1), "%)"))
  
  p <- ggplot(plot_data, aes(x = "", y = count, fill = Developer)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              size = 3,
              hjust = 0.5,     
              vjust = 0.5,     
              lineheight = 1.2) + 
    scale_fill_manual(values = developer_colors) +
    labs(title = title) +
    theme_void() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5, size = 10))
  return(p)
}


total_plot <- plot_pie(data_raw, title = "All Categories Combined")


category_plots <- list()
for(cat in all_categories){
  subset_data <- data_raw %>% filter(Category == cat)
  if(nrow(subset_data) == 0) next
  p <- plot_pie(subset_data, title = cat)
  category_plots <- append(category_plots, list(p))
}


all_plots <- c(list(total_plot), category_plots)
n_plots <- length(all_plots)


if(n_plots == 6){
  combined <- wrap_plots(all_plots, ncol = 3) 
} else {
  combined <- wrap_plots(all_plots) 
}


print(combined)
ggsave("combined_Developer.pdf", plot = combined, width = 8, height = 4, dpi = 300)