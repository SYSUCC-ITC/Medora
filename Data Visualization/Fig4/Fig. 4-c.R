library(ggplot2)
library(readxl)
library(knitr)
library(reshape2)

file_path <- "\\Fig4\\data.xlsx"
data <- read_excel(file_path, sheet = "data")
long_data <- melt(data, 
                  id.vars = "ID",
                  measure.vars = c("manual_time", "ai_time"),
                  variable.name = "group",
                  value.name = "time")

long_data$group <- factor(long_data$group, 
                          levels = c("manual_time", "ai_time"),
                          labels = c("Manual", "AI-assisted"))

P <- ggplot(data = long_data, aes(x = group, y = time)) +
  geom_boxplot(aes(fill = group), width = 0.4, alpha = 0.7, 
               outlier.shape = NA) +
  geom_line(aes(group = ID), 
            color = "gray40", 
            linetype = "dashed",
            alpha = 0.2, 
            size = 0.5) +
  geom_point(aes(color = group, group = ID), 
             size = 2, 
             alpha = 0.7) +
  scale_fill_manual(values = c("Manual" = "#2189AC", "AI-assisted" = "#B2182B")) +
  scale_color_manual(values = c("Manual" = "#2189AC", "AI-assisted" = "#B2182B")) +
  labs(
    y = "Audit Time (min)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5)
  )
P
ggsave("time_difference.pdf", P, width = 2.6, height = 4.5)