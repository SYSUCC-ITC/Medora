library(ggplot2)
library(scales)

data <- data.frame(
  Method = c("ALL", "Exclusion", "Enrollment"),
  Accurate_Count = c(1073, 684, 389),
  Total_Count = c(1086, 690, 396)
)

data$Method <- factor(data$Method, 
                      levels = c("ALL", "Exclusion", "Enrollment"))

data$Accuracy_Rate <- data$Accurate_Count / data$Total_Count
data$Percentage <- percent(data$Accuracy_Rate)
data$Label <- paste0(data$Accurate_Count, "/", data$Total_Count, "\n", data$Percentage)

P <- ggplot(data, aes(x = Method, y = Accuracy_Rate, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.6) +
  geom_text(aes(label = Label), vjust = -0.3, size = 3) +
  scale_y_continuous(
    limits = c(0, 1.05),
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  scale_fill_manual(values = c("#FFC89D", "#F1AEA7", "#B17F9F"))

P
ggsave("accuracy.pdf", P, width = 3, height = 4.5)