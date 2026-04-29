library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

data <- read_excel("\\Fig5\\data.xlsx")

str(data)
head(data)

data <- data %>%
  mutate(
    Group = factor(Group, levels = c("Negative", "Positive")),
    flag = factor(flag, levels = c("-1", "0", "1"))
  )

count_data_original <- data %>%
  group_by(Group, flag) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    percent_label = paste0(count, "\n(", scales::percent(proportion, accuracy = 0.1), ")")
  ) %>%
  ungroup()

count_data_all <- data %>%
  group_by(flag) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    Group = "All",
    total = sum(count),
    proportion = count / total,
    percent_label = paste0(count, "\n(", scales::percent(proportion, accuracy = 0.1), ")")
  ) %>%
  select(Group, flag, count, total, proportion, percent_label)

count_data <- bind_rows(count_data_original, count_data_all) %>%
  mutate(Group = factor(Group, levels = c("Negative", "Positive", "All")))

count_data <- count_data %>%
  mutate(
    label_with_percent = paste0(count, " (", sprintf("%.1f%%", proportion * 100), ")")
  )

print(count_data)

p1 <- ggplot(count_data, aes(x = Group, y = count, fill = flag)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = label_with_percent), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 3, fontface = "bold") +
  scale_fill_manual(
    values = c("-1" = "#CAE0CA", "0" = "#F1AEA7", "1" = "#839FBF"),
    name = "Flag",
    labels = c("Overestimation", "Consistent", "Underestimation")
  ) +
  labs(
    x = NULL,
    y = "Count",
    fill = "Flag"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 9),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.background = element_rect(color = "black", size = 0.2),
    legend.box.margin = margin(2, 2, 2, 2),
    legend.title = element_text(face = "bold", size = 6),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.15, "cm")
  )

print(p1)
ggsave("stacked_bar_chart_with_all.pdf", 
       plot = p1,
       device = "pdf",
       width = 3.2,
       height = 3.5,
       units = "in",
       dpi = 300)