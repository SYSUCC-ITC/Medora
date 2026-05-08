library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

data <- read_excel("\\Fig5\\data-d.xlsx")

str(data)
head(data)

data_filtered <- data %>%
  filter(flag %in% c(-1, 1)) %>%
  mutate(
    flag = factor(flag, levels = c(-1, 1), labels = c("Overestimation", "Underestimation")),
    re_evaluated = factor(re_evaluated, 
                          levels = c("AI_correct", "Manual_correct", "Both_incorrect"))
  )

count_data_flag <- data_filtered %>%
  group_by(flag, re_evaluated) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(flag) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    percent_label = paste0(count, "\n(", scales::percent(proportion, accuracy = 0.1), ")")
  ) %>%
  ungroup()

count_data_all <- data_filtered %>%
  group_by(re_evaluated) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    flag = "All",
    total = sum(count),
    proportion = count / total,
    percent_label = paste0(count, "\n(", scales::percent(proportion, accuracy = 0.1), ")")
  ) %>%
  select(flag, re_evaluated, count, total, proportion, percent_label)

count_data <- bind_rows(count_data_flag, count_data_all) %>%
  mutate(flag = factor(flag, levels = c("Overestimation", "Underestimation", "All")))

print(count_data)

p <- ggplot(count_data, aes(x = flag, y = count, fill = re_evaluated)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = percent_label), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("AI_correct" = "#E15351", "Manual_correct" = "#4EA660", "Both_incorrect" = "#49c2d9"),
    name = "Expert Verification"
  ) +
  labs(
    x = NULL, 
    y = "Count",
    fill = "Expert Verification"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, face = "bold", color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.y = element_line(color = "black", size = 0.5)
  )

print(p)

ggsave("expert_verification_stacked_bar_with_all.pdf", 
       plot = p,
       device = "pdf",
       width = 4.8,
       height = 3.5,
       units = "in",
       dpi = 300)