library(ggplot2)
library(dplyr)
library(scales)


file_path <- "/data.csv"
df <- read.csv(file_path, fileEncoding = "UTF-8")

scene_groups <- split(df, df$Scene)
result_list <- lapply(names(scene_groups), function(scene) {
  group_data <- scene_groups[[scene]]
  freq_table <- table(group_data$Usefulness, useNA = "ifany")
  prop_table <- prop.table(freq_table) * 100
  data.frame(
    Scene = scene,
    score = names(freq_table),
    N = as.integer(freq_table)
  )
})
final_result <- do.call(rbind, result_list)
print(final_result)

write.csv(final_result, 
          file = "/Usefulness.csv")



file_path <- "/Usefulness.csv"
df <- read.csv(file_path, fileEncoding = "UTF-8")

df_category <- df %>%
  mutate(category = ifelse(score < 4, "<4", "≥4"),
         category = factor(category, levels = c("≥4", "<4")))

summary_data <- df_category %>%
  group_by(Scene, category) %>%
  summarise(total_N = sum(N, na.rm = TRUE), .groups = "drop")

all_summary <- df_category %>%
  group_by(category) %>%
  summarise(total_N = sum(N, na.rm = TRUE), .groups = "drop") %>%
  mutate(Scene = "ALL")

summary_data <- bind_rows(summary_data, all_summary)

desired_order <- c("ALL", "Inpatient", "Outpatient", "Imaging", "Nursing")
summary_data <- summary_data %>%
  filter(Scene %in% desired_order) %>%
  mutate(Scene = factor(Scene, levels = desired_order))

summary_data <- summary_data %>%
  group_by(Scene) %>%
  mutate(percentage = total_N / sum(total_N) * 100) %>%
  ungroup()

summary_data <- summary_data %>%
  mutate(label_text = paste0(round(percentage, 1), "%(", total_N, ")"))

p <- ggplot(summary_data, aes(x = Scene, y = percentage, fill = category, label = label_text)) +
  geom_col(position = "stack") +
  geom_text(position = position_stack(vjust = 0.5), size = 3.5, color = "black") +
  coord_flip() +
  labs(
    x = "Scene",
    y = "Percentage (%)",
    fill = "Usefulness Score"
  ) +
  scale_fill_manual(values = c("<4" = alpha("#2E86AB", 0.6), "≥4" = alpha("#E67E22", 0.6))) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.1, "cm")
  )

print(p)
ggsave("Usefulness.pdf", p, width = 13, height = 2.5)