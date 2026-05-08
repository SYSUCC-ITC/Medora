library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

data <- read_csv("//cancer.csv")

cancer_sample_sizes <- data %>%
  group_by(cancer_type) %>%
  summarise(sample_size = first(number))

cancer_type_order <- c("Total Cancer", "NPC", "LC", 
                       "TC", "BC", "CRC", 
                       "EC", "GC", "RCC")

cancer_type_with_sample <- sapply(cancer_type_order, function(cancer_type) {
  sample_size <- cancer_sample_sizes$sample_size[cancer_sample_sizes$cancer_type == cancer_type]
  if (length(sample_size) > 0 && !is.na(sample_size)) {
    return(paste0(cancer_type, " (N=", sample_size, ")"))
  } else {
    return(cancer_type)
  }
})

custom_colors <- c(
  "T" = "#CAE0CA",
  "N" = "#839FBF", 
  "M" = "#F1AEA7"
)

metric_labels <- c(
  "T" = "T stage",
  "N" = "N stage", 
  "M" = "M stage"
)

plot_data <- data %>%
  select(cancer_type, `T`, `N`, `M`) %>%
  pivot_longer(cols = -cancer_type, 
               names_to = "Metric", 
               values_to = "Agreement_Rate") %>%
  mutate(
    cancer_type = factor(cancer_type, levels = cancer_type_order,
                         labels = cancer_type_with_sample),
    Metric = factor(Metric, levels = names(metric_labels))
  ) %>%
  filter(!is.na(cancer_type), !is.na(Metric))

max_agreement <- max(plot_data$Agreement_Rate, na.rm = TRUE)
y_upper_limit <- max_agreement * 1.2

alpha_values <- seq(1, 0.3, length.out = length(cancer_type_order))
names(alpha_values) <- cancer_type_with_sample

create_subplot <- function(metric_name) {
  sub_data <- plot_data %>% filter(Metric == metric_name)
  
  p <- ggplot(sub_data, aes(x = cancer_type, y = Agreement_Rate)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, 
             aes(alpha = cancer_type),
             fill = custom_colors[metric_name]) +
    geom_text(aes(label = sprintf("%.1f", Agreement_Rate * 100)),
              position = position_dodge(0.8), 
              vjust = 0.5,
              angle = 90,
              hjust = -0.2,
              size = 2.5,
              color = "black") +
    scale_alpha_manual(values = alpha_values, guide = "none") +
    labs(
      x = "Cancer Type",
      y = "Consistent (%)",
      title = metric_labels[metric_name]
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm")
    ) +
    scale_y_continuous(
      labels = function(x) sprintf("%.0f", x * 100),
      limits = c(0, y_upper_limit),
      expand = expansion(mult = c(0, 0.05))
    )
  
  return(p)
}

T_plot <- create_subplot("T")
N_plot <- create_subplot("N") 
M_plot <- create_subplot("M")

print(T_plot)
print(N_plot)
print(M_plot)

ggsave("T_stage_agreement.pdf", T_plot, width = 4, height = 5)
ggsave("N_stage_agreement.pdf", N_plot, width = 4, height = 5)
ggsave("M_stage_agreement.pdf", M_plot, width = 4, height = 5)