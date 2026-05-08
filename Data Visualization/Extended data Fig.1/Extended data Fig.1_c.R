
library(readxl)   
library(ggplot2)  
library(dplyr)   
file_path <- "\\Extended data Fig.1_b and c.xlsx"


data_raw <- read_excel(file_path, sheet = 1)
valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")
data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))
plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)
p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 50)) +   
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        axis.text.y = element_text(size = 7),                       
        axis.title.x = element_text(size = 8),                      
        axis.title.y = element_text(size = 8))                    

print(p)
ggsave("Developer——time-all.pdf", plot = p, width = 2, height = 1.8, dpi = 300)






data_raw <- read_excel(file_path, sheet = 1)

data_raw <- data_raw %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category == "Diagnostic & Therapeutic Decision Support")

valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")

data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))

plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 50)) + 
  theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


print(p)

ggsave("Developer_time_Diagnostic_Therapeutic.pdf", plot = p, width = 2, height = 1.8, dpi = 300)









data_raw <- read_excel(file_path, sheet = 1)


data_raw <- data_raw %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category == "Medical Record Generation & Summarization")


valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")


data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))


plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)


p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 80)) +  
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


print(p)

ggsave("Developer_time_Medical Record Generation & Summarization.pdf", plot = p, width = 2, height = 1.8, dpi = 300)













data_raw <- read_excel(file_path, sheet = 1)


data_raw <- data_raw %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category == "Medical Quality Control and Management")


valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")


data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))


plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)


p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 60)) +  
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


print(p)


ggsave("Developer_time_Medical Quality Control and Management.pdf", plot = p, width = 2, height = 1.8, dpi = 300)








data_raw <- read_excel(file_path, sheet = 1)


data_raw <- data_raw %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category == "Treatment Efficacy Assessment & Prognosis")


valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")

data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))


plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)


p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 80)) +  
  theme_classic() +  
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


print(p)


ggsave("Developer_time_Treatment Efficacy Assessment & Prognosis.pdf", plot = p, width = 2, height = 1.8, dpi = 300)










data_raw <- read_excel(file_path, sheet = 1)


data_raw <- data_raw %>%
  mutate(Category = trimws(Category)) %>%
  filter(Category == "Doctor-Patient Communication & Education")


valid_time_levels <- c("<3", "4~7", "8~14", "14~30", ">30")


data_raw <- data_raw %>%
  mutate(Development_Time = trimws(Development_Time)) %>%
  filter(Development_Time %in% valid_time_levels) %>%
  mutate(Development_Time = factor(Development_Time, levels = valid_time_levels))


plot_data <- data_raw %>%
  group_by(Development_Time, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)


p <- ggplot(plot_data, aes(x = Development_Time, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "\n(N=", count, ")")), 
            vjust = -0.1, size = 1.8, lineheight = 1.1) +
  labs(x = "Person day", y = "Frequency (%)") +
  coord_cartesian(ylim = c(0, 80)) +  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


print(p)


ggsave("Developer_time_Doctor-Patient Communication & Education.pdf", plot = p, width = 2, height = 1.8, dpi = 300)
