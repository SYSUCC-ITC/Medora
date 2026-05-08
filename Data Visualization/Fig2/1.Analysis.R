library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

data_path <- "\\Fig2\\data.xlsx"
data <- read_excel(data_path)

cat("\n=== Overall ===\n")
total_counts <- nrow(data)
consistency_stats <- data.frame(
  Indicator = c("T", "N", "M"),
  Consistent_Count = c(
    sum(data$"T_consistent" == 1, na.rm = TRUE),
    sum(data$"N_consistent" == 1, na.rm = TRUE),
    sum(data$"M_consistent" == 1, na.rm = TRUE)
  ),
  Total_Sample_Size = rep(total_counts, 3),
  Consistency_Rate = c(
    round(sum(data$"T_consistent" == 1, na.rm = TRUE) / total_counts, 4),
    round(sum(data$"N_consistent" == 1, na.rm = TRUE) / total_counts, 4),
    round(sum(data$"M_consistent" == 1, na.rm = TRUE) / total_counts, 4)
  )
)

print(consistency_stats)

cat("\n=== By Cancer Type ===\n")

data$cancer_type <- as.character(data$cancer_type)

result <- data %>%
  group_by(cancer_type) %>%
  summarise(
    Sample_Size = n(),
    T_Consistent_Count = sum(`T_consistent` == 1, na.rm = TRUE),
    N_Consistent_Count = sum(`N_consistent` == 1, na.rm = TRUE),
    M_Consistent_Count = sum(`M_consistent` == 1, na.rm = TRUE),
    T_Consistency_Rate = round(T_Consistent_Count / Sample_Size, 4),
    N_Consistency_Rate = round(N_Consistent_Count / Sample_Size, 4),
    M_Consistency_Rate = round(M_Consistent_Count / Sample_Size, 4)
  ) %>%
  select(cancer_type, Sample_Size, 
         T_Consistent_Count, T_Consistency_Rate,
         N_Consistent_Count, N_Consistency_Rate,
         M_Consistent_Count, M_Consistency_Rate)


result <- result %>%
  rename(
    "Cancer_Type" = cancer_type,
    "T_Staging_Consistent_Count" = T_Consistent_Count,
    "T_Staging_Consistency_Rate" = T_Consistency_Rate,
    "N_Staging_Consistent_Count" = N_Consistent_Count,
    "N_Staging_Consistency_Rate" = N_Consistency_Rate,
    "M_Staging_Consistent_Count" = M_Consistent_Count,
    "M_Staging_Consistency_Rate" = M_Consistency_Rate
  )


print(result, n = nrow(result))


result_sorted <- result %>%
  arrange(desc(Sample_Size))

cat("\n=== Sorted by Sample Size ===\n")
print(result_sorted, n = nrow(result_sorted))