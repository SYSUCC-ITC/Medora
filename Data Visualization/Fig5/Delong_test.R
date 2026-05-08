library(pROC)
library(readxl)
library(dplyr)

file_path <- "\\Fig5\\data-e.xlsx"
combined_data <- read_excel(file_path)

if (!"Group" %in% colnames(combined_data)) {
  stop("Column 'Group' not found in data")
}

combined_data <- combined_data %>%
  mutate(Group_numeric = ifelse(Group == "Positive", 1, 0))

roc_models <- list()

if ("total_score_manual" %in% colnames(combined_data)) {
  roc_models$total_score_manual <- roc(Group_numeric ~ total_score_manual, 
                                       data = combined_data, 
                                       direction = "<", 
                                       levels = c(0, 1),
                                       na.action = na.omit)
} else {
  warning("Column 'total_score_manual' not found")
}

if ("total_score_AI" %in% colnames(combined_data)) {
  roc_models$total_score_AI <- roc(Group_numeric ~ total_score_AI, 
                                   data = combined_data, 
                                   direction = "<", 
                                   levels = c(0, 1),
                                   na.action = na.omit)
} else {
  warning("Column 'total_score_AI' not found")
}

if ("modify_score" %in% colnames(combined_data)) {
  roc_models$modify_score <- roc(Group_numeric ~ modify_score, 
                                 data = combined_data, 
                                 direction = "<", 
                                 levels = c(0, 1),
                                 na.action = na.omit)
} else {
  warning("Column 'modify_score' not found")
}

model_names <- c("total_score_manual", "total_score_AI", "modify_score")
existing_models <- model_names[model_names %in% names(roc_models)]

if (length(existing_models) >= 2) {
  cat("\n========== DeLong Test Results ==========\n")
  for (i in 1:(length(existing_models)-1)) {
    for (j in (i+1):length(existing_models)) {
      roc1 <- roc_models[[existing_models[i]]]
      roc2 <- roc_models[[existing_models[j]]]
      
      delong_test <- roc.test(roc1, roc2, method = "delong")
      
      cat(sprintf("\nComparison: %s vs %s\n", existing_models[i], existing_models[j]))
      cat(sprintf("AUC1 = %.4f, AUC2 = %.4f\n", auc(roc1), auc(roc2)))
      cat(sprintf("AUC difference = %.4f (95%% CI: %.4f-%.4f)\n", 
                  delong_test$estimate[2] - delong_test$estimate[1],
                  delong_test$conf.int[1], delong_test$conf.int[2]))
      cat(sprintf("P value = %.6f\n", delong_test$p.value))
      cat("------------------------\n")
    }
  }
} else {
  cat("At least two models are needed for DeLong test.\n")
}