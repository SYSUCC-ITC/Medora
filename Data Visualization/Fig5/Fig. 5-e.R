library(pROC)
library(readxl)
library(ggplot2)
library(dplyr)

file_path <- "\\Fig5\\data-e.xlsx"
combined_data <- read_excel(file_path)

print("Column names:")
print(colnames(combined_data))
print("Data structure:")
print(str(combined_data))

if (!"Group" %in% colnames(combined_data)) {
  stop("Column 'Group' not found in data")
}

combined_data <- combined_data %>%
  mutate(Group_numeric = ifelse(Group == "Positive", 1, 0))

roc_models <- list()

model_order <- c("total_score_manual", "total_score_AI", "modify_score")

if ("modify_score" %in% colnames(combined_data)) {
  roc_models$modify_score <- roc(Group_numeric ~ modify_score, 
                                 data = combined_data, 
                                 direction = "<", 
                                 levels = c(0, 1),
                                 na.action = na.omit)
} else {
  warning("Column 'modify_score' not found in data")
}

if ("total_score_AI" %in% colnames(combined_data)) {
  roc_models$total_score_AI <- roc(Group_numeric ~ total_score_AI, 
                                   data = combined_data, 
                                   direction = "<", 
                                   levels = c(0, 1),
                                   na.action = na.omit)
} else {
  warning("Column 'total_score_AI' not found in data")
}

if ("total_score_manual" %in% colnames(combined_data)) {
  roc_models$total_score_manual <- roc(Group_numeric ~ total_score_manual, 
                                       data = combined_data, 
                                       direction = "<", 
                                       levels = c(0, 1),
                                       na.action = na.omit)
} else {
  warning("Column 'total_score_manual' not found in data")
}

auc_values <- list()
auc_ci_values <- list()

for (model_name in model_order) {
  if (!is.null(roc_models[[model_name]])) {
    auc_values[[model_name]] <- auc(roc_models[[model_name]])
    auc_ci_values[[model_name]] <- ci.auc(roc_models[[model_name]])
  }
}

print("AUC values (in specified order):")
for (model_name in model_order) {
  if (!is.null(auc_values[[model_name]])) {
    cat(sprintf("%s: AUC = %.3f\n", model_name, auc_values[[model_name]]))
  }
}

roc_data_list <- list()
colors <- c("#CAE0CA", "#F1AEA7", "#839FBF")
names(colors) <- model_order

for (model_name in model_order) {
  if (!is.null(roc_models[[model_name]])) {
    roc_obj <- roc_models[[model_name]]
    auc_val <- auc_values[[model_name]]
    auc_ci <- auc_ci_values[[model_name]]
    
    roc_data_list[[model_name]] <- data.frame(
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities,
      Model = paste0(model_name, 
                     " (AUC = ", sprintf("%.3f", auc_val), 
                     ", 95% CI: ", 
                     sprintf("%.3f-%.3f", auc_ci[1], auc_ci[3]), 
                     ")")
    )
  }
}

pdf(
  file = "ROC_curves.pdf",
  width = 6,
  height = 6
)

par(
  cex.axis = 1.0,
  cex.lab = 1.0,
  tck = -0.02,
  mar = c(4, 4, 3, 2) + 0.1
)

first_plotted <- FALSE
for (model_name in model_order) {
  if (!is.null(roc_models[[model_name]])) {
    if (!first_plotted) {
      plot(roc_models[[model_name]], 
           col = colors[model_name], 
           lwd = 2, 
           legacy.axes = TRUE,
           xlab = "1 - Specificity (False Positive Rate)",
           ylab = "Sensitivity (True Positive Rate)",
           main = "ROC Curves Comparison",
           cex.lab = 1.0)
      first_plotted <- TRUE
    } else {
      plot(roc_models[[model_name]], 
           col = colors[model_name], 
           lwd = 2, 
           add = TRUE,
           legacy.axes = TRUE)
    }
  }
}

if (!first_plotted) {
  plot(0, 0, type = "n", xlab = "", ylab = "", main = "No valid ROC data")
  text(0, 0, "No valid ROC data", cex = 1.2)
} else {
  legend_labels <- c()
  legend_colors <- c()
  
  for (model_name in model_order) {
    if (!is.null(roc_models[[model_name]]) && !is.null(auc_values[[model_name]])) {
      if (model_name == "modify_score") {
        display_name <- "Modified Padua score"
      } else if (model_name == "total_score_AI") {
        display_name <- "AI Padua score"
      } else if (model_name == "total_score_manual") {
        display_name <- "Manual Padua score"
      } else {
        display_name <- model_name
      }
      
      legend_labels <- c(legend_labels, 
                         paste0(display_name, " (AUC = ", 
                                sprintf("%.3f", auc_values[[model_name]]), ")"))
      legend_colors <- c(legend_colors, colors[model_name])
    }
  }
  
  legend("bottomright", 
         legend = legend_labels,
         col = legend_colors,
         lwd = 2,
         bty = "n",
         cex = 0.9)
}

dev.off()

print("ROC curve saved as 'ROC_curves.pdf'")