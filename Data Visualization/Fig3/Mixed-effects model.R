library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(performance)

df <- read.csv("data.csv", stringsAsFactors = TRUE)

str(df)
head(df)

dependent_vars <- c(
  "Readability",
  "Completeness",
  "Curation",
  "Usefulness",
  "Correctness_Hallucination",
  "Correctness_Knowledge.Gap",
  "Correctness_Faulty.Logic",
  "Correctness_Bias",
  "Correctness_Overall"
)

p_values <- numeric(length(dependent_vars))
names(p_values) <- dependent_vars

for (i in seq_along(dependent_vars)) {
  dv <- dependent_vars[i]
  cat("\n==============================\n")
  cat("Fitting model for dependent variable:", dv, "\n")
  
  formula_str <- paste0("`", dv, "` ~ Scene + (1 | Patient_id) + (1 | doctor_name)")
  model <- lmer(as.formula(formula_str), data = df)
  
  cat("\nModel summary:\n")
  print(summary(model))
  
  anova_res <- anova(model)
  cat("\nANOVA results (F‑test):\n")
  print(anova_res)
  
  p_val <- anova_res["Scene", "Pr(>F)"]
  p_values[i] <- p_val
  cat(sprintf("\nRaw p‑value for Scene fixed effect = %.6f\n", p_val))
}

adjusted_p <- p.adjust(p_values, method = "bonferroni")

result_table <- data.frame(
  Dependent_Variable = dependent_vars,
  Raw_P_Value = p_values,
  Adjusted_P_Bonferroni = adjusted_p
)

cat("\n\n========== Multiple comparison correction results ==========\n")
print(result_table)