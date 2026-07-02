###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Using Statistical and Machine Learning Methods in R
#
# Author: Zara Akhtar
# Script: 07_model_evaluation.R
# Purpose: Evaluate and compare predictive models
###############################################################

rm(list = ls())

library(caret)
library(pROC)
library(rpart)
library(randomForest)
library(openxlsx)

# Import cleaned dataset
heart <- read.csv("data/processed/heart_clean.csv", stringsAsFactors = FALSE)

# Create folders if needed
if (!dir.exists("results")) {
  dir.create("results")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}

# Remove leakage/source variables if present
if ("num" %in% names(heart)) {
  heart$num <- NULL
}

if ("id" %in% names(heart)) {
  heart$id <- NULL
}

if ("dataset" %in% names(heart)) {
  heart$dataset <- NULL
}

# Convert outcome correctly
heart$heart_disease <- factor(
  heart$heart_disease,
  levels = c("No Heart Disease", "Heart Disease")
)

# Convert predictors to factors
factor_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")

for (var in factor_vars) {
  heart[[var]] <- factor(heart[[var]])
}

# Quality checks
cat("Outcome distribution:\n")
print(table(heart$heart_disease))

cat("\nMissing values:\n")
print(colSums(is.na(heart)))

# Train-test split
set.seed(123)

train_index <- createDataPartition(
  heart$heart_disease,
  p = 0.80,
  list = FALSE
)

train_data <- heart[train_index, ]
test_data  <- heart[-train_index, ]

# Explicit model formula
model_formula <- heart_disease ~ age + sex + cp + trestbps + chol +
  fbs + restecg + thalch + exang + oldpeak + slope + ca + thal

# Train Logistic Regression
log_model <- glm(
  model_formula,
  family = binomial,
  data = train_data
)

# Train Decision Tree
tree_model <- rpart(
  model_formula,
  data = train_data,
  method = "class"
)

# Train Random Forest
set.seed(123)

rf_model <- randomForest(
  model_formula,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Evaluation function
evaluate_model <- function(model, model_name, logistic = FALSE) {
  
  if (logistic) {
    
    probs <- predict(model, test_data, type = "response")
    
    pred <- ifelse(
      probs > 0.5,
      "Heart Disease",
      "No Heart Disease"
    )
    
    pred <- factor(
      pred,
      levels = levels(test_data$heart_disease)
    )
    
  } else {
    
    probs <- predict(model, test_data, type = "prob")[, "Heart Disease"]
    
    pred <- predict(model, test_data, type = "class")
    
    pred <- factor(
      pred,
      levels = levels(test_data$heart_disease)
    )
  }
  
  cm <- confusionMatrix(
    pred,
    test_data$heart_disease,
    positive = "Heart Disease"
  )
  
  roc_curve <- roc(
    response = test_data$heart_disease,
    predictor = probs,
    levels = c("No Heart Disease", "Heart Disease"),
    direction = "<"
  )
  
  list(
    summary = data.frame(
      Model = model_name,
      Accuracy = round(as.numeric(cm$overall["Accuracy"]), 3),
      Precision = round(as.numeric(cm$byClass["Pos Pred Value"]), 3),
      Recall = round(as.numeric(cm$byClass["Sensitivity"]), 3),
      Specificity = round(as.numeric(cm$byClass["Specificity"]), 3),
      F1 = round(as.numeric(cm$byClass["F1"]), 3),
      AUC = round(as.numeric(auc(roc_curve)), 3)
    ),
    confusion = cm,
    roc = roc_curve
  )
}

# Evaluate all models
log_results  <- evaluate_model(log_model, "Logistic Regression", TRUE)
tree_results <- evaluate_model(tree_model, "Decision Tree")
rf_results   <- evaluate_model(rf_model, "Random Forest")

comparison_table <- rbind(
  log_results$summary,
  tree_results$summary,
  rf_results$summary
)

print(comparison_table)

# Save comparison table
write.csv(
  comparison_table,
  "results/model_comparison.csv",
  row.names = FALSE
)

write.xlsx(
  comparison_table,
  "results/model_comparison.xlsx",
  overwrite = TRUE
)

# Save detailed output
sink("results/model_evaluation_results.txt")

cat("MODEL COMPARISON\n\n")
print(comparison_table)

cat("\n\nLOGISTIC REGRESSION\n")
print(log_results$confusion)

cat("\n\nDECISION TREE\n")
print(tree_results$confusion)

cat("\n\nRANDOM FOREST\n")
print(rf_results$confusion)

sink()

# Save ROC comparison
png(
  "figures/roc_comparison.png",
  width = 1800,
  height = 1400,
  res = 300
)

plot(
  log_results$roc,
  col = "#4E79A7",
  lwd = 3,
  main = "ROC Curve Comparison"
)

lines(tree_results$roc, col = "#E15759", lwd = 3)
lines(rf_results$roc, col = "#59A14F", lwd = 3)

legend(
  "bottomright",
  legend = c("Logistic Regression", "Decision Tree", "Random Forest"),
  col = c("#4E79A7", "#E15759", "#59A14F"),
  lwd = 3
)

dev.off()

cat("\n============================================\n")
cat("07_model_evaluation.R completed successfully.\n")
cat("Results saved to results/model_comparison.csv and .xlsx\n")
cat("ROC curve saved to figures/roc_comparison.png\n")
cat("============================================\n")

comparison_table
