###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Author: Zara Akhtar
# Script: 06_machine_learning.R
# Purpose: Train machine learning models for heart disease prediction
###############################################################

rm(list = ls())

library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

heart <- read.csv("data/processed/heart_clean.csv", stringsAsFactors = FALSE)

if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("results")) dir.create("results")

# Remove leakage/source variables
if ("num" %in% names(heart)) heart$num <- NULL
if ("id" %in% names(heart)) heart$id <- NULL
if ("dataset" %in% names(heart)) heart$dataset <- NULL

# Outcome
heart$heart_disease <- factor(
  heart$heart_disease,
  levels = c("No Heart Disease", "Heart Disease")
)

# Predictors
factor_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")

for (var in factor_vars) {
  heart[[var]] <- factor(heart[[var]])
}

set.seed(123)

train_index <- createDataPartition(
  heart$heart_disease,
  p = 0.8,
  list = FALSE
)

train_data <- heart[train_index, ]
test_data <- heart[-train_index, ]

model_formula <- heart_disease ~ age + sex + cp + trestbps + chol +
  fbs + restecg + thalch + exang + oldpeak + slope + ca + thal

# Decision tree
tree_model <- rpart(
  model_formula,
  data = train_data,
  method = "class",
  control = rpart.control(
    cp = 0.001,
    minsplit = 25,
    minbucket = 10,
    maxdepth = 4
  )
)

printcp(tree_model)

# Moderately prune the tree
# This keeps the tree interpretable but not too simple
cp_table <- tree_model$cptable

selected_cp <- cp_table[
  min(4, nrow(cp_table)),
  "CP"
]

pruned_tree <- prune(
  tree_model,
  cp = selected_cp
)

png(
  filename = "figures/pruned_decision_tree.png",
  width = 2200,
  height = 1600,
  res = 300
)

rpart.plot(
  pruned_tree,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  cex = 0.72,
  main = "Pruned Decision Tree for Heart Disease Prediction"
)

dev.off()

# Random Forest
set.seed(123)

rf_model <- randomForest(
  model_formula,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

png(
  filename = "figures/random_forest_variable_importance.png",
  width = 1800,
  height = 1400,
  res = 300
)

varImpPlot(
  rf_model,
  main = "Random Forest Variable Importance"
)

dev.off()

sink("results/machine_learning_results.txt")

cat("Heart Disease Prediction and Risk Factor Analysis\n")
cat("Machine Learning Results\n")
cat("===============================================\n\n")

cat("Outcome distribution in full dataset:\n")
print(table(heart$heart_disease))

cat("\nOutcome distribution in training set:\n")
print(table(train_data$heart_disease))

cat("\nOutcome distribution in test set:\n")
print(table(test_data$heart_disease))

cat("\n\nDecision Tree Complexity Parameter Table\n\n")
printcp(tree_model)

cat("\n\nSelected CP for moderate pruning:\n")
print(selected_cp)

cat("\n\nPruned Decision Tree\n\n")
print(pruned_tree)

cat("\n\nRandom Forest\n\n")
print(rf_model)

cat("\n\nRandom Forest Variable Importance\n\n")
print(importance(rf_model))

sink()

cat("\n============================================\n")
cat("06_machine_learning.R completed successfully.\n")
cat("Moderately pruned tree saved to figures/pruned_decision_tree.png\n")
cat("============================================\n")

