###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Using Statistical and Machine Learning Methods in R
#
# Author: Zara Samee Akhtar
# Script: 05_logistic_regression.R
# Purpose: Perform univariable and multivariable logistic regression,
#          estimate odds ratios with 95% confidence intervals, evaluate
#          model diagnostics, and save results.
###############################################################

rm(list = ls())

library(dplyr)
library(car)
library(pscl)
library(ResourceSelection)

# Import cleaned dataset
heart <- read.csv("data/processed/heart_clean.csv", stringsAsFactors = FALSE)

# Create results folder if needed
if (!dir.exists("results")) {
  dir.create("results")
}

# Safety check: remove leakage/source variables if present
if ("num" %in% names(heart)) {
  heart$num <- NULL
}

if ("id" %in% names(heart)) {
  heart$id <- NULL
}

# Optional: remove dataset from regression because it is data source, not a clinical predictor
if ("dataset" %in% names(heart)) {
  heart$dataset <- NULL
}

# Convert outcome to factor with correct reference level
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

# Save output
sink("results/logistic_regression_results.txt", split = TRUE)

cat("Heart Disease Prediction and Risk Factor Analysis\n")
cat("Logistic Regression Results\n")
cat("================================================\n\n")

cat("Outcome distribution:\n")
print(table(heart$heart_disease))

cat("\nMissing values:\n")
print(colSums(is.na(heart)))

# Function for univariable logistic regression
run_univariable <- function(variable) {
  
  cat("\n=====================================================\n")
  cat("Variable:", variable, "\n")
  cat("=====================================================\n")
  
  formula <- as.formula(paste("heart_disease ~", variable))
  
  model <- glm(
    formula,
    family = binomial,
    data = heart
  )
  
  cat("\nRegression Summary\n")
  print(summary(model))
  
  cat("\nOdds Ratios\n")
  print(exp(coef(model)))
  
  cat("\n95% Confidence Intervals for Odds Ratios\n")
  print(exp(confint(model)))
}

# Univariable Logistic Regression Analysis
univariable_vars <- c(
  "age",
  "sex",
  "trestbps",
  "chol",
  "thalch",
  "exang",
  "oldpeak",
  "cp"
)

for (var in univariable_vars) {
  run_univariable(var)
}

# Multiple Logistic Regression Model
model_final <- glm(
  heart_disease ~ age + sex + trestbps + chol + thalch + exang + oldpeak + cp,
  family = binomial,
  data = heart
)

cat("\n=====================================================\n")
cat("Multiple Logistic Regression Model\n")
cat("=====================================================\n")

print(summary(model_final))

cat("\nOdds Ratios\n")
print(exp(coef(model_final)))

cat("\n95% Confidence Intervals for Odds Ratios\n")
print(exp(confint(model_final)))

# Model Diagnostics
cat("\nPseudo R-squared\n")
print(pR2(model_final))

cat("\nAkaike Information Criterion (AIC)\n")
print(AIC(model_final))

cat("\nVariance Inflation Factors (VIF)\n")
print(vif(model_final))

cat("\nHosmer-Lemeshow Goodness-of-Fit Test\n")
print(
  hoslem.test(
    as.numeric(heart$heart_disease) - 1,
    fitted(model_final),
    g = 10
  )
)

sink()

cat("\n============================================\n")
cat("05_logistic_regression.R completed successfully.\n")
cat("Results saved to results/logistic_regression_results.txt\n")
cat("============================================\n")

