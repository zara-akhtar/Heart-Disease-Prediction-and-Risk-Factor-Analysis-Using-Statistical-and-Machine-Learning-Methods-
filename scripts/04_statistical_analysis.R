###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Using Statistical and Machine Learning Methods in R
#
# Author: Zara Akhtar
# Script: 04_statistical_analysis.R
# Purpose: Statistical analysis of risk factors
###############################################################

rm(list = ls())

library(dplyr)

# Import cleaned dataset
heart <- read.csv("data/processed/heart_clean.csv", stringsAsFactors = FALSE)

# Create results folder if it does not exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Convert categorical variables to factors
heart$heart_disease <- factor(
  heart$heart_disease,
  levels = c("No Heart Disease", "Heart Disease")
)

heart$sex <- factor(heart$sex)
heart$dataset <- factor(heart$dataset)
heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$slope <- factor(heart$slope)
heart$thal <- factor(heart$thal)

# Quality checks
cat("Outcome distribution:\n")
print(table(heart$heart_disease))

cat("\nMissing values:\n")
print(colSums(is.na(heart)))

# Descriptive Statistics
summary(heart)

# Function for Continuous Variable Analysis
analyze_continuous <- function(variable) {
  
  cat("\n=====================================================\n")
  cat("Variable:", variable, "\n")
  cat("=====================================================\n")
  
  formula <- as.formula(paste(variable, "~ heart_disease"))
  
  no_hd <- heart[[variable]][heart$heart_disease == "No Heart Disease"]
  hd <- heart[[variable]][heart$heart_disease == "Heart Disease"]
  
  cat("\nGroup Summary\n")
  print(
    heart %>%
      group_by(heart_disease) %>%
      summarise(
        n = n(),
        mean = mean(.data[[variable]], na.rm = TRUE),
        median = median(.data[[variable]], na.rm = TRUE),
        sd = sd(.data[[variable]], na.rm = TRUE),
        IQR = IQR(.data[[variable]], na.rm = TRUE),
        .groups = "drop"
      )
  )
  
  cat("\nShapiro-Wilk Normality Test\n")
  print(shapiro.test(no_hd))
  print(shapiro.test(hd))
  
  cat("\nVariance Test\n")
  print(var.test(formula, data = heart))
  
  cat("\nWelch Two Sample t-test\n")
  print(t.test(formula, data = heart))
  
  cat("\nWilcoxon Rank Sum Test\n")
  print(wilcox.test(formula, data = heart))
}

# Function for Categorical Variable Analysis
analyze_categorical <- function(variable) {
  
  cat("\n=====================================================\n")
  cat("Variable:", variable, "\n")
  cat("=====================================================\n")
  
  tbl <- table(
    heart[[variable]],
    heart$heart_disease
  )
  
  cat("\nContingency Table\n")
  print(tbl)
  
  cat("\nExpected Counts\n")
  chi_result <- suppressWarnings(chisq.test(tbl))
  print(chi_result$expected)
  
  if (any(chi_result$expected < 5)) {
    cat("\nFisher's Exact Test used because at least one expected count is < 5\n")
    print(fisher.test(tbl))
  } else {
    cat("\nChi-square Test\n")
    print(chi_result)
  }
}

# Save statistical analysis output
sink("results/statistical_analysis_results.txt")

cat("Heart Disease Prediction and Risk Factor Analysis\n")
cat("Statistical Analysis Results\n")
cat("================================================\n\n")

cat("Dataset Summary\n")
print(summary(heart))

cat("\nOutcome distribution:\n")
print(table(heart$heart_disease))

cat("\nMissing values:\n")
print(colSums(is.na(heart)))

# Continuous variables
continuous_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

for (var in continuous_vars) {
  analyze_continuous(var)
}

# Categorical variables
categorical_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")

for (var in categorical_vars) {
  analyze_categorical(var)
}

sink()

cat("\n============================================\n")
cat("04_statistical_analysis.R completed successfully.\n")
cat("Results saved to results/statistical_analysis_results.txt\n")
cat("============================================\n")

