###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Author: Zara Samee Akhtar
# Script: 02_data_cleaning.R
# Purpose: Clean raw data, handle missing values and impossible
#          values, create binary outcome variable, remove leakage
#          variables, and save processed dataset.
###############################################################

rm(list = ls())
library(dplyr)

# Import raw dataset
heart <- read.csv("data/raw/heart_disease_uci.csv", stringsAsFactors = FALSE)

cat("Missing values before cleaning:\n")
print(colSums(is.na(heart)))

# Remove ID variable
if ("id" %in% names(heart)) {
  heart$id <- NULL
}

# Create binary outcome from UCI 'num'
# num = 0: No heart disease
# num > 0: Heart disease present
heart$heart_disease <- ifelse(heart$num > 0, 1, 0)

# Remove original target variable to prevent data leakage
heart$num <- NULL

# Treat impossible clinical values as missing
# 0 mmHg blood pressure and 0 mg/dL cholesterol are not physiologically valid
heart$trestbps[heart$trestbps == 0] <- NA
heart$chol[heart$chol == 0] <- NA

# Impute numeric variables using median
numeric_vars <- c("trestbps", "chol", "thalch", "oldpeak", "ca")

for (var in numeric_vars) {
  heart[[var]][is.na(heart[[var]])] <- median(heart[[var]], na.rm = TRUE)
}

# Function to calculate mode
get_mode <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
}

# Impute categorical variables using mode
categorical_vars <- c("sex", "dataset", "cp", "fbs", "restecg", "exang", "slope", "thal")

for (var in categorical_vars) {
  heart[[var]][is.na(heart[[var]])] <- get_mode(heart[[var]])
  heart[[var]] <- factor(heart[[var]])
}

# Convert outcome to labelled factor ONCE
heart$heart_disease <- factor(
  heart$heart_disease,
  levels = c(0, 1),
  labels = c("No Heart Disease", "Heart Disease")
)

# Final quality checks
cat("\nMissing values after cleaning:\n")
print(colSums(is.na(heart)))

cat("\nImpossible zero values after cleaning:\n")
print(colSums(heart[, c("trestbps", "chol")] == 0))

cat("\nOutcome distribution:\n")
print(table(heart$heart_disease))

cat("\nCleaned dataset structure:\n")
str(heart)

# Save cleaned dataset
write.csv(heart, "data/processed/heart_clean.csv", row.names = FALSE)

cat("\n============================================\n")
cat("02_data_cleaning.R completed successfully.\n")
cat("============================================\n")

