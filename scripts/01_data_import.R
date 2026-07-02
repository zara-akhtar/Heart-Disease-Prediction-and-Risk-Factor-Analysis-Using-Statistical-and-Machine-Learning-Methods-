###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Using Statistical and Machine Learning Methods in R
#
# Author: Zara Akhtar
#
# Script: 01_data_import.R
#
# Purpose:
# Import the raw heart disease dataset and perform an
# initial exploration of its structure and contents.
###############################################################

#--------------------------------------------------------------
# Clear Workspace
#--------------------------------------------------------------
rm(list = ls())

#--------------------------------------------------------------
# Load Required Packages
#--------------------------------------------------------------
library(dplyr)

#--------------------------------------------------------------
# Import Raw Dataset
#--------------------------------------------------------------
heart <- read.csv("data/raw/heart_disease_uci.csv")

#--------------------------------------------------------------
# Initial Data Exploration
#--------------------------------------------------------------

# Display the first six observations
head(heart)

# Display the last six observations
tail(heart)

# Examine the structure of the dataset
str(heart)

# Display the number of rows and columns
dim(heart)

# Display variable names
names(heart)

# Generate summary statistics for all variables
summary(heart)

#--------------------------------------------------------------
# End of Script
#--------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("01_data_import.R completed successfully.\n")
cat("============================================\n")

