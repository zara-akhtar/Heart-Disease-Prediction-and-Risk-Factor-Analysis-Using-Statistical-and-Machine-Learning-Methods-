###############################################################
# Project: Heart Disease Prediction and Risk Factor Analysis
# Using Statistical and Machine Learning Methods in R
#
# Author: Zara Akhtar
# Script: 03_exploratory_data_analysis.R
# Purpose: Exploratory Data Analysis (EDA)
###############################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggcorrplot)

# Import cleaned dataset
heart <- read.csv("data/processed/heart_clean.csv", stringsAsFactors = FALSE)

# Create figures folder if it does not exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Convert variables to factors
heart$heart_disease <- factor(
  heart$heart_disease,
  levels = c("No Heart Disease", "Heart Disease")
)

heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$slope <- factor(heart$slope)
heart$thal <- factor(heart$thal)
heart$dataset <- factor(heart$dataset)

# Quality checks
cat("Outcome distribution:\n")
print(table(heart$heart_disease))

cat("\nMissing values:\n")
print(colSums(is.na(heart)))

summary(heart)

# Custom Plot Theme
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Colours
disease_cols <- c(
  "No Heart Disease" = "#4E79A7",
  "Heart Disease" = "#E15759"
)

# Distribution of Heart Disease
p1 <- ggplot(heart, aes(x = heart_disease, fill = heart_disease)) +
  geom_bar(color = "black", linewidth = 0.4) +
  scale_fill_manual(values = disease_cols) +
  labs(
    title = "Distribution of Heart Disease",
    x = "Heart Disease Status",
    y = "Number of Patients",
    fill = "Heart Disease Status"
  ) +
  my_theme

p1

ggsave(
  filename = "figures/heart_disease_distribution.png",
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)

# Chest Pain Type Distribution
p2 <- ggplot(heart, aes(x = cp, fill = cp)) +
  geom_bar(color = "black", linewidth = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Chest Pain Types",
    x = "Chest Pain Type",
    y = "Number of Patients",
    fill = "Chest Pain Type"
  ) +
  my_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

p2

ggsave(
  filename = "figures/chest_pain_distribution.png",
  plot = p2,
  width = 9,
  height = 6,
  dpi = 300
)

# Resting Blood Pressure Distribution
p3 <- ggplot(heart, aes(x = trestbps)) +
  geom_histogram(
    bins = 30,
    fill = "#4E79A7",
    color = "black"
  ) +
  labs(
    title = "Distribution of Resting Blood Pressure",
    x = "Resting Blood Pressure (mmHg)",
    y = "Number of Patients"
  ) +
  my_theme

p3

ggsave(
  filename = "figures/resting_bp_distribution.png",
  plot = p3,
  width = 8,
  height = 6,
  dpi = 300
)

# Serum Cholesterol Distribution
p4 <- ggplot(heart, aes(x = chol)) +
  geom_histogram(
    bins = 30,
    fill = "#F28E2B",
    color = "black"
  ) +
  labs(
    title = "Distribution of Serum Cholesterol",
    x = "Serum Cholesterol (mg/dL)",
    y = "Number of Patients"
  ) +
  my_theme

p4

ggsave(
  filename = "figures/cholesterol_distribution.png",
  plot = p4,
  width = 8,
  height = 6,
  dpi = 300
)

# Sex vs Heart Disease
p5 <- ggplot(heart, aes(x = sex, fill = heart_disease)) +
  geom_bar(color = "black", position = "dodge") +
  scale_fill_manual(values = disease_cols) +
  labs(
    title = "Heart Disease Status by Sex",
    x = "Sex",
    y = "Number of Patients",
    fill = "Heart Disease Status"
  ) +
  my_theme

p5

ggsave(
  filename = "figures/sex_vs_heart_disease.png",
  plot = p5,
  width = 8,
  height = 6,
  dpi = 300
)

# Chest Pain Type vs Heart Disease
p6 <- ggplot(heart, aes(x = cp, fill = heart_disease)) +
  geom_bar(color = "black", position = "dodge") +
  scale_fill_manual(values = disease_cols) +
  labs(
    title = "Heart Disease Status by Chest Pain Type",
    x = "Chest Pain Type",
    y = "Number of Patients",
    fill = "Heart Disease Status"
  ) +
  my_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p6

ggsave(
  filename = "figures/chest_pain_vs_heart_disease.png",
  plot = p6,
  width = 10,
  height = 6,
  dpi = 300
)

# Cholesterol by Heart Disease
p7 <- ggplot(
  heart,
  aes(x = heart_disease, y = chol, fill = heart_disease)
) +
  geom_boxplot() +
  scale_fill_manual(values = disease_cols) +
  labs(
    title = "Serum Cholesterol by Heart Disease Status",
    x = "Heart Disease Status",
    y = "Serum Cholesterol (mg/dL)",
    fill = "Heart Disease Status"
  ) +
  my_theme

p7

ggsave(
  filename = "figures/cholesterol_boxplot.png",
  plot = p7,
  width = 8,
  height = 6,
  dpi = 300
)

# Maximum Heart Rate by Heart Disease
p8 <- ggplot(
  heart,
  aes(x = heart_disease, y = thalch, fill = heart_disease)
) +
  geom_boxplot() +
  scale_fill_manual(values = disease_cols) +
  labs(
    title = "Maximum Heart Rate by Heart Disease Status",
    x = "Heart Disease Status",
    y = "Maximum Heart Rate",
    fill = "Heart Disease Status"
  ) +
  my_theme

p8

ggsave(
  filename = "figures/thalach_boxplot.png",
  plot = p8,
  width = 8,
  height = 6,
  dpi = 300
)

# Correlation Heatmap
numeric_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak", "ca")
numeric_data <- heart[, numeric_vars]

cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_plot <- ggcorrplot(
  cor_matrix,
  type = "upper",
  lab = TRUE,
  lab_size = 3,
  colors = c("#4E79A7", "white", "#E15759"),
  outline.color = "white",
  title = "Correlation Heatmap of Continuous Variables"
) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

cor_plot

ggsave(
  filename = "figures/correlation_heatmap.png",
  plot = cor_plot,
  width = 9,
  height = 7,
  dpi = 300
)

# Scatter Plot: Age vs Maximum Heart Rate
scatter1 <- ggplot(
  heart,
  aes(x = age, y = thalch, color = heart_disease)
) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = disease_cols) +
  labs(
    title = "Age vs Maximum Heart Rate by Heart Disease Status",
    x = "Age (Years)",
    y = "Maximum Heart Rate",
    color = "Heart Disease Status"
  ) +
  my_theme

scatter1

ggsave(
  filename = "figures/age_vs_thalach.png",
  plot = scatter1,
  width = 8,
  height = 6,
  dpi = 300
)

# Scatter Plot: Age vs Cholesterol
scatter2 <- ggplot(
  heart,
  aes(x = age, y = chol, color = heart_disease)
) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = disease_cols) +
  labs(
    title = "Age vs Serum Cholesterol by Heart Disease Status",
    x = "Age (Years)",
    y = "Serum Cholesterol (mg/dL)",
    color = "Heart Disease Status"
  ) +
  my_theme

scatter2

ggsave(
  filename = "figures/age_vs_cholesterol.png",
  plot = scatter2,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\n============================================\n")
cat("03_exploratory_data_analysis.R completed successfully.\n")
cat("============================================\n")

