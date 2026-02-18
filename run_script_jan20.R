# ============================================
# Boston Housing – Exploratory Visualizations
# Outcome Variable: medv
# ============================================

######install packages needed#################
install.packages(c("tidyverse","corrplot"))


# Load required packages
library(tidyverse)
library(readxl)
library(corrplot)

# --------------------------------------------
# 1. Load the Dataset
# --------------------------------------------

boston <- read_excel("Boston (4).xlsx")

# Inspect data
glimpse(boston)

# --------------------------------------------
# 2. Histograms
# --------------------------------------------

# Histogram of MEDV (Outcome Variable)
ggplot(boston, aes(x = medv)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Median House Value (MEDV)",
    x = "Median House Value",
    y = "Frequency"
  ) +
  theme_minimal()

# Histograms for all numeric predictors
boston %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = -medv) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "gray70", color = "white") +
  facet_wrap(~ name, scales = "free") +
  labs(
    title = "Histograms of Numeric Variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

# --------------------------------------------
# 3. Boxplots
# --------------------------------------------

# Boxplot of MEDV
ggplot(boston, aes(y = medv)) +
  geom_boxplot(fill = "tomato") +
  labs(
    title = "Boxplot of Median House Value (MEDV)",
    y = "Median House Value"
  ) +
  theme_minimal()

# Boxplots of MEDV by selected predictors (example)
ggplot(boston, aes(x = factor(chas), y = medv)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "MEDV by Charles River Proximity",
    x = "Borders Charles River (0 = No, 1 = Yes)",
    y = "Median House Value"
  ) +
  theme_minimal()

# --------------------------------------------
# 4. Scatterplots (Predictors vs MEDV) 
# --------------------------------------------

# Ensure medv is numeric (Excel sometimes imports it as text)
boston <- boston %>%
  mutate(medv = as.numeric(medv))

# Keep only numeric predictors (excluding medv)
predictors <- boston %>%
  select(where(is.numeric)) %>%
  select(-medv) %>%
  names()

# Loop through predictors safely using .data[[var]]
for (var in predictors) {
  p <- ggplot(boston, aes(x = .data[[var]], y = medv)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("MEDV vs", var),
      x = var,
      y = "Median House Value (medv)"
    ) +
    theme_minimal()

  print(p)
}
###################################################################
boston %>%
  select(medv, where(is.numeric)) %>%
  pivot_longer(cols = -medv, names_to = "predictor", values_to = "value") %>%
  ggplot(aes(x = value, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(
    title = "Scatterplots of MEDV vs Numeric Predictors",
    x = "Predictor Value",
    y = "Median House Value (medv)"
  ) +
  theme_minimal()


# --------------------------------------------
# 5. Correlation Heatmap
# --------------------------------------------

# Compute correlation matrix
cor_matrix <- boston %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# Correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7,
  addCoef.col = "black"
)


# ============================================
# Train–Test Split for Regression Modeling
# Outcome Variable: medv
# ============================================

# --------------------------------------------
# 1. Ensure outcome variable is numeric
# --------------------------------------------
boston <- boston %>%
  mutate(medv = as.numeric(medv))

# --------------------------------------------
# 2. Set seed for reproducibility
# --------------------------------------------
set.seed(123)

# --------------------------------------------
# 3. Create Training Index (80% training)
# --------------------------------------------
train_index <- sample(
  seq_len(nrow(boston)),
  size = 0.80 * nrow(boston)
)

# --------------------------------------------
# 4. Split the Data
# --------------------------------------------
train_data <- boston[train_index, ]
test_data  <- boston[-train_index, ]

# --------------------------------------------
# 5. Verify the Split
# --------------------------------------------
cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations :", nrow(test_data), "\n")

# Optional: check outcome distribution
summary(train_data$medv)
summary(test_data$medv)


# ============================================
# Build the Initial (Baseline) Multiple Linear Regression Model
# Outcome: medv
# Uses: train_data (created from your train-test split step)
# ============================================

library(tidyverse)

# 1) Safety checks: make sure medv is numeric and no missing values in train_data
train_data <- train_data %>%
  mutate(medv = as.numeric(medv)) %>%
  drop_na()

# 2) Fit the baseline model: medv ~ all other variables
baseline_model <- lm(medv ~ ., data = train_data)

# 3) View model results
summary(baseline_model)

# 4) Put coefficients into a clean table (easy to interpret)
coef_table <- broom::tidy(baseline_model) %>%
  arrange(p.value)

print(coef_table)

# 5) Quick checks requested:
#    A) Signs (+/-): look at estimate column (positive/negative)
#    B) Magnitude: look at estimate size and interpret in units of medv
#    C) p-values: look at p.value column

# Flag statistically significant predictors at alpha = 0.05
sig_predictors <- coef_table %>%
  filter(term != "(Intercept)") %>%
  mutate(sign = if_else(estimate > 0, "+", "-"),
         significant_0_05 = p.value < 0.05) %>%
  select(term, sign, estimate, std.error, statistic, p.value, significant_0_05)

print(sig_predictors)

# 6) Optional: quickly count how many predictors are significant
cat("Number of significant predictors (p < 0.05): ",
    sum(sig_predictors$significant_0_05), "\n")

# 7) Optional: save the baseline results to a CSV for reporting
write_csv(coef_table, "baseline_model_coefficients.csv")


# ============================================
# Check Multicollinearity using VIF
# ============================================

# Load required package
library(car)

# 1) Compute VIF values
vif_values <- vif(baseline_model)

# 2) Convert to a clean table
vif_table <- tibble(
  predictor = names(vif_values),
  VIF = as.numeric(vif_values)
) %>%
  arrange(desc(VIF))

print(vif_table)

# ============================================
# Remove 'tax' and Recalculate VIF
# ============================================

library(tidyverse)
library(car)

# --------------------------------------------
# 1. Remove 'tax' from training data
# --------------------------------------------
train_data_no_tax <- train_data %>%
  select(-tax)

# --------------------------------------------
# 2. Refit the regression model
# --------------------------------------------
model_no_tax <- lm(medv ~ ., data = train_data_no_tax)

# --------------------------------------------
# 3. Recalculate VIF
# --------------------------------------------
vif_values_no_tax <- vif(model_no_tax)

vif_table_no_tax <- tibble(
  predictor = names(vif_values_no_tax),
  VIF = as.numeric(vif_values_no_tax)
) %>%
  arrange(desc(VIF))

print(vif_table_no_tax)

# ============================================
# Remove 'age' and 'tax' and Refit Regression
# Outcome: medv
# ============================================

library(tidyverse)

# --------------------------------------------
# 1. Remove 'age' and 'tax' from training data
# --------------------------------------------
train_data_reduced <- train_data %>%
  select(-age, -tax)

# --------------------------------------------
# 2. Refit the regression model
# --------------------------------------------
reduced_model <- lm(medv ~ ., data = train_data_reduced)

# --------------------------------------------
# 3. View model results
# --------------------------------------------
summary(reduced_model)

# --------------------------------------------
# 4. (Optional) Clean coefficient table
# --------------------------------------------
coef_table_reduced <- broom::tidy(redu

# ============================================
# Remove age, indus, tax and Refit Regression
# ============================================

library(tidyverse)

# --------------------------------------------
# 1. Remove selected predictors from training data
# --------------------------------------------
train_data_reduced <- train_data %>%
  select(-age, -indus, -tax)

# --------------------------------------------
# 2. Refit the regression model
# --------------------------------------------
reduced_model <- lm(medv ~ ., data = train_data_reduced)

# --------------------------------------------
# 3. View updated regression results
# --------------------------------------------
summary(reduced_model)

# --------------------------------------------
# 4. Optional: Clean coefficient table
# --------------------------------------------
coef_table_reduced <- broom::tidy(reduced_model) %>%
  arrange(p.value)

print(coef_table_reduced)

# ============================================
# RMSE Calculation for Reduced Regression Model
# ============================================

library(tidyverse)

# --------------------------------------------
# 1. Generate predictions on the TEST data
# --------------------------------------------
test_predictions <- predict(
  reduced_model,
  newdata = test_data
)

# --------------------------------------------
# 2. Calculate RMSE
# --------------------------------------------
rmse <- sqrt(mean((test_data$medv - test_predictions)^2))

rmse


# ============================================
# RMSE Calculation for Training Data
# Reduced Regression Model
# ============================================

library(tidyverse)

# --------------------------------------------
# 1. Generate predictions on the TRAINING data
# --------------------------------------------
train_predictions <- predict(
  reduced_model,
  newdata = train_data_reduced
)

# --------------------------------------------
# 2. Calculate RMSE
# --------------------------------------------
rmse_train <- sqrt(mean((train_data_reduced$medv - train_predictions)^2))

rmse_train


# --------------------------------------------
# End of Script
# --------------------------------------------
