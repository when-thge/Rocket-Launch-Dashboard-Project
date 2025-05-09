library(tidyverse)
library(tidymodels)
library(baguette)
library(nnet)
library(xgboost)
library(yardstick)  # Ensure this is loaded for all metric functions

# Data loading and preparation (your existing code)
setwd("C:/Users/Theo Benedict/Desktop/Personal Docus/2nd Year - 2nd Sem/CS 226 - Statistics with R/Theo_Jems LE Stuff/Launch_Data")
config_data <- read_csv("Configs.csv")
launch_data <- read_csv("Launches.csv") |>
  rename(
    Config = "Rocket Name"
  )
mission_data <- read_csv("Missions.csv")
raw_data <- inner_join(config_data, launch_data, by = "Config")
raw_data <- inner_join(raw_data, mission_data, by = "Launch Id")
data <- raw_data |>
  select(
    `Launch Status`,
    `Fairing Height`,
    `Fairing Diameter`,
    `Rocket Height`,
    `Payload to LEO`,
    `Payload to GTO`,
    Stages,
    `Strap-ons`,
    `Liftoff Thrust`,
    Payloads,
    Mass
  ) |>
  mutate(
    `Launch Status` = as.factor(`Launch Status`),
    across(
      c(
        `Fairing Height`,
        `Fairing Diameter`,
        `Rocket Height`,
        `Payload to LEO`,
        `Payload to GTO`,
        `Liftoff Thrust`
      ),
      ~ as.numeric(str_remove_all(str_remove_all(., ","), "[^0-9.]"))
    )
  )

# Splitting Data
set.seed(030805)
datasplit <- initial_split(
  data,
  prop = 0.80,
  strata = `Launch Status`
)
train_data <- training(datasplit)
test_data <- testing(datasplit)

# Creating Recipe
ml_recipe <- recipe(`Launch Status` ~ ., data = data) |>
  step_normalize(all_numeric_predictors())

# Setting Mode and Engine
ml_model <- boost_tree() |> 
  set_mode("classification") |>
  set_engine("xgboost",
             na.action = "na.pass",
             missing = NA)

# Setting Workflow
ml_wf <- workflow() |>
  add_recipe(ml_recipe) |>
  add_model(ml_model)

# Training Data
ml_trained <- ml_wf |>
  fit(train_data)

###############################################
# IMPROVED MODEL EVALUATION WITH MULTIPLE METRICS
###############################################

# Generate class predictions
class_preds <- ml_trained |>
  predict(test_data)

# Generate probability predictions
prob_preds <- ml_trained |>
  predict(test_data, type = "prob")

# Combine all predictions with the test data
pred_results <- bind_cols(
  test_data,
  class_preds,
  prob_preds
)

# Check the levels of the Launch Status to determine column names
levels(test_data$`Launch Status`)

# Display column names to identify prediction probability columns
names(pred_results)

# Create a comprehensive metrics table
# Note: You'll need to replace ".pred_level1", ".pred_level2", etc. with 
# your actual column names from the probability predictions
launch_status_levels <- levels(test_data$`Launch Status`)
prob_col_name <- paste0(".pred_", launch_status_levels[1])

# Calculate multiple metrics at once
model_metrics <- bind_rows(
  accuracy(pred_results, truth = `Launch Status`, estimate = .pred_class),
  precision(pred_results, truth = `Launch Status`, estimate = .pred_class),
  recall(pred_results, truth = `Launch Status`, estimate = .pred_class),
  f_meas(pred_results, truth = `Launch Status`, estimate = .pred_class), # F1 score
  specificity(pred_results, truth = `Launch Status`, estimate = .pred_class),
  kap(pred_results, truth = `Launch Status`, estimate = .pred_class) # Cohen's Kappa
)

# Display all metrics
print("Comprehensive Model Evaluation Metrics:")
model_metrics

# Confusion Matrix with more details
conf_matrix <- pred_results |>
  conf_mat(`Launch Status`, .pred_class)

print("Confusion Matrix:")
conf_matrix

# Visualize the confusion matrix
conf_matrix_plot <- conf_matrix |>
  autoplot(type = "heatmap")

print("Confusion Matrix Visualization:")
print(conf_matrix_plot)

# If you have multi-class classification, get class-specific metrics
if(length(levels(test_data$`Launch Status`)) > 2) {
  class_metrics <- pred_results |>
    metrics(truth = `Launch Status`, estimate = .pred_class) |>
    bind_rows(
      precision(pred_results, truth = `Launch Status`, estimate = .pred_class, estimator = "macro"),
      recall(pred_results, truth = `Launch Status`, estimate = .pred_class, estimator = "macro"),
      f_meas(pred_results, truth = `Launch Status`, estimate = .pred_class, estimator = "macro")
    )
  
  print("Class-specific metrics (for multi-class):")
  class_metrics
}

# If it's binary classification and you have ROC curve
if(length(levels(test_data$`Launch Status`)) == 2) {
  # Make sure to use the actual probability column name
  roc_data <- pred_results |>
    roc_curve(truth = `Launch Status`, !!sym(prob_col_name))
  
  roc_plot <- roc_data |>
    autoplot()
  
  print("ROC Curve (for binary classification):")
  print(roc_plot)
  
  # Calculate ROC AUC
  roc_auc_value <- pred_results |>
    roc_auc(truth = `Launch Status`, !!sym(prob_col_name))
  
  print("ROC AUC:")
  roc_auc_value
  
  # PR Curve
  pr_data <- pred_results |>
    pr_curve(truth = `Launch Status`, !!sym(prob_col_name))
  
  pr_plot <- pr_data |>
    autoplot()
  
  print("Precision-Recall Curve:")
  print(pr_plot)
  
  # Calculate PR AUC
  pr_auc_value <- pred_results |>
    pr_auc(truth = `Launch Status`, !!sym(prob_col_name))
  
  print("PR AUC:")
  pr_auc_value
}

# Calculate gain and lift curves (useful for business applications)
if(length(levels(test_data$`Launch Status`)) == 2) {
  gain_curve_data <- pred_results |>
    gain_curve(truth = `Launch Status`, !!sym(prob_col_name))
  
  gain_plot <- gain_curve_data |>
    autoplot()
  
  print("Gain Curve:")
  print(gain_plot)
  
  lift_curve_data <- pred_results |>
    lift_curve(truth = `Launch Status`, !!sym(prob_col_name))
  
  lift_plot <- lift_curve_data |>
    autoplot()
  
  print("Lift Curve:")
  print(lift_plot)
}

final_model <- ml_trained

saveRDS(final_model, "./rocket_launch_model_boosted_tree.rds")
