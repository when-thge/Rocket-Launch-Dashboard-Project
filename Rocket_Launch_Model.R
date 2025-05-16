library(tidyverse)
library(tidymodels)
library(baguette)
library(nnet)
library(xgboost)
library(yardstick)  # Ensure this is loaded for all metric functions
library(plotly)     # Add plotly for interactive visualizations

# Data loading and preparation
data <- read_csv("Aggregated_Launch_Mission_Configs.csv") |>
  mutate(
    `Launch_Status` = as.factor(`Launch_Status`),
    across(
      c(
        `Fairing_Height`,
        `Fairing_Diameter`,
        `Rocket_Height`,
        `Payload_to_LEO`,
        `Payload_to_GTO`,
        `Liftoff_Thrust`
      ),
      ~ as.numeric(str_remove_all(str_remove_all(., ","), "[^0-9.]"))
    )
  ) |>
  select(
    `Launch_Status`,
    `Fairing_Height`,
    `Fairing_Diameter`,
    `Rocket_Height`,
    `Payload_to_LEO`,
    `Payload_to_GTO`,
    `Stages`,
    `Strap_Ons`,
    `Liftoff_Thrust`,
    `Payloads`,
    `Mass`
  )

# Splitting Data
set.seed(030805)
datasplit <- initial_split(
  data,
  prop = 0.80,
  strata = `Launch_Status`
)
train_data <- training(datasplit)
test_data <- testing(datasplit)

# Creating Recipe
ml_recipe <- recipe(`Launch_Status` ~ ., data = data) |>
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
launch_status_levels <- levels(test_data$`Launch_Status`)
prob_col_name <- paste0(".pred_", launch_status_levels[1])

# Calculate multiple metrics at once
model_metrics <- bind_rows(
  accuracy(pred_results, truth = `Launch_Status`, estimate = .pred_class),
  precision(pred_results, truth = `Launch_Status`, estimate = .pred_class),
  recall(pred_results, truth = `Launch_Status`, estimate = .pred_class),
  f_meas(pred_results, truth = `Launch_Status`, estimate = .pred_class), # F1 score
  specificity(pred_results, truth = `Launch_Status`, estimate = .pred_class),
  kap(pred_results, truth = `Launch_Status`, estimate = .pred_class) # Cohen's Kappa
)

# Display all metrics
print("Comprehensive Model Evaluation Metrics:")
print(model_metrics)

# Confusion Matrix with more details
conf_matrix <- pred_results |>
  conf_mat(`Launch_Status`, .pred_class)

print("Confusion Matrix:")
print(conf_matrix)

# Get class-specific metrics
class_metrics <- pred_results |>
  metrics(truth = `Launch_Status`, estimate = .pred_class) |>
  bind_rows(
    precision(pred_results, truth = `Launch_Status`, estimate = .pred_class, estimator = "macro"),
    recall(pred_results, truth = `Launch_Status`, estimate = .pred_class, estimator = "macro"),
    f_meas(pred_results, truth = `Launch_Status`, estimate = .pred_class, estimator = "macro")
  )

print("Class-specific metrics:")
print(class_metrics)

# Save the final model
final_model <- ml_trained
saveRDS(final_model, "./rocket_launch_model_boosted_tree.rds")

# Print model summary
print("Model Summary:")
print(summary(final_model))
