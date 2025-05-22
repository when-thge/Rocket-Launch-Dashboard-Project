# --- SCRIPT: train_price_model_v2.R ---
# Purpose: Train a model to predict rocket price (log_Rocket_Price) with KNN Imputation
# This script assumes 'preprocessed_raw_data.rds' exists and is correctly formatted.

# --- Libraries ---
library(tidyverse)
library(tidymodels)
library(ranger) # For Random Forest
library(vip)    # For variable importance

# --- Step 1: Load Preprocessed Data ---
full_processed_data <- readRDS("preprocessed_raw_data.rds")

message("Columns available from preprocessed_raw_data.rds:")
print(colnames(full_processed_data))

if (!"log_Rocket_Price" %in% names(full_processed_data)) {
  stop("'log_Rocket_Price' column is missing from 'preprocessed_raw_data.rds'. Please run pre-processing.")
}

# --- Step 2: Define Predictor Features and Select Data for Price Model ---
price_model_predictor_features <- c(
  "Payload_to_LEO", "Rocket_Height", "Liftoff_Thrust", "Mass",
  "Fairing_Diameter", "Fairing_Height", "Stages", "Strap_Ons"
  # Ensure "Launch_Year" is excluded if that was the intent for predictors
)

missing_predictors <- setdiff(price_model_predictor_features, names(full_processed_data))
if (length(missing_predictors) > 0) {
  stop(paste("The following predictor columns are missing from 'preprocessed_raw_data.rds':",
             paste(missing_predictors, collapse = ", ")))
}

# Select ONLY the outcome (log_Rocket_Price) and the defined predictors
# We will NOT use drop_na() here, as KNN imputation will handle missing predictor values.
# We still need to filter out rows where the OUTCOME (log_Rocket_Price) is NA.
price_data_for_training <- full_processed_data |>
  select(log_Rocket_Price, all_of(price_model_predictor_features)) |>
  filter(!is.na(log_Rocket_Price)) # Remove rows where the target itself is NA

# Check for NAs in predictors to see if KNN imputation will be active
print("Summary of NAs in selected predictor columns before imputation:")
print(sapply(price_data_for_training[price_model_predictor_features], function(x) sum(is.na(x))))


if (nrow(price_data_for_training) < 50) {
  stop(paste("Not enough observations with non-NA 'log_Rocket_Price' (found", nrow(price_data_for_training), ") to train price model. Minimum 50 required."))
}

message(paste("Using", nrow(price_data_for_training), "observations (with non-NA target) for price model training."))
message("Columns being used for price model training (outcome + predictors):")
print(colnames(price_data_for_training))


# --- Step 3: Splitting Data ---
set.seed(5678)
price_split <- initial_split(price_data_for_training, prop = 0.80)
price_train <- training(price_split)
price_test  <- testing(price_split)

# --- Step 4: Creating Recipe with KNN Imputation ---
price_recipe_v2_knn <- recipe(log_Rocket_Price ~ ., data = price_train) |>
  # Impute missing values in all numeric predictors using KNN
  # Ensure 'k' (neighbors) is less than the number of complete cases in training data for predictors
  # A common default is k=5. Adjust if your data is very small.
  step_impute_knn(all_numeric_predictors(), neighbors = 5) |> 
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors())

message("Price Model Recipe (v2 with KNN Imputation):")
print(price_recipe_v2_knn)

# --- Step 5: Setting Model Specification ---
price_model_spec_v2 <- rand_forest(
  trees = 200, 
  mtry = tune(), 
  min_n = tune()
) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation", num.threads = parallel::detectCores() - 1)

# --- Step 6: Setting Workflow ---
price_workflow_v2_knn <- workflow() |>
  add_recipe(price_recipe_v2_knn) |>
  add_model(price_model_spec_v2)

# --- Step 7: Hyperparameter Tuning (Simplified) ---
set.seed(6789)
price_folds <- vfold_cv(price_train, v = 3, repeats = 1)

num_actual_predictors <- length(price_model_predictor_features)
mtry_upper_bound <- if (num_actual_predictors > 1) min(5, num_actual_predictors) else 1

tune_grid_simple_price_knn <- grid_regular(
  mtry(range = c(2, mtry_upper_bound)),
  min_n(range = c(2, 10)),
  levels = 2
)
if (mtry_upper_bound < 2) {
  tune_grid_simple_price_knn <- grid_regular(mtry(range = c(1,1)), min_n(range = c(2,10)), levels = 2)
}

message("Starting hyperparameter tuning for price model (with KNN imputation)...")
if(nrow(tune_grid_simple_price_knn) == 0) {
  warning("Tune grid for price model is empty. Using default ranger params.")
  default_price_model_spec_knn <- rand_forest(trees = 100) |>
    set_mode("regression") |>
    set_engine("ranger", importance = "permutation", num.threads = parallel::detectCores() - 1)
  final_price_workflow_v2_knn <- price_workflow_v2_knn |> update_model(default_price_model_spec_knn)
  message("Training final price model with default hyperparameters...")
  final_price_model_v2 <- fit(final_price_workflow_v2_knn, data = price_train)
} else {
  tuned_price_model_knn <- tune_grid(
    price_workflow_v2_knn,
    resamples = price_folds,
    grid = tune_grid_simple_price_knn,
    metrics = metric_set(rmse, rsq),
    control = control_grid(save_pred = TRUE, verbose = FALSE)
  )
  best_hps_price_knn <- select_best(tuned_price_model_knn, metric = "rmse")
  message("Best hyperparameters for price model (KNN):")
  print(best_hps_price_knn)
  final_price_workflow_v2_knn <- finalize_workflow(price_workflow_v2_knn, best_hps_price_knn)
  message("Training final price model with best hyperparameters (KNN)...")
  final_price_model_v2 <- fit(final_price_workflow_v2_knn, data = price_train)
}

# --- Step 9: Model Evaluation ---
predictions_on_log_scale <- predict(final_price_model_v2, new_data = price_test)

price_predictions_test_eval <- predictions_on_log_scale |>
  bind_cols(price_test |> select(log_Rocket_Price)) |>
  mutate(
    Rocket_Price_truth_orig_scale = exp(log_Rocket_Price),
    .pred_orig_scale = exp(.pred)
  )

price_metrics_log <- price_predictions_test_eval |>
  metrics(truth = log_Rocket_Price, estimate = .pred)
print("Price Model Metrics (on log_Rocket_Price scale with KNN imputation):")
print(price_metrics_log)

price_metrics_orig <- price_predictions_test_eval |>
  metrics(truth = Rocket_Price_truth_orig_scale, estimate = .pred_orig_scale)
print("Price Model Metrics (on original Rocket_Price scale with KNN imputation):")
print(price_metrics_orig)

# --- Step 10: Variable Importance ---
tryCatch({
  print("Extracting variable importance for price model (KNN)...")
  ranger_fit_price_knn <- extract_fit_engine(final_price_model_v2)
  vi_plot_price_knn <- vip(ranger_fit_price_knn, geom = "col", num_features = length(price_model_predictor_features)) +
    labs(title = "Feature Importance for Rocket Price (KNN Imputation)") + theme_bw()
  print(vi_plot_price_knn)
}, error = function(e) {
  warning("Could not generate variable importance plot for price model (KNN): ", e$message)
})

# --- Step 11: Save the Model ---
# Save with a name indicating KNN imputation was used, or overwrite your v2 model
saveRDS(final_price_model_v2, "rocket_price_model_v2_knn.rds")
message("New price prediction model (with KNN imputation) saved as 'rocket_price_model_v2_knn.rds'")
message("Ensure 'preprocessed_raw_data.rds' and 'rocket_price_model_v2_knn.rds' are in your Shiny app's directory.")

# --- End of train_price_model_v2.R ---