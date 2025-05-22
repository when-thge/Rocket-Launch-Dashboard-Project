# --- SCRIPT 1: one_time_preprocess.R (Simplified V2 - Run this ONCE) ---
library(tidyverse) # For readr, dplyr, stringr

# --- Configuration ---
# Set this to TRUE if you want "Rocket_Price" to be available as a numeric column
# for display in the "Rocket Parameters" tab in your Shiny app.
# Set to FALSE if "Rocket_Price" is not needed at all.
process_rocket_price_for_display <- TRUE 

# --- Load Original Data ---
original_raw_data <- readr::read_csv("Aggregated_Launch_Mission_Configs.csv") # Ensure this file is in your working directory

# --- Define Columns and their Target Types ---
# Columns for the success model + general display
numeric_cols_to_process <- c(
  "Fairing_Height", "Fairing_Diameter", "Rocket_Height",
  "Payload_to_LEO", "Payload_to_GTO", "Liftoff_Thrust", "Mass" 
  # Note: Rocket_Price is handled conditionally below
)
integer_cols_to_process <- c("Stages", "Strap_Ons", "Payloads", "Launch_Year")
factor_cols <- c("Launch_Status")


# --- Pre-processing ---
preprocessed_data <- original_raw_data |>
  # Convert specified factor columns
  dplyr::mutate(across(all_of(factor_cols), as.factor)) |>
  
  # Convert specified numeric columns (that need string cleaning)
  dplyr::mutate(across(all_of(numeric_cols_to_process), 
                       ~ as.numeric(stringr::str_remove_all(as.character(.), "[^0-9.]")))) |>
  
  # Convert specified integer columns
  dplyr::mutate(across(all_of(integer_cols_to_process), as.integer))

# Conditionally process Rocket_Price if requested and if it exists
if (process_rocket_price_for_display && "Rocket_Price" %in% names(preprocessed_data)) {
  preprocessed_data <- preprocessed_data |>
    dplyr::mutate(Rocket_Price = as.numeric(stringr::str_remove_all(as.character(Rocket_Price), "[^0-9.]")))
  message("'Rocket_Price' column processed for display.")
} else if (process_rocket_price_for_display && !"Rocket_Price" %in% names(preprocessed_data)) {
  warning("'Rocket_Price' was requested for processing but not found in the CSV.")
} else {
  message("'Rocket_Price' column not processed (as per configuration or absence).")
}

# --- Save Processed Data ---
saveRDS(preprocessed_data, "preprocessed_raw_data.rds")
message("'preprocessed_raw_data.rds' (simplified) created/updated successfully.")
message("Final columns in preprocessed_raw_data.rds:")
print(colnames(preprocessed_data))

# --- Optional: Quick Check of Processed Data ---
# print(head(preprocessed_data))
# print(sapply(preprocessed_data, class))
# if ("Rocket_Price" %in% names(preprocessed_data)) {
#   print(summary(preprocessed_data$Rocket_Price))
# }
# --- END OF SCRIPT 1 ---