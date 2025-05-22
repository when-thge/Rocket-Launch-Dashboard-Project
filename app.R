# Authors:
# James Ga-as
# Theo Benedict Pasia
# 2nd Year Students - University of Southeastern Philippines - Obrero Campus

# --- Libraries ---
library(shiny)
library(bslib)
library(tidyverse)
library(tidymodels)
library(xgboost)
# library(ranger) # Only if price model was kept and used ranger
library(scales)
library(plotly)

# --- Data Loading and Initial Preparation ---
# Ensure 'preprocessed_raw_data.rds' is in the same directory as app.R
# This file should be created by your one_time_preprocess.R script.
raw_data <- readRDS("preprocessed_raw_data.rds") 

data_ts <- raw_data |>
  select(Launch_Year, Rocket_Name, Rocket_Organisation, Launch_Status)

# Ensure Rocket_Price is handled if it's meant to be in rocket_param_choices
# If Rocket_Price was removed from preprocessed_raw_data.rds, it won't be here.
config_data_cols <- c(
  "Rocket_Name", "Fairing_Height", "Fairing_Diameter", "Rocket_Height",
  "Payload_to_LEO", "Payload_to_GTO", "Stages", "Strap_Ons",
  "Liftoff_Thrust", "Payloads", "Mass", "Launch_Year"
)
if ("Rocket_Price" %in% names(raw_data)) {
  config_data_cols <- c(config_data_cols, "Rocket_Price")
}
config_data <- raw_data |>
  select(all_of(config_data_cols)) |>
  drop_na(Rocket_Name)


# --- Load Launch Success Model ---
launch_model <- NULL
model_levels <- NULL 
tryCatch({
  model_path_launch <- "rocket_launch_model_boosted_tree.rds"
  if (file.exists(model_path_launch)) {
    launch_model <- readRDS(model_path_launch)
    # Robust model_levels extraction
    if (!is.null(launch_model$lvl)) model_levels <- launch_model$lvl
    else if (!is.null(extract_spec_parsnip(launch_model)$meta$lvl_names)) model_levels <- extract_spec_parsnip(launch_model)$meta$lvl_names
    else if (!is.null(launch_model$classes)) model_levels <- launch_model$classes
    else if (is.factor(data_ts$Launch_Status)) {
      model_levels <- levels(data_ts$Launch_Status)
      message("Using Launch_Status levels from data_ts as model_levels fallback.")
    } else {
      warning("Could not auto-determine class levels for launch model. Using hardcoded fallback.")
      model_levels <- c("Success", "Failure", "Partial Failure", "Prelaunch Failure") 
    }
    message("Launch success model loaded. Model levels: ", paste(model_levels, collapse=", "))
  } else { warning("Launch success model file '", model_path_launch, "' NOT FOUND. Path checked: ", file.path(getwd(), model_path_launch)) }
}, error = function(e) { warning(paste("Error loading launch success model:", e$message)) })


# --- Parameter Choices & Model Features ---
rocket_param_choices_base <- c("Fairing_Height","Fairing_Diameter","Rocket_Height","Payload_to_LEO","Payload_to_GTO","Stages","Strap_Ons","Liftoff_Thrust","Payloads","Mass")
rocket_param_choices <- if ("Rocket_Price" %in% names(config_data)) c(rocket_param_choices_base, "Rocket_Price") else rocket_param_choices_base

launch_model_features <- c("Fairing_Height","Fairing_Diameter","Rocket_Height","Payload_to_LEO","Payload_to_GTO","Stages","Strap_Ons","Liftoff_Thrust","Payloads","Mass")
all_prediction_inputs_features <- launch_model_features


# --- Prediction Functions ---
predict_launch_all_probs <- function(inputs_for_launch_model) {
  if (is.null(launch_model)) return(NULL)
  observation <- tibble(!!!inputs_for_launch_model)
  for(col in launch_model_features) {
    if(col %in% names(observation)){
      if(col %in% c("Stages", "Strap_Ons", "Payloads")) observation[[col]] <- as.integer(observation[[col]])
      else observation[[col]] <- as.numeric(observation[[col]])
    } else { return(NULL) } # Should not happen if inputs_for_launch_model is built correctly
  }
  
  # Ensure all features expected by the model are present in the observation, even if NA (model should handle NAs if trained to)
  # This step might be redundant if inputs_for_launch_model is already perfectly structured
  # and launch_model_features is the definitive list.
  # missing_cols <- setdiff(launch_model_features, names(observation))
  # if(length(missing_cols) > 0) {
  #   warning(paste("Missing columns for prediction:", paste(missing_cols, collapse=", ")))
  #   return(NULL) # Or add NA columns: for(mc in missing_cols) observation[[mc]] <- NA_real_
  # }
  
  pred_probs_raw <- predict(launch_model, observation, type = "prob")
  
  # Tidymodels usually returns a tibble with .pred_class, .pred_LEVEL1, .pred_LEVEL2 etc.
  # Let's ensure the column names match model_levels if they don't already.
  if (is.matrix(pred_probs_raw)) { # Some older model objects might return a matrix
    if (!is.null(model_levels) && ncol(pred_probs_raw) == length(model_levels)) {
      colnames(pred_probs_raw) <- paste0(".pred_", model_levels)
      pred_probs_raw <- as_tibble(pred_probs_raw)
    } else { 
      warning("Mismatched columns in matrix probability prediction.")
      return(NULL) 
    }
  } else if (is.data.frame(pred_probs_raw)) {
    # Check if names already have .pred_ prefix
    if (!all(startsWith(names(pred_probs_raw), ".pred_"))) {
      # If not, and names match model_levels, add the prefix
      if (!is.null(model_levels) && ncol(pred_probs_raw) == length(model_levels) && all(names(pred_probs_raw) %in% model_levels)) {
        names(pred_probs_raw) <- paste0(".pred_", names(pred_probs_raw))
      } else if (!is.null(model_levels) && ncol(pred_probs_raw) == length(model_levels)) { # If names don't match but count does, assume order
        names(pred_probs_raw) <- paste0(".pred_", model_levels)
      } else {
        warning("Mismatched or unexpected column names in data.frame probability prediction.")
        return(NULL)
      }
    }
    # Ensure all expected .pred_LEVEL columns are present
    expected_pred_cols <- paste0(".pred_", model_levels)
    if (!all(expected_pred_cols %in% names(pred_probs_raw))) {
      warning("Not all expected probability columns found in prediction output.")
      # Attempt to reorder/select if possible, or return NULL
      if (all(model_levels %in% sub("^\\.pred_", "", names(pred_probs_raw)) )) {
        pred_probs_raw <- pred_probs_raw %>% select(all_of(expected_pred_cols))
      } else {
        return(NULL)
      }
    }
    
  } else {
    warning("Prediction output is not a matrix or data.frame.")
    return(NULL)
  }
  return(pred_probs_raw)
}

# --- UI Definition ---
ui <- page_navbar(
  title = "Rocket Launches Dashboard",
  theme = bs_theme(version = 5, bootswatch = "cyborg", base_font = font_google("Exo 2"), heading_font = font_google("Orbitron"), "font-size-base" = "0.9rem"),
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode_toggle", mode = "dark")),
  
  nav_panel("Time Series Analysis", icon = shiny::icon("chart-line"),
            layout_sidebar(
              sidebar = sidebar(width = 350, title = "Filter by...",
                                accordion(
                                  accordion_panel("Year", icon = shiny::icon("calendar-alt"), sliderInput("year_slider", "Filter by Year", min = min(data_ts$Launch_Year, na.rm = TRUE), max = max(data_ts$Launch_Year, na.rm = TRUE), value = c(min(data_ts$Launch_Year, na.rm = TRUE), max(data_ts$Launch_Year, na.rm = TRUE)), step = 1, tick = FALSE, sep = "")),
                                  accordion_panel("Rocket", icon = shiny::icon("rocket"), selectizeInput("select_rocket", "Filter by Rocket:", choices = unique(data_ts$Rocket_Name), multiple = TRUE, options = list(placeholder = "All Rockets"))),
                                  accordion_panel("Organization", icon = shiny::icon("building"), selectizeInput("select_organization", "Filter by Organization:", choices = unique(data_ts$Rocket_Organisation), multiple = TRUE, options = list(placeholder = "All Organizations"))),
                                  accordion_panel("Launch Status", icon = shiny::icon("check-circle"), selectizeInput("select_status", "Filter by Launch Status:", choices = if(!is.null(model_levels)) model_levels else levels(data_ts$Launch_Status), multiple = TRUE, options = list(placeholder = "All Statuses")))
                                )
              ),
              navset_card_tab(
                nav_panel("Launch Counts",
                          layout_columns(col_widths = c(3,3,3,3),
                                         value_box("Success", textOutput("success_vbox"), showcase = shiny::icon("check-circle"), theme = "success"),
                                         value_box("Partial Failure", textOutput("partial_failure_vbox"), showcase = shiny::icon("exclamation-triangle"), theme = "warning"),
                                         value_box("Failure", textOutput("failure_vbox"), showcase = shiny::icon("times-circle"), theme = "danger"),
                                         value_box("Prelaunch Failure", textOutput("prelaunch_failure_vbox"), showcase = shiny::icon("minus-circle"), theme = "secondary")
                          ),
                          card(full_screen = TRUE, card_header("Launch Volume and Outcomes Over Time"), plotlyOutput("Time_series_line", height = "500px"))
                ),
                nav_panel("Success Rate Trend", card(full_screen = TRUE, card_header("Weighted Launch Success Rate Trend"), plotlyOutput("success_rate_trend", height = "550px")))
              )
            )
  ),
  
  nav_panel("Rocket Parameters", icon = shiny::icon("cogs"),
            layout_sidebar(
              sidebar = sidebar(width = 300, title = "Plot Variables", 
                                selectInput("x_var_config", "X-axis:", choices = rocket_param_choices, selected = "Payload_to_LEO"), 
                                selectInput("y_var_config", "Y-axis:", choices = rocket_param_choices, selected = "Rocket_Height")),
              card(full_screen = TRUE, card_header(textOutput("rocket_params_plot_title")), plotlyOutput("rocket_params_plot", height = "600px"))
            )
  ),
  
  nav_panel("Launch Outcome Prediction", icon = shiny::icon("brain"),
            layout_sidebar(
              sidebar = sidebar(title = "Input Rocket Parameters", width=375,
                                lapply(all_prediction_inputs_features, function(param) {
                                  default_val <- if(param %in% names(config_data)) median(config_data[[param]], na.rm = TRUE) else 0
                                  if(is.na(default_val) || !is.numeric(default_val)) default_val <- 0 # Robust default
                                  param_step <- if(param %in% c("Stages","Strap_Ons","Payloads")) 1 else 0.1
                                  param_label <- str_replace_all(param, "_", " ")
                                  numericInput(paste0("pred_param_", param), param_label, 
                                               value=round(default_val, if(param_step==1)0 else 2), 
                                               step=param_step)
                                }),
                                actionButton("get_launch_outcome_predictions_button", "Predict Launch Outcome",
                                             icon=shiny::icon("rocket"), class="btn-primary w-100 mt-3")
              ),
              card(full_screen=TRUE,
                   card_header(h5("Predicted Launch Outcome Probabilities")), 
                   uiOutput("launch_all_probs_ui")
              ) 
            )
  ),
  
  nav_panel("About", icon = shiny::icon("info-circle"),
            div(class = "container py-4", 
                style = "max-width: 960px; margin: auto;", 
                
                card(
                  class = "mb-4", 
                  card_header(h4(shiny::icon("rocket", lib="font-awesome"), " Rocket Launch Analytics Dashboard")),
                  card_body(
                    h5("Project Summary", class="card-title"),
                    p("This interactive dashboard provides a comprehensive platform for analyzing historical rocket launch data. It allows users to explore trends in launch activity, success rates, and specific rocket parameters. Additionally, it incorporates a predictive model to estimate launch outcome probabilities based on vehicle specifications."),
                    p("Developed by ", strong("James Ga-as & Theo Benedict Pasia"), " (2nd Year BSCS Students, University of Southeastern Philippines - Obrero Campus), this project demonstrates capabilities in data processing, visualization with R (Shiny, ggplot2, plotly), and predictive modeling with tidymodels and XGBoost."),
                    hr(),
                    h5("Data Source"),
                    tags$p(
                      "The primary dataset, ",
                      tags$a(href = "https://www.kaggle.com/datasets/maccaroo/rocket-launch-industry?select=Launches.csv", 
                             target = "_blank", 
                             "Rocket Launch Industry dataset on Kaggle"),
                      ", compiles publicly available information on space launches, typically sourced from repositories like Kaggle, supplemented by public space agency reports and encyclopedic resources."
                    )
                  )
                ),
                
                h4("Dashboard Features & Interpretation Guide", class="text-center mb-4"),
                
                accordion(
                  id = "about_accordion", 
                  open = FALSE, 
                  
                  accordion_panel(
                    title = "Time Series Analysis: Historical Trends & Outcomes",
                    value = "about_timeseries", 
                    icon = shiny::icon("chart-line"),
                    p(strong("Overview:"), " This section offers two primary visualizations for understanding historical launch patterns:"),
                    tags$ul(
                      tags$li(strong("Launch Counts by Year:"), " The ",tags$em("value boxes"), " provide aggregate counts of launch outcomes (Success, Partial Failure, Failure, Prelaunch Failure) based on current filter selections. The ",tags$em("stacked bar chart"), " below offers a year-by-year breakdown of launch volume and the proportion of each outcome."),
                      tags$li(strong("Success Rate Trend:"), " The ",tags$em("line graph"), " visualizes the weighted launch success rate over time. A 'Success' contributes 1.0, 'Partial Failure' 0.5, and others 0.0 to the annual average. Point size indicates the total number of launches in a given year.")
                    ),
                    tags$strong("Interpretation & Key Insights:"),
                    tags$ul(
                      tags$li("Identify periods of high/low launch activity and shifts in success/failure ratios."),
                      tags$li("Analyze the impact of specific organizations or rocket types on overall trends by using the sidebar filters."),
                      tags$li("Observe the trajectory of launch reliability: Is it improving, declining, or volatile? Correlate with launch volume.")
                    )
                  ),
                  
                  accordion_panel(
                    title = "Rocket Parameters: Exploring Vehicle Specifications",
                    value = "about_parameters",
                    icon = shiny::icon("cogs"),
                    p(strong("Overview:"), " This interactive ",tags$em("scatter plot"), " allows for the exploration of relationships between various physical and performance characteristics of launch vehicles."),
                    p("Select any two parameters from the sidebar dropdowns to plot them against each other. Hover over data points to reveal individual rocket names and their specific values for the chosen parameters."),
                    tags$strong("Interpretation & Key Insights:"),
                    tags$ul(
                      tags$li("Investigate potential correlations, e.g., between rocket height and payload capacity, or liftoff thrust and vehicle mass."),
                      tags$li("Identify clusters or outliers representing distinct classes or unique designs of launch systems.")
                    )
                  ),
                  
                  accordion_panel(
                    title = "Launch Outcome Prediction: Estimating Probabilities",
                    value = "about_prediction",
                    icon = shiny::icon("brain"),
                    p(strong("Overview:"), " This section utilizes a machine learning model (XGBoost classifier) to estimate the probabilities of different launch outcomes (Success, Failure, etc.) based on user-inputted rocket specifications."),
                    p("Enter the parameters for a hypothetical or existing rocket in the sidebar. Upon clicking 'Predict Launch Outcome', the dashboard will display the model's calculated probabilities for each potential outcome category."),
                    tags$strong("Interpretation & Key Considerations:"),
                    tags$ul(
                      tags$li("The probabilities represent the model's confidence for each outcome based on patterns learned from historical data."),
                      tags$li("This feature is intended for exploratory and educational purposes. Real-world launch success is influenced by numerous factors beyond those included in this model (e.g., weather, specific mission complexities, last-minute technical issues)."),
                      tags$li("Model performance is dependent on the quality and representativeness of the training data. The accuracy for minority classes (e.g., 'Prelaunch Failure') may be lower if they were rare in the training set.")
                    )
                  )
                ), 
                
                hr(class="my-4"),
                p(class="text-center text-muted", em("Dashboard developed using R, Shiny, and the Tidyverse/Tidymodels ecosystem."))
                
            ) 
  ) 
) 


# --- Server Logic ---
server <- function(input, output, session) {
  print(paste("Shiny app started. bslib version:", as.character(utils::packageVersion("bslib"))))
  if(is.null(launch_model)) print("NOTE: Launch model object is NULL at app start.")
  
  # == Time Series Tab Logic ==
  filtered_data_ts <- reactive({
    req(input$year_slider); current_data <- data_ts |> filter(Launch_Year >= input$year_slider[1] & Launch_Year <= input$year_slider[2])
    if (!is.null(input$select_rocket) && length(input$select_rocket) > 0) current_data <- filter(current_data, Rocket_Name %in% input$select_rocket)
    if (!is.null(input$select_organization) && length(input$select_organization) > 0) current_data <- filter(current_data, Rocket_Organisation %in% input$select_organization)
    if (!is.null(input$select_status) && length(input$select_status) > 0) current_data <- filter(current_data, Launch_Status %in% input$select_status)
    current_data
  })
  status_counts <- reactive({
    req(filtered_data_ts()); defined_levels_sc <- if(!is.null(model_levels)) model_levels else levels(data_ts$Launch_Status)
    if (nrow(filtered_data_ts()) == 0) return(tibble(Launch_Status=factor(levels=defined_levels_sc), count=integer()))
    filtered_data_ts() |> group_by(Launch_Status) |> summarize(count = n(), .groups = "drop")
  })
  output$success_vbox<-renderText({ val<-filter(status_counts(),Launch_Status=="Success")$count; if(length(val)==0)"0" else as.character(val) })
  output$partial_failure_vbox<-renderText({ val<-filter(status_counts(),Launch_Status=="Partial Failure")$count; if(length(val)==0)"0" else as.character(val) })
  output$failure_vbox<-renderText({ val<-filter(status_counts(),Launch_Status=="Failure")$count; if(length(val)==0)"0" else as.character(val) })
  output$prelaunch_failure_vbox<-renderText({ val<-filter(status_counts(),Launch_Status=="Prelaunch Failure")$count; if(length(val)==0)"0" else as.character(val) })
  output$Time_series_line<-renderPlotly({
    req(nrow(filtered_data_ts())>0); defined_levels_plot<-if(!is.null(model_levels)) model_levels else c("Success","Partial Failure","Failure","Prelaunch Failure")
    d<-filtered_data_ts()|>group_by(Launch_Year,Launch_Status)|>summarize(count=n(),.groups="drop")|>mutate(Launch_Status=factor(Launch_Status,levels=defined_levels_plot))
    # Ensure all defined_levels_plot are present in d$Launch_Status for consistent coloring, even if count is 0 for some.
    # This can be handled by tidyr::complete if necessary, or ensuring factor levels are set before plotting.
    # For now, relying on ggplot's `drop=FALSE` in scale_fill_manual.
    clrs<-c("Success"="#28a745","Partial Failure"="#ffc107","Failure"="#dc3545","Prelaunch Failure"="#6c757d"); 
    clrs_plot<-clrs[names(clrs)%in%levels(d$Launch_Status)] # Ensure only relevant colors are passed
    p<-ggplot(d,aes(x=factor(Launch_Year),y=count,fill=Launch_Status,text=paste0("Status: ",Launch_Status,"<br>Count: ",count)))+
      geom_bar(stat="identity",position="stack")+
      scale_fill_manual(values=clrs_plot,name="Launch Status",drop=FALSE)+ # drop=FALSE is important
      labs(x="Year",y="Number of Launches")+
      theme_minimal(base_size=10)+
      theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="top"); 
    ggplotly(p,tooltip="text")
  })
  output$success_rate_trend<-renderPlotly({
    req(nrow(filtered_data_ts())>0); d<-filtered_data_ts()|>mutate(o=case_when(Launch_Status=="Success"~1,Launch_Status=="Partial Failure"~.5,TRUE~0))|>group_by(Launch_Year)|>summarize(r=mean(o,na.rm=T),t=n(),.groups="drop")
    p<-ggplot(d,aes(x=Launch_Year,y=r))+
      geom_line(color="#0dcaf0",linewidth=1)+
      geom_point(aes(size=t,text=paste0("Rate: ",scales::percent(r,.1),"<br>Total: ",t)),color="#0dcaf0",alpha=.6)+
      scale_y_continuous(labels=scales::percent_format(1),limits=c(0,1))+
      scale_size_continuous(name="Total Launches",range=c(2.5,9))+
      labs(x="Year",y="Weighted Success Rate")+
      theme_minimal(base_size=10)+
      theme(legend.position="top"); 
    ggplotly(p,tooltip="text")
  })
  
  # == Rocket Parameters Tab Logic ==
  output$rocket_params_plot_title <- renderText({ paste(str_replace_all(input$y_var_config,"_"," "), "vs.", str_replace_all(input$x_var_config,"_"," ")) })
  output$rocket_params_plot <- renderPlotly({
    req(input$x_var_config, input$y_var_config)
    req(input$x_var_config %in% names(config_data), input$y_var_config %in% names(config_data))
    df<-config_data|>select(Rocket_Name,x_var=all_of(input$x_var_config),y_var=all_of(input$y_var_config))|>drop_na(); 
    req(nrow(df)>0)
    p<-ggplot(df,aes(x=x_var,y=y_var))+
      geom_point(aes(text=paste0("Rocket: ",Rocket_Name,"<br>",str_replace_all(input$x_var_config,"_"," "),": ",x_var,"<br>",str_replace_all(input$y_var_config,"_"," "),": ",y_var)),alpha=.7,color="cyan",size=3)+
      labs(x=str_replace_all(input$x_var_config,"_"," "),y=str_replace_all(input$y_var_config,"_"," "))+
      theme_minimal(base_size=10); 
    ggplotly(p,tooltip="text")
  })
  
  # == Predictions Tab Logic (Only Launch Outcome) ==
  launch_outcome_predictions_rv <- reactiveValues(launch_probs = NULL)
  observeEvent(input$get_launch_outcome_predictions_button, {
    inputs_for_launch_model <- sapply(launch_model_features, function(param) input[[paste0("pred_param_", param)]], simplify = FALSE)
    
    # Validate all inputs are present and numeric before prediction
    if(any(sapply(inputs_for_launch_model, is.null)) || !all(sapply(inputs_for_launch_model, is.numeric))) { 
      launch_outcome_predictions_rv$launch_probs <- NULL; 
      warning("One or more prediction inputs are NULL or non-numeric.")
      return() 
    }
    
    if (!is.null(launch_model)) {
      if(all(launch_model_features %in% names(inputs_for_launch_model))) {
        launch_outcome_predictions_rv$launch_probs <- predict_launch_all_probs(inputs_for_launch_model)
      } else { 
        launch_outcome_predictions_rv$launch_probs <- NULL; 
        warning("Launch model inputs incomplete for prediction.") 
      }
    } else {
      launch_outcome_predictions_rv$launch_probs <- NULL
      warning("Launch model is not loaded, cannot predict.")
    }
  })
  
  output$launch_all_probs_ui <- renderUI({
    probs_tibble <- launch_outcome_predictions_rv$launch_probs
    
    if (is.null(launch_model)) return(div(class="text-center p-3", p("Launch outcome model not loaded.")))
    if (is.null(probs_tibble) && !isTruthy(input$get_launch_outcome_predictions_button)) return(div(class="text-center p-3", p("Enter parameters and click 'Predict Launch Outcome'.")))
    if (is.null(probs_tibble) && isTruthy(input$get_launch_outcome_predictions_button)) return(div(class="text-center p-3 text-danger", p("Could not generate outcome probabilities. Check inputs and model.")))
    
    # Ensure model_levels is available
    current_model_levels <- if(!is.null(model_levels)) model_levels else c("Success", "Failure", "Partial Failure", "Prelaunch Failure") # Fallback
    
    prob_col_names <- paste0(".pred_", current_model_levels)
    # Filter probs_tibble to only include expected columns and ensure they exist
    if (!all(prob_col_names %in% names(probs_tibble))) {
      # Try to match based on model_levels without .pred_ if that's what predict_launch_all_probs returned
      raw_level_cols <- current_model_levels[current_model_levels %in% names(probs_tibble)]
      if(length(raw_level_cols) == length(current_model_levels)) {
        names(probs_tibble)[names(probs_tibble) %in% raw_level_cols] <- paste0(".pred_", raw_level_cols)
      } else {
        warning("Predicted probability columns do not match expected model levels.")
        return(div(class="text-center p-3 text-danger", p("Prediction error: Output columns mismatch.")))
      }
    }
    
    # Order: Success, Partial Failure, Failure, Prelaunch Failure (if they exist)
    desired_order <- c(".pred_Success", ".pred_Partial Failure", ".pred_Failure", ".pred_Prelaunch Failure")
    ordered_col_names <- desired_order[desired_order %in% names(probs_tibble)]
    # Add any other columns from probs_tibble that weren't in desired_order (shouldn't happen if model_levels is correct)
    ordered_col_names <- unique(c(ordered_col_names, names(probs_tibble)[startsWith(names(probs_tibble), ".pred_")]))
    
    prob_value_boxes <- lapply(ordered_col_names, function(col_name) {
      if (!col_name %in% names(probs_tibble)) return(NULL) # Defensive
      class_name <- sub("^\\.pred_", "", col_name)
      prob_value <- probs_tibble[[col_name]][1] # Get the first (and only) row
      
      if (is.na(prob_value) || !is.numeric(prob_value)) return(NULL)
      prob_percent <- scales::percent(prob_value, accuracy = 0.1)
      
      vb_theme <- case_when(
        class_name == "Success" ~ "success", 
        class_name == "Partial Failure" ~ "warning", # Changed from orange to bslib warning
        class_name == "Failure" ~ "danger",        # Changed from orange to bslib danger
        class_name == "Prelaunch Failure" ~ "secondary", 
        TRUE ~ "info"
      )
      vb_icon_name <- case_when(
        class_name == "Success" ~ "check-circle", 
        class_name == "Failure" ~ "times-circle", 
        class_name == "Partial Failure" ~ "exclamation-triangle", 
        class_name == "Prelaunch Failure" ~ "minus-circle", 
        TRUE ~ "question-circle"
      )
      
      # Icon is now standard size and explicitly positioned top right
      showcase_icon <- shiny::icon(vb_icon_name) 
      
      value_box(
        title = h6(paste0("P(", class_name, ")"), style = "font-size:0.9em; margin-bottom:0px; text-align: left;"), 
        value = h3(prob_percent, style = "margin-top:0px; margin-bottom:2px; text-align: left;"), 
        showcase = showcase_icon,
        showcase_layout = "top right", # Explicitly set layout
        theme = vb_theme, 
        p(paste("Probability:", prob_percent), style = "font-size:0.8em; margin-top:0px; text-align: left;")
      )
    })
    
    prob_value_boxes <- Filter(Negate(is.null), prob_value_boxes)
    if (length(prob_value_boxes) > 0) {
      num_classes <- length(prob_value_boxes)
      col_widths <- if (num_classes >= 4) c(3,3,3,3) else if (num_classes == 3) c(4,4,4) else if (num_classes == 2) c(6,6) else 12
      # Ensure layout_columns gets a list of value_box objects, not a list of lists
      if (num_classes == 1 && is.list(prob_value_boxes[[1]])) {
        prob_value_boxes <- prob_value_boxes[[1]]
      }
      do.call(layout_columns, c(list(col_widths = col_widths, gap = "10px", class = "mb-3"), prob_value_boxes))
    } else {
      p("No valid probability predictions to display.")
    }
  })
}
# --- Run Application ---
shinyApp(ui, server)