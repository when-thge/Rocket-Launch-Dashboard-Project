# Authors:
# James Ga-as
# Theo Benedict Pasia
# 2nd Year Students - University of Southeastern Philippines - Obrero Campus

library(tidyverse)
library(tidymodels)
library(xgboost)
library(shiny)
library(shinythemes)
library(bslib)
library(plotly)
library(here)

# --- Data Loading and Initial Preparation ---
raw_data <- read_csv(here::here("./Aggregated_Launch_Mission_Configs.csv")) |>
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
  )

# Data for the "Time Series Analysis" tab filters
data <- raw_data |>
  select(
    Launch_Year,
    Rocket_Name,
    Rocket_Organisation,
    Launch_Status
  )

# Data for the "Individual Rocket Parameters" tab plot
# Assuming raw_data contains all necessary columns for plotting rocket parameters.
# If specific cleaning or selection is needed for this plot, it would happen here.
config_data <- raw_data |>
  select(
    Fairing_Height, 
    Fairing_Diameter,
    Rocket_Height,
    Payload_to_LEO,
    Payload_to_GTO,
    Stages,
    Strap_Ons, 
    Liftoff_Thrust,
    Payloads, 
    Mass  
  )# This will be used for the scatter plot

# Pre-trained XGBoost model for launch prediction
model <- readRDS(here::here("./rocket_launch_model_boosted_tree.rds"))

# Choices for X and Y axis selection in the "Individual Rocket Parameters" plot
# These MUST match R-friendly column names in 'config_data' (i.e., 'raw_data')
rocket_param_choices <- c(
  "Fairing_Height", 
  "Fairing_Diameter",
  "Rocket_Height",
  "Payload_to_LEO",
  "Payload_to_GTO",
  "Stages",
  "Strap_Ons", 
  "Liftoff_Thrust",
  "Payloads", 
  "Mass"      
)

# --- Prediction Function ---
# Predicts launch status based on rocket parameters.
# Arguments and internal tibble use R-friendly names matching model features.
predict_launch <- function(Fairing_Height,   
                           Fairing_Diameter, 
                           Rocket_Height,    
                           Payload_to_LEO,   
                           Payload_to_GTO,   
                           Stages,
                           Strap_Ons,        
                           Liftoff_Thrust,   
                           Payloads,
                           Mass) {
  observation <- tibble(
    Fairing_Height = Fairing_Height,
    Fairing_Diameter = Fairing_Diameter,
    Rocket_Height = Rocket_Height,
    Payload_to_LEO = Payload_to_LEO,
    Payload_to_GTO = Payload_to_GTO,
    Stages = Stages,
    Strap_Ons = Strap_Ons, 
    Liftoff_Thrust = Liftoff_Thrust,
    Payloads = Payloads,
    Mass = Mass
  )
  
  prediction <- predict(model, observation)
  
  return(prediction$.pred_class)
}

# --- UI Definition ---
ui <- page_navbar(
  title = "Rocket Launches Dashboard",
  
  # == Tab 1: Time Series Analysis ==
  nav_panel(
    title = "Time Series Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Filter by...",
        accordion(
          accordion_panel(
            title = "Year",
            icon = bsicons::bs_icon("calendar-range", size = "3rem"),
            sliderInput("year_slider", "Filter by Year",
                        min = min(data$Launch_Year, na.rm = TRUE), 
                        max = max(data$Launch_Year, na.rm = TRUE),
                        value = c(min(data$Launch_Year, na.rm = TRUE), max(data$Launch_Year, na.rm = TRUE)),
                        step = 1, tick = FALSE, sep = "")
          ),
          accordion_panel(
            title = "Rocket",
            icon = bsicons::bs_icon("rocket-takeoff", size = "3rem"),
            selectizeInput("select_rocket", "Filter by Rocket:",
                           choices = unique(data$Rocket_Name), multiple = TRUE,
                           options = list(placeholder = "Select rocket(s)..."))
          ),
          accordion_panel(
            title = "Organization",
            icon = bsicons::bs_icon("buildings", size = "3rem"),
            selectizeInput("select_organization", "Filter by Organization:",
                           choices = unique(data$Rocket_Organisation), multiple = TRUE,
                           options = list(placeholder = "Select Organization(s)..."))
          ),
          accordion_panel(
            title = "Launch Status",
            icon = bsicons::bs_icon("bar-chart", size = "3rem"),
            selectizeInput("select_status", "Filter by Launch Status:",
                           choices = unique(data$Launch_Status), multiple = TRUE,
                           options = list(placeholder = "Select Status..."))
          )
        )
      ),
      navset_card_tab(
        nav_panel(
          title = "Launch Counts by Year",
          layout_columns(
            value_box(title = "Success", value = textOutput("success_vbox"), showcase = bsicons::bs_icon("check-circle-fill"), showcase_layout = "left center", theme_color = "success", height = "75px", full_screen = FALSE),
            value_box(title = "Partial Failure", value = textOutput("partial_failure_vbox"), showcase = bsicons::bs_icon("exclamation-triangle-fill"), showcase_layout = "left center", theme_color = "warning", height = "75px", full_screen = FALSE),
            value_box(title = "Failure", value = textOutput("failure_vbox"), showcase = bsicons::bs_icon("x-circle-fill"), showcase_layout = "left center", theme_color = "danger", height = "75px", full_screen = FALSE),
            value_box(title = "Prelaunch Failure", value = textOutput("prelaunch_failure_vbox"), showcase = bsicons::bs_icon("dash-circle-fill"), showcase_layout = "left center", theme_color = "secondary", height = "75px", full_screen = FALSE),
            col_widths = c(3, 3, 3, 3), row_heights = "auto"
          ),
          layout_columns(
            card(plotlyOutput(outputId = "Time_series_line", height = "500px")),
            col_widths = 12
          )
        ),
        nav_panel(
          title = "Success Rate Trend Over Time",
          card(plotlyOutput(outputId = "success_rate_trend", height = "600px"))
        )
      )
    )
  ),
  
  # == Tab 2: Individual Rocket Parameters ==
  nav_panel(
    title = "Individual Rocket Parameters",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Choose Variables",
        selectInput("x_var_config", "Select X-axis Variable:",
                    choices = rocket_param_choices, 
                    selected = rocket_param_choices[4]), # Default: Payload_to_LEO
        selectInput("y_var_config", "Select Y-axis Variable:",
                    choices = rocket_param_choices, 
                    selected = rocket_param_choices[3])  # Default: Rocket_Height
      ),
      card(
        plotlyOutput("rocket_params_plot", height = "600px") 
      )
    )
  ),
  
  # == Tab 3: Prediction Model (Placeholder) ==
  nav_panel(title = "Prediction Model"),
  
  # == Tab 4: About (Placeholder) ==
  nav_panel(title = "About"),
  
  nav_spacer(),
  
  nav_item(
    input_dark_mode(id = "mode")
  ),
  
  theme = bs_theme(preset = "minty")
  
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive expression for data filtered by user inputs on "Time Series Analysis" tab
  filtered_data <- reactive({
    req(input$year_slider) # Ensure year slider input is available
    
    current_data <- data # Start with the base data for this tab
    
    # Apply year filter
    current_data <- current_data |> 
      dplyr::filter(
        Launch_Year >= input$year_slider[1] & 
          Launch_Year <= input$year_slider[2]
      )
    
    # Apply rocket filter (if any selected)
    if (!is.null(input$select_rocket) && length(input$select_rocket) > 0) {
      current_data <- current_data |>
        dplyr::filter(Rocket_Name %in% input$select_rocket)
    }
    
    # Apply organization filter (if any selected)
    if (!is.null(input$select_organization) && length(input$select_organization) > 0) {
      current_data <- current_data |>
        dplyr::filter(Rocket_Organisation %in% input$select_organization)
    }
    
    # Apply status filter (if any selected)
    if (!is.null(input$select_status) && length(input$select_status) > 0) {
      current_data <- current_data |>
        dplyr::filter(Launch_Status %in% input$select_status)
    }
    
    current_data # Return the fully filtered data
  })
  
  # Reactive expression for launch status counts based on filtered_data
  status_counts <- reactive({
    filtered_data() %>%
      group_by(Launch_Status) %>%
      summarize(count = n(), .groups = "drop")
  })
  
  # Outputs for value boxes displaying launch status counts
  output$success_vbox <- renderText({
    count <- status_counts() %>% filter(Launch_Status == "Success") %>% pull(count)
    if(length(count) == 0) "0" else as.character(count)
  })
  output$partial_failure_vbox <- renderText({
    count <- status_counts() %>% filter(Launch_Status == "Partial Failure") %>% pull(count)
    if(length(count) == 0) "0" else as.character(count)
  })
  output$failure_vbox <- renderText({
    count <- status_counts() %>% filter(Launch_Status == "Failure") %>% pull(count)
    if(length(count) == 0) "0" else as.character(count)
  })
  output$prelaunch_failure_vbox <- renderText({
    count <- status_counts() %>% filter(Launch_Status == "Prelaunch Failure") %>% pull(count)
    if(length(count) == 0) "0" else as.character(count)
  })
  
  # Plot: Launch counts by year, stacked by launch status
  output$Time_series_line <- renderPlotly({
    status_counts_by_year <- filtered_data() %>%
      group_by(Launch_Year, Launch_Status) %>%
      summarize(count = n(), .groups = "drop") %>%
      mutate(Launch_Status = factor(Launch_Status, levels = c("Success", "Partial Failure", "Failure", "Prelaunch Failure")))
    
    status_plot <- ggplot(status_counts_by_year, aes(x = factor(Launch_Year), y = count, fill = Launch_Status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Success"="#4CAF50", "Partial Failure"="#FFC107", "Failure"="#F44336", "Prelaunch Failure"="#9E9E9E")) +
      labs(x="Year", y="Number of Launches", fill="Launch Status") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(status_plot)
  })
  
  # Plot: Success rate trend over time
  output$success_rate_trend <- renderPlotly({
    success_trend <- filtered_data() %>%
      mutate(is_success = case_when(Launch_Status == "Success" ~ 1, Launch_Status == "Partial Failure" ~ 0.5, TRUE ~ 0)) %>%
      group_by(Launch_Year) %>%
      summarize(success_rate = mean(is_success, na.rm = TRUE), total_launches = n(), .groups = "drop") # Added na.rm = TRUE
    
    trend_plot <- ggplot(success_trend, aes(x = Launch_Year, y = success_rate)) +
      geom_line(color = "#2196F3", linewidth = 1) + # Changed size to linewidth
      geom_point(aes(size = total_launches, text = paste0("Year: ", Launch_Year, "<br>Success Rate: ", scales::percent(success_rate, accuracy = 0.1), "<br>Total Launches: ", total_launches)), color = "#2196F3", alpha = 0.4) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_size_continuous(range = c(2, 7)) +
      labs(x="Year", y="Success Rate", size="Number of Launches") +
      theme_minimal() + theme(legend.position = "bottom")
    
    ggplotly(trend_plot, tooltip = "text")
  })
  
  # Plot: Scatter plot for individual rocket parameters, using 'config_data'
  output$rocket_params_plot <- renderPlotly({
    req(input$x_var_config, input$y_var_config) # Ensure X and Y variables are selected
    
    # Validate that config_data is loaded and has content
    if (!exists("config_data") || is.null(config_data) || nrow(config_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Rocket configuration data is not loaded or is empty.", x = 0.5)))
    }
    # Validate that selected columns exist in config_data
    if (!(input$x_var_config %in% names(config_data)) || !(input$y_var_config %in% names(config_data))) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Selected column(s) not found in rocket configuration data.", x = 0.5)))
    }
    # Validate that selected columns are numeric
    if (!is.numeric(config_data[[input$x_var_config]]) || !is.numeric(config_data[[input$y_var_config]])) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Selected column(s) must be numeric for this plot.", x = 0.5)))
    }
    
    plot_df <- config_data 
    
    p <- ggplot(plot_df, aes(x = .data[[input$x_var_config]], y = .data[[input$y_var_config]])) +
      geom_point(alpha = 0.7, color = "steelblue", size = 2.5) +
      labs(x = input$x_var_config, y = input$y_var_config) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    
    # Add informative tooltips, using 'Rocket_Name' from config_data for identification
    # Ensure 'Rocket_Name' (or a similar identifier) exists in your 'config_data'
    if ("Rocket_Name" %in% names(config_data)) {
      p <- p + aes(text = paste0(
        "Rocket: ", .data[["Rocket_Name"]], "<br>", 
        input$x_var_config, ": ", .data[[input$x_var_config]], "<br>",
        input$y_var_config, ": ", .data[[input$y_var_config]]
      ))
    } else {
      # Fallback tooltip if 'Rocket_Name' is not found
      p <- p + aes(text = paste0(
        input$x_var_config, ": ", .data[[input$x_var_config]], "<br>",
        input$y_var_config, ": ", .data[[input$y_var_config]]
      ))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(text = paste(input$y_var_config, "vs.", input$x_var_config), x = 0.5, xanchor = 'center'))
  })
}

# --- Run the Shiny Application ---
shinyApp(ui, server)

