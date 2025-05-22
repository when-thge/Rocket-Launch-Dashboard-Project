# Authors:
# James Ga-as
# Theo Benedict Pasia
# 2nd Year Students - University of Southeastern Philippines - Obrero Campus

library(tidyverse)
library(tidymodels)
library(xgboost)
library(shiny)
library(bslib)
library(plotly)
library(here)
library(thematic) 

# --- Data Loading and Initial Preparation ---
raw_data <- read_csv(here::here("./Aggregated_Launch_Mission_Configs.csv")) |>
  mutate(
    `Launch_Status` = as.factor(`Launch_Status`),
    across(
      c(
        "Fairing_Height", "Fairing_Diameter", "Rocket_Height",
        "Payload_to_LEO", "Payload_to_GTO", "Liftoff_Thrust"
      ),
      ~ as.numeric(str_remove_all(str_remove_all(as.character(.), ","), "[^0-9.]+"))
    ),
    # Ensure other model predictors are numeric
    Stages = as.numeric(Stages),
    Strap_Ons = as.numeric(Strap_Ons),
    Payloads = as.numeric(Payloads),
    Mass = as.numeric(Mass)
  )

data <- raw_data |>
  select(
    Launch_Year, Rocket_Name, Rocket_Organisation, Launch_Status
  )

config_data <- raw_data |>
  select(
    Rocket_Name, Fairing_Height, Fairing_Diameter, Rocket_Height,
    Payload_to_LEO, Payload_to_GTO, Stages, Strap_Ons,
    Liftoff_Thrust, Payloads, Mass
  ) |>
  drop_na(Rocket_Name) |> 
  mutate(across(where(is.character) & !c(Rocket_Name), as.numeric))


rocket_param_choices <- c(
  "Fairing_Height", "Fairing_Diameter", "Rocket_Height",
  "Payload_to_LEO", "Payload_to_GTO", "Stages", "Strap_Ons",
  "Liftoff_Thrust", "Payloads", "Mass"
)

# --- Global Theme and Plot Styling ---
thematic::thematic_shiny(font = "auto")
ggplot_theme_transparent <- theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  panel.grid.major = element_line(linewidth = 0.25),
  panel.grid.minor = element_line(linewidth = 0.1)
)
dark_mode_text_color <- "#adb5bd"
light_mode_text_color <- "#212529"
dark_mode_secondary_text_color <- "#6c757d"
light_mode_secondary_text_color <- "#6c757d"
dark_mode_grid_color <- "rgba(173, 181, 189, 0.2)"
light_mode_grid_color <- "rgba(33, 37, 41, 0.1)"

# --- UI Definition ---
ui <- page_navbar(
  title = "Rocket Launches Dashboard",
  tags$head(
    tags$style(HTML("
      /* Ensure numeric inputs match dark theme */
      .shiny-input-container input[type='number'].form-control {
        background-color: #2A2A2A !important; /* Cyborg input background */
        color: #adb5bd !important;             /* Cyborg input text color */
        border: 1px solid #444 !important;   /* Cyborg input border */
      }
      .card-header .bs-icon { margin-right: 0.3rem; } /* Add some space between icon and text */
    "))
  ),
  
  nav_panel(
    title = "Time Series Analysis",
    icon = bsicons::bs_icon("bar-chart-line-fill"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Filter by...",
        accordion(
          accordion_panel(
            title = "Year",
            icon = bsicons::bs_icon("calendar-range", size = "2rem"),
            sliderInput("year_slider", "Filter by Year",
                        min = min(data$Launch_Year, na.rm = TRUE),
                        max = max(data$Launch_Year, na.rm = TRUE),
                        value = c(min(data$Launch_Year, na.rm = TRUE), max(data$Launch_Year, na.rm = TRUE)),
                        step = 1, tick = FALSE, sep = "")
          ),
          accordion_panel(
            title = "Rocket",
            icon = bsicons::bs_icon("rocket-takeoff", size = "2rem"),
            selectizeInput("select_rocket", "Filter by Rocket:",
                           choices = unique(data$Rocket_Name), multiple = TRUE,
                           options = list(placeholder = "Select rocket(s)..."))
          ),
          accordion_panel(
            title = "Organization",
            icon = bsicons::bs_icon("buildings", size = "2rem"),
            selectizeInput("select_organization", "Filter by Organization:",
                           choices = unique(data$Rocket_Organisation), multiple = TRUE,
                           options = list(placeholder = "Select Organization(s)..."))
          )
        )
      ),
      div( # Main content area for Time Series Analysis
        div(
          h4("Launch Counts by Year & Status Overview", style = "margin-bottom: 1rem;"),
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            value_box(
              title = "Success",
              value = textOutput("success_vbox"),
              showcase = bsicons::bs_icon("check-circle", size = "2.5rem"),
              showcase_layout = "top right",
              theme = "success"
            ),
            value_box(
              title = "Partial Failure",
              value = textOutput("partial_failure_vbox"),
              showcase = bsicons::bs_icon("exclamation-triangle", size = "2.5rem"),
              showcase_layout = "top right",
              theme = "warning"
            ),
            value_box(
              title = "Failure",
              value = textOutput("failure_vbox"),
              showcase = bsicons::bs_icon("x-circle", size = "2.5rem"),
              showcase_layout = "top right",
              theme = "danger"
            ),
            value_box(
              title = "Prelaunch Failure",
              value = textOutput("prelaunch_failure_vbox"),
              showcase = bsicons::bs_icon("dash-circle", size = "2.5rem"),
              showcase_layout = "top right",
              theme = "secondary"
            )
          ),
          plotlyOutput(outputId = "Time_series_line", height = "680px") 
        )
      )
    )
  ),
  nav_panel(
    title = "Success Rate of Rocket Launches",
    icon = bsicons::bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Filter by...",
        accordion(
          accordion_panel(
            title = "Year",
            icon = bsicons::bs_icon("calendar-range", size = "2rem"),
            sliderInput("year_slider", "Filter by Year",
                        min = min(data$Launch_Year, na.rm = TRUE),
                        max = max(data$Launch_Year, na.rm = TRUE),
                        value = c(min(data$Launch_Year, na.rm = TRUE), max(data$Launch_Year, na.rm = TRUE)),
                        step = 1, tick = FALSE, sep = "")
          ),
          accordion_panel(
            title = "Rocket",
            icon = bsicons::bs_icon("rocket-takeoff", size = "2rem"),
            selectizeInput("select_rocket", "Filter by Rocket:",
                           choices = unique(data$Rocket_Name), multiple = TRUE,
                           options = list(placeholder = "Select rocket(s)..."))
          ),
          accordion_panel(
            title = "Organization",
            icon = bsicons::bs_icon("buildings", size = "2rem"),
            selectizeInput("select_organization", "Filter by Organization:",
                           choices = unique(data$Rocket_Organisation), multiple = TRUE,
                           options = list(placeholder = "Select Organization(s)..."))
          )
        )
      ),
      div( # Main content area for Time Series Analysis
        div(
          h4("Success Rate Trend Over Time", style = "margin-bottom: 1rem;"),
          plotlyOutput(outputId = "success_rate_trend", height = "840px") 
        )
      )
    )
  ),
  # --- MODIFIED: Individual Rocket Parameters Tab ---
  nav_panel(
    title = "Individual Rocket Parameters",
    icon = bsicons::bs_icon("rocket-takeoff"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Plot Controls",
        accordion(
          open = "Plot Variables",
          accordion_panel(
            title = "Plot Variables",
            icon = bsicons::bs_icon("sliders"),
            selectInput("x_var_config", "Select X-axis Variable:",
                        choices = rocket_param_choices,
                        selected = rocket_param_choices[4]),
            selectInput("y_var_config", "Select Y-axis Variable:",
                        choices = rocket_param_choices,
                        selected = rocket_param_choices[3])
          ),
          accordion_panel(
            title = "Filter by Rocket",
            icon = bsicons::bs_icon("rocket-takeoff"),
            selectizeInput("select_rocket_param_tab", "Filter by Rocket:",
                           choices = unique(data$Rocket_Name), multiple = TRUE,
                           options = list(placeholder = "Select rocket(s)..."))
          ),
          accordion_panel(
            title = "Filter by Organization",
            icon = bsicons::bs_icon("buildings"),
            selectizeInput("select_organization_param_tab", "Filter by Organization:",
                           choices = unique(data$Rocket_Organisation), multiple = TRUE,
                           options = list(placeholder = "Select Organization(s)..."))
          )
        )
      ),
      # Replaced card with div and added uiOutput for header
      div(
        style = "padding: 1rem;", # Optional: adds some spacing around the content
        uiOutput("rocket_params_plot_header_ui"), 
        plotlyOutput("rocket_params_plot", height = "calc(100vh - 230px)") # Adjusted height for header
      )
    )
  ),
  # --- END MODIFICATION ---
  
  # --- PREDICTION MODEL TAB ---
  nav_panel(
    title = "Prediction Model",
    icon = bsicons::bs_icon("cpu-fill"),
    layout_columns(
      fill = TRUE,
      fillable = TRUE,
      col_widths = c(6, 6), 
      gap = "1rem",
      
      list(
        h5("Rocket Parameters", style = "margin-bottom: 0.5rem; text-align: center;"),
        layout_columns(
          fill = TRUE,
          fillable = TRUE,
          col_widths = c(6, 6), 
          row_heights = rep("1fr", 5),
          gap = "0.5rem",
          
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("arrows-vertical"), "Fairing Height")), 
                card_body( numericInput(inputId = "fheight_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                # MODIFIED ICON for Fairing Diameter
                card_header(tagList(bsicons::bs_icon("circle"), "Fairing Diameter")), 
                card_body( numericInput(inputId = "fdiameter_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("rulers"), "Rocket Height")), 
                card_body( numericInput(inputId = "rheight_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("box-seam"), "Payload to LEO")), 
                card_body( numericInput(inputId = "payloadleo_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("box-arrow-up"), "Payload to GTO")), 
                card_body( numericInput(inputId = "payloadgto_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("stack"), "Stages")), 
                card_body( numericInput(inputId = "stages_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("plus-square-dotted"), "Strap-Ons")), 
                card_body( numericInput(inputId = "strapons_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("fire"), "Liftoff Thrust")), 
                card_body( numericInput(inputId = "lthrust_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("boxes"), "Payloads")), 
                card_body( numericInput(inputId = "payloads_input", value = 0, label = " ", width="100%") ) ),
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("speedometer2"), "Mass")), 
                card_body( numericInput(inputId = "mass_input", value = 0, label = " ", width="100%") ) )
        ) 
      ),
      
      list(
        h5("Launch Status and Price Predictions", style = "margin-bottom: 0.5rem; text-align: center;"),
        layout_columns(
          fill = TRUE, fillable = TRUE, col_widths = 12, 
          row_heights = c("auto", "auto"), gap = "0.5rem",
          card( class = "bg-transparent border-secondary", 
                card_header(tagList(bsicons::bs_icon("clipboard-data"), "Predicted Launch Status")), 
                card_body(tags$p("Prediction results will appear here.", style = "text-align: center; padding: 1rem; color: #6c757d;"))),
          actionButton("predict_button", "Predict Launch Status",
                       icon = icon(name = "circle-play", lib = "font-awesome"), 
                       class = "btn-primary w-100 mt-3")
        )
      )
    )
  ),
  
  nav_panel(title = "About", icon = bsicons::bs_icon("info-circle")),
  nav_spacer(),
  nav_item(input_dark_mode(id = "mode")),
  
  theme = bs_theme(
    version = 5, bootswatch = "cyborg",
    base_font = font_google("Exo 2", local = FALSE),
    heading_font = font_google("Orbitron", local = FALSE),
    "font-size-base" = "0.92rem"
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  plot_colors <- reactive({
    current_mode <- if (is.null(input$mode) || !input$mode %in% c("light", "dark")) "dark" else input$mode
    active_theme_object <- session$getCurrentTheme()
    dark_mode_primary_fallback <- "#4582EC"
    light_mode_primary_fallback <- "#0d6efd"
    primary_color_val <- light_mode_primary_fallback
    if (is.null(active_theme_object)) {
      primary_color_val <- if (current_mode == "dark") dark_mode_primary_fallback else light_mode_primary_fallback
    } else {
      primary_color_candidate <- tryCatch({
        bslib::bs_get_variables(active_theme_object, "primary")
      }, error = function(e) { NULL })
      if (is.null(primary_color_candidate) || (is.character(primary_color_candidate) && (primary_color_candidate == "" || tolower(primary_color_candidate) == "null"))) {
        primary_color_val <- if (current_mode == "dark") dark_mode_primary_fallback else light_mode_primary_fallback
      } else {
        primary_color_val <- primary_color_candidate
      }
    }
    current_grid_color <- if (current_mode == "dark") dark_mode_grid_color else light_mode_grid_color
    current_fg_color <- if (current_mode == "dark") dark_mode_text_color else light_mode_text_color
    current_secondary_fg_color <- if (current_mode == "dark") dark_mode_secondary_text_color else light_mode_secondary_text_color
    list(fg = current_fg_color, secondary_fg = current_secondary_fg_color, grid = current_grid_color, primary = primary_color_val)
  })
  
  filtered_data <- reactive({
    req(input$year_slider)
    current_data <- data |>
      dplyr::filter(
        Launch_Year >= input$year_slider[1] & Launch_Year <= input$year_slider[2]
      )
    if (!is.null(input$select_rocket) && length(input$select_rocket) > 0) {
      current_data <- current_data |> dplyr::filter(Rocket_Name %in% input$select_rocket)
    }
    if (!is.null(input$select_organization) && length(input$select_organization) > 0) {
      current_data <- current_data |> dplyr::filter(Rocket_Organisation %in% input$select_organization)
    }
    current_data
  })
  
  status_counts <- reactive({
    filtered_data() %>%
      group_by(Launch_Status) %>%
      summarize(count = n(), .groups = "drop")
  })
  
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
  
  output$Time_series_line <- renderPlotly({
    cols <- plot_colors()
    status_counts_by_year_data <- filtered_data() %>%
      group_by(Launch_Year, Launch_Status) %>%
      summarize(count = n(), .groups = "drop")
    
    if(nrow(status_counts_by_year_data) == 0) {
      return(plotly_empty(type="bar") %>% layout(title = list(text = "No data to display for current filters.", font=list(color=cols$fg)), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    expected_levels <- c("Success", "Partial Failure", "Failure", "Prelaunch Failure")
    status_counts_by_year <- status_counts_by_year_data %>%
      mutate(Launch_Status = factor(Launch_Status, levels = expected_levels))
    custom_bar_fill_colors <- c("Success" = "#77B300", "Partial Failure" = "#ff8800", "Failure" = "#CC0000", "Prelaunch Failure" = "#3d3e3d")
    
    min_plot_year <- min(status_counts_by_year$Launch_Year, na.rm = TRUE)
    xaxis_tick0 <- floor(min_plot_year / 5) * 5
    
    p <- ggplot(status_counts_by_year, aes(x = Launch_Year, y = count, fill = Launch_Status)) +
      geom_bar(stat = "identity", position = "stack", width = 0.9) +
      scale_fill_manual(values = custom_bar_fill_colors, name = "Launch Status", drop = FALSE) +
      labs(x="Year", y="Number of Launches") +
      theme_minimal() + ggplot_theme_transparent + theme()
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = cols$fg),
             xaxis = list(showgrid = TRUE, dtick = 5, tick0 = xaxis_tick0, gridcolor = cols$grid, linecolor = cols$grid, zerolinecolor = cols$grid, titlefont = list(color = cols$secondary_fg), tickfont = list(color = cols$secondary_fg)),
             yaxis = list(showgrid = TRUE, dtick = 50, gridcolor = cols$grid, linecolor = cols$grid, zerolinecolor = cols$grid, titlefont = list(color = cols$secondary_fg), tickfont = list(color = cols$secondary_fg)),
             showlegend = FALSE)
  })
  
  output$success_rate_trend <- renderPlotly({
    cols <- plot_colors()
    success_trend_data <- filtered_data() %>%
      mutate(is_success = case_when(Launch_Status == "Success" ~ 1, Launch_Status == "Partial Failure" ~ 0.5, TRUE ~ 0)) %>%
      group_by(Launch_Year) %>%
      summarize(success_rate = mean(is_success, na.rm = TRUE), total_launches = n(), .groups = "drop")
    
    if(nrow(success_trend_data) == 0) {
      return(plotly_empty(type="scatter", mode="lines") %>% layout(title = list(text = "No data for success rate trend.", font=list(color=cols$fg)), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    p <- ggplot(success_trend_data, aes(x = Launch_Year, y = success_rate)) +
      geom_line(aes(group=1), color = cols$primary, linewidth = 1) +
      geom_point(aes(size = total_launches, text = paste0("Year: ", Launch_Year, "<br>Success Rate: ", scales::percent(success_rate, accuracy = 0.1), "<br>Total Launches: ", total_launches)), color = cols$primary, alpha = 0.6) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_size_continuous(range = c(3, 9)) +
      labs(x="Year", y="Success Rate", size="Number of Launches") +
      theme_minimal() + ggplot_theme_transparent + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(color = cols$fg),
             xaxis = list(gridcolor = cols$grid, linecolor = cols$grid, zerolinecolor = cols$grid, titlefont = list(color = cols$secondary_fg), tickfont = list(color = cols$secondary_fg)),
             yaxis = list(gridcolor = cols$grid, linecolor = cols$grid, zerolinecolor = cols$grid, titlefont = list(color = cols$secondary_fg), tickfont = list(color = cols$secondary_fg)),
             legend = list(orientation = "h", yanchor = "bottom", y = 1.02, xanchor = "right", x = 1, font = list(color = cols$fg), bgcolor = "rgba(0,0,0,0.1)"))
  })
  
  # --- ADDED: Server logic for the dynamic plot header ---
  output$rocket_params_plot_header_ui <- renderUI({
    req(input$x_var_config, input$y_var_config)
    
    # Format variable names for the title (e.g., "Payload To LEO" from "Payload_to_LEO")
    pretty_y_var <- tools::toTitleCase(tolower(str_replace_all(input$y_var_config, "_", " ")))
    pretty_x_var <- tools::toTitleCase(tolower(str_replace_all(input$x_var_config, "_", " ")))
    
    title_text <- paste(pretty_y_var, "vs.", pretty_x_var)
    h4(title_text, style = "margin-bottom: 1rem; text-align: center;") # Centered title
  })
  # --- END ADDITION ---
  
  output$rocket_params_plot <- renderPlotly({
    cols <- plot_colors()
    req(input$x_var_config, input$y_var_config)
    
    if (!exists("config_data") || is.null(config_data) || nrow(config_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Rocket configuration data is not loaded or is empty.", x = 0.5, font = list(color = cols$fg)), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    plot_df_filtered <- config_data
    
    if (!is.null(input$select_organization_param_tab) && length(input$select_organization_param_tab) > 0) {
      rockets_in_selected_orgs <- data %>%
        filter(Rocket_Organisation %in% input$select_organization_param_tab) %>%
        distinct(Rocket_Name) %>%
        pull(Rocket_Name)
      
      plot_df_filtered <- plot_df_filtered %>%
        filter(Rocket_Name %in% rockets_in_selected_orgs)
    }
    
    if (!is.null(input$select_rocket_param_tab) && length(input$select_rocket_param_tab) > 0) {
      plot_df_filtered <- plot_df_filtered %>%
        filter(Rocket_Name %in% input$select_rocket_param_tab)
    }
    
    if (nrow(plot_df_filtered) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "No data matches the selected filters for rocket parameters.", x = 0.5, font = list(color = cols$fg)),
                      paper_bgcolor = "rgba(0,0,0,0)",
                      plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    if (!(input$x_var_config %in% names(plot_df_filtered)) || !(input$y_var_config %in% names(plot_df_filtered))) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Selected column(s) not found in rocket configuration data.", x = 0.5, font = list(color = cols$fg)), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    if (!is.numeric(plot_df_filtered[[input$x_var_config]]) || !is.numeric(plot_df_filtered[[input$y_var_config]])) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "Selected column(s) must be numeric for this plot.", x = 0.5, font = list(color = cols$fg)), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"))
    }
    
    p <- ggplot(plot_df_filtered, aes(x = .data[[input$x_var_config]], y = .data[[input$y_var_config]])) +
      geom_point(color = cols$primary, alpha = 0.6, size = 4) +
      labs(
        # Labels for axes are good, title is handled by the h4 above and/or plotly layout title
        x = tools::toTitleCase(tolower(str_replace_all(input$x_var_config, "_", " "))), 
        y = tools::toTitleCase(tolower(str_replace_all(input$y_var_config, "_", " ")))
      ) +
      theme_minimal() +
      ggplot_theme_transparent +
      theme(plot.title = element_text(hjust = 0.5)) # ggplot's internal title, can be removed if preferred
    
    tooltip_text_expr_str <- if ("Rocket_Name" %in% names(plot_df_filtered)) {
      sprintf(
        "paste0('Rocket: ', .data[['Rocket_Name']], '<br>', '%s: ', format(.data[['%s']], big.mark=',' , scientific=FALSE), '<br>', '%s: ', format(.data[['%s']], big.mark=',', scientific=FALSE))",
        str_replace_all(input$x_var_config, "_", " "), input$x_var_config,
        str_replace_all(input$y_var_config, "_", " "), input$y_var_config
      )
    } else {
      sprintf(
        "paste0('%s: ', format(.data[['%s']], big.mark=',' , scientific=FALSE), '<br>', '%s: ', format(.data[['%s']], big.mark=',', scientific=FALSE))",
        str_replace_all(input$x_var_config, "_", " "), input$x_var_config,
        str_replace_all(input$y_var_config, "_", " "), input$y_var_config
      )
    }
    p <- p + aes(text = !!rlang::parse_expr(tooltip_text_expr_str))
    
    # Note: The plotly layout title might be redundant with the h4 header.
    # You can remove `title = list(...)` from layout() if you only want the h4 header.
    # Or, set layout(title = list(text = "")) to remove the plotly internal title.
    ggplotly(p, tooltip = "text") %>%
      layout(
        # title = list( # This is the title INSIDE the plotly canvas
        #   text = paste(tools::toTitleCase(tolower(str_replace_all(input$y_var_config, "_", " "))), 
        #                "vs.", 
        #                tools::toTitleCase(tolower(str_replace_all(input$x_var_config, "_", " ")))),
        #   x = 0.5, xanchor = 'center',
        #   font = list(color = cols$fg)
        # ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = cols$fg),
        xaxis = list(
          gridcolor = cols$grid,
          linecolor = cols$grid,
          zerolinecolor = cols$grid,
          titlefont = list(color = cols$secondary_fg),
          tickfont = list(color = cols$secondary_fg)
        ),
        yaxis = list(
          gridcolor = cols$grid,
          linecolor = cols$grid,
          zerolinecolor = cols$grid,
          titlefont = list(color = cols$secondary_fg),
          tickfont = list(color = cols$secondary_fg)
        )
      )
  })
}

shinyApp(ui, server)

