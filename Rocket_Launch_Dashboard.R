# Authors:
# James Ga-as
# Theo Benedict Pasia
# 2nd Year Students - University of Southeastern Philippines - Obrero Campus

library(tidyverse)
library(tidymodels)
library(xgboost)
library(shiny)
# library(shinythemes) # bslib handles theming now
library(bslib)
library(plotly)
library(here)
library(thematic) # For ggplot theme consistency

# --- Data Loading and Initial Preparation ---
# Ensure your CSV path is correct
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

data <- raw_data |>
  select(
    Launch_Year,
    Rocket_Name,
    Rocket_Organisation,
    Launch_Status
  )

config_data <- raw_data |>
  select(
    Rocket_Name, # Ensure Rocket_Name is here for filtering
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
  ) |> 
  drop_na(Rocket_Name) 

# Ensure your RDS model path is correct
model <- readRDS(here::here("./rocket_launch_model_boosted_tree.rds"))

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
  
  nav_panel(
    title = "Time Series Analysis",
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
      div(
        layout_columns(
          col_widths = c(8, 4),
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
            plotlyOutput(outputId = "Time_series_line", height = "550px")
          ),
          div( 
            style = "text-align: center;", 
            h4("Understanding Launch Activity & Outcomes", style = "margin-bottom: 1rem;"),
            tags$p("This section provides a dual view of rocket launch history: aggregate statistics and yearly trends. The value boxes above offer a quick snapshot of total launch outcomes (Success, Partial Failure, Failure, Prelaunch Failure) based on your current filter selections (year range, specific rockets, or organizations)."),
            tags$p("Below them, the stacked bar chart offers a granular, year-by-year visualization. Each bar represents a year, and its segments show the count of launches by their outcome. This allows for an intuitive comparison of launch volume and success profiles over time."),
            tags$strong("What to look for:"),
            tags$ul(
              style = "display: inline-block; text-align: left; margin-top: 0.5rem;", 
              tags$li(tags$strong("Overall Trends:"), " Are there distinct eras of increased or decreased launch activity? Do failure rates seem to concentrate in certain periods?"),
              tags$li(tags$strong("Proportional Changes:"), " Observe how the proportion of successful launches to failures changes year-over-year. A tall bar with a large green segment is ideal!"),
              tags$li(tags$strong("Impact of Filters:"), " When you filter by specific rockets or organizations, how does their performance profile compare to the overall trends? Does a particular rocket show a learning curve with initially higher failures followed by improvements?"),
              tags$li(tags$strong("Anomalies:"), " Are there specific years with unusually high numbers of a particular outcome (e.g., a spike in failures)?"),
              tags$li(tags$strong("Data Context:"), " Remember that the data reflects reported launches. The definition of 'Partial Failure' can vary, and not all launch activities might be public.")
            ),
            tags$p("Use the sidebar filters to dynamically explore these patterns for different subsets of the data.")
          )
        ),
        
        br(),
        
        layout_columns(
          col_widths = c(4, 8),
          div( 
            style = "text-align: center;", 
            h4("Analyzing Launch Reliability Over Time", style = "margin-bottom: 1rem;"),
            tags$p("This line graph specifically tracks the evolution of launch success rates. We define success on a weighted scale: a full 'Success' contributes 1.0, a 'Partial Failure' contributes 0.5, and other outcomes contribute 0.0 to the yearly average. This provides a nuanced view beyond a simple binary success/failure metric."),
            tags$p("The size of each point on the line corresponds to the total number of launches in that year. Larger points indicate that the success rate for that year is based on a more substantial sample of launches, making it a more statistically robust data point."),
            tags$strong("Key questions this chart helps answer:"),
            tags$ul(
              style = "display: inline-block; text-align: left; margin-top: 0.5rem;", 
              tags$li(tags$strong("Reliability Trajectory:"), " Is the overall trend in launch success rates upwards (indicating improving reliability), downwards, or volatile?"),
              tags$li(tags$strong("Inflection Points:"), " Are there specific years or periods where the success rate changed significantly? What might have driven these changes (e.g., introduction of new technologies, changes in operational procedures, specific high-profile failures impacting subsequent designs)?"),
              tags$li(tags$strong("Volume vs. Rate:"), " How does the success rate correlate with launch volume (point size)? Are periods of high launch activity associated with higher or lower success rates? A high success rate on many launches is a strong indicator of maturity."),
              tags$li(tags$strong("Consistency:"), " How consistent is the success rate from year to year? High variability might suggest instability or ongoing challenges in launch operations for the selected scope."),
              tags$li(tags$strong("Filtered Insights:"), " When filtering by specific rockets or organizations, does their success rate trend differ from the global average? Does it show a faster improvement curve or periods of notable decline?")
            ),
            tags$p("Combining insights from this chart with the launch count chart above can provide a comprehensive understanding of both the quantity and quality of launch activities.")
          ),
          div( 
            h4("Success Rate Trend Over Time", style = "margin-bottom: 1rem;"),
            plotlyOutput(outputId = "success_rate_trend", height = "800px")
          )
        )
      )
    )
  ),
  
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
      card(
        plotlyOutput("rocket_params_plot", height = "550px")
      )
    )
  ),
  
  nav_panel(
    title = "Prediction Model",
    icon = bsicons::bs_icon("cpu-fill")
  ),
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("info-circle")
  ),
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "mode")
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cyborg",
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
    req(nrow(filtered_data()) > 0)
    status_counts_by_year_data <- filtered_data() %>%
      group_by(Launch_Year, Launch_Status) %>%
      summarize(count = n(), .groups = "drop")
    expected_levels <- c("Success", "Partial Failure", "Failure", "Prelaunch Failure")
    status_counts_by_year <- status_counts_by_year_data %>%
      mutate(Launch_Status = factor(Launch_Status, levels = expected_levels))
    custom_bar_fill_colors <- c("Success" = "#77B300", "Partial Failure" = "#ff8800", "Failure" = "#CC0000", "Prelaunch Failure" = "#3d3e3d")
    if(nrow(status_counts_by_year) == 0) {
      return(plotly_empty(type="bar") %>% layout(title = list(text = "No data to display for current filters.", font=list(color=cols$fg))))
    }
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
    req(nrow(filtered_data()) > 0)
    success_trend <- filtered_data() %>%
      mutate(is_success = case_when(Launch_Status == "Success" ~ 1, Launch_Status == "Partial Failure" ~ 0.5, TRUE ~ 0)) %>%
      group_by(Launch_Year) %>%
      summarize(success_rate = mean(is_success, na.rm = TRUE), total_launches = n(), .groups = "drop")
    if(nrow(success_trend) == 0) {
      return(plotly_empty(type="scatter", mode="lines") %>% layout(title = list(text = "No data for success rate trend.", font=list(color=cols$fg))))
    }
    p <- ggplot(success_trend, aes(x = Launch_Year, y = success_rate)) +
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
             legend = list(font = list(color = cols$fg), bgcolor = "rgba(0,0,0,0.1)"))
  })
  
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
    
    # Base plot with x and y aesthetics
    p <- ggplot(plot_df_filtered, aes(x = .data[[input$x_var_config]], y = .data[[input$y_var_config]])) +
      # Apply fixed visual properties to geom_point
      geom_point(color = cols$primary, alpha = 0.6, size = 4) + 
      labs(x = str_replace_all(input$x_var_config, "_", " "),
           y = str_replace_all(input$y_var_config, "_", " ")) +
      theme_minimal() +
      ggplot_theme_transparent +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Prepare tooltip text
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
    # Add text aesthetic separately for tooltip
    p <- p + aes(text = !!rlang::parse_expr(tooltip_text_expr_str)) 
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(
          text = paste(str_replace_all(input$y_var_config, "_", " "), "vs.", str_replace_all(input$x_var_config, "_", " ")),
          x = 0.5, xanchor = 'center',
          font = list(color = cols$fg)
        ),
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

# --- Run the Shiny Application ---
shinyApp(ui, server)

