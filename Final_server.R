library(tidyverse)
library(viridis)
library(patchwork)

server <- function(input, output, session){
  ### Blake Server
  df1 <- read.csv('new_data.csv')
  
  Data1 <- reactive({
    df1 %>% filter(year >= input$year[1], year <= input$year[2], 
                   location %in% input$location, cause %in% input$cause)
  })
  
  
  df2 <- reactive({
    df_temp <- Data1() %>% 
      group_by(location, cause) %>% 
      summarize(rate = mean(val), .groups = 'drop')
    
    rate_stats <- df_temp %>% 
      group_by(cause) %>% 
      summarise(mean_rate = mean(rate), sd_rate = sd(rate), .groups = 'drop')
    
    df_temp %>% 
      left_join(rate_stats, by = 'cause') %>%
      mutate(z_score = (rate - mean_rate) / sd_rate) %>%
      group_by(cause) %>%
      mutate(mean_cause_rate = mean(rate)) %>%
      ungroup() %>%
      mutate(cause = reorder(cause, mean_cause_rate))
  })
  
  output$heatmap_output <- renderPlot({
    ggplot(df2(), aes(x = location, y = cause, fill = z_score)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_viridis(option = "inferno", direction = -1, name = "Z-score") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(size = 12),
            panel.grid = element_blank(),
            legend.position = "right",
            legend.title = element_text(size = 9)) +
      labs(title = "Relative Death Rate by Cause and Location", x = "", y = "")
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(df2(), aes(x = z_score, y = cause, color = location)) +
      geom_point(size = 3, alpha = 0.8) +             
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +  
      scale_color_viridis_d(option = "plasma", end = 0.9) + 
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(size = 10),
            legend.position = "right", 
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9)) +
      labs(x = "Z-score", y = "", color = "Location")
  })
  
  output$timeseries <- renderPlot({
    ggplot(Data1(), aes(x = year, y = val, group = location, color = location)) +
      geom_line(linewidth = 0.8, alpha = 0.9) +  
      facet_wrap(~cause, scales = "free_y") +
      scale_color_viridis_d(option = "plasma", end = 0.9) +
      expand_limits(y = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45),
            axis.title.x = element_blank(),
            panel.grid.major = element_blank()) + 
      labs(title = 'Death rate of diseases by year and country', y = 'Death rate')
  })
  
  ### Rui Server
  disease_df <- read.csv("451_data.csv")
  
  observe({
    updateSelectInput(session, "country",
                      choices  = sort(unique(disease_df$location)),
                      selected = sort(unique(disease_df$location))[1]
    )
    updateSliderInput(session, "year_single",
                      min   = min(disease_df$year, na.rm = TRUE),
                      max   = max(disease_df$year, na.rm = TRUE),
                      value = min(disease_df$year, na.rm = TRUE),
                      step  = 1
    )
  })
  
  country_max <- reactive({
    req(input$country)
    disease_df %>%
      filter(location == input$country) %>%
      summarise(m = max(val, na.rm = TRUE)) %>%
      pull(m) %>% { if (is.finite(.)) . else 0 }
  })
  
  bars_df <- reactive({
    req(input$country, input$year_single)
    disease_df %>%
      filter(location == input$country,
             year == input$year_single) %>%
      slice_max(order_by = val, n = 5, with_ties = FALSE) %>%
      arrange(desc(val))
  })
  
  output$barplot <- renderPlot({
    df <- bars_df()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(cause, val), y = val)) +
      geom_col(width = 0.65, fill = "orange") +
      geom_text(aes(label = round(val, 2)), hjust = -0.1, size = 4) +
      coord_flip() +
      labs(
        title = paste0("Top 5 diseases in ", input$country, " — ", input$year_single),
        x = NULL, y = "The Age-Standardized Death Rate (per 100,000 population)",
      ) +
      scale_y_continuous(limits = c(0, country_max() * 1.05)) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", margin = margin(b = 10)),
        panel.grid.minor = element_blank()
      )
  })
  
  
}







