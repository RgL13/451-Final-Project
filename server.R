library(shiny)
library(dplyr)
library(ggplot2)
library(readr)


disease_df <- read.csv("451_data.csv")

server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "country",
                      choices  = sort(unique(disease_df$location)),
                      selected = sort(unique(disease_df$location))[1]
    )
    updateSliderInput(session, "year",
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
    req(input$country, input$year)
    disease_df %>%
      filter(location == input$country,
             year == input$year) %>%
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
        title = paste0("Top 5 diseases in ", input$country, " — ", input$year),
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
