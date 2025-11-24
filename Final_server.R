server <- function(input, output, session){
  df1 <- read.csv('new_data.csv')
  ihme_raw <- read.csv("IHME-DATA.csv", stringsAsFactors = FALSE)
  
  ihme <- ihme_raw %>%
    filter(
      measure_name == "Prevalence",
      metric_name  == "Percent",
      age_name     == "All ages"
    )
  
  region_names <- c(
    "Latin America and Caribbean",
    "Caribbean",
    "Andean Latin America",
    "Central Latin America",
    "Tropical Latin America"
  )
  
  base_data_all_causes <- ihme %>%
    select(location_name, cause_name, sex_name, val)
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  lac_shapes_base <- world %>%
    mutate(location_name = name) %>%
    filter(location_name %in% unique(base_data_all_causes$location_name)) %>%
    select(location_name, geometry)
  
  Data_Dash <- reactive({
    req(input$location_dash, input$cause_dash, input$year_dash)
    df1 %>% filter(year >= input$year_dash[1], year <= input$year_dash[2], 
                   location %in% input$location_dash, cause %in% input$cause_dash)
  })
  
  Data_Hot <- reactive({
    req(input$location_hot, input$cause_hot, input$year_hot)
    df1 %>% filter(year >= input$year_hot[1], year <= input$year_hot[2], 
                   location %in% input$location_hot, cause %in% input$cause_hot)
  })
  
  Data_Time <- reactive({
    req(input$location_time, input$cause_time, input$year_time)
    df1 %>% filter(year >= input$year_time[1], year <= input$year_time[2], 
                   location %in% input$location_time, cause %in% input$cause_time)
  })
  
  summary_stats <- reactive({
    req(nrow(Data_Dash()) > 0)
    Data_Dash() %>%
      group_by(cause) %>%
      summarize(mean_val = mean(val, na.rm = TRUE)) %>%
      arrange(desc(mean_val))
  })
  
  output$total_loc_box <- renderValueBox({
    num_locs <- length(unique(Data_Dash()$location))
    valueBox(
      num_locs, 
      "Regions Selected", 
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  output$top_killer_box <- renderValueBox({
    req(nrow(summary_stats()) > 0)
    top_cause <- summary_stats()$cause[1]
    valueBox(
      top_cause, 
      "Leading Cause of Death", 
      icon = icon("skull"), 
      color = "red"
    )
  })
  
  output$avg_rate_box <- renderValueBox({
    req(nrow(Data_Dash()) > 0)
    avg_rate <- round(mean(Data_Dash()$val, na.rm = TRUE), 2)
    valueBox(
      avg_rate, 
      "Avg Death Rate (per 100k)", 
      icon = icon("heart-pulse"),
      color = "purple"
    )
  })
  
  output$summary_bar <- renderPlot({
    req(nrow(summary_stats()) > 0)
    top_5 <- summary_stats()
    
    ggplot(top_5, aes(x = reorder(cause, mean_val), y = mean_val)) +
      geom_col(fill = "#2C3E50") + 
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Average Death Rate") +
      theme(text = element_text(size = 12),
            panel.grid.major.y = element_blank())
  })
  
  df2 <- reactive({
    df_temp <- Data_Hot() %>% 
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
    ggplot(Data_Time(), aes(x = year, y = val, group = location, color = location)) +
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
  
  disease_df <- read.csv("451_data.csv")
  
  observe({
    updateSelectInput(session, "country",
                      choices  = sort(unique(disease_df$location)),
                      selected = sort(unique(disease_df$location))[1]
    )
    updateSliderInput(session, "year_single",
                      min    = min(disease_df$year, na.rm = TRUE),
                      max    = max(disease_df$year, na.rm = TRUE),
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
  
  base_data <- reactive({
    df <- base_data_all_causes %>%
      filter(cause_name == input$cause_ziu) %>%
      pivot_wider(
        names_from  = sex_name,
        values_from = val
      ) %>%
      filter(!is.na(Male), !is.na(Female)) %>%
      mutate(
        gap     = Male - Female,
        abs_gap = abs(gap),
        location_type = if_else(
          location_name %in% region_names,
          "Region",
          "Country / territory"
        )
      )
    df
  })
  
  map_data <- reactive({
    df <- base_data()
    if (nrow(df) == 0) return(NULL)
    
    df_map <- df %>%
      select(location_name, Male, Female, gap)
    
    shp <- lac_shapes_base %>%
      left_join(df_map, by = "location_name") %>%
      mutate(
        diff_cat = dplyr::case_when(
          is.na(gap) ~ "No data",
          gap > 0 ~ "Male higher",
          gap < 0 ~ "Female higher",
          TRUE ~ "No difference"
        )
      )
    shp
  })
  
  selected_country <- reactiveVal(NULL)
  
  observe({
    shp <- map_data()
    if (is.null(shp)) return(NULL)
    if (is.null(selected_country())) {
      locs <- shp$location_name[!is.na(shp$gap)]
      if (length(locs) > 0) selected_country(locs[1])
    }
  })
  
  output$gap_hist <- renderPlot({
    df <- base_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = gap * 100)) +
      geom_histogram(binwidth = 0.5, color = "white", fill = "#3182bd") +
      labs(
        x = "Male - Female (percentage points)",
        y = "Number of locations",
        title = paste("Distribution of male–female gaps in",
                      tolower(input$cause_ziu), "(Latin America & Caribbean, 2023)")
      ) +
      theme_minimal()
  })
  
  output$gap_summary <- renderPrint({
    df <- base_data()
    if (nrow(df) == 0) {
      cat("No data available for this cause.")
      return()
    }
    
    n_all  <- nrow(df)
    n_male <- sum(df$gap > 0)
    n_fem  <- sum(df$gap < 0)
    n_equal <- sum(abs(df$gap) < 1e-9)
    mean_gap <- mean(df$gap) * 100
    
    cat("Cause:", input$cause_ziu, "\n\n")
    cat("Total locations:", n_all, "\n")
    cat("Locations where male share > female share:", n_male, "\n")
    cat("Locations where female share > male share:", n_fem, "\n")
    cat("Locations with almost no difference:", n_equal, "\n\n")
    cat("Average male–female gap (percentage points):",
        round(mean_gap, 2), "\n")
  })
  
  output$dumbbell_plot <- renderPlot({
    df <- base_data()
    if (nrow(df) == 0) return(NULL)
    
    if (input$country_subset == "Only countries/territories") {
      df <- df %>% filter(location_type == "Country / territory")
    }
    
    df_top <- df %>%
      arrange(desc(abs_gap)) %>%
      head(input$n_countries)
    
    if (nrow(df_top) == 0) return(NULL)
    
    ggplot(
      df_top,
      aes(y = fct_reorder(location_name, gap))
    ) +
      geom_segment(
        aes(x = Female * 100, xend = Male * 100,
            yend = location_name),
        colour = "grey80",
        linewidth = 1
      ) +
      geom_point(
        aes(x = Female * 100, colour = "Female"),
        size = 3
      ) +
      geom_point(
        aes(x = Male * 100, colour = "Male"),
        size = 3
      ) +
      scale_color_manual(
        name   = "Sex",
        values = c("Female" = "#e41a1c", "Male" = "#377eb8")
      ) +
      labs(
        x = "Share of total prevalence (%)",
        y = NULL,
        title = paste(
          "Male–female gap in", tolower(input$cause_ziu),
          "share of total prevalence (top locations)"
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 9)
      )
  })
  
  output$lac_map <- renderLeaflet({
    shp <- map_data()
    if (is.null(shp)) return(NULL)
    
    pal <- colorFactor(
      palette = c("Female higher" = "#F781BF",
                  "Male higher" = "#377EB8",
                  "No difference" = "grey80",
                  "No data" = "grey95"),
      domain = shp$diff_cat
    )
    
    leaflet(shp) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(diff_cat),
        weight = 1,
        color = "white",
        fillOpacity = 0.85,
        layerId = ~location_name,
        label = ~location_name
      ) %>%
      setView(lng = -70, lat = 0, zoom = 3)
  })
  
  observeEvent(input$lac_map_shape_click, {
    click <- input$lac_map_shape_click
    if (!is.null(click$id)) {
      selected_country(click$id)
    }
  })
  
  output$map_country_bar <- renderPlot({
    df <- base_data()
    loc <- selected_country()
    if (is.null(loc) || nrow(df) == 0) return(NULL)
    
    row <- df %>% filter(location_name == loc)
    if (nrow(row) == 0) return(NULL)
    
    d <- tibble(
      Sex = c("Female", "Male"),
      share = c(row$Female * 100, row$Male * 100)
    )
    
    ggplot(d, aes(x = Sex, y = share, fill = Sex)) +
      geom_col() +
      scale_fill_manual(values = c("Female" = "#F781BF", "Male" = "#377EB8")) +
      labs(x = NULL, y = "Share of total prevalence (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$map_country_table <- renderTable({
    df <- base_data()
    loc <- selected_country()
    if (is.null(loc) || nrow(df) == 0) return(NULL)
    
    row <- df %>% filter(location_name == loc)
    if (nrow(row) == 0) return(NULL)
    
    tibble(
      Sex = c("Female", "Male", "Male - Female"),
      `Share of total prevalence (%)` = c(
        row$Female * 100,
        row$Male * 100,
        row$gap  * 100
      ) %>% round(2)
    )
  })
  
  output$map_country_comment <- renderText({
    df <- base_data()
    loc <- selected_country()
    if (is.null(loc) || nrow(df) == 0) return("")
    
    row <- df %>% filter(location_name == loc)
    if (nrow(row) == 0) return("")
    
    gap_pp <- round(row$gap * 100, 2)
    if (is.na(gap_pp)) return("No data for this location.")
    
    if (gap_pp > 0) {
      paste0(
        "In ", loc, ", the male share of ",
        tolower(input$cause_ziu),
        " is about ", gap_pp,
        " percentage points higher than the female share."
      )
    } else if (gap_pp < 0) {
      paste0(
        "In ", loc, ", the female share of ",
        tolower(input$cause_ziu),
        " is about ", abs(gap_pp),
        " percentage points higher than the male share."
      )
    } else {
      paste0(
        "In ", loc, ", the male and female shares of ",
        tolower(input$cause_ziu),
        " are almost identical in 2023."
      )
    }
  })
}
