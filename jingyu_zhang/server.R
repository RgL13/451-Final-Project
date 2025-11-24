library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(tibble)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

ihme_raw <- read.csv("/Users/zia/Desktop/UW/Quarter/26AUTU/stat451/project/part3/IHME-DATA.csv", stringsAsFactors = FALSE)

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

server <- function(input, output, session) {
  
  base_data <- reactive({
    df <- base_data_all_causes %>%
      filter(cause_name == input$cause) %>%
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
                      tolower(input$cause), "(Latin America & Caribbean, 2023)")
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
    
    cat("Cause:", input$cause, "\n\n")
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
          "Male–female gap in", tolower(input$cause),
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
        tolower(input$cause),
        " is about ", gap_pp,
        " percentage points higher than the female share."
      )
    } else if (gap_pp < 0) {
      paste0(
        "In ", loc, ", the female share of ",
        tolower(input$cause),
        " is about ", abs(gap_pp),
        " percentage points higher than the male share."
      )
    } else {
      paste0(
        "In ", loc, ", the male and female shares of ",
        tolower(input$cause),
        " are almost identical in 2023."
      )
    }
  })
}
