library(tidyverse)
library(viridis)
library(patchwork)
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

header <- dashboardHeader(title = "Death Rates of Leading Diseases")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",  
    menuItem(
      'Dashboard',
      tabName = 'dashboard',
      icon = icon("dashboard")
    ),
    
    menuItem(
      'Disease Hotspots',
      tabName = 'hotspot',
      icon = icon('fire')
    ),
    
    menuItem(
      'Disease Timeseries',
      tabName = 'timeseries',
      icon = icon('chart-line')
    ),
    
    menuItem(
      'Top 5 diseases',
      tabName = 'top5diseases',
      icon = icon("chart-bar")
    ),
    menuItem(
      "Country gaps",
      tabName = "gaps",
      icon = icon("venus-mars")
    ),
    menuItem(
      "Country profile",
      tabName = "country_ziu",
      icon = icon("flag")
    )
  ), 
  
  conditionalPanel(
    condition = "input.tabs == 'hotspot'", 
    
    selectInput(
      'location_hot',
      'Select Locations',
      choices = c('South Asia', 'Western Europe', 'East Asia', 
                  'High-income North America', 'Sub-Saharan Africa',
                  'Oceania', 'Central Europe, Eastern Europe, and Central Asia',
                  'North Africa and Middle East', 'Latin America and Caribbean'),
      selected = c('South Asia', 'Western Europe', 'East Asia', 'North Africa and Middle East', 'Sub-Saharan Africa'),
      multiple = TRUE
    ),
    
    selectInput(
      'cause_hot',
      'Select Causes',
      choices = c('Lower respiratory infections', 'Ischemic heart disease', 
                  'Self-harm', 'Tuberculosis',
                  'Malaria', 'Stroke','Road injuries', 'HIV/AIDS', 'Tracheal, bronchus, and lung cancer',
                  'Chronic kidney disease', 'Diabetes mellitus', 'Diarrheal diseases', 
                  'Chronic obstructive pulmonary disease', "Alzheimer's disease and other dementias"),
      selected = c('Malaria', 'Stroke', 'HIV/AIDS', 'Ischemic heart disease', 'Diabetes mellitus'),
      multiple = TRUE
    ),
    
    sliderInput(
      'year_hot',
      'Select Year',
      min = 1980, 
      max = 2023,
      value = c(1980, 2023),
      sep = '',
      step = 1
    )
  ),
  
  conditionalPanel(
    condition = "input.tabs == 'timeseries'", 
    
    selectInput(
      'location_time',
      'Select Locations',
      choices = c('South Asia', 'Western Europe', 'East Asia', 
                  'High-income North America', 'Sub-Saharan Africa',
                  'Oceania', 'Central Europe, Eastern Europe, and Central Asia',
                  'North Africa and Middle East', 'Latin America and Caribbean'),
      selected = c('South Asia', 'Western Europe', 'East Asia', 'North Africa and Middle East', 'Sub-Saharan Africa'),
      multiple = TRUE
    ),
    
    selectInput(
      'cause_time',
      'Select Causes',
      choices = c('Lower respiratory infections', 'Ischemic heart disease', 
                  'Self-harm', 'Tuberculosis',
                  'Malaria', 'Stroke','Road injuries', 'HIV/AIDS', 'Tracheal, bronchus, and lung cancer',
                  'Chronic kidney disease', 'Diabetes mellitus', 'Diarrheal diseases', 
                  'Chronic obstructive pulmonary disease', "Alzheimer's disease and other dementias"),
      selected = c('Malaria', 'Stroke', 'HIV/AIDS', 'Ischemic heart disease', 'Diabetes mellitus'),
      multiple = TRUE
    ),
    
    sliderInput(
      'year_time',
      'Select Year',
      min = 1980, 
      max = 2023,
      value = c(1980, 2023),
      sep = '',
      step = 1
    )
  ),
  
  conditionalPanel(
    condition = "input.tabs == 'dashboard'", 
    
    selectInput(
      'location_dash',
      'Select Locations',
      choices = c('South Asia', 'Western Europe', 'East Asia', 
                  'High-income North America', 'Sub-Saharan Africa',
                  'Oceania', 'Central Europe, Eastern Europe, and Central Asia',
                  'North Africa and Middle East', 'Latin America and Caribbean'),
      selected = c('South Asia', 'Western Europe', 'East Asia', 
                   'High-income North America', 'Sub-Saharan Africa',
                   'Oceania', 'Central Europe, Eastern Europe, and Central Asia',
                   'North Africa and Middle East', 'Latin America and Caribbean'),
      multiple = TRUE
    ),
    
    selectInput(
      'cause_dash',
      'Select Causes',
      choices = c('Lower respiratory infections', 'Ischemic heart disease', 
                  'Self-harm', 'Tuberculosis',
                  'Malaria', 'Stroke','Road injuries', 'HIV/AIDS', 'Tracheal, bronchus, and lung cancer',
                  'Chronic kidney disease', 'Diabetes mellitus', 'Diarrheal diseases', 
                  'Chronic obstructive pulmonary disease', "Alzheimer's disease and other dementias"),
      selected = c('Lower respiratory infections', 'Ischemic heart disease', 
                   'Self-harm', 'Tuberculosis',
                   'Malaria', 'Stroke','Road injuries', 'HIV/AIDS', 'Tracheal, bronchus, and lung cancer',
                   'Chronic kidney disease', 'Diabetes mellitus', 'Diarrheal diseases', 
                   'Chronic obstructive pulmonary disease', "Alzheimer's disease and other dementias"),
      multiple = TRUE
    ),
    
    sliderInput(
      'year_dash',
      'Select Year',
      min = 1980, 
      max = 2023,
      value = c(1980, 2023),
      sep = '',
      step = 1
    )
  ),
  
  conditionalPanel(
    condition = "input.tabs == 'top5diseases'", 
    
    selectInput("country",
                "Select Country",
                choices = NULL,    
                selected = NULL),
    
    sliderInput(
      "year_single", 
      "Year:",
      min = 1980, max = 2023, value = 1980, step = 1, sep = "",
      animate = animationOptions(interval = 800, loop = FALSE)
    )
  ),
  
  conditionalPanel(
    condition = "input.tabs == 'overview' || input.tabs == 'gaps' || input.tabs == 'country_ziu'",
    selectInput(
      "cause_ziu",
      "Cardiovascular cause:",
      choices = c(
        "Cardiovascular diseases",
        "Ischemic heart disease",
        "Stroke"
      ),
      selected = "Ischemic heart disease"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'dashboard',
      h2("Global Health Overview"),
      fluidRow(
        valueBoxOutput("total_loc_box", width = 4),
        valueBoxOutput("top_killer_box", width = 4),
        valueBoxOutput("avg_rate_box", width = 4)
      ),
      fluidRow(
        box(
          title = "Causes of Death (Selected Region/Period)",
          status = "primary",
          solidHeader = TRUE,
          width = 8,
          plotOutput("summary_bar")
        ),
        box(
          title = "About this Dashboard",
          status = "warning", 
          width = 4,
          p("This dashboard visualizes death rates across various global regions."),
          br(),
          strong("How to use:"),
          tags$ul(
            tags$li("Use the sidebar to filter by Region, Cause, and Year."),
            tags$li("Check 'Hotspots' for Z-score heatmaps."),
            tags$li("Check 'Timeseries' for trends over decades.")
          )
        )
      )
    ),
    
    tabItem(
      tabName = 'hotspot',
      fluidRow(
        box(
          title = "Relative Death Rate Heatmap (Z-score)",
          width = 12, 
          plotOutput("heatmap_output")
        )
      ),
      fluidRow(
        box(
          title = "Location-Specific Deviation",
          width = 12,
          plotOutput('scatter_plot')
        )
      )
    ),
    
    tabItem(
      tabName = 'timeseries',
      fluidRow(
        box(
          title = "Death Rate Over Time",
          width = 12,
          plotOutput('timeseries')
        )
      )
    ),
    
    tabItem(
      tabName = 'top5diseases',
      fluidRow(
        box(
          width = 12, 
          plotOutput('barplot')
        )
      )
    ),
    
    
    tabItem(
      tabName = "gaps",
      fluidRow(
        box(
          width = 4,
          title = "Controls",
          status = "primary",
          solidHeader = TRUE,
          selectInput(
            "country_subset",
            "Country subset:",
            choices = c("All locations", "Only countries/territories"),
            selected = "Only countries/territories"
          ),
          sliderInput(
            "n_countries",
            "Number of locations to show (sorted by absolute gap):",
            min = 5, max = 30, value = 15, step = 1
          )
        ),
        box(
          width = 8,
          title = "Male–female gap by location (share of total prevalence)",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("dumbbell_plot", height = 500)
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "How to read this plot",
          status = "info",
          solidHeader = TRUE,
          p("Each horizontal line represents one location."),
          tags$ul(
            tags$li("Left dot = female share of total prevalence."),
            tags$li("Right dot = male share of total prevalence."),
            tags$li("Lines leaning to the right indicate a higher share among men."),
            tags$li("Lines leaning to the left indicate a higher share among women.")
          )
        )
      )
    ),
    
    tabItem(
      tabName = "country_ziu",
      fluidRow(
        box(
          width = 8,
          title = "IHD (2023) — Male vs Female cause fraction",
          status = "primary",
          solidHeader = TRUE,
          leafletOutput("lac_map", height = 500)
        ),
        box(
          width = 4,
          title = "Selected location (2023)",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("map_country_bar", height = 220),
          br(),
          tableOutput("map_country_table"),
          br(),
          textOutput("map_country_comment")
        )
      )
    )
  )
)



ui <- dashboardPage(header, sidebar, body)
