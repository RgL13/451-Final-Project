library(shiny)
library(shinydashboard)

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
    
    ### Rui 
    menuItem(
      'Top 5 diseases',
      tabName = 'top5diseases',
      icon = icon("chart-bar")
      
    )
  ), 
  


  conditionalPanel(
    condition = "input.tabs == 'hotspot' || input.tabs == 'timeseries'", 
    
    selectInput(
      'location',
      'Select Locations',
      choices = c('Global','South Asia', 'Western Europe', 'East Asia', 
                  'High-income North America', 'Sub-Saharan Africa',
                  'Oceania', 'Central Europe, Eastern Europe, and Central Asia',
                  'North Africa and Middle East', 'Latin America and Caribbean'),
      selected = c('South Asia', 'Western Europe', 'East Asia', 'North Africa and Middle East', 'Sub-Saharan Africa'),
      multiple = TRUE
    ),
    
    selectInput(
      'cause',
      'Select Causes',
      choices = c('All causes','Lower respiratory infections', 'Ischemic heart disease', 
                  'Self-harm', 'Tuberculosis',
                  'Malaria', 'Stroke','Road injuries', 'HIV/AIDS', 'Tracheal, bronchus, and lung cancer',
                  'Chronic kidney disease', 'Diabetes mellitus', 'Diarrheal diseases', 
                  'Chronic obstructive pulmonary disease', "Alzheimer's disease and other dementias"),
      selected = c('Malaria', 'Stroke', 'HIV/AIDS', 'Ischemic heart disease', 'Diabetes mellitus'),
      multiple = TRUE
    ),
    
    sliderInput(
      'year',
      'Select Year',
      min = 1980, 
      max = 2023,
      value = c(1980, 2023),
      sep = '',
      step = 1
    )
    
  ),
  
  ### Rui
  conditionalPanel(
    condition = "input.tabs == 'top5diseases'", 
    
    selectInput("country",
                "Select Country",
                choices = countries$location,    
                selected = NULL),
    
    
    # Year slider with play button
    sliderInput(
      "year_single", 
      "Year:",
      min = 1980, max = 2023, value = 1980, step = 1, sep = "",
      animate = animationOptions(interval = 800, loop = FALSE)
    )
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'dashboard',
      h2("Dashboard")
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
      
      
    )
    
  )
)

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui = ui, server = server)