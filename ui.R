library(tidyverse)

countries <- read_csv("451_data.csv", col_select=c("location")) %>% distinct()

ui <- fluidPage(
  titlePanel("Disease Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country",
                  "Select Country",
                  choices = countries$location,    
                  selected = NULL),
      

      # Year slider with play button
      sliderInput(
        "year", 
        "Year:",
        min = 1980, max = 2023, value = 1980, step = 1, sep = "",
        animate = animationOptions(interval = 800, loop = FALSE)
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "theTabs",
                  tabPanel("Plot", plotOutput("barplot"), value = "plot")
      )
    )
  )
)
