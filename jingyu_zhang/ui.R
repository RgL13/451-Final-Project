library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Gender Gaps in CVD Burden (LAC, 2023)"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",       tabName = "overview", icon = icon("info-circle")),
      menuItem("Country gaps",   tabName = "gaps",     icon = icon("venus-mars")),
      menuItem("Country profile",tabName = "country",  icon = icon("flag"))
    ),
    hr(),
    selectInput(
      "cause",
      "Cardiovascular cause:",
      choices = c(
        "Cardiovascular diseases",
        "Ischemic heart disease",
        "Stroke"
      ),
      selected = "Ischemic heart disease"
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Project question",
            status = "primary",
            solidHeader = TRUE,
            p("Question: Do men consistently bear a higher share of cardiovascular disease (CVD) burden than women in Latin America and the Caribbean?"),
            p("This dashboard uses IHME Global Burden of Disease (GBD) 2023 estimates to compare the share of total disease prevalence attributable to selected cardiovascular causes for males and females (all ages, age-standardised).")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Distribution of male–female gaps (percentage points)",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("gap_hist")
          ),
          box(
            width = 6,
            title = "Summary numbers",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("gap_summary")
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
        tabName = "country",
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
)
