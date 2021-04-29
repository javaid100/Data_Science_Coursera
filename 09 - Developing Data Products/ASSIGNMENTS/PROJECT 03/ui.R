library(shiny)
library(plotly)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Global Terrorism",
    tabPanel(p(icon("fas fa-globe","fa-2x"), " Attacks"),
      sidebarPanel(
          h2(icon("fas fa-globe","fa-2x"), " Attacks", align = 'center'),
          tags$hr(),
          h5("Displays the number of ",tags$em('terrorist attacks'), "per country."),
          tags$ul(tags$li("Use the slider to filter by date.")),
          tags$hr(),
        sliderInput("range",
                    "Years:",
                    min = 1970,
                    max=2016,
                    value = c(2000,2016),
                    sep = "")
      ),
      mainPanel(
          h2("Number of Terrorist Attacks"),
          tags$hr(),
          plotlyOutput("plotly_map")
      )
    ),
    tabPanel(p(icon("fas fa-globe","fa-2x"), " Weapon Type"),
      sidebarPanel(
          h2(icon("fas fa-globe","fa-2x"), " Weapon Type", align = 'center'),
          tags$hr(),
          h5("Displays the number of ",tags$em('casualties'), "per country."),
          tags$ul(tags$li("Use the slider to filter by date, and use the check boxes to filter by weapon type."),
          tags$li("Click ", tags$em("Calculate"), "to display the map.")),
          tags$hr(),
        sliderInput("range_w",
                    "Years:",
                    min = 1970,
                    max=2016,
                    value = c(2000,2016)),
        uiOutput("weaponControls"),
        actionButton("do","Calculate", class = "btn-primary")
        ),
        mainPanel(
            h2("Number of Casualties"),
            tags$hr(),
            plotlyOutput("plotly_weapon_map")
        )
    ),
    tabPanel(p(icon("fas fa-globe","fa-2x"), " Attack Type"),
      sidebarPanel(
          h2(icon("fas fa-globe","fa-2x"), " Attack Type", align = 'center'),
          tags$hr(),
          h5("Displays the number of ",tags$em('casualties'), "per country."),
          tags$ul(tags$li("Use the slider to filter by date, and use the check boxes to filter by attack type."),
                  tags$li("Click ", tags$em("Calculate"), "to display the map.")),
          tags$hr(),
        sliderInput("range_a",
                    "Years:",
                    min = 1970,
                    max=2016,
                    value = c(2000,2016)),
        uiOutput("attackControls"),
        actionButton("go","Calculate", class = "btn-primary")
        ),
        mainPanel(
            h2("Number of Casualties"),
            tags$hr(),
            plotlyOutput("plotly_attack_map")
        )
    ),
    tabPanel(p(icon("map-marker","fa-2x"), " Marker Map"),
      sidebarPanel(
          h3(icon("map-marker","fa-2x"), " Marker Map", align = 'center'),
          tags$hr(),
          h5("Displays the geo-coordinates of ",tags$em('terrorist attacks'), "based on the the selected country."),
          tags$ul(tags$li("Use the slider to filter by date, and use the drop down box to filter by country."),
          tags$li("Click ", tags$em("Calculate"), "to display the map.")),
          tags$hr(),
        sliderInput("range_c",
                    "Years:",
                    min = 1970,
                    max=2016,
                    value = c(2000,2016)),
        uiOutput("countryControls"),
        actionButton("display","Calculate", class = "btn-primary")
        ),
        mainPanel(
            h2("Geo-Coordinates of Terrorist Attacks"),
            tags$hr(),
            leafletOutput("leaflet_map")
        )
    ),
    tabPanel(p(icon("fas fa-database","fa-2x"), " Data Table"),
      sidebarPanel(
          h3(icon("fas fa-database","fa-2x"), " Data Table", align = 'center'),
          tags$hr(),
          h5("Displays a data table filterd by the selected options."),
          tags$ul(tags$li("Use the slider to filter by date, and use the drop down boxes to filter each classifier."),
          tags$li("Click ", tags$em("Calculate"), "to display the data table.")),
          tags$hr(),
        sliderInput("range_d",
                    "Years:",
                    min = 1970,
                    max=2016,
                    value = c(1980,2000)),
        uiOutput("countryControls_d"),
        uiOutput("weaponControls_d"),
        uiOutput("attackControls_d"),
        actionButton("serve","Calculate", class = "btn-primary")
        ),
        mainPanel(h2("Data Table of Attacks"),
                  tags$hr(),
            dataTableOutput(outputId="table")
        )
    )
  )
)