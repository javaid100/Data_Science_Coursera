library(shiny)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)
library(reshape)
library(countrycode)

terrorism <- read.csv("terrorism_short.csv", stringsAsFactors = FALSE)

attacks_by_country <-as.data.frame(sort(table(terrorism$Country),decreasing = TRUE))
names(attacks_by_country) <- c("Country","Count")

unique_countries <- as.data.frame(unique(attacks_by_country$Country))
names(unique_countries) <- "Country"
unique_countries$Country <- as.character(unique_countries$Country)
unique_countries$Country <- sort(unique_countries$Country)

unique_weapons <- as.data.frame(unique(terrorism$WeaponType))
names(unique_weapons) <- "Weapons"
unique_weapons$Weapons <- as.character(unique_weapons$Weapons)
unique_weapons$Weapons <- sort(unique_weapons$Weapons)

unique_attacks <- as.data.frame(unique(terrorism$AttackType))
names(unique_attacks) <- "Attacks"
unique_attacks$Attacks <- as.character(unique_attacks$Attacks)
unique_attacks$Attacks <- sort(unique_attacks$Attacks)

shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    values$unique_countries <- unique_countries$Country
    
    values_w <- reactiveValues()
    values_w$unique_weapons <- unique_weapons$Weapons
    
    output$countryControls <- renderUI({
        selectizeInput('unique_countries', 'Countries:', unique_countries$Country, selected = "United States", multiple = TRUE, options = list(placeholder = 'Please select 1 or more countries'))
    })
    
    output$weaponControls <- renderUI({
        checkboxGroupInput('unique_weapons', 'Weapons:', unique_weapons$Weapons, selected = "Explosives/Bombs/Dynamite")
    })
    
    output$attackControls <- renderUI({
        checkboxGroupInput('unique_attacks', 'Attacks:', unique_attacks$Attacks, selected = "Assassination")
    })
    
    output$countryControls_d <- renderUI({
        selectizeInput('unique_countries_d', 'Countries:', unique_countries$Country, selected = "United States", multiple = TRUE, options = list(placeholder = 'Please select 1 or more countries'))
    })
    
    output$weaponControls_d <- renderUI({
        selectizeInput('unique_weapons_d', 'Weapons:', unique_weapons$Weapons, selected = "Explosives/Bombs/Dynamite", multiple = TRUE, options = list(placeholder = 'Please select 1 or more weapon types'))
    })
    
    output$attackControls_d <- renderUI({
        selectizeInput('unique_attacks_d', 'Attacks:', unique_attacks$Attacks, selected = "Assassination", multiple = TRUE, options = list(placeholder = 'Please select 1 or more attack types'))
    })
    
    
    

    filtered <- reactive({
        attacks_by_country <- filter(terrorism, Year >= input$range[1] & Year <= input$range[2])
        attacks_by_country <-as.data.frame(sort(table(attacks_by_country$Country),decreasing = TRUE))
        names(attacks_by_country) <- c("Country","Count")
        attacks_by_country$Code <- countrycode(attacks_by_country$Country,'country.name','iso3c')
        attacks_by_country[attacks_by_country$Country=='Kosovo','Code'] <- 'KSV'
        attacks_by_country[attacks_by_country$Country=='Yugoslavia','Code'] <- 'YUG'
        attacks_by_country[attacks_by_country$Country=='North Yemen','Code'] <- 'YEM'
        attacks_by_country[attacks_by_country$Country=='South Yemen','Code'] <- 'YEM'
        attacks_by_country[attacks_by_country$Country=='Czechoslovakia','Code'] <- 'CSK'
        attacks_by_country[attacks_by_country$Country=='East Germany (GDR)','Code'] <- 'DEU'
        attacks_by_country[attacks_by_country$Country=='Serbia-Montenegro','Code'] <- 'SCG'
        attacks_by_country
    })
    
    filtered_w <- eventReactive(input$do, {
        weapon_country <- filter(terrorism, Year >= input$range_w[1] & Year <= input$range_w[2] & WeaponType %in% input$unique_weapons)
        temp <- weapon_country %>% group_by(Country) %>% summarize(Total = n())
        weapon_country <- cast(weapon_country, Country ~ WeaponType)
        weapon_country$Total <- temp$Total
        weapon_country$Code <- countrycode(weapon_country$Country,'country.name','iso3c')
        weapon_country[weapon_country$Country=='Kosovo','Code'] <- 'KSV'
        weapon_country[weapon_country$Country=='Yugoslavia','Code'] <- 'YUG'
        weapon_country[weapon_country$Country=='North Yemen','Code'] <- 'YEM'
        weapon_country[weapon_country$Country=='South Yemen','Code'] <- 'YEM'
        weapon_country[weapon_country$Country=='Czechoslovakia','Code'] <- 'CSK'
        weapon_country[weapon_country$Country=='East Germany (GDR)','Code'] <- 'DEU'
        weapon_country[weapon_country$Country=='Serbia-Montenegro','Code'] <- 'SCG'
        weapon_country
    })
    
    
    
    filtered_a <- eventReactive(input$go,{
        attack_country <- filter(terrorism, Year >= input$range_a[1] & Year <= input$range_a[2] & AttackType %in% input$unique_attacks)
        temp <- attack_country %>% group_by(Country) %>% summarize(Total = n())
        attack_country <- cast(attack_country, Country ~ AttackType)
        attack_country$Total <- temp$Total
        attack_country$Code <- countrycode(attack_country$Country,'country.name','iso3c')
        attack_country[attack_country$Country=='Kosovo','Code'] <- 'KSV'
        attack_country[attack_country$Country=='Yugoslavia','Code'] <- 'YUG'
        attack_country[attack_country$Country=='North Yemen','Code'] <- 'YEM'
        attack_country[attack_country$Country=='South Yemen','Code'] <- 'YEM'
        attack_country[attack_country$Country=='Czechoslovakia','Code'] <- 'CSK'
        attack_country[attack_country$Country=='East Germany (GDR)','Code'] <- 'DEU'
        attack_country[attack_country$Country=='Serbia-Montenegro','Code'] <- 'SCG'
        attack_country
    })
    
    filtered_c <- eventReactive(input$display,{
        info_marks <- filter(terrorism, Year >= input$range_c[1] & Year <= input$range_c[2] & Country %in%  input$unique_countries)
        info_marks
    })
    
    filtered_d <- eventReactive(input$serve,{
        filtered_data <- filter(terrorism, Year >= input$range_d[1] & Year <= input$range_d[2] & Country %in%  input$unique_countries_d & WeaponType %in% input$unique_weapons_d & AttackType %in% input$unique_attacks_d)
        filtered_data
    })
    
    output$table <- DT::renderDataTable({
        filtered_d()
    })
    
    output$plotly_map <- renderPlotly({
        l <- list(color = toRGB("grey"), width = 0.5)
        
        g <- list(
            showframe = TRUE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator'),
            lataxis = list(
                range = c(-60, 90),
                showgrid = TRUE,
                tickmode = "linear",
                dtick = 10
            ),
            lonaxis = list(
                range = c(-180, 180),
                showgrid = TRUE,
                tickmode = "linear",
                dtick = 20
            )
        )
        
        p <- plot_geo(filtered()) %>%
            add_trace(
                z = ~Count, color = ~Count, colors = 'Blues',
                text = ~Country, locations = ~Code, marker = list(line = l)
            ) %>%
            colorbar(title = '<br><br><br><br><br><br><br><br><br><br>Number of  <br>Terrorist Attacks') %>%
            layout(
                geo = g
            )
        p
    })
    
        output$leaflet_map <- renderLeaflet({
        leaflet(data = filtered_c()) %>% addTiles() %>%
            addMarkers(~Longitude, ~Latitude, popup = ~as.character(paste0("<span style = 'color:#2874A6;'>Date: </span><span style = 'color:#FF9473;'>",Month,"/",Day,"/",Year,
                                                                           "</span><br><span style = 'color:#2874A6;'>Group: </span><span style = 'color:#FF9473;'>",Group,
                                                                           "</span><br><span style = 'color:#2874A6;'>Type: </span><span style = 'color:#FF9473;'>",AttackType,
                                                                           "</span><br><span style = 'color:#2874A6;'>Target: </span><span style = 'color:#FF9473;'>",TargetType,"</span>")))
    })
        
        output$plotly_weapon_map <- renderPlotly({
            l <- list(color = toRGB("grey"), width = 0.5)
            
            g <- list(
                showframe = TRUE,
                showcoastlines = FALSE,
                projection = list(type = 'Mercator'),
                lataxis = list(
                    range = c(-60, 90),
                    showgrid = TRUE,
                    tickmode = "linear",
                    dtick = 10
                ),
                lonaxis = list(
                    range = c(-180, 180),
                    showgrid = TRUE,
                    tickmode = "linear",
                    dtick = 20
                )
            )
            
            p <- plot_geo(filtered_w()) %>%
                add_trace(
                    z = ~Total, color = ~Total, colors = 'Blues',
                    text = ~Country, locations = ~Code, marker = list(line = l)
                ) %>%
                colorbar(title = '<br><br><br><br><br><br><br><br><br><br>Number of  <br>Casualties') %>%
                layout(
                    geo = g
                )
            p
        })
        
        output$plotly_attack_map <- renderPlotly({
            l <- list(color = toRGB("grey"), width = 0.5)
            
            g <- list(
                showframe = TRUE,
                showcoastlines = FALSE,
                projection = list(type = 'Mercator'),
                lataxis = list(
                    range = c(-60, 90),
                    showgrid = TRUE,
                    tickmode = "linear",
                    dtick = 10
                ),
                lonaxis = list(
                    range = c(-180, 180),
                    showgrid = TRUE,
                    tickmode = "linear",
                    dtick = 20
                )
            )
            
            p <- plot_geo(filtered_a()) %>%
                add_trace(
                    z = ~Total, color = ~Total, colors = 'Blues',
                    text = ~Country, locations = ~Code, marker = list(line = l)
                ) %>%
                colorbar(title = '<br><br><br><br><br><br><br><br><br><br>Number of  <br>Casualties') %>%
                layout(
                    geo = g
                )
            p
        })
})