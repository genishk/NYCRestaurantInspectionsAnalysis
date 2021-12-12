#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(lubridate)

df <- readRDS(file='post_missing.Rda') %>%
    filter(!is.na(dba)) %>%
    filter(!is.na(zip)) %>%
    filter(!is.na(vio_code)) %>%
    filter(!is.na(vio_desc)) %>%
    filter(!is.na(score)) %>%
    filter(!is.na(latitude)) %>%
    filter(!is.na(longitude)) %>%
    filter(!flag=='Not Applicable') %>%
    filter(longitude < -74.0060 + 1) %>%
    filter(longitude > -74.0060 - 1) %>%
    filter(latitude < 40.7128 + 0.8) %>%
    filter(latitude > 40.7128 - 0.8)

df$insp_year <- year(df$insp_date) 
df <- df[order(df$insp_date, decreasing=TRUE),]

cuisine_list <- as.character(df$cuisine)
cuisine_list <- unique(cuisine_list)
cuisine_list <- sort(cuisine_list)
cuisine_list <- c("All", cuisine_list)


# Define UI for application
ui <- shinyUI(bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, left = 50,
                  selectInput(inputId = "insp_year_id","Inspection Period:",
                              c('2021', '2020-2021', '2019-2021', '2018-2021', '2017-2021', '2017-2021', '2016-2021')
                  ),
                  selectInput(inputId = "boro_id", "Borough:",
                              c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
                  ),
                  selectInput(inputId = "cuisine_id", "Cuisine:",
                              cuisine_list
                  ),
                  sliderInput("score_bar", "Current Score:", 
                              0, 160,
                              value = range(0:160), step = 10
                  )
    )
)
)


# Define server logic
server <- function(input, output, session) {
    
    filteredData <- reactive({
        temp <- df
        if(input$insp_year_id !='2021'){
            yr_id <- as.double(unlist(strsplit(input$insp_year_id, split = "-"))[1]) 
            temp <- temp[temp$insp_year >= yr_id, ]
        }
        if(input$insp_year_id =='2021'){
            temp <- temp[temp$insp_year == 2021, ]
        }
        if (input$boro_id !='All') {
            temp <- temp[temp$boro == input$boro_id, ]
        }
        if (input$cuisine_id !='All') {
            temp <- temp[temp$cuisine == input$cuisine_id, ]
        }
        
        temp <- temp %>%
            group_by(camis, dba, boro, zip, latitude, longitude, cuisine, street) %>% 
            summarize(total_inspections=n(),
                      last_inspection=first(insp_date),
                      current_score=first(score),
                      mean_score = mean(score), 
                      num_critical = sum(flag=='Critical'),
                      violated_code = names(which.max(table(vio_code))),
                      violated_desc = names(which.max(table(vio_desc)))) %>%
            ungroup()
        
        temp <- temp[temp$current_score >= input$score_bar[1] & temp$current_score <= input$score_bar[2],]
    })
    
    color = rev(brewer.pal(11, "RdYlBu"))
    
    pal <- colorNumeric(
        palette = color,
        domain = df$score
    )
    
    output$map <- renderLeaflet({
        leaflet(df) %>% setView(lat = 40.7128, lng = -74.0060, zoom = 11) %>%
            addTiles() %>% 
            addCircleMarkers(data = filteredData(),  
                             lng = ~longitude, 
                             lat = ~latitude, 
                             radius = 4, 
                             color = ~pal(current_score),
                             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                             popup = ~paste("<strong>Name:</strong>", dba, "(ID: ", camis,")",
                                            "<br /><strong>Address:</strong>", street," ", zip, " ", boro, "<br />",
                                            "<br /><strong>Total Inspections:</strong>", total_inspections,
                                            "<br /><strong>Last Inspection:</strong>", format(last_inspection, "%b %Y"),
                                            "<br /><strong>Current Score:</strong>", current_score,
                                            "<br /><strong>Critical Flags:</strong>", num_critical,
                                            "<br /><strong>Violated:</strong>",violated_code, 
                                            ": ", violated_desc))
        
    })
}


# Run the application 
shinyApp(ui, server)




