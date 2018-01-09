library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(httr)
library(jsonlite)
library(RColorBrewer)


fileName <- "tflApi.txt"
queryString <-readChar(fileName, file.info(fileName)$size)
l = readLines(queryString, encoding="UTF-8", warn=FALSE)

d = fromJSON(l)

accidents <- data.frame(lapply(as.data.frame(d), as.character), stringsAsFactors=FALSE)

#also make sure data is in date format
accidents$date2 <- as.Date(accidents$date, "%Y-%m-%d")
accidents$lon <- as.numeric(accidents$lon)
accidents$lat <- as.numeric(accidents$lat)

ui <- fluidPage(
  titlePanel("Accidents in London in 2015"),
  
  sidebarLayout(
    sidebarPanel( 
      h4("Map to display accidents retreived for 2015 from the TfL API"),
      #date selector goes here 
      dateRangeInput("Date range", inputId = "date_range",
                     start = "2015-01-01",
                     end = "2015-12-31",
                     format = "yyyy-mm-dd"), 
      uiOutput('severitySelector',selected = "Fatal")
    ),
    mainPanel(
      #leaflet output goes here
      leafletOutput("map", height = 800)
      
    )
  )
)

server <- function(input, output) {
  
  severityChoices <- sort(unique(as.character(accidents$severity)))
  
  #create the drop down menu with name country selector to put in placeholder in UI
  output$severitySelector <- renderUI({
    selectInput("severitySelect", label = "Select severity",
                choices = as.list(severityChoices), selected = "Fatal")
  })
  #filter data based on dates
  dateFiltered <- reactive({
    thing <- accidents %>% filter(date2 %in% seq(input$date_range[1],     input$date_range[2], by = "day") & severity %in% input$severitySelect)
    
  })
  #reactive map
  output$map <- renderLeaflet({
    leaflet(accidents) %>%  
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
      addLegend(position = "bottomleft", colors = c("#b10026", "#fd8d3c", "#ffeda0"),
                labels = c("Fatal", "Serious", "Slight"), opacity = 1, title = "Severity")
  })
  
  observe({
    pal <- colorFactor(c("#b10026", "#fd8d3c", "#ffeda0"), domain = c("Fatal", "Serious", "Slight"), ordered = TRUE)
    leafletProxy("map", data = dateFiltered()) %>% clearMarkerClusters() %>%
      addCircleMarkers(~lon, ~lat,
                       color = "#636363", stroke = TRUE, weight = 1,
                       fillColor = ~pal(severity), fillOpacity = 0.8,
                       radius = 5,
                       popup = ~location, 
                       clusterOptions = markerClusterOptions())
  })
  
  
}


shinyApp(ui = ui, server = server)