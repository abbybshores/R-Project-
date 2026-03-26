#Final Exam 

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(reactable)
library(leaflet)
library(RColorBrewer)
library(classInt)
library(gridExtra)


OhioEJI <- 
  readr::read_csv("OhioEJI.csv")

OhioEJI <- mutate(rename(OhioEJI, censustracts = GEOID))

Tracts <- st_read("ohio_census_tracts.geojson")

Tracts <- Tracts %>% select(GEOID) %>% mutate(GEOID = as.numeric(GEOID)) 

merged_data <- merge(Tracts, OhioEJI, by.x = "GEOID", by.y = "censustracts")



pal_water<-
  leaflet::colorNumeric(
    palette = "YlGn",
    domain = merged_data$RPL_EBM_DOM5
  )

pal_minority<-
  leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = merged_data$EPL_MINRTY
  )

leaflet_map <- leaflet::leaflet(
  data = merged_data
) %>%
  leaflet::addTiles() %>% 
  
#impaired water
  
  leaflet::addPolygons(
    group = "RPL_EBM_DOM5",
    stroke = TRUE,
    color = ~pal_water(RPL_EBM_DOM5),
    weight = 1,
    opacity = 0.5,
    dashArray = "3",
    fillOpacity = 0.7,
    
    label = ~ paste0(
      "<b>", COUNTY, "</b>", "</br>",
      "<b>RPL_EBM_DOM5: </b>", "</br>",
      RPL_EBM_DOM5, "%", "</br>"
    ) %>% lapply(htmltools::HTML)
  )  %>%

#Minority percentile
  
  leaflet::addPolygons(
    group = "EPL_MINRTY",
    stroke = TRUE,
    color = ~pal_minority(EPL_MINRTY),
    weight = 1,
    opacity = 0.5,
    dashArray = "3",
    fillOpacity = 0.7,
    
    label = ~ paste0(
      "<b>", COUNTY, "</b>", "</br>",
      "<b>EPL_MINRTY: </b>", "</br>",
     EPL_MINRTY, "%", "</br>"
    ) %>% lapply(htmltools::HTML)
  )  %>%
  
#labels 
  
  leaflet::addLegend(
    "bottomright", 
    pal = pal_water,  
    values = ~RPL_EBM_DOM5,
    title = "Percentage of Domain Consisting
of Impaired Water
Bodies", 
    opacity = 1
  ) %>%
  
  leaflet::addLegend(
    "bottomright", 
    pal = pal_minority,  
    values = ~EPL_MINRTY, 
    title = "Percentage of Minority Persons", 
    opacity = 1
  ) %>%

  #layers control
  
  leaflet::addLayersControl(
    baseGroups = c(
      "RPL_EBM_DOM5",
      "EPL_MINRTY"
    ),
    position = "topright",
    options = layersControlOptions(collapsed = FALSE)
  ) 
  
ui <- function() {
  shiny::fluidPage(
    shiny::column(
      width = 10,
      shiny::fluidRow(
        leaflet::leafletOutput(outputId = "map", height = 980)
      ),
      fluidPage(
        fluidRow(
          selectInput(inputId = "input1",
                      label = "Input 1 (X axis):",
                      choices = c("EPL_MINRTY"),
          ),
          selectInput(inputId = "input2",
                      label = "Input 2 (Y axis):",
                      choices = c("RPL_EBM_DOM5"),
          )
          
        )
      ),
      column(width = 10,
             plotOutput(outputId = "scatter", height = 500)))
  )
  
}

server <- function(input, output, session) {
  
  output$map <- leaflet::renderLeaflet({
    
    leaflet_map
    
  })
  
  output$scatter <- renderPlot(
    {
      ggplot(data = merged_data,
             mapping = aes(
               x = get(input$input1), y = get(input$input2,))) +
        geom_point() +
        
        xlab(input$input1) +
        ylab(input$input2)
    }
  )
  
}

shinyApp(ui = ui, server = server)

  