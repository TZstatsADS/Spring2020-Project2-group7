library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)
library(tidyverse)
library(viridis)
library(htmltools)
library(leaflet.extras)
library(magrittr)

load("../output/Econ_state_map.RData")
Econ_data_state <- Econ_data_state %>% mutate(Name = str_to_upper(Name))
mapStates = maps::map("state", fill = TRUE, plot = FALSE)
names <- tibble(Name=str_to_upper(mapStates$names)) %>% 
  separate(Name, c('Name', 'sub'), ':') %>% 
  select(Name)

state_map <- function(df, values){
  # values <- c(unlist(df$Value))
  # pal <- colorBin("plasma", domain = values)
  # labels <- sprintf(
  #   "<strong>%s</strong><br/>%g",
  #   df$Name,values) %>%
  #   lapply(htmltools::HTML)
  #pal <- colorBin("plasma", domain = values)
  # State Map
  leaflet(data = mapStates) %>%  
    setView(-96, 37.8, 4.3) %>%
    addTiles() %>%
    addResetMapButton() %>% 
    addPolygons(
      #fillColor = pal(values),
      weight = 2,
      opacity = 1,
      color='white',
      dashArray = 3,
      fillOpacity = .7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = .75,
        bringToFront = T
      ))
    #   label = labels,
    #   labelOptions = labelOptions(
    #     style = list("font-weight" = "normal", 'padding' = "10px 15px"),
    #     textsize = "15px",
    #     direction = "auto")
    #   ) %>%
    # addLegend(pal = pal,
    #         values = values,
    #         opacity = 0.85,
    #         title = NULL,
    #         position = "bottomright")
}
