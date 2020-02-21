library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)
library(tidyverse)
library(viridis)
library(htmltools)
library(leaflet.extras)
library(magrittr)
devtools::session_info()


load("../output/Econ_state_map.RData")
Econ_data_state <- Econ_data_state %>% mutate(Name = str_to_upper(Name))

mapStates = maps::map("state", fill = TRUE, plot = FALSE)
names <- tibble(Name=str_to_upper(mapStates$names)) %>% 
  separate(Name, c('Name', 'sub'), ':') %>% 
  select(Name)

state_map <- function(df, input_metric){
  pal <- colorBin("plasma", domain = df$Value)
  labels <- sprintf(
    "<strong>%s</strong><br/>%s</strong><p/>%.4g",
    df$Name, input_metric, df$Value) %>%
    lapply(htmltools::HTML)

  #State Map
  leaflet(data = mapStates) %>%  
    setView(-96, 37.8, 4.3) %>%
    addTiles() %>%
    addResetMapButton() %>% 
    addPolygons(
      fillColor = pal(df$Value),
      weight = 2,
      opacity = 1,
      color='white',
      dashArray = 3,
      fillOpacity = .7,
      highlight = highlightOptions(weight = 4,
                                   color = "#666",
                                   dashArray = "",
                                   fillOpacity = .75,
                                   bringToFront = T
                                   ), 
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", 'padding' = "10px 15px"),
        textsize = "15px",
        direction = "auto")
      ) %>%
    addLegend(pal = pal,
            values = df$Value,
            opacity = 0.85,
            title = NULL,
            position = "bottomright")
}
