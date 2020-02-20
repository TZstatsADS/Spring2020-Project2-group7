library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(leaflet)
library(maps)
library(tidyverse)
library(RColorBrewer)
library(htmltools)
library(leaflet.extras)
library(magrittr)

load("../output/Econ_county_map.RData")
zoom = rep(7, length(state.abb))
zoom[c(5,12,23,28,43)] = 6
zoom[c(20,21,29,30,38,40,45,48)] = 8
zoom[c(7,8)] = 9
zoom[c(39)] = 10

county_map <- function(df){
  mapcounty <- maps::map("county", regions = state.name[which(state.abb == df$State[1])], fill = TRUE, plot = FALSE)
  names <- tibble(Name = mapcounty$names) %>% 
    separate(Name, c('state', 'county'), ',') %>% 
    mutate(state = paste0(toupper(substr(state,1,1)), substr(state, 2, nchar(state)))) %>% 
    mutate(county = paste0(toupper(substr(county,1,1)), substr(county, 2, nchar(county)), ifelse(state == 'Louisiana', ' Parish', ' County')))
  

  
  df <- df %>% 
    right_join(names, by = c('Name' = 'county'))
  values <- c(unlist(df[,4]))
  metric <- colnames(df)[4]
  year <- df[1,3]
  state <- state.name[which(state.abb == df$State[1])]
  pal <- colorBin("YlOrRd", domain = values, bins = 9)
  labels <- sprintf(
    "<strong>%s, %s </strong><br/><strong>%s in %s</strong><br/>%g",    
    state, df$Name, metric, year, values) %>% 
    lapply(htmltools::HTML)
  
  range <- mapcounty$range
  
  # County Map
  leaflet(data = mapcounty) %>% 
    setView(mean(range[1:2]), mean(range[3:4]), zoom[which(state.abb == df$State[1])]) %>% 
    addTiles() %>%
    addResetMapButton() %>% 
    addPolygons(
      fillColor = pal(values),
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
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", 'padding' = "10px 15px"),
        textsize = "15px",
        direction = "auto")
      ) %>%  
    addLegend(pal = pal,
            values = values,
            opacity = 0.85,
            title = NULL,
            position = "bottomright")
}
