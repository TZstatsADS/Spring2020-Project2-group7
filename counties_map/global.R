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

load("../output/Econ_county_map_modified.RData")
zoom = rep(7, length(state.abb))
zoom[c(4)] = 6
zoom[c(1,3,9,12,13,14,15,16,17,22,23,25,27,31,32,33,34,36,38,39,40,44,46)] = 8
zoom[c(6,7,18,19,28,43)] = 9
zoom[c(37)] = 10


state_choise = state.abb
names(state_choise) = state.name
state_choise = c(state_choise[1],state_choise[3:10], state_choise[12:length(state_choise)])


county_map <- function(df){
  mapcounty <- maps::map("county", regions = state.name[which(state.abb == df$State[1])], fill = TRUE, plot = FALSE)
  names <- tibble(Name = mapcounty$names) %>% 
    separate(Name, c('state', 'county'), ',') %>% 
    mutate(state = paste0(toupper(substr(state,1,1)), substr(state, 2, nchar(state)))) %>% 
    mutate(county = paste0(toupper(substr(county,1,1)), substr(county, 2, nchar(county)), ifelse(state == 'Louisiana', ' Parish', ' County')))
  
  df <- df %>% 
    right_join(names, by = c('Name' = 'county')) %>% 
    arrange(State, Year)
  values <- c(unlist(df[,4]))
  metric <- colnames(df)[4]
  chs <- df[1,5]
  year <- df[1,3]
  state <- state.name[which(state.abb == df$State[1])]
  pal <- colorBin("YlOrRd", domain = values, bins = 9)
  if(chs == 1)
    labels <- sprintf(
      "<strong>%s, %s </strong><br/><strong>%s<br/>in %s</strong><br/>%g",    
      state, df$Name, metric, year, values)
  else
    labels <- sprintf(
      "<strong>%s, %s </strong><br/><strong>%s<br/>from %s</strong><br/>%g",    
      state, df$Name, metric, year, values)
  if(chs == 3)
    labels <- paste0(labels, '%')
  labels = labels%>% 
    lapply(htmltools::HTML)
  
  range <- mapcounty$range
  
  # County Map
  leaflet(data = mapcounty) %>% 
    setView(mean(range[1:2]), mean(range[3:4]), zoom[which(state_choise== df$State[1])]) %>% 
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


