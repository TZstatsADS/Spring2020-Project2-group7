library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(maps)
library(maptools)
library(usmap)

load("../output/counties.RData")
state_choise = counties$State %>% unique()



ui <- dashboardPage(
    dashboardHeader(
        title = "Basic dashboard",
        titleWidth = 200
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map"))
        ),
        width = 200
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "map",
                div(
                    absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                  top = 50, left = "auto", right = 10, bottom = "auto", width = 250, height = "auto",
                        title = "Select",
                        selectInput(inputId = "state", 
                                    label = "Choose your state", 
                                    choices = state_choise,
                                    selected = state_choise[1]),
                        uiOutput("state_metric"),
                        uiOutput("state_year")
                    ),
              

                    title = "Map",
                    plotOutput("map", height = 750, 
                               click = "state_map_click", 
                               dblclick = "state_map_dblclick",
                               brush = brushOpts(
                                 id = "state_map_brush",
                                 resetOnNew = TRUE)),
                    verbatimTextOutput("info")

                )
            )
        )
    )
)

server <- function(input, output) {
    value = reactive({counties %>%
        filter(State == input$state) %>%
        filter(Metric == input$state_metric) %>%
        filter(Year %in% c(input$state_year)) %>%
        group_by(FIPS, Name) %>%
        summarize(value = mean(Value)) %>%
        select(fips = FIPS, value = value, name = Name)
    })  
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$state_map_dblclick, {
      brush <- input$state_map_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
  
    output$state_metric <- renderUI({
        metric = with(counties %>% 
                        filter(State == input$state) %>% 
                        arrange(Metric), 
                      unique(Metric))
      
        selectInput(inputId = "state_metric", 
                    label = paste0("Choose your metrics in ", 
                                   state.name[which(state.abb == input$state)]), 
                    choices = metric,
                    selected = metric[1])
    })
  
    output$state_year <- renderUI({
        year = with(counties %>% 
                      filter(State == input$state) %>% 
                      filter(Metric == input$state_metric) %>%
                      arrange(Year), 
                    unique(Year))
      
        selectInput(inputId = "state_year", 
                    label = paste0("Choose your year in ", 
                                   state.name[which(state.abb == input$state)], 
                                   " on ", 
                                   input$state_metric), 
                    choices = year,
                    selected = year[1], 
                    multiple = TRUE)
    })
  
    output$map <- renderPlot({
        value = value()  
      
        plot_usmap(data = value,
                   regions = "counties",
                   include = input$state,
                   values = "value",
                   color = "white",
                   labels = TRUE) +
          scale_fill_continuous(name = input$state_metric, 
                                label = scales::comma, 
                                low = "white", 
                                high = "blue") +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + 
          theme(legend.position = "right")


    })
    
    output$info <- renderText({
      
      get_text <- function(click) {
        if(is.null(click)) return("Choose a county by clicking!\n")
        value = value()  
        x0 = click$x
        y0 = click$y
        
        coord = us_map(regions = 'county') %>%
          filter(abbr == input$state)
        dist = coord %>% 
          
          mutate(Dist = sqrt((x-x0)^2+(y-y0)^2)) %>% 
          group_by(county) %>% 
          summarize(dist = mean(Dist)) %>% 
          arrange(dist)
        
        county = dist[1,1]
        len = length(input$state_year)
        sort_year = input$state_year %>% sort()
        
        text = 'The '
        if(len > 1){
          text = paste0(text, 'average ')
        }
        text = paste0(text, input$state_metric,
                      ' of ', county, 'in ')
        if(len == 1){
          text = paste0(text, sort_year[1], ' ')
        }
        else if(len > 1){
          text = paste0(text, sort_year[1])
        }
        if(len == 2){
          text = paste0(text, ' and ', sort_year[2], ' ')
        }
        else if(len > 2){
          text = paste0(text, ', ')
          for(i in 2:(len-1)){
            text = paste0(text, sort_year[i], ', ')
          }
          text = paste0(text, 'and ', sort_year[len], ' ')
        }
        text = paste0(text, 'is ', with(value%>%filter(name == county), value))
        
        index = 0
        for(i in 1:nchar(text)){
          if(i-index > 40 & substr(text,i,i) == ' '){
            text = paste0(substr(text,1,i), '\n ', substr(text,i+1,nchar(text)))
            index = i
          }
        }
        return(text)
      }
      
      text = get_text(input$state_map_click)
      text
      
    })

}

shinyApp(ui, server)














