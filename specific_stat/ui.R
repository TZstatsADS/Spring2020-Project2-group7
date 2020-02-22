#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
# devtools::install_github("rstudio/leaflet#346")
library(leaflet)
library(crosstalk)
library(htmltools)

# Header
header <- dashboardHeader(title='Project_2 Group_7')

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Home', 
             tabName = 'Home', 
             icon = icon('home')
    ), 
    menuItem('Maps', 
             tabName='map', 
             icon = icon("map"), 
             menuSubItem('State Map', 
                         tabName = 'state_map'
             ),
             menuSubItem('County Map', 
                         tabName = 'county_map'
             )
             
    ),
    menuItem('Comparison Stats', 
             tabName = 'stats', 
             icon = icon("chart-line"), 
             menuSubItem('By State', 
                         tabName = 'comparison_state_stats'
             ),
             menuSubItem('By County', 
                         tabName = 'comparison_county_stats'
             ) 
    ), 
    menuItem('Specific Stats', 
             tabName = 'stats', 
             icon = icon("chart-line"), 
             menuSubItem('By State', 
                         tabName = 'specific_state_stats'
             ),
             menuSubItem('By County', 
                         tabName = 'specific_county_stats'
             ) 
    ), 
    menuItem('Reference', 
             tabName = 'ref', 
             icon = icon('alipay'))
  )
)


body <- dashboardBody(
tabItems(
  
  tabItem(tabName='Home',
          fluidRow(
            tabBox(title='', id='home_tabs', width = 12,
                   tabPanel(h4('Project Description') ,
                            'This is a summary'),
                   tabPanel(h4('User Guide'), 
                            'Here is the user guide')
            ),
            box(width=12, DT::dataTableOutput('test'))
          )
          
  ),
  tabItem(tabName = 'state_map', 
          "code of state map"
  ),
  tabItem(tabName = 'county_map', 
          "code of county map"
  ),
  tabItem(tabName = 'comparison_state_stats', 
          "code of comparison state stats"
  ),
  tabItem(tabName = 'comparison_county_stats', 
          "code of comparison county stats"
  ),
    tabItem(tabName="specific_state_stats",
        fluidRow(
          tabBox(id=' ', width =9, title=" ",
            tabPanel('Data',  
                        DT::dataTableOutput("table1")),
            tabPanel('Graph', 
                     plotOutput("barplot1"))),
             box(width = 3, status='info',
                 selectInput(inputId = 'state', 
                             label = 'State', 
                             choices = 'AL' 
                 ),
                 selectInput(inputId = 'county', 
                             label = 'County', 
                             choices = 'Autauga County' 
                 ),
                 selectInput(inputId = 'metric1', 
                             label = 'Choose First Metric', 
                             choices =  Econ_data_county %>% 
                               select(-Name, -State,-Year) %>%  
                               colnames()
                 ), 
                 selectInput(inputId = 'metric2', 
                             label = 'Choose Second Metric', 
                             choices = 'Employed'
                 ), 
                 selectInput(inputId = 'year', 
                             label = 'Year', 
                             choices = '' 
                 )
             )
        )   
    ),
  tabItem(tabName="specific_county_stats",
          fluidRow(
            tabBox(id=' ', width =9, title=" ",
                   tabPanel('Data',  
                            DT::dataTableOutput("table2")),
                   tabPanel('Graph', 
                            plotOutput("barplot2"))),
            box(width = 3, status='info',
                selectInput(inputId = 'basic_metric', 
                            label = 'Metrics',
                            choices = c('Education', 'Population', 'Employment'), 
                            selected = "Population"
                ),
                selectInput(inputId = 'metrics', 
                            label = 'Sub-Metric', 
                            choices = 'Employed'
                ), 
                selectInput(inputId = 'states', 
                            label = 'State', 
                            choices = 'AL' 
                ),
                selectInput(inputId = 'counties', 
                            label = 'County', 
                            choices = 'Autauga County' 
                )
            )
          )   
  ),
  tabItem(tabName = 'ref', 
          'Here shows the references')
)   


)

 
ui <- dashboardPage(
  skin='green',
  header=header,
  sidebar=sidebar,
  body=body
)