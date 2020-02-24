library(shiny)
library(shinydashboard)
library(tidyr)
library(tidyverse)

#load("../app/counties_new.RData")
load("/Users/apple/Documents/GitHub/Spring2020-Project2-group7/app/counties_new.RData")
basemetric_choice <- c("Education", "Employment", "Population")
metric_choice <- counties_new$Metric %>% unique()
state_choice <-counties_new$State %>% unique()

header <- dashboardHeader(
  title = "Simple tabs"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('comparison', 
             tabName='Comparison', 
             icon = icon("chart-area"),
             menuSubItem('By States', 
                         tabName='comparison_state_stats', 
                         icon = icon("state")),
             menuSubItem('By Counties', 
                         tabName='comparison_county_stats', 
                         icon = icon("state")))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "comparison_state_stats", 
      fluidRow(
        box(width = 9,
            tabBox(title = "The output",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1", height = "300px", width = 12,
                   tabPanel("Plot Output", plotOutput("year_change_plot_states_vk")),
                   tabPanel("Data Output", DT::dataTableOutput("year_change_data_states_vk"), downloadButton("downloadid_vk", "Download data"))
            )
        ),
        box(width = 3, status='info',
               selectInput("base_metric_vk", "Choose a Category:",
                           choices = basemetric_choice, selected = basemetric_choice[1]),
            selectInput("metric_vk", "Choose a Metric", choices = "Population"),
            radioButtons(inputId = "sort_vk", label = "Variable:",
                         choices = c("Top" = "descending", "Bottom" = "ascending")),
            numericInput("top_n_vk", "Number of States:",
                        min = 1, max = 52, value = 5)
          )
        )
      ),
##########
      tabItem(tabName = "comparison_county_stats", 
        fluidRow(
          box(width = 9,
              tabBox(title = "The output",
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset1", height = "300px", width = 12,
                     tabPanel("Plot Output", plotOutput("year_change_plot_counties_vk")),
                     tabPanel("Data Output", DT::dataTableOutput("year_change_data_counties_vk"), downloadButton("downloadid2_vk", "Download data"))
              )
          ),
          box(width = 3, status='info',
              selectInput("base_metric2_vk", "Choose a Category:",
                          choices = basemetric_choice, selected = basemetric_choice[1]),
              selectInput("metric2_vk", "Choose a Metric", choices = "Population"),
              selectInput("state2_vk", "Choose a State", choices = state_choice),
              radioButtons(inputId = "sort2_vk", label = "Variable:",
                           choices = c("Top" = "descending", "Bottom" = "ascending")),
              numericInput("top_n2_vk", "Number of counties:", value = 5, min = 1, max = 30)
          )
        )
    )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  base_metric_select_vk <- reactive({
    counties_new %>% filter(base_metric == input$base_metric_vk)
  })
  
  observeEvent(input$base_metric_vk, {
    choice <- base_metric_select_vk()$Metric %>% unique() 
    updateSelectInput(session, "metric_vk", choices=c(choice))
  })
  
  metric_select_vk <- reactive({
    base_metric_select_vk() %>% filter(Metric == input$metric_vk)
  })
  
  data_select_vk <- reactive({
    metric_select_vk() %>% group_by(Year, State) %>% summarise(Value = mean(Value, na.rm = TRUE)) %>% pivot_wider(names_from = Year, values_from = Value)
  })
  
  
  year_sort_vk <- reactive({
    dt<-data_select_vk()
    temp_name<-colnames(dt)
    n <- dim(dt)[2]
    colnames(dt)[n]<-"use_this_sort"
    if(input$sort_vk == "ascending"){
      dt<-dt %>% arrange(use_this_sort) %>% mutate(State = factor(State, levels = fct_reorder(State, use_this_sort, .desc = FALSE)))
    }
    else if(input$sort_vk == "descending"){
      dt<-dt %>% arrange(desc(use_this_sort)) %>% mutate(State = factor(State, levels = fct_reorder(State, use_this_sort, .desc = TRUE)))
    }
    dt <- dt[1:min(max(input$top_n_vk,1),40),]
    colnames(dt)<-temp_name
    dt
  })

  
  output$year_change_plot_states_vk<- renderPlot({
    dt<-year_sort_vk()
    n <- dim(dt)[2]
    temp_name<-colnames(dt)
    colnames(dt)[2:n]<-c(2:n)
    dt %>% pivot_longer(2:n) %>% mutate(name = as.numeric(name)) %>% ggplot(aes(x=name, y=value, color = State))+ geom_line() + geom_point() + scale_x_continuous(breaks = 2:n, labels = temp_name[2:n])
  })
  
  output$year_change_data_states_vk <- DT::renderDataTable({
    DT::datatable(year_sort_vk())
  })
  
  output$downloadid_vk <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(year_sort_vk(), con)
    }
  )

##############
  base_metric_select2_vk <- reactive({
    counties_new %>% filter(base_metric == input$base_metric2_vk)
  })
  
  observeEvent(input$base_metric2_vk, {
    choice <- base_metric_select2_vk() %>% select(Metric) %>% unique() 
    updateSelectInput(session, "metric2_vk", choices=c(choice))
  })
  
  metric_select2_vk <- reactive({
    base_metric_select2_vk() %>% filter(Metric == input$metric2_vk) %>% filter(State == input$state2_vk)
  })
  
  data_select2_vk <- reactive({
    metric_select2_vk() %>% group_by(Year, Name) %>% summarise(Value = mean(Value, na.rm = TRUE)) %>% pivot_wider(names_from = Year, values_from = Value)
  })
  
  
  year_sort2_vk <- reactive({
    dt<-data_select2_vk()
    temp_name<-colnames(dt)
    n <- dim(dt)[2]
    colnames(dt)[n]<-"use_this_sort"
    if(input$sort2_vk == "ascending"){
      dt<-dt %>% arrange(use_this_sort) %>% mutate(Name = factor(Name, levels = fct_reorder(Name, use_this_sort, .desc = FALSE)))
    }
    else if(input$sort2_vk == "descending"){
      dt<-dt %>% arrange(desc(use_this_sort)) %>% mutate(Name = factor(Name, levels = fct_reorder(Name, use_this_sort, .desc = TRUE)))
    }
    dt <- dt[1:min(max(input$top_n2_vk,1),40),]
    colnames(dt)<-temp_name
    dt
  })
  
  
  output$year_change_plot_counties_vk<- renderPlot({
    dt<-year_sort2_vk()
    n <- dim(dt)[2]
    temp_name<-colnames(dt)
    colnames(dt)[2:n]<-c(2:n)
    dt %>% pivot_longer(2:n) %>% mutate(name = as.numeric(name)) %>% ggplot(aes(x=name, y=value, color = Name))+ geom_line() + geom_point() + scale_x_continuous(breaks = 2:n, labels = temp_name[2:n])
  })
  
  output$year_change_data_counties_vk <- DT::renderDataTable({
    DT::datatable(year_sort2_vk())
})
  
  output$downloadid2_vk <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(year_sort2_vk(), con)
    }
  )
}

shinyApp(ui, server)

