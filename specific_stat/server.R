#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
load("~/Documents/GitHub/Spring2020-Project2-group7/output/Econ_county_map.Rdata")
index1<-function(df){
  df[,3]%>%unlist()
}
index2<-function(df){
  df[,4]%>%unlist()
}
index<-function(df){
  df%>%select(-Name,-Year,-State)%>%unlist()
}


serever <- function(input, output, session){

  # By Basic Metric
  
metric_select <- reactive({
    
    Econ_data_county %>%select(State,Name,input$metric,Year)
  }) 

  observeEvent(input$metric, {
   state<-  metric_select()$State
    updateSelectInput(session, 'state', choices=unique(state))
  })

  # By State
  data_select <- reactive({
    metric_select() %>% 
      filter(State==input$state)%>%drop_na()
  })
  
  observeEvent(input$state, {
    year <- data_select()$Year
    
    updateSelectInput(session, 'year', choices=unique(year))
  })
  
  year_select <- reactive({
    data_select() %>% 
      filter(Year==input$year)%>%drop_na()
  })
  
  observeEvent(input$year, {
    county <- year_select()$Name
    
    updateSelectInput(session, 'county', choices=unique(county))
  })


 
output$barplot1<- renderPlot({
  ggplot(year_select(),aes(x=index1(year_select()), y=index2(year_select())))+
    geom_point()+
    geom_point(aes(y = index2(year_select()%>%filter(Name==input$county)),
                  x= index1(year_select()%>%filter(Name==input$county)) ),
                  color='red',size=4)+
    geom_text(aes(y = index2(year_select()%>%filter(Name==input$county))+.2,
                  x= index1(year_select()%>%filter(Name==input$county)) ,
                  label=input$county))
})

output$table1<-DT::renderDataTable({
  DT::datatable(year_select())
})

##########################

basic_metric_select <- reactive({
  sel <- if(input$basic_metric=='Education') colnames(Econ_data_county)[4:7]
  else if(input$basic_metric=='Population') colnames(Econ_data_county)[8:22]
  else if(input$basic_metric=='Employment') colnames(Econ_data_county)[23:26]
  
  
  Econ_data_county %>% 
    select(State, Name, Year, sel)
})

observeEvent(input$basic_metric, {
  choice <- basic_metric_select() %>% 
    select(-State, -Name, -Year) %>%  
    colnames()
  
  updateSelectInput(session, 'metrics', choices=c(choice))
})

metric_sel<- reactive({
  Econ_data_county %>% 
    select(State, Name, Year, input$metrics) %>% 
    drop_na()
})

observeEvent(input$metrics, {
  state <- metric_sel()$State
  
  updateSelectInput(session, 'states', choices=unique(state))
}) 

state_sel <- reactive({
  metric_sel() %>% 
    filter(State==input$states)%>% 
    drop_na()
})

observeEvent(input$states, {
  counties <- state_sel()$Name
  
  updateSelectInput(session, 'counties', choices=unique(counties))
}) 

county_select <- reactive({
  state_sel() %>% 
    filter(Name==input$counties)%>%drop_na()
})


output$barplot2<- renderPlot({
  ggplot(county_select())+
    geom_point(aes(x=Year, y=index(county_select())))+
    geom_line(aes(x=Year, y=index(county_select())),group=1)
})

output$table2<-DT::renderDataTable({
  DT::datatable(county_select())
})
}
