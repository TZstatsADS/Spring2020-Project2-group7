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


observeEvent(input$state, {
    state<-Econ_data_county %>% select(State)
    updateSelectInput(session, 'state', choices=unique(state))
  })
  
state_select<-reactive({
  Econ_data_county%>% 
    filter(State==input$state)%>%drop_na()
})

observeEvent(input$county, {
  con<- Econ_data_county%>%filter(State==input$state)%>%select(Name)
  updateSelectInput(session, 'county', choices=unique(con))
})


metric_choose <- reactive({
  
 yr<- Econ_data_county%>%select(Year,input$metric1)%>%drop_na()%>%select(Year)%>%unique()
 
   Econ_data_county%>%
   
   filter(Year %in% unlist(yr) & State==input$state & Name==input$county)%>%
   
   pivot_longer(c(-State,-Year,-Name,-input$metric1), names_to = 'Metrics', values_to = 'Value') %>%
  
   drop_na() %>%
     
   select(Metrics) %>% 
     
   unique()
 
}) 

observeEvent(metric_choose(), {

  updateSelectInput(session, 'metric2', choices=unique(metric_choose()))
})

metric_s<-reactive({
  Econ_data_county %>%filter(State==input$state)%>%
    select(State,Name,input$metric1,input$metric2,Year)%>%drop_na()
})

  
  observeEvent(input$metric2, {
    year <- metric_s()$Year
    
    updateSelectInput(session, 'year', choices=unique(year))
  })
  
  year_select <- reactive({
    metric_s() %>% 
      filter(Year==input$year)%>%drop_na()
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