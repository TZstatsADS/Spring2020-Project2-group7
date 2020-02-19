
serever <- function(input, output, session){
# State Map
  # By Basic Metric
  basic_metric_select <- reactive({
    sel <- if(input$basic_metric=='Education') colnames(Econ_data_state)[4:7]
           else if(input$basic_metric=='Population') colnames(Econ_data_state)[8:22]
           else if(input$basic_metric=='Employment') colnames(Econ_data_state)[23:26]
           else if(input$basic_metric=='Poverty') colnames(Econ_data_state)[27:31]
    
    Econ_data_state %>% 
      select(Name, Year, sel)
  })
  
  observeEvent(input$basic_metric, {
    choice <- basic_metric_select() %>% 
      select(-Name, -Year) %>%  
      colnames()
    
    updateSelectInput(session, 'metric', choices=c(choice))
  })
  
  # By Metric
  metric_select <- reactive({
    Econ_data_state %>% 
      select(Name, Year, input$metric) %>% 
      drop_na()
  })
  
  observeEvent(input$metric, {
    year <- metric_select()$Year
    
    updateSelectInput(session, 'year', choices=unique(year))
  })
  
  # By Year
  data_select <- reactive({
    metric_select() %>% 
      filter(Year==input$year)
  })
  
  output$stmaps <- renderLeaflet(state_map(data_select()))
  
  
  # #test
  # output$test <- DT::renderDataTable({
  #   metric_select()
  # })
}


