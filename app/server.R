
serever <- function(input, output, session){
# State Map
  
  end_year <- reactive({
    if(input$chs!='Snapshot' & input$basic_metric!='Poverty'){
      selectInput(inputId = 'end_year',
                  label = 'End Year',
                  choices = '2011')
    }
  })
  
  output$if_end <- renderUI(end_year())
  observeEvent(input$chs, {
    if(input$chs!='Snapshot' & input$basic_metric!='Poverty'){
      updateSelectInput(session, 'year', label = 'Start Year')
    }
    else updateSelectInput(session, 'year', label = 'Year')
  })
  
  
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
    
    if(input$chs!='Snapshot'){
      updateSelectInput(session, 'end_year', label = 'End Year', choices=unique(year)[-1])
    }
    
    
  })
  
  # By Year
  data_select <- reactive({
    if(input$chs=='Changes by time' & input$basic_metric!='Poverty'){
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      
      dt <- dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      dt$Value <- dt[,3]-dt[,2]
      dt %>%
        select(Name, Value) %>% 
        right_join(names)
      
    }
    else if(input$chs=='%Change by time' & input$basic_metric!='Poverty') {
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      
      dt <- dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      dt$Value <- (dt[,3]-dt[,2])/dt[,3]
      dt %>%
        select(Name, Value) %>% 
        right_join(names)
      
    }
    else{
      dt <- metric_select() %>%
        filter(Year==input$year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>% 
        right_join(names)
    }
  })
  values <- reactive({
    c(unlist(data_select() %>% select(Value)))
  })
  output$stmaps <- renderLeaflet(state_map(data_select(), values()))
  
  
  
  
  #test
  output$test <- DT::renderDataTable({
    if(input$chs=='Changes by time' & input$basic_metric!='Poverty'){
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'

      dt <- dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      dt$Value <- dt[,3]-dt[,2]
      dt %>%
        select(Name, Value) %>% 
        right_join(names)

    }
    else if(input$chs=='%Change by time' & input$basic_metric!='Poverty') {
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'

      dt <- dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      dt$Value <- (dt[,3]-dt[,2])/dt[,3]
      dt %>%
        select(Name, Value) %>% 
        right_join(names)

    }
    else{
      dt <- metric_select() %>%
        filter(Year==input$year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>% 
        right_join(names)
    }
  })
}


