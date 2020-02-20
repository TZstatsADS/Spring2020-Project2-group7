
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
  
  # choose the type —— change UI
  end_year <- reactive({
    if(input$chs!='Snapshot' & input$basic_metric!='Poverty'){
      selectInput(inputId = 'end_year',
                  label = 'End Year',
                  choices = c('2011', '2012', '2013', '2014', 
                              '2015', '2016', '2017', '2018')
                  )
    }
  })
  output$if_end <- renderUI(end_year())
  
  observeEvent(input$metric, {
    year <- metric_select()$Year
    updateSelectInput(session, 'year', choices=unique(year))
    
    if(input$chs!='Snapshot'& input$basic_metric!='Poverty'){
      updateSelectInput(session, 'end_year', choices=unique(year)[-1])
    }
  })
  
  # Get the df of map
  data_select <- reactive({
    if(input$chs=='Changes by time' & input$basic_metric!='Poverty'){
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value1 <- dt[, 3]
      colnames(Value1) <-'Value1'
      
      dt <- dt %>%
        cbind(Value1) %>%
        select(Name, Year, Value1) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value1')
      Value <- dt[, 3]-dt[, 2]
      colnames(Value) <-'Value'
      
      dt %>%
        cbind(Value) %>% 
        select('Name', 'Value') %>% 
        right_join(names, by = "Name")
    }
    else if(input$chs=='%Change by time' & input$basic_metric!='Poverty') {
      dt <- metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value1 <- dt[, 3]
      colnames(Value1) <-'Value1'
      
      dt <- dt %>%
        cbind(Value1) %>%
        select(Name, Year, Value1) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value1')
      Value <- (dt[, 3]-dt[, 2])/dt[, 2]
      colnames(Value) <-'Value'
      
      dt %>%
        cbind(Value) %>% 
        select('Name', 'Value') %>% 
        right_join(names, by = "Name")
    }
    else{
      dt <- metric_select() %>%
        filter(Year==input$year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>% 
        right_join(names, by = "Name")
    }
  })
  
  values <- reactive({
    c(unlist(data_select()[,2]))
  })
  
  observeEvent(input$chs, {
    if(input$chs!='Snapshot' & input$basic_metric!='Poverty'){
      updateSelectInput(session, 'year', label = 'Start Year')
    }
    else updateSelectInput(session, 'year', label = 'Year')
  })
  
  output$stmaps <- renderLeaflet(state_map(values()))
  
  
  #test
  output$test <- renderPrint({
    data_select()
  })
}


