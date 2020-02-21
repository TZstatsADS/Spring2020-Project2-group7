
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
  
  # choose the type —— change UI
  end_year <- reactive({
    if(input$chs!='Snapshot'){
      selectInput(inputId = 'end_year',
                  label = 'End Year',
                  choices = c('2011', '2012', '2013', '2014', 
                              '2015', '2016', '2017', '2018')
      )
    }
  })
  
  output$if_end <- renderUI(end_year())
  
  observeEvent(list(input$chs), {
    if(input$chs!='Snapshot'){
      updateSelectInput(session, 
                        'basic_metric', 
                        choices = c('Education', 'Population', 'Employment'), 
                        selected = 'Population'
                        )
      updateSelectInput(session, 'year', label = 'Start Year')
    }
    else{
      updateSelectInput(session, 
                        'basic_metric', 
                        choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                        selected = 'Population'
                        )
      
      updateSelectInput(session, 'year')
    }
  })
  #
  
  # The Basic Metric Changes --> The Selected Metric Change
  observeEvent(input$basic_metric, {
    choice <- basic_metric_select() %>% 
      select(-Name, -Year) %>%  
      colnames()
    updateSelectInput(session, 'metric', choices=c(choice))
  })
  #
  
  # The Metric Changes --> The Selected Years Change 
  ##   data selected by metric
  metric_select <- reactive({
    Econ_data_state %>% 
      select(Name, Year, input$metric) %>%
      drop_na()
  })
  
  ## Years changes
  observeEvent(list(input$chs, input$basic_metric, input$metric), {
    year <- metric_select()$Year %>% unique()
    updateSelectInput(session, 
                      'year', 
                      choices = year, 
                      selected = year[1])
    if(input$chs!='Snapshot'){
      id <- match(input$year, year)
      updateSelectInput(session, 
                        'end_year', 
                        choices = year[-1], 
                        selected = year[-1][1])
    }
  })
  #
  
  # Start Year changes --> End Year changes
  observeEvent(input$year, {
    if(input$chs!='Snapshot'){
      year <- metric_select()$Year %>% unique()
      id <- match(input$year, year)
      choices <- year[-c(1:id)]
      updateSelectInput(session, 
                        'end_year', 
                        choices = choices, 
                        selected = choices[1])
    }
  })
  
  # Get the df of map
  data_select <- reactive({
    if(input$chs!='Snapshot'){
      dt <- 
        metric_select() %>%
        filter(Year==input$year | Year==input$end_year)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt <- 
        dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      if(input$chs=='Changes by time'){
        Value <- dt[, 3] - dt[, 2]
      }
      else {
        Value <- (dt[, 3]-dt[, 2])/dt[, 2]
      }
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>%
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

  output$stmaps <- renderLeaflet(state_map(data_select(), input$metric))
  
  
  #test
  output$test <- renderPrint({
    if(input$chs!='Snapshot'){
    # dt <-
    #   metric_select() %>%
    #   filter(Year==input$year | Year==input$end_year)
    # Value <- dt[, 3]
    # colnames(Value) <-'Value'
    # Value
    # dt <- dt %>%
    #   cbind(Value) %>%
    #   select(Name, Year, Value) %>%
    #   pivot_wider(names_from = 'Year', values_from = 'Value')
    }
   #metric_select()
  })
}


