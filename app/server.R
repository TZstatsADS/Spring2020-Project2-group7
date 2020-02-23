
serever <- function(input, output, session){

#################County map written by Jinxu Xiang 
  
  observeEvent(list(input$chs_xjx), {
    if(input$chs_xjx!='Snapshot'){
      updateSelectInput(session, 
                        'basic_metric_xjx', 
                        choices = c('Education', 'Employment', 'Population'), 
                        selected = 'Population'
      )
      updateSelectInput(session, 'year_xjx', label = 'Start Year')
    }
    else{
      updateSelectInput(session, 
                        'basic_metric_xjx', 
                        choices = c('Education', 'Employment', 'Population', 'Poverty'), 
                        selected = 'Population'
      )
      
      updateSelectInput(session, 'year_xjx', label = 'Year')
    }
  })
  
  observeEvent(input$basic_metric_xjx, {
    choice <- basic_metric_select() %>% 
      select(-State, -Name, -Year) %>%  
      colnames()
    
    updateSelectInput(session, 'metric_xjx', choices=c(choice))
  })
  
  observeEvent(input$metric_xjx, {
    year <- metric_select()$Year %>% unique()
    if(input$chs_xjx == 'Snapshot'){
      updateSelectInput(session, 'year_xjx', choices=year, select = year[1])
    }
    else{
      if(input$basic_metric_xjx == 'Poverty'){
        updateSelectInput(session, 'year_xjx', choices=year, select = year[1])
      }
      else{
        updateSelectInput(session, 'year_xjx', choices=year[-length(year)], select = year[1])
      }
    }
  })
  
  # observeEvent(input$year_xjx,{
  #   if(input$chs_xjx!='Snapshot'){
  #     e_year = e_year()
  #     updateSelectInput(session, 'end_year_xjx', choices = e_year, selected = e_year[1])
  #   }
  # })
  
  state_select <- reactive({
    Econ_data_county %>% 
      filter(State == input$state_xjx)
  })
  
  # By Basic Metric
  basic_metric_select <- reactive({
    sel <- if(input$basic_metric_xjx == 'Education') colnames(Econ_data_county)[4:7]
           else if(input$basic_metric_xjx == 'Population') colnames(Econ_data_county)[13:17]
           else if(input$basic_metric_xjx == 'Employment') colnames(Econ_data_county)[8:9]
           else if(input$basic_metric_xjx == 'Poverty') colnames(Econ_data_county)[10:12]
    
    state_select() %>% 
      select(State, Name, Year, sel)
  })
  
  # By Metric
  metric_select <- reactive({
    state_select() %>% 
      select(State, Name, Year, input$metric_xjx) %>% 
      drop_na()
  })
  
  e_year <- reactive({
    ey = metric_select() %>% 
      select(Year) %>%
      filter(Year > input$year_xjx) %>%
      unique() %>% 
      c()
    ey[[1]]
  })
  
  end_year <- reactive({
    if(input$chs_xjx!='Snapshot'){
      e_year = e_year()
      selectInput(inputId = 'end_year_xjx',
                  label = 'End Year',
                  choices = e_year,
                  selected = e_year[1]
      )
    }
  })
  
  output$if_end_xjx <- renderUI(end_year())
  
  # By Year
  data_select <- reactive({
    if(input$chs_xjx == 'Snapshot'){
      metric_select() %>% 
        filter(Year == input$year_xjx) %>%
        mutate(chs = 1)
    }
    else{
      dt <- metric_select() %>%
        filter(Year == input$year_xjx | Year == input$end_year_xjx)
      mname = colnames(dt)[4]
      colnames(dt) = c('State', 'Name', 'Year', 'Value')
      dt <- dt %>% 
        pivot_wider(names_from = 'Year', values_from = 'Value')
      cname = colnames(dt)
      colnames(dt) = c('State', 'Name', 'Year1', 'Year2')
      if(input$chs_xjx == 'Changes by time'){
        dt <- dt %>% mutate(Value = Year2 - Year1) 
      }
      else {
        dt <- dt %>% mutate(Value = (Year2 - Year1)/Year1*100) 
      }
      chs = switch(input$chs_xjx, `Changes by time` = 2, `%Change by time` = 3)
      dt <- dt %>% mutate(Year = paste0(cname[3], ' to ', cname[4])) %>% 
        select(State, Name, Year, Value) %>% 
        mutate(chs = chs)
      colnames(dt) = c('State', 'Name', 'Year', mname, 'chs')
      dt
    }
    
  })
  
  output$stmaps_xjx <- renderLeaflet(county_map_xjx(data_select()))

####################################################################################################
  # Yuqiao Liu
  # State Map
  # By Basic Metric
  
  basic_metric_select_lyq <- reactive({
    sel <- if(input$basic_metric_lyq=='Education') colnames(Econ_data_state)[4:7]
    else if(input$basic_metric_lyq=='Population') colnames(Econ_data_state)[8:12]
    else if(input$basic_metric_lyq=='Employment') colnames(Econ_data_state)[13:14]
    else if(input$basic_metric_lyq=='Poverty') colnames(Econ_data_state)[15:17]
    
    Econ_data_state %>% 
      select(Name, Year, sel)
  })
  
  # choose the type --> change UI
  end_year_lyq <- reactive({
    if(input$chs_lyq!='Snapshot'){
      selectInput(inputId = 'end_year_lyq',
                  label = 'End Year',
                  choices = c('2011', '2012', '2013', '2014', 
                              '2015', '2016', '2017', '2018')
      )
    }
  })
  
  output$if_end_lyq <- renderUI(end_year_lyq())
  
  observeEvent(input$chs_lyq, {
    if(input$chs_lyq!='Snapshot'){
      updateSelectInput(session, 
                        'basic_metric_lyq', 
                        choices = c('Education', 'Employment', 'Population'), 
                        selected = 'Population'
      )
      updateSelectInput(session, 'year_lyq', label = 'Start Year')
    }
    else{
      updateSelectInput(session, 
                        'basic_metric_lyq', 
                        choices = c('Education', 'Employment', 'Population', 'Poverty'), 
                        selected = 'Population'
      )
      
      updateSelectInput(session, 'year_lyq')
    }
  })
  #
  
  # The Basic Metric Changes --> The Selected Metric Change
  observeEvent(input$basic_metric_lyq, {
    choice <- basic_metric_select_lyq() %>% 
      select(-Name, -Year) %>%  
      colnames()
    updateSelectInput(session, 'metric_lyq', choices=c(choice))
  })
  #
  
  # The Metric Changes --> The Selected Years Change 
  ##   data selected by metric
  metric_select_lyq <- reactive({
    Econ_data_state %>% 
      select(Name, Year, input$metric_lyq) %>%
      drop_na()
  })
  
  ## Years changes
  observeEvent(  metric_select_lyq(), {
    year <-   metric_select_lyq()$Year %>% unique()
    updateSelectInput(session, 
                      'year_lyq', 
                      choices = year, 
                      selected = year[1])
    if(input$chs_lyq!='Snapshot'){
      id <- match(input$year_lyq, year)
      updateSelectInput(session, 
                        'end_year_lyq', 
                        choices = year[-1], 
                        selected = year[-1][1])
    }
  })
  #
  
  # Start Year changes --> End Year changes
  observeEvent(input$year_lyq, {
    if(input$chs_lyq!='Snapshot'){
      year <-   metric_select_lyq()$Year %>% unique()
      id <- match(input$year_lyq, year)
      choices <- year[-c(1:id)]
      updateSelectInput(session,
                        'end_year_lyq',
                        choices = choices,
                        selected = choices[1])
    }
  })
  
  # Get the df of map
  data_select_lyq <- reactive({
    if(input$chs_lyq!='Snapshot'){
      dt <- 
        metric_select_lyq() %>%
        filter(Year==input$year_lyq | Year==input$end_year_lyq)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt <- 
        dt %>%
        cbind(Value) %>%
        select(Name, Year, Value) %>%
        pivot_wider(names_from = 'Year', values_from = 'Value')
      if(input$chs_lyq=='Changes by time'){
        Value <- dt[, 3] - dt[, 2]
      }
      else {
        Value <- (dt[, 3]-dt[, 2])/dt[, 2]
        unit <- '%'
      }
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>%
        right_join(names, by = "Name")
    }
    else{
      dt <- metric_select_lyq() %>%
        filter(Year==input$year_lyq)
      Value <- dt[, 3]
      colnames(Value) <-'Value'
      dt %>%
        cbind(Value) %>%
        select(Name, Value) %>%
        right_join(names, by = "Name")
    }
  })
  
  output$stmaps_lyq <- renderLeaflet({
    chs <- input$chs_lyq
    map_data <- data_select_lyq()
    met <- input$metric_lyq
    state_map(map_data, met, chs)
  })
####################################################################################################
  
  
 
}


