
serever <- function(input, output, session){
# County Map

  
  observeEvent(list(input$chs), {
    if(input$chs!='Snapshot'){
      updateSelectInput(session, 
                        'basic_metric', 
                        choices = c('Education', 'Employment', 'Population'), 
                        selected = 'Population'
      )
      updateSelectInput(session, 'year', label = 'Start Year')
    }
    else{
      updateSelectInput(session, 
                        'basic_metric', 
                        choices = c('Education', 'Employment', 'Population', 'Poverty'), 
                        selected = 'Population'
      )
      
      updateSelectInput(session, 'year')
    }
  })
  
  observeEvent(input$basic_metric, {
    choice <- basic_metric_select() %>% 
      select(-State, -Name, -Year) %>%  
      colnames()
    
    updateSelectInput(session, 'metric', choices=c(choice))
  })
  
  observeEvent(input$metric, {
    year <- metric_select()$Year %>% unique()
    if(input$basic_metric == 'Poverty'){
      updateSelectInput(session, 'year', choices=year, select = year[1])
    }
    else{
      updateSelectInput(session, 'year', choices=year[-length(year)], select = year[1])
    }
  })
  
  # observeEvent(input$year,{
  #   if(input$chs!='Snapshot'){
  #     e_year = e_year()
  #     updateSelectInput(session, 'end_year', choices = e_year, selected = e_year[1])
  #   }
  # })
  
  state_select <- reactive({
    Econ_data_county %>% 
      filter(State == input$state)
  })
  
  # By Basic Metric
  basic_metric_select <- reactive({
    sel <- if(input$basic_metric == 'Education') colnames(Econ_data_county)[4:7]
           else if(input$basic_metric == 'Population') colnames(Econ_data_county)[18:31]
           else if(input$basic_metric == 'Employment') colnames(Econ_data_county)[8:11]
           else if(input$basic_metric == 'Poverty') colnames(Econ_data_county)[12:17]
    
    state_select() %>% 
      select(State, Name, Year, sel)
  })
  
  # By Metric
  metric_select <- reactive({
    state_select() %>% 
      select(State, Name, Year, input$metric) %>% 
      drop_na()
  })
  
  e_year <- reactive({
    ey = metric_select() %>% 
      select(Year) %>%
      filter(Year > input$year) %>%
      unique() %>% 
      c()
    ey[[1]]
  })
  
  end_year <- reactive({
    if(input$chs!='Snapshot'){
      e_year = e_year()
      
      selectInput(inputId = 'end_year',
                  label = 'End Year',
                  choices = e_year,
                  selected = e_year[1]
      )
    }
  })
  
  output$if_end <- renderUI(end_year())
  
  # By Year
  data_select <- reactive({
    if(input$chs == 'Snapshot'){
      metric_select() %>% 
        filter(Year == input$year) %>%
        mutate(chs = 1)
    }
    else{
      dt <- metric_select() %>%
        filter(Year == input$year | Year == input$end_year)
      mname = colnames(dt)[4]
      colnames(dt) = c('State', 'Name', 'Year', 'Value')
      dt <- dt %>% 
        pivot_wider(names_from = 'Year', values_from = 'Value')
      cname = colnames(dt)
      colnames(dt) = c('State', 'Name', 'Year1', 'Year2')
      if(input$chs == 'Changes by time'){
        dt <- dt %>% mutate(Value = Year2 - Year1) 
      }
      else {
        dt <- dt %>% mutate(Value = (Year2 - Year1)/Year1*100) 
      }
      chs = switch(input$chs, `Changes by time` = 2, `%Change by time` = 3)
      dt <- dt %>% mutate(Year = paste0(cname[3], ' to ', cname[4])) %>% 
        select(State, Name, Year, Value) %>% 
        mutate(chs = chs)
      colnames(dt) = c('State', 'Name', 'Year', mname, 'chs')
      dt
    }
    
  })
  
  output$stmaps <- renderLeaflet(county_map(data_select()))
  
  
  # #test
  # output$test <- DT::renderDataTable({
  #   metric_select()
  # })
}


