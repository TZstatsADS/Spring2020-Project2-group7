
serever <- function(input, output, session){

  #### County map written by Jinxu Xiang 
  
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
      
      updateSelectInput(session, 'year_xjx')
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
  
  
  # #test
  # output$test <- DT::renderDataTable({
  #   metric_select()
  # })
}


