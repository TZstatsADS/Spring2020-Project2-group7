
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
  
  observeEvent(list(input$metric_xjx, input$chs_xjx), {
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
  observeEvent(list(metric_select_lyq(), input$chs_lyq), {
    year <- metric_select_lyq()$Year %>% unique()
    updateSelectInput(session, 
                      'year_lyq', 
                      choices = year[1:length(year)], 
                      selected = year[1])
    if(input$chs_lyq!='Snapshot'){
      id <- match(input$year_lyq, year)
      updateSelectInput(session, 
                        'end_year_lyq', 
                        choices = year[-1], 
                        selected = year[-1][1])
      updateSelectInput(session, 
                        'year_lyq', 
                        choices = year[1:(length(year)-1)], 
                        selected = year[1])
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

###Specific Stats
## Zidi Hong
  
#####By state
  metric_choose_zh <- reactive({
    yr<- Econ_data_county %>%
      select(Year,input$metric1_zh) %>%
      drop_na() %>%
      select(Year) %>%
      unique()
    
    Econ_data_county %>%
      filter(Year %in% unlist(yr) & State==input$state_zh ) %>%
      pivot_longer(c(-State,-Year,-Name,-input$metric1_zh), names_to = 'Metrics', values_to = 'Value') %>%
      drop_na() %>%
      select(Metrics) %>% 
      unique()
  }) 
  
  observeEvent(metric_choose_zh(), {
    updateSelectInput(session, 'metric2_zh', choices=unique(metric_choose_zh()))
  })
  
  metric_s<-reactive({
    Econ_data_county %>%filter(State==input$state_zh)%>%
      select(State,Name,input$metric1_zh,input$metric2_zh,Year)%>%drop_na()
  })
  
  
  observeEvent(input$metric2_zh, {
    year_zh <- metric_s()$Year
    
    updateSelectInput(session, 'year_zh', choices=unique(year_zh))
  })
  
  year_select_zh <- reactive({
    metric_s() %>% 
      filter(Year==input$year_zh)%>%drop_na()
  })
  
  observeEvent(input$state_zh, {
    con_zh <- Econ_data_county %>% 
      filter(State==input$state_zh) %>%
      select(Name)
    updateSelectInput(session, 'county_zh', choices=unique(con_zh))
  })
  
###output
  
  output$barplot1_zh <- renderPlotly({
    data111 <- year_select_zh() %>% filter(Name != input$county_zh)
    data222 <- year_select_zh() %>% filter(Name == input$county_zh)
    p1_zh <- ggplot(data111)+
      geom_point(
        aes(y = index2(data111),
            x = index1(data111))
      )+
      geom_point(
        y = index2(data222),
        x = index1(data222),
        color='red',size=4
      )+
      labs(x=names(index1(year_select_zh()%>%filter(Name==input$county_zh))),
           y=names(index2(year_select_zh()%>%filter(Name==input$county_zh))))
    
    plotly1 <- ggplotly(p1_zh)
    
    plotly1 %>%
      style(text = paste0(data111$Name,"</br></br>",
                          "( ",round(index1(data111),4), ", ",
                          round(index2(data111),4)," )"
                          ), 
            traces = 1
            ) %>% 
      style(text = paste0(data222$Name,"</br></br>",
                          "( ",round(index1(data222),4), ", ",
                          round(index2(data222),4)," )"
                          ),
            traces = 2
            )
  })
  
  output$table1_zh<-DT::renderDataTable({
    DT::datatable(year_select_zh())
  })
  
  output$downloadData1_zh <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(year_select_zh(), file)
    }
  )
  
###########by county
  
  basic_metric_select_zh <- reactive({
    sel_zh <- if(input$basic_metric_zh=='Education') colnames(Econ_data_county)[4:7]
    else if(input$basic_metric_zh=='Population') colnames(Econ_data_county)[13:17]
    else if(input$basic_metric_zh=='Employment') colnames(Econ_data_county)[8:9]
    
    Econ_data_county %>% 
      select(State, Name, Year, sel_zh)
  })
  
  observeEvent(input$basic_metric_zh, {
    choi <- basic_metric_select_zh() %>% 
      select(-State, -Name, -Year) %>%  
      colnames()
    
    updateSelectInput(session, 'metrics_zh', choices=c(choi))
  })
  
  metric_sel_zh<- reactive({
    Econ_data_county %>% 
      select(State, Name, Year, input$metrics_zh) %>% 
      drop_na()
  })
  
  observeEvent(input$metrics_zh, {
    stateq <- metric_sel_zh()$State
    
    updateSelectInput(session, 'states_zh', choices=unique(stateq))
  }) 
  
  state_sel_zh <- reactive({
    metric_sel_zh() %>% 
      filter(State==input$states_zh)%>% 
      drop_na()
  })
  
  observeEvent(input$states_zh, {
    counties_zh <- state_sel_zh()$Name
    
    updateSelectInput(session, 'counties_zh', choices=unique(counties_zh))
  }) 
  
  county_select_zh <- reactive({
    state_sel_zh() %>% 
      filter(Name==input$counties_zh)%>%drop_na()
  })
  
  
###output
  
  output$barplot2_zh <- renderPlotly({
    p2_zh <- ggplot(county_select_zh())+theme_light()+
      geom_point(aes(x=Year, y=index(county_select_zh())))+
      geom_line(aes(x=Year, y=index(county_select_zh())),group=1)+labs(x="Year",y=input$metrics_zh)
   
    plotly2<-ggplotly(p2_zh)
    
    plotly2%>%
      style(text = round(index(county_select_zh()), 4),traces = 1) 
  })
  
  output$table2_zh<-DT::renderDataTable({
    DT::datatable(county_select_zh())
  })
  
  output$downloadData2_zh <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(county_select_zh(), file)
    }
  )
##############################################################################################  
## comparison stats
## Vikki Sui
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
    metric_select_vk() %>% 
      group_by(Year, State) %>% 
      summarise(Value = mean(Value, na.rm = TRUE)) %>% 
      pivot_wider(names_from = Year, values_from = Value)
  })
  
  
  year_sort_vk <- reactive({
    dt<-data_select_vk()
    temp_name<-colnames(dt)
    n <- dim(dt)[2]
    colnames(dt)[n]<-"use_this_sort"
    if(input$sort_vk == "ascending"){
      dt<-dt %>% 
        arrange(use_this_sort) %>% 
        mutate(State = factor(State, levels = fct_reorder(State, use_this_sort, .desc = FALSE)))
    }
    else if(input$sort_vk == "descending"){
      dt<-dt %>% 
        arrange(desc(use_this_sort)) %>% 
        mutate(State = factor(State, levels = fct_reorder(State, use_this_sort, .desc = TRUE)))
    }
    dt <- dt[1:min(max(input$top_n_vk,1),40),]
    colnames(dt)<-temp_name
    dt
  })
  
  
  output$year_change_plot_states_vk <- renderPlotly({
    dt <- year_sort_vk()
    n <- dim(dt)[2]
    temp_name <- colnames(dt)
    colnames(dt)[2:n] <- c(2:n)
    p222 <- dt %>% 
      pivot_longer(2:n) %>% 
      mutate(name = as.numeric(name)) %>% 
      ggplot(aes(x=name, y=value, color = State))+ 
      geom_line() + 
      geom_point() + 
      scale_x_continuous(breaks = 2:n, labels = temp_name[2:n]) + 
      xlab("year") + 
      ylab(input$metric_vk)
    ggplotly(p222)
  })
  
  output$year_change_data_states_vk <- DT::renderDataTable({
    year_sort_vk()
  })
  
  output$downloadid_vk <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(year_sort_vk(), con)
    }
  )
  
  
  base_metric_select2_vk <- reactive({
    counties_new %>% filter(base_metric == input$base_metric2_vk)
  })
  
  observeEvent(input$base_metric2_vk, {
    choice <- base_metric_select2_vk() %>% select(Metric) %>% unique() 
    updateSelectInput(session, "metric2_vk", choices=c(choice))
  })
  
  metric_select2_vk <- reactive({
    base_metric_select2_vk() %>% 
      filter(Metric == input$metric2_vk) %>% 
      filter(State == input$state2_vk)
  })
  
  data_select2_vk <- reactive({
    metric_select2_vk() %>% 
      group_by(Year, Name) %>% 
      summarise(Value = mean(Value, na.rm = TRUE)) %>% 
      pivot_wider(names_from = Year, values_from = Value)
  })
  
  
  year_sort2_vk <- reactive({
    dt <- data_select2_vk()
    temp_name <- colnames(dt)
    n <- dim(dt)[2]
    colnames(dt)[n] <- "use_this_sort"
    if(input$sort2_vk == "ascending"){
      dt <- dt %>% arrange(use_this_sort) %>% 
        mutate(Name = factor(Name, levels = fct_reorder(Name, use_this_sort, .desc = FALSE)))
    }
    else if(input$sort2_vk == "descending"){
      dt <- dt %>% arrange(desc(use_this_sort)) %>% 
        mutate(Name = factor(Name, levels = fct_reorder(Name, use_this_sort, .desc = TRUE)))
    }
    dt <- dt[1:min(max(input$top_n2_vk,1),40),]
    colnames(dt)<-temp_name
    dt
  })
  
  
  output$year_change_plot_counties_vk <- renderPlotly({
    dt <- year_sort2_vk()
    n <- dim(dt)[2]
    temp_name <- colnames(dt)
    colnames(dt)[2:n] <- c(2:n)
    p111 <- dt %>% 
      pivot_longer(2:n) %>% 
      mutate(name = as.numeric(name)) %>% 
      ggplot(aes(x=name, y=value, color = Name))+ 
      geom_line() + 
      geom_point() + 
      scale_x_continuous(breaks = 2:n, 
                         labels = temp_name[2:n]) + 
      xlab("year") + 
      ylab(input$metric2_vk)
    ggplotly(p111)
  })
  
  output$year_change_data_counties_vk <- DT::renderDataTable({
    year_sort2_vk()
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


