# Header
header <- dashboardHeader(title='Project_2 Group_7')


# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Home', 
             tabName = 'Home', 
             icon = icon('home')
    ), 
    menuItem('Maps', 
             tabName='map', 
             icon = icon("map"), 
             menuSubItem('State Map', 
                         tabName = 'state_map'
                         ),
             menuSubItem('County Map', 
                         tabName = 'county_map'
                         )
             
    ),
    menuItem('Stats', 
             tabName = 'map', 
             icon = icon("chart-line"), 
             menuSubItem('By State', 
                         tabName = 'state_stats'
             ),
             menuSubItem('By County', 
                         tabName = 'county_stats'
             ) 
    ), 
    menuItem('Reference', 
             tabName = 'ref', 
             icon = icon('alipay'))
  )
)
  
  
# Body 
body <- dashboardBody(
  tabItems(
    # Home
    tabItem(tabName='Home',
            fluidRow(
              tabBox(title='', id='home_tabs', width = 12,
                     tabPanel(h4('Project Description') ,
                              'This is a summary'),
                     tabPanel(h4('User Guide'), 
                              'Here is the user guide')
                )
              #box(width=12, DT::dataTableOutput('test'))
              )
                     
      ),
    # States MAPS
    tabItem(tabName = 'state_map', 
            "code of state map"
    ),
    # County Maps
    tabItem(tabName = 'county_map',
            fluidRow(
              box('The County Map', width=9, status='primary',
                  leafletOutput('stmaps', height=750)
              ),
              box(width = 3, status='info',
                  selectInput(inputId = 'state', 
                              label = 'State',
                              choices = c(state.abb[1],state.abb[3:10], state.abb[12:length(state.abb)]), 
                              selected = "AL"
                  ),
                  selectInput(inputId = 'basic_metric', 
                              label = 'Metrics',
                              choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                              selected = 'Population'
                  ),
                  selectInput(inputId = 'metric', 
                              label = 'Sub-Metrics', 
                              choices = 'Population'
                  ), 
                  selectInput(inputId = 'year', 
                              label = 'Year', 
                              choices = '' 
                  )
              )
            )
            ),
    # State Statd
    tabItem(tabName = 'state_stats',
            "code of stats by states"
           ),
    # Counties
    tabItem(tabName = 'county_stats',
            "code of stats by counties"
            ), 
    tabItem(tabName = 'ref', 
            'Here shows the references')
  )
)





ui <- dashboardPage(
  skin='green',
  header=header,
  sidebar=sidebar,
  body=body
)
