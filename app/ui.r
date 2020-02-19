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
    )
  )
)
  
  
# Body 
body <- dashboardBody(
  tabItems(
    # Home
    tabItem(tabName='Home',
            fluidRow(
              tabBox(title='', id='home_tabs', width = 12,
                     tabPanel(h3('Introduction') ,
                              'This is a summary'),
                     tabPanel(h3('User Guide'), 
                              'Here is the user guide')
                )
              #box(width=12, DT::dataTableOutput('test'))
              )
                     
      ),
    # States MAPS
    tabItem(tabName = 'state_map', 
            fluidRow(
              box('The State Map', width=9, status='primary',
                  leafletOutput('stmaps', height=700)
              ),
              box(width = 3, status='info',
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
    # County Maps
    tabItem(tabName = 'county_map',
            "code of county map"
            ),
    # State Statd
    tabItem(tabName = 'state_stats',
            "code of stats by states"
           ),
    tabItem(tabName = 'county_stats',
            "code of stats by counties"
            )
  )
)





ui <- dashboardPage(
  skin='green',
  header=header,
  sidebar=sidebar,
  body=body
)
