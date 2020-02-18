# Header
header <- dashboardHeader(title='Project_2 Group_7')


# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Home', 
             tabName='Home'
            ), 
    menuItem('Map', 
             tabName='map'
            )
  )
)
  
  
# Body 
body <- dashboardBody(
  tabItems(
    # Home
    tabItem(tabName='Home',
              fluidRow(
                box(width = 12, 
                    title = "Proj Summary", 
                    status = "primary"
                    ),
                box(width=12, 
                    title="...", 
                    status='primary')
              )
    ),
    # MAPS
    tabItem(tabName = 'map', 
            fluidRow(
              box(width = 4, status='info',
                  selectInput(inputId = 'basic_metric', 
                              label = 'Metrics',
                              choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                              selected = 'Population'
                  )),
              box(width =4, status='info',
                  selectInput(inputId = 'metric', 
                              label = 'Sub-Metrics', 
                              choices = 'Population'
                  )),
              box(width = 4, status='info',
                  selectInput(inputId = 'year', 
                              label = 'Year', 
                              choices = '' 
                  )
            )
            ),#end FluidRow
            fluidRow(
              box('The State Map', width=12, status='primary',
                  leafletOutput('stmaps', height=600)
              )
            )#end FluidRow
           )#end tabItem
          )#end tabItems
  )





ui <- dashboardPage(
  skin='green',
  header=header,
  sidebar=sidebar,
  body=body
)
