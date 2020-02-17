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
              box('The State Map', width=9, status='primary',
                  leafletOutput('stmaps')
                  ),
              box(title = 'Select ', width =3, status='info',
                  selectInput(inputId = 'basic_metric', 
                              label = 'Metrics:',
                              choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                              selected = 'Population'
                              ),
                  selectInput(inputId = 'metric', 
                              label = ' ', 
                              choices = 'Population'
                              ),
                  selectInput(inputId = 'year', 
                              label = 'Year:', 
                              choices = '', 
                              selected = ' '
                              )
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
