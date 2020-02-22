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
    menuItem('Stat', 
             tabName = 'map', 
             icon = icon("chart-line"), 
             menuSubItem('Comparison', 
                         tabName = 'state_stats'
             ),
             menuSubItem('Sepecific', 
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
            fluidPage(
              img(src = "us-map-money.jpg",
                  style = 'position: absolute; opacity: 0.2'),
              fluidRow(h1("State and County Economic Data"),
                       h4("By Zidi Hong, Yuqiao Liu, Michael Petkun, Vikki Sui, and Jinxu Xiang"),
                       h2("Overview"),
                       h4("The United States is a nation of wide geographic and economic diversity. While high-level statistics may illustrate broad patterns in the country's well-being, one must explore trends on a more granular level to truly understand regional economic health."),
                       h4("With that in mind, this app is a tool for real estate developers and investors who are looking for areas that are ripe for investment. For example, a developer of luxury buildings may envision opportunities in counties with high income levels and a growing population. A speculative investor, on the other hand, may seek undervalued investments in areas with high, but falling, unemployment or poverty rates. This app gives users the tools to discover these opportunities by identifying and further examining states and counties that exhibit certain characteristics from a wide array of economic data."),
                       h2("App Guide"),
                       h4(strong("Maps: "), "This tab contains maps (by state and by county), shaded by user-chosen metrics. The user can choose the relevant region, metrics, and timeframe that they wish to analyze."),
                       h4(strong("Comparison Stats: "), "This tab allows the user to identify interesting areas by comparing across states or across counties within a state. For example, the user can view the top 10 Virginia counties in a chosen metric, as well as how those counties have evolved over time."),
                       h4(strong("Specific Stats: "), "This tab allows the user to zoom in on a specific state or county of interest, looking at how it has evolved over time and how it stacks up against other regions."),
                       h2("Data"),
                       h4("This app uses data from ",
                          tags$a(href = "https://catalog.data.gov/dataset/county-level-data-sets", "data.gov"),
                          ", specifically four state-level and county-level socioeconomic data sets:"),
                       h4(strong("Education"), " data collected by the U.S. Census Bureau and American Community Survey"),
                       h4(strong("Unemployment"), " data collected by the U.S. Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS) Program"),
                       h4(strong("Poverty"), " estimates from the U.S. Census Bureau, Small Area Income and Poverty Estimates (SAIPE) Program"),
                       h4(strong("Population"), " estimates from the U.S. Census Bureau")
                       )
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
                  radioButtons('chs', label = 'Please Choose:', 
                               choices = c('Snapshot', 'Changes by time', '%Change by time'), 
                               selected = 'Snapshot'
                  ),
                  selectInput(inputId = 'state', 
                              label = 'State',
                              choices = state_choise, 
                              selected = "AL"
                  ),
                  selectInput(inputId = 'basic_metric', 
                              label = 'Metrics',
                              choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                              selected = 'Education'
                  ),
                  selectInput(inputId = 'metric', 
                              label = 'Sub-Metrics', 
                              choices = 'Population'
                  ), 
                  selectInput(inputId = 'year', 
                              label = 'Year', 
                              choices = '' 
                  ),
                  uiOutput('if_end')
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
