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
    menuItem('Comparison Stats', 
             tabName = 'stats', 
             icon = icon("chart-line"), 
             menuSubItem('By State', 
                         tabName = 'comparison_state_stats'
             ),
             menuSubItem('By County', 
                         tabName = 'comparison_county_stats'
             ) 
    ), 
    menuItem('Specific Stats', 
             tabName = 'stats', 
             icon = icon("chart-line"), 
             menuSubItem('By State', 
                         tabName = 'specific_state_stats'
             ),
             menuSubItem('By County', 
                         tabName = 'specific_county_stats'
             ) 
    ),  
    menuItem('Reference', 
             tabName = 'ref', 
             icon = icon('ad'))
  )
)
  
  
# Body 
body <- dashboardBody(
  tags$head(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
  ),
  tabItems(
    # Home
    tabItem(tabName='Home',
            fluidPage(
              img(src = 'https://www.wealthmanagement.com/sites/wealthmanagement.com/files/us-map-money.jpg',
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
                       h4(strong("Education"), " data includes the % of adults with various levels of education"),
                       h4(strong("Unemployment"), " data includes the unemployment rate and the total civilian labor force"),
                       h4(strong("Poverty"), " data includes the poverty rates for the entire population and for people ages 0-17, as well as median household income"),
                       h4(strong("Population"), " data includes total population estimates, as well as rates of natural population changes (from births and deaths) and net migration")
              )
            )
    ),
    # States map
    tabItem(tabName = 'state_map', 
            fluidRow(
              box(h3(strong('The State Map')), width=9, status='primary',
                  leafletOutput('stmaps_lyq', height = 580),
              ),
              box(status='info', width=3,
                  radioButtons(inputId = 'chs_lyq', 
                               label = 'Please Choose:', 
                               choices = c('Snapshot', 'Changes by time', '%Change by time'), 
                               selected = 'Snapshot'
                  ),
                  selectInput(inputId = 'basic_metric_lyq', 
                              label = 'Category',
                              choices = c('Education', 'Employment', 'Population', 'Poverty'), 
                              selected = 'Population'
                  ),
                  selectInput(inputId = 'metric_lyq', 
                              label = 'Metrics', 
                              choices = 'Population'
                  ), 
                  selectInput(inputId = 'year_lyq', 
                              label = 'Year', 
                              choices = c('2010', '2011', '2012', '2013', '2014', 
                                          '2015', '2016', '2017', '2018')
                  ), 
                  uiOutput('if_end_lyq')
              )
            )
    ),
    # County map written by Jinxu Xiang
    tabItem(tabName = 'county_map',
            fluidRow(
              box(h3(strong('The County Map')), width=9, status='primary',
                  leafletOutput('stmaps_xjx', height = 580)
              ),
              box(width = 3, status='info',
                  radioButtons(inputId = 'chs_xjx', label = 'Please Choose:', 
                               choices = c('Snapshot', 'Changes by time', '%Change by time'), 
                               selected = 'Snapshot'
                  ),
                  selectInput(inputId = 'state_xjx', 
                              label = 'State',
                              choices = state_choise_xjx, 
                              selected = "AL"
                  ),
                  selectInput(inputId = 'basic_metric_xjx', 
                              label = 'Category',
                              choices = c('Education', 'Population', 'Employment', 'Poverty'), 
                              selected = 'Education'
                  ),
                  selectInput(inputId = 'metric_xjx', 
                              label = 'Metric', 
                              choices = 'Population'
                  ), 
                  selectInput(inputId = 'year_xjx', 
                              label = 'Year', 
                              choices = '' 
                  ),
                  uiOutput('if_end_xjx')
              )
            )
    ),
    # State Statd
    tabItem(tabName = 'comparison_state_stats', 
            "code of comparison state stats"
    ),
    tabItem(tabName = 'comparison_county_stats', 
            "code of comparison county stats"
    ),
    tabItem(tabName="specific_state_stats",
            "......"
    ),
    tabItem(tabName="specific_county_stats", 
            "CODE"
    ),
    tabItem(tabName = 'ref',
            fluidPage(
              img(src = "us-map-money.jpg",
                  style = 'position: absolute; opacity: 0.2'),
              fluidRow(h2("Data Sources"),
                       h4("This app uses data from ",
                          tags$a(href = "https://catalog.data.gov/dataset/county-level-data-sets", "data.gov"),
                          ", specifically four state-level and county-level socioeconomic data sets:"),
                       h4(strong("Education"), " data collected by the U.S. Census Bureau and American Community Survey"),
                       h4(strong("Unemployment"), " data collected by the U.S. Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS) Program"),
                       h4(strong("Poverty"), " estimates from the U.S. Census Bureau, Small Area Income and Poverty Estimates (SAIPE) Program"),
                       h4(strong("Population"), " estimates from the U.S. Census Bureau"),
                       h2("Image Sources"),
                       h4(tags$a(href = "https://www.wealthmanagement.com/sites/wealthmanagement.com/files/us-map-money.jpg",
                                 "https://www.wealthmanagement.com/sites/wealthmanagement.com/files/us-map-money.jpg")
                          )
                       )
              )
            )
  )
)


ui <- dashboardPage(
  skin='green',
  header=header,
  sidebar=sidebar,
  body=body
)
