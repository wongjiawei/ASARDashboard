ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "My App"),
    dashboardSidebar(
      pickerInput("RegionInput","Region", choices=unique(stb_4$region), options = list(`actions-box` = T),multiple = T, selected=unique(stb_4$region)),
      pickerInput("CountryInput","Country", choices=stb_4$place_of_residence, options = list(`actions-box` = T),multiple = T, selected=stb_4$place_of_residence),
      sidebarMenu(
          menuItem("Dashboard", tabName = "Dashboard"),
          menuItem("Raw Data", tabName = "RawData"),
          menuItem("Forecast", tabName = "Prediction")
        )
        
      ),
    
    # dashboardBody first try (works no problem):
    #dashboardBody(DT::dataTableOutput(outputId = 'mytable'))
    
    #dashboardBody second try (data table does not appear):
    dashboardBody(
      tabItems(
        tabItem( tabName = "Dashboard",
                 fluidRow(box(width=6, height=80, style='padding:10px',
                          pickerInput("ClusterType","Cluster Type:", choices=c('Ward','Average'), options = list(`actions-box` = F),multiple = F, selected='Ward'))),
                 fluidRow(box(width=12, style='padding:10px',
                          plotOutput('cluster_avg')))),
        
        tabItem(tabName = "RawData",
                DT::dataTableOutput(outputId = 'rawtable')),
        
        tabItem(tabName = "Prediction",
                h2("Predictions"),
                fluidRow(box(width=12, style='padding:10px',
                             plotOutput('prediction_graph'))))
      ))
  ))