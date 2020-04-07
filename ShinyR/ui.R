ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "My App"),
    dashboardSidebar(
      pickerInput("YearMonthInput","Choose the month year to look at:", choices=unique(stb_2$yearmonth), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$yearmonth)),
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
                 fluidRow(box(width=12, style='padding:10px',
                          girafeOutput('map'))),
                 fluidRow(box(width=6, style='padding:10px', 
                            pickerInput("RegionInput","Choose the Region of interest:", choices=unique(stb_2$region), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$region))),
                          box(width=6, style='padding:10px', 
                            pickerInput("CountryInput","Choose the Country of interest:", choices=stb_2$place_of_residence, options = list(`actions-box` = T),multiple = T, selected=stb_2$place_of_residence),
                 )),
                 fluidRow(box(title = "Arrival By Gender", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', 
                     plotOutput("arrivalsbygender", height = "300px")
                 ),
                   box(title = "Arrival By Age Groups", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', 
                     plotOutput("arrivalsbyage", height = "300px")
                 )),
                 fluidRow(
                   box(title = "Visit Duration", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, style='padding:10px', 
                     plotOutput("visitduration", height = "300px")
                 ))),
        
        tabItem(tabName = "RawData",
                DT::dataTableOutput(outputId = 'rawtable')),
        
        tabItem(tabName = "Prediction",
                h2("Predictions"),
                fluidRow(box(width=12, style='padding:10px',
                             plotOutput('prediction_graph'))))
      ))
  ))
