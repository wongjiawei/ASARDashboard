ui <- fluidPage(
  tags$style(make_css(list('.box-title', 
                           c('font-weight'), 
                           c('bold')))),
  dashboardPage(
    dashboardHeader(title=span("Singapore's Tourism Market", style="font-size:16px")),
    dashboardSidebar(
      pickerInput("YearMonthInput","Choose the year month of interest:", choices=unique(stb_2$yearmonth), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$yearmonth)),
      sidebarMenu(
          menuItem("Dashboard", tabName = "Dashboard"),
          menuItem("Forecast", tabName = "Prediction"),
          menuItem("Raw Data", tabName = "RawData")
        )
        
      ),
    
    # dashboardBody first try (works no problem):
    #dashboardBody(DT::dataTableOutput(outputId = 'mytable'))
    
    #dashboardBody second try (data table does not appear):
    dashboardBody(
      tabItems(
        tabItem(tabName = "Dashboard",
                fluidRow(box(title='Heatmap of tourists from all over the world', status = 'primary', solidHeader = TRUE, collapsible = TRUE, width=12, style='padding:10px',
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
                fluidRow(box(width=6, style='padding:10px', 
                             pickerInput("RegionInput","Choose the Region of interest:", choices=unique(stb_2$region), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$region))),
                         box(width=6, style='padding:10px', 
                             pickerInput("CountryInput","Choose the Country of interest:", choices=stb_2$place_of_residence, options = list(`actions-box` = T),multiple = T, selected=stb_2$place_of_residence),
                         )),
                fluidRow(box(title = "Master Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, style='padding:10px',
                             DT::dataTableOutput(outputId = 'rawtable'), style = "overflow-y: scroll;overflow-x: scroll;"),
                         downloadButton("downloadCsv", "Download as CSV"))),
        
        tabItem(tabName = "Prediction",
                fluidRow(box(width=6, style='padding:10px', 
                          pickerInput("RegionInput","Choose the Region of interest:", choices=unique(stb_2$region), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$region))),
                        box(width=6, style='padding:10px', 
                          pickerInput("CountryInput","Choose the Country of interest:", choices=stb_2$place_of_residence, options = list(`actions-box` = T),multiple = T, selected=stb_2$place_of_residence),
                )),
                fluidRow(box(title = "Time Series Chart with Forecast for 2020", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', height = 500,
                             plotOutput('prediction_graph')),
                         box(title = "Number of Tourist Forecasted for 2020", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', height = 500,
                             DT::dataTableOutput(outputId = 'predictionTable'))))
      ))
  ))
