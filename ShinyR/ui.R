ui <- fluidPage(
  tags$style(make_css(list('.box-title', 
                           c('font-weight'), 
                           c('bold')))),
  dashboardPage(
    dashboardHeader(title=span("Singapore's Tourism Market", style="font-size:15px; font-weight:bold")),
    dashboardSidebar(
      tags$h4("Filter that applies to all charts except the forecast", style='font-weight:bold; text-decoration: underline; padding:16px'),
      pickerInput("YearMonthInput","Year-Month:", choices=unique(stb_2$yearmonth), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$yearmonth)),
      tags$h4("Filters that apply to all charts except the map", style='font-weight:bold; text-decoration: underline; padding:16px'),
      pickerInput("RegionInput","Region:", choices=unique(sort(stb_2$region)), options = list(`actions-box` = T),multiple = T, selected=unique(stb_2$region)),
      pickerInput("CountryInput","Country:", choices=unique(sort(stb_2$place_of_residence)), options = list(`actions-box` = T),multiple = T, selected=unique(sort(stb_2$place_of_residence))),
    
      sidebarMenu(
          menuItem("Profile of tourists", tabName = "Dashboard"),
          menuItem("Forecasting number of tourists", tabName = "Prediction"),
          menuItem("Detailed figures", tabName = "RawData")
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
                fluidRow(box(title = "Detailed Master Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=12, style='padding:10px',
                             DT::dataTableOutput(outputId = 'rawtable'), style = "overflow-y: scroll;overflow-x: scroll;"),
                         downloadButton("downloadCsv", "Download as CSV"))),
        
        tabItem(tabName = "Prediction",
                tags$h3('This tab shows the forecast for each region/country. Please select a region/country from the filter.', style='font-weight:bold; text-decoration: underline'),
                fluidRow(box(title = "Time Series Chart with Forecast for 2020", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', height = 550,
                             plotOutput('prediction_graph')),
                         box(title = "Number of Tourists Forecasted for 2020", status = "primary", solidHeader = TRUE, collapsible = TRUE, width=6, style='padding:10px', height = 550,
                             formattableOutput(outputId = 'predictionTable'))))
      ))
  ))
