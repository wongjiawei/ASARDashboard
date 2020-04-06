function(input, output, session) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)
  
  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5
  
  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)
  
  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  dlCount <- downloadCount(pkgStream)
  
  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)
  
  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())
  
  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)

    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = "Downloads per sec (last 5 min)",
      icon = icon("area-chart"),
      color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })

  output$count <- renderValueBox({
    valueBox(
      value = dlCount(),
      subtitle = "Total downloads",
      icon = icon("download")
    )
  })

  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique users",
      icon = icon("users")
    )
  })

  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()

    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)

    bubbles(df$n, df$package, key = df$package)
  })

  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Package name" = package, "% of downloads" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)

  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- DT::renderDataTable({
                                          myFilteredDf <- filter1()
                                          DT::datatable(myFilteredDf)
                                        })
  filter1 <-  reactive({
    stb_4 %>% filter(region %in% input$RegionInput) %>%
              filter(place_of_residence %in% input$CountryInput)
  })
  
  observe({
    dt <- unique(stb_4$place_of_residence[stb_4$region %in% input$RegionInput])
    updatePickerInput(session, "CountryInput", choices = dt,selected = dt)
  })
  
  output$cluster_avg = renderPlot({
    myFilteredDf <- filter1()
    clusterList = cluster(myFilteredDf)
    clustertype <- input$ClusterType
    if (clustertype=='Ward') {
      plot(clusterList$ward)
    }
    else {
      plot(clusterList$avg)
    }
  })
  
  filter2 <-  reactive({
    stb_2 %>% filter(region %in% input$RegionInput) %>%
      filter(place_of_residence %in% input$CountryInput)
  })
  
  output$prediction_graph = renderPlot({
    myFilteredDf <- filter2()
    fore_arima = timeseriesPredict(myFilteredDf)
    plot(fore_arima)
  })
}


