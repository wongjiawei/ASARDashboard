function(input, output, session) {
  output$downloadCsv <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(filter4(), file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- DT::renderDataTable({
                                          myFilteredDf <- filter4()
                                          DT::datatable(myFilteredDf)
                                        }, options = list(autoWidth = TRUE, scrollX = TRUE))
  
  output$predictionTable <- renderFormattable({
    myFilteredDf <- filter2()
    filteredTable <- predictionTable(myFilteredDf)
    
    customGreen = "#71CA97"
    customGreen0 = "#DeF7E9"
    customRed = "#ff7f7f"
    improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", color = ifelse(x > 0 , customGreen, ifelse(x < 0 & x!='' , customRed, "white"))), x ~ icontext(ifelse(x>0 , "arrow-up", "arrow-down"), x))
    
    formattable(filteredTable, align =c("c","c","c","c","c"), list('M-O-M (%)'= improvement_formatter, 'Y-O-Y (%)'= improvement_formatter ))
  })
  
  filter1 <-  reactive({
    stb_4 %>% filter(region %in% input$RegionInput) %>%
              filter(place_of_residence %in% input$CountryInput)
  })
  
  observe({
    dt <- unique(sort(stb_2$place_of_residence[stb_2$region %in% input$RegionInput]))
    updatePickerInput(session, "CountryInput", choices = dt,selected = dt)
  })
  

  
  # output$cluster_avg = renderPlot({
  #   myFilteredDf <- filter1()
  #   clusterList = cluster(myFilteredDf)
  #   clustertype <- input$ClusterType
  #   if (clustertype=='Ward') {
  #     plot(clusterList$ward)
  #   }
  #   else {
  #     plot(clusterList$avg)
  #   }
  # })
  
  filter2 <-  reactive({
    stb_2 %>% filter(region %in% input$RegionInput) %>%
      filter(place_of_residence %in% input$CountryInput)
  })
  
  filter3 <-  reactive({
    stb_4 %>% filter(yearmonth %in% input$YearMonthInput)
  })
  
  filter4 <-  reactive({
    stb_4 %>% filter(region %in% input$RegionInput & place_of_residence %in% input$CountryInput & yearmonth %in% input$YearMonthInput)
  })
  
  output$prediction_graph = renderPlot({
    myFilteredDf <- filter2()
    fore_arima = timeseriesPredict(myFilteredDf)
    plot(fore_arima)
  })
  
  output$map <- renderGirafe({
    myFilteredDf <- filter3()
    ggiraph(print(worldMaps(myFilteredDf)), width_svg=10, zoom_max=10,width=1)
  })
  
  output$arrivalsbygender <- renderPlot({
    myFilteredDf = filter4()
    stb_4_2015_2019_gender = myFilteredDf[,1:5]
    stb_4_2015_2019_gender_melt = melt(stb_4_2015_2019_gender,id=c('month','region','place_of_residence'))
    stb_4_2015_2019_gender_melt <- aggregate(stb_4_2015_2019_gender_melt$value, by=list(variable=stb_4_2015_2019_gender_melt$variable), FUN=sum)
    stb_4_2015_2019_gender_melt <- stb_4_2015_2019_gender_melt %>% rename(
      value = "x"
    )
    ggplot(data = stb_4_2015_2019_gender_melt, aes(x=variable, x.label=TRUE, y = value, fill = variable)) +
      geom_bar(position = 'dodge',stat="identity") +
      labs(x ='Gender', y = '', fill = 'Gender') +
      theme_classic() + scale_y_continuous(labels = comma) + geom_text(aes(label=scales::comma(value)), position=position_dodge(width=0.9), vjust=-0.25)
  })
  
  output$arrivalsbyage <- renderPlot({
    myFilteredDf = filter4()
    stb_4_2015_2019 <- subset(myFilteredDf, as.numeric(format(myFilteredDf$month,'%Y'))>=2015 & as.numeric(format(myFilteredDf$month,'%Y'))<=2019)
    stb_4_2015_2019_age <- subset(stb_4_2015_2019, select = c(month,region,place_of_residence,`14 & Below`,`15 - 19`,`20 - 24`,`25 - 34`,`35 - 44`,`45 - 54`,`55 - 64`,`65 & Above`,`Age not stated`))
    stb_4_2015_2019_age_melt = melt(stb_4_2015_2019_age,id=c('month','region','place_of_residence'))
    stb_4_2015_2019_age_melt <- aggregate(stb_4_2015_2019_age_melt$value, by=list(variable=stb_4_2015_2019_age_melt$variable), FUN=sum)
    stb_4_2015_2019_age_melt <- stb_4_2015_2019_age_melt %>% rename(
      value = "x"
    )
    ggplot(data = stb_4_2015_2019_age_melt, aes(x=variable, xlabel=TRUE, y=value, fill=variable)) +
      geom_bar(position = 'dodge', stat="identity" ) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x ="Age Groups", y = "Number of Arrivals") +
      theme(legend.position = "none") + 
      scale_y_continuous(labels = comma) + geom_text(aes(label=scales::comma(value)), position=position_dodge(width=0.9), vjust=-0.25)
  })
  
  output$visitduration <- renderPlot({
    myFilteredDf = filter4()
    stb_4_2015_2019 <- subset(myFilteredDf, as.numeric(format(myFilteredDf$month,'%Y'))>=2015 & as.numeric(format(myFilteredDf$month,'%Y'))<=2019)
    stb_4_2015_2019_vdur = subset(stb_4_2015_2019, select = -c(male,female, not_stated_gender, average_age, `14 & Below`,`15 - 19`,`20 - 24`,`25 - 34`,`35 - 44`,`45 - 54`,`55 - 64`,`65 & Above`,`Age not stated`,average_duration,visitor_days))
    stb_4_2015_2019_vdur = stb_4_2015_2019_vdur[, !colnames(stb_4_2015_2019_vdur) %in% c('yearmonth','Value')]
    stb_4_2015_2019_vdur_melt = melt(stb_4_2015_2019_vdur,id=c('month','region','place_of_residence'))
    stb_4_2015_2019_vdur_melt <- aggregate(stb_4_2015_2019_vdur_melt$value, by=list(variable=stb_4_2015_2019_vdur_melt$variable), FUN=sum)
    stb_4_2015_2019_vdur_melt <- stb_4_2015_2019_vdur_melt %>% rename(
      value = "x"
    )
    ggplot(data = stb_4_2015_2019_vdur_melt, aes(x=variable, xlabel=TRUE, y=value, fill=variable)) +
      geom_bar(position = 'dodge',stat="identity") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x ="Duration", y = "Frequency of Occurrence")+
      theme(legend.position = "none") + scale_y_continuous(labels = comma) + geom_text(aes(label=scales::comma(value)), position=position_dodge(width=0.9), vjust=-0.25)
  })
  
  
}


