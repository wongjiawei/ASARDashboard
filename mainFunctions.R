transformData <- function(stb_4,stb_2) {
  ## Rename columns
  stb_2 <- stb_2 %>% rename(
    month = "Month", region = "Region", place_of_residence = "Place of Residence", arrivals = "Number of Arrivals"
  )
  stb_4 <- stb_4 %>% rename(
    month = "Month", region = "Region", place_of_residence = "Place of Residence", 
    # gender
    male = "Male", female = "Female", not_stated_gender = "Not Stated...6", 
    # age
    age_14andbelow = "14 & Below", age_15to19 = "15 - 19",  age_20to24 = "20 - 24", age_25to34 = "25 - 34", age_35to44 = "35 - 44", 
    age_45to54 = "45 - 54", age_55to64 = "55 - 64", age_65andabove = "65 & Above", not_stated_age = "Not Stated...15", average_age = "Average Age", 
    # length of stay
    dur_under1day = "Under 1 Day", dur_1day = "1 Day", dur_2days = "2 Days", dur_3days = "3 Days", dur_4days = "4 Days", dur_5days = "5 Days", 
    dur_6days = "6 Days", dur_7days = "7 Days", dur_8to10days = "8 - 10 Days", dur_11to14days = "11 - 14 Days", dur_15to29days = "15 - 29 Days", 
    dur_30to59days = "30 - 59 Days", dur_60daysandover = "60 Days & Over", average_duration = "Average Length of Stay (Days)", visitor_days = "Visitor Days"
  )
  
  ## Convert "month" column to date format
  stb_2$month <- as.Date(paste("01-", stb_2$month, sep = ""), format = "%d-%b-%y")
  stb_4$month <- as.Date(paste("01-", stb_4$month, sep = ""), format = "%d-%b-%y")
  stb_4$yearmonth <- format(as.Date(stb_4$month), "%Y-%m")
  stb_2$yearmonth <- format(as.Date(stb_2$month), "%Y-%m")
  
  ## Calculate Total
  stb_4$Value = stb_4$female + stb_4$male
  old_names <- c("USA", "Vietnam", "Hong Kong SAR", "Taiwan","South Korea", "UK","South Africa (Rep of)")
  new_names <- c("United States of America", "Viet Nam", "Hong Kong, SAR China", "Taiwan, Republic of China", "Korea (South)", "	United Kingdom", "South Africa")
  for (i in 1:length(old_names)){
    stb_4$place_of_residence[stb_4$place_of_residence == old_names[i]] <- new_names[i]
  }
  
  returnList = list("stb_2" = stb_2, "stb_4" = stb_4)
  return(returnList)
}

cluster <- function(stb_4){
  stb_4_clus <- stb_4[stb_4$month >= as.Date("2019-01-01") & stb_4$month <= as.Date("2019-12-01"), ] # subset for 2019 rows
  stb_4_clus <- aggregate(. ~ region + place_of_residence, stb_4_clus, sum) # sum all 2019 data by country
  stb_4_clus <- stb_4_clus[ , !(names(stb_4_clus) %in% c("region", "month", "average_duration", "visitor_days"))] # remove irrelevant cols
  stb_4_clus$total <- stb_4_clus$male + stb_4_clus$female + stb_4_clus$not_stated_gender # create total col
  
  ## Convert absolute numbers into percentages
  # gender
  stb_4_clus$pct_male <- stb_4_clus$male / stb_4_clus$total * 100
  stb_4_clus$pct_female <- stb_4_clus$female / stb_4_clus$total * 100
  # age
  stb_4_clus$pct_14andbelow <- stb_4_clus$age_14andbelow / stb_4_clus$total * 100
  stb_4_clus$pct_15to24 <- (stb_4_clus$age_15to19 +stb_4_clus$age_20to24) / stb_4_clus$total * 100
  stb_4_clus$pct_25to34 <- stb_4_clus$age_25to34 / stb_4_clus$total * 100
  stb_4_clus$pct_35to44 <- stb_4_clus$age_35to44 / stb_4_clus$total * 100
  stb_4_clus$pct_45to54 <- stb_4_clus$age_45to54 / stb_4_clus$total * 100
  stb_4_clus$pct_55to64 <- stb_4_clus$age_55to64 / stb_4_clus$total * 100
  stb_4_clus$pct_65andabove <- stb_4_clus$age_65andabove / stb_4_clus$total * 100
  # length of stay
  stb_4_clus$pct_under1day <- stb_4_clus$dur_under1day / stb_4_clus$total * 100
  stb_4_clus$pct_1to3days <- (stb_4_clus$dur_1day + stb_4_clus$dur_2days + stb_4_clus$dur_3days)/ stb_4_clus$total * 100
  stb_4_clus$pct_4to7days <- (stb_4_clus$dur_4days + stb_4_clus$dur_5days + stb_4_clus$dur_6days + stb_4_clus$dur_7days) / stb_4_clus$total * 100
  stb_4_clus$pct_8to14days <- (stb_4_clus$dur_8to10days + stb_4_clus$dur_11to14days) / stb_4_clus$total * 100
  stb_4_clus$pct_15to29days <- stb_4_clus$dur_15to29days / stb_4_clus$total * 100
  stb_4_clus$pct_30daysandover <- (stb_4_clus$dur_30to59days + stb_4_clus$dur_60daysandover) / stb_4_clus$total * 100
  
  ## Input countries as row names
  stb_4_clus_lab <- stb_4_clus %>% select(pct_male:pct_30daysandover)
  rownames(stb_4_clus_lab) <- stb_4_clus[ , 1]
  
  ## Perform clustering
  dist_mat <- dist(stb_4_clus_lab, method = 'euclidean')
  hclust_ward <- hclust(dist_mat, method = 'ward.D')
  hclust_avg <- hclust(dist_mat, method = 'average')
  
  ## Return Dendogram
  returnList = list('ward' = hclust_ward, 'avg' = hclust_avg)
  return(returnList)
}

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


timeseriesPredict <- function(stb_2) {
  ##Subset to keep only Jan-2015 to Dec-2019 records
  stb_2_2015_2019 <- subset(stb_2, as.numeric(format(stb_2$month,'%Y'))>=2015 & as.numeric(format(stb_2$month,'%Y'))<=2019)
  
  ## Create data subsets
  
  #Overall Arrival
  stb_2_Overall <- stb_2_2015_2019 %>% group_by(month) %>% summarise(total_arrival = sum(arrivals))
  
  #Arrival by Region
  stb_2_Region <- stb_2_2015_2019 %>% group_by(month, region) %>% summarise(total_arrival_region = sum(arrivals))
  
  #Arrival by Country
  stb_2_Country <- stb_2_2015_2019 %>% group_by(month, place_of_residence) %>% summarise(total_arrival_country = sum(arrivals))
  
  ## Subset to keep only Jan-2016 to Dec-2019 records
  stb_2_Overall_2016_2019 <- subset(stb_2_Overall, as.numeric(format(stb_2_Overall$month,'%Y'))>=2016 & as.numeric(format(stb_2_Overall$month,'%Y'))<=2019)
  
  #Convert into timeseries data and plot the timeseries
  dat_ts_2016_2019 <- ts(stb_2_Overall_2016_2019[, 2], start = c(2016, 1), end = c(2019, 12), frequency = 12)
  
  #######Forecast using Auto ARIMA method########
  arima_model_2016_2019 <- auto.arima(dat_ts_2016_2019)
  fore_arima_2016_2019 = forecast::forecast(arima_model_2016_2019, h=12)
  df_arima_2016_2019 = as.data.frame(fore_arima_2016_2019)
  df_arima_2016_2019
  return(fore_arima_2016_2019)
}

worldMaps <- function(stb_4 ) {
  url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
  iso_codes <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="CountryCode"]') %>%
    html_table()
  iso_codes <- iso_codes[[1]][, -1]
  iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
  names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
  
  world_data <- ggplot2::map_data('world')
  world_data <- fortify(world_data)
  
  stb_4$Value = as.numeric(stb_4$Value)
  stb_4['ISO3'] <- iso_codes$ISO3[match(stb_4$place_of_residence, iso_codes$Country)]
  world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]
  
  old_names1 <- c("UK", "South Korea", "Taiwan", "USA", "Vietnam")
  new_names1 <- c("United Kingdom", "Korea (South)", "Taiwan, Republic of China","United States of America", "Viet Nam")
  for (i in 1:length(old_names1)){
    world_data$region[world_data$region == old_names1[i]] <- new_names1[i]
  }
  
  ##it's time to define the function that we'll use for building our world maps.
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "right",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  plotdf <- stb_4
  world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'grey70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", region, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    labs(fill = NULL, title = NULL, x = NULL, y = NULL) + 
    my_theme()
  
  return(g)
}


