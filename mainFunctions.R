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
  
  ##(Training data) Subset to keep only Jan-2015 to Dec-2018 records
  stb_2_Overall_2015_2018 <- subset(stb_2_Overall, as.numeric(format(stb_2_Overall$month,'%Y'))>=2015 & as.numeric(format(stb_2_Overall$month,'%Y'))<=2018)
  
  ##(Testing data) Subset to keep only Jan-2019 to Dec-2019 records
  stb_2_Overall_2019 <- subset(stb_2_Overall, as.numeric(format(stb_2_Overall$month,'%Y'))==2019)
  
  #Convert into timeseries data and plot the timeseries
  dat_ts <- ts(stb_2_Overall_2015_2018[, 2], start = c(2015, 1), end = c(2018, 12), frequency = 12)
  plot(dat_ts, ylab="Total Arrivals")
  
  
  #######METHOD 1: Naive Forecasting method########
  naive_mod <- naive(dat_ts, h = 12)
  summary(naive_mod) 
  
  #Predict and test using 2019 data
  stb_2_Overall_2019$naive = 1603217
  mape(stb_2_Overall_2019$total_arrival, stb_2_Overall_2019$naive) 
  
  #######METHOD 2: Exponential Smoothing method########
  se_model <- ses(dat_ts, h = 12)
  summary(se_model)
  
  #Predict and test using 2019 data
  df_fc = as.data.frame(se_model)
  stb_2_Overall_2019$simplexp = df_fc$`Point Forecast`
  mape(stb_2_Overall_2019$total_arrival, stb_2_Overall_2019$simplexp)  
  
  #######METHOD 3: Holt's Trend method########
  holt_model <- holt(dat_ts, h = 12)
  summary(holt_model) 
  
  #Predict and test using 2019 data
  df_holt = as.data.frame(holt_model)
  stb_2_Overall_2019$holt = df_holt$`Point Forecast`
  mape(stb_2_Overall_2019$total_arrival, stb_2_Overall_2019$holt) 
  
  #######METHOD 4: Auto ARIMA method########
  arima_model <- auto.arima(dat_ts)
  summary(arima_model)
  
  #Predict and test using 2019 data
  fore_arima = forecast::forecast(arima_model, h=12)
  df_arima = as.data.frame(fore_arima)
  stb_2_Overall_2019$arima = df_arima$`Point Forecast`
  mape(stb_2_Overall_2019$total_arrival, stb_2_Overall_2019$arima)
  
  #Plot the forecast chart out
  return(fore_arima)
}

