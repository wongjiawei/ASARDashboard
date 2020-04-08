#################################################################################################################################################
#### 1. DATA PREPARATION ----------
#################################################################################################################################################

## Clear environment
rm(list=ls())

## Packages used
library(readxl)
library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)
library(forecast)
library(tseries)
library(stlplus)

#library(readr)
#library(fpp2)
#library(TTR)



## Read in datasets
stb_2 <- read_excel("./ShinyR/data/2.0.xlsx")
stb_4 <- read_excel("./ShinyR/data/4.0.xlsx")

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

##Subset to keep only Jan-2015 to Dec-2019 records
stb_2_2015_2019 <- subset(stb_2, as.numeric(format(stb_2$month,'%Y'))>=2015 & as.numeric(format(stb_2$month,'%Y'))<=2019)
stb_4_2015_2019 <- subset(stb_4, as.numeric(format(stb_4$month,'%Y'))>=2015 & as.numeric(format(stb_4$month,'%Y'))<=2019)

## Create data subsets

#Overall Arrival
stb_2_Overall <- stb_2_2015_2019 %>% group_by(month) %>% summarise(total_arrival = sum(arrivals))

#Arrival by Region
stb_2_Region <- stb_2_2015_2019 %>% group_by(month, region) %>% summarise(total_arrival_region = sum(arrivals))

#Arrival by Country
stb_2_Country <- stb_2_2015_2019 %>% group_by(month, place_of_residence) %>% summarise(total_arrival_country = sum(arrivals))

#################################################################################################################################################
#### 2. DESCRIPTIVE STATISTICS ----------
#################################################################################################################################################

## Agglomerative Hierarchical Clustering (Using stb_4)

## Create overall 2019 arrivals by country by demographics
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

## Plot dendrogram
plot(hclust_ward)
plot(hclust_avg)

## Clustering Results
cluster1 <- c("Thailand", "Vietnam")
cluster2 <- c("Netherlands", "USA", "Germany")
cluster3 <- c("Canada", "UK")
cluster4 <- c("South Korea", "Taiwan", "China")
cluster5 <- c("France", "South Africa")
cluster6 <- c("Philippines", "Russian Federation")
cluster7 <- c("Australia", "New Zealand")
cluster8 <- c("Indonesia", "Malaysia")
cluster9 <- c("Hong Kong", "Japan")
cluster10 <- c("India", "Others")

similarcountries <- list(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10)
sapply(similarcountries, function(x) x[grep('Name',names(x))]=='India')

#################################################################################################################################################
#### 3. Time Series Prediction Model Evaluation----------
#################################################################################################################################################

#Run this function to compute the mean absolute percentage error (MAPE)
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#########################
######## Overall ########
#########################

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
plot(fore_arima)


########################
######## Region ########
########################

##(Training data) Subset to keep only Jan-2015 to Dec-2018 records
stb_2_EUROPE_2015_2018 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2015 & as.numeric(format(stb_2_Region$month,'%Y'))<=2018 & stb_2_Region$region=="EUROPE")

##(Testing data) Subset to keep only Jan-2019 to Dec-2019 records
stb_2_EUROPE_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))==2019 & stb_2_Region$region=="EUROPE")

#Convert into timeseries data and plot the timeseries
dat_ts_EUROPE <- ts(stb_2_EUROPE_2015_2018[, 3], start = c(2015, 1), end = c(2018, 12), frequency = 12)
plot(dat_ts_EUROPE)

#Auto ARIMA method
arima_model_EUROPE <- auto.arima(dat_ts_EUROPE)
summary(arima_model_EUROPE) #MAPE 2.82%

#Predict and test using 2019 data
fore_arima_EUROPE = forecast::forecast(arima_model_EUROPE, h=12)
df_arima_EUROPE = as.data.frame(fore_arima_EUROPE)
stb_2_EUROPE_2019$arima = df_arima_EUROPE$`Point Forecast`
mape(stb_2_EUROPE_2019$total_arrival_region, stb_2_EUROPE_2019$arima)  #MAPE 5.39%

#Plot the forecast chart out
plot(fore_arima_EUROPE)

#########################
######## Country ########
#########################

##(Training data) Subset to keep only Jan-2015 to Dec-2018 records
stb_2_UK_2015_2018 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2015 & as.numeric(format(stb_2_Country$month,'%Y'))<=2018 & stb_2_Country$place_of_residence=="UK")

##(Testing data) Subset to keep only Jan-2019 to Dec-2019 records
stb_2_UK_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))==2019 & stb_2_Country$place_of_residence=="UK")

#Convert into timeseries data and plot the timeseries
dat_ts_UK <- ts(stb_2_UK_2015_2018[, 3], start = c(2015, 1), end = c(2018, 12), frequency = 12)
plot(dat_ts_UK)

#Auto ARIMA method
arima_model_UK <- auto.arima(dat_ts_UK)
summary(arima_model_UK) #MAPE 4.07%

#Predict and test using 2019 data
fore_arima_UK = forecast::forecast(arima_model_UK, h=12)
df_arima_UK = as.data.frame(fore_arima_UK)
stb_2_UK_2019$arima = df_arima_UK$`Point Forecast`
mape(stb_2_UK_2019$total_arrival_country, stb_2_UK_2019$arima)  #MAPE 9.84

#Plot the forecast chart out
plot(fore_arima_UK)

#################################################################################################################################################
#### 4. Time Series Prediction for 2020----------
#################################################################################################################################################

####################################################################################################
######## Overall ###################################################################################
####################################################################################################

## Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Overall_2016_2019 <- subset(stb_2_Overall, as.numeric(format(stb_2_Overall$month,'%Y'))>=2016 & as.numeric(format(stb_2_Overall$month,'%Y'))<=2019)

#Convert into timeseries data and plot the timeseries
dat_ts_2016_2019 <- ts(stb_2_Overall_2016_2019[, 2], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#######Forecast using Auto ARIMA method########
arima_model_2016_2019 <- auto.arima(dat_ts_2016_2019)
fore_arima_2016_2019 = forecast::forecast(arima_model_2016_2019, h=12)
df_arima_2016_2019 = as.data.frame(fore_arima_2016_2019)
df_arima_2016_2019

#Plot trend
plot(fore_arima_2016_2019, ylab="Total arrival")

#To get summary table
dat_ts_2019 <- subset(stb_2_Overall, as.numeric(format(stb_2_Overall$month,'%Y'))==2019)
sumtab_Overall <- cbind(df_arima_2016_2019, dat_ts_2019)
sumtab_Overall$Y_O_Y <- round(((sumtab_Overall$"Point Forecast" - sumtab_Overall$total_arrival)/sumtab_Overall$total_arrival)*100, digits=2)

Jan2020_Nov_2020 <- df_arima_2016_2019 %>% select('Point Forecast') %>% slice(1:11) %>% rename(lag_data = "Point Forecast")
Dec2019 <- dat_ts_2019 %>% select(total_arrival) %>% slice(12) %>% rename(lag_data = total_arrival)
Dec2019_Nov_2020 <- rbind(Dec2019,Jan2020_Nov_2020) 

sumtab_Overall <- cbind(sumtab_Overall,Dec2019_Nov_2020)
sumtab_Overall$M_O_M <- round(((sumtab_Overall$"Point Forecast" - sumtab_Overall$lag_data)/sumtab_Overall$lag_data)*100, digits=2)
sumtab_Overall$`Point Forecast` <- round(sumtab_Overall$"Point Forecast"*1, digits = 0)
sumtab_Overall$`Lo 95` <- round(sumtab_Overall$"Lo 95"*1, digits = 0)
sumtab_Overall$`Hi 95` <- round(sumtab_Overall$"Hi 95"*1, digits = 0)

sumtab_Overall <- sumtab_Overall %>% rename('Predicted Value'= 'Point Forecast', 'Lower Limit (95% CI)' = 'Lo 95', 'Upper Limit (95% CI)' = 'Hi 95', 'M-O-M (%)' = 'M_O_M', 'Y-O-Y (%)' = 'Y_O_Y') 
sumtab_Overall <- sumtab_Overall %>% select('Predicted Value', 'Lower Limit (95% CI)', 'Upper Limit (95% CI)', 'M-O-M (%)', 'Y-O-Y (%)')
sumtab_Overall$`Lower Limit (95% CI)` <- format(sumtab_Overall$`Lower Limit (95% CI)`, big.mark = ",")
sumtab_Overall$`Upper Limit (95% CI)` <- format(sumtab_Overall$`Upper Limit (95% CI)`, big.mark = ",")

sumtab_Overall_total <- sumtab_Overall %>% summarise('Predicted Value' = sum(`Predicted Value`))
row.names(sumtab_Overall_total) <- "Total"
sumtab_Overall_total$'Lower Limit (95% CI)' <- ''
sumtab_Overall_total$'Upper Limit (95% CI)' <- ''
sumtab_Overall_total$'M-O-M (%)' <- ''
sumtab_Overall_total$'Y-O-Y (%)' <- ''
sumtab_Overall <- rbind(sumtab_Overall, sumtab_Overall_total)
sumtab_Overall$`Predicted Value` <- format(sumtab_Overall$`Predicted Value`, big.mark = ",")

library(formattable)
customGreen = "#71CA97"
customGreen0 = "#DeF7E9"
customRed = "#ff7f7f"
improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", color = ifelse(x > 0 , customGreen, ifelse(x < 0 & x!='' , customRed, "white"))), x ~ icontext(ifelse(x>0 , "arrow-up", "arrow-down"), x))
formattable(sumtab_Overall, align =c("c","c","c","c","c"), list('M-O-M (%)'= improvement_formatter, 'Y-O-Y (%)'= improvement_formatter ))

###################################################################################################
######## Region ###################################################################################
###################################################################################################

###############################################
##################1) AMERICAS##################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_AMERICAS_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="AMERICAS")

#Convert into timeseries data and plot the timeseries
dat_ts_AMERICAS_2016_2019 <- ts(stb_2_AMERICAS_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_AMERICAS_2016_2019 <- auto.arima(dat_ts_AMERICAS_2016_2019)
fore_arima_AMERICAS_2016_2019 = forecast::forecast(arima_model_AMERICAS_2016_2019, h=12)
df_arima_AMERICAS_2016_2019 = as.data.frame(fore_arima_AMERICAS_2016_2019)
df_arima_AMERICAS_2016_2019

#Plot the forecast chart out
plot(fore_arima_AMERICAS_2016_2019, ylab="Total arrival")

###############################################
##################2) SOUTHEAST ASIA############
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_SOUTHEAST_ASIA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="SOUTHEAST ASIA")

#Convert into timeseries data and plot the timeseries
dat_ts_SOUTHEAST_ASIA_2016_2019 <- ts(stb_2_SOUTHEAST_ASIA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_SOUTHEAST_ASIA_2016_2019 <- auto.arima(dat_ts_SOUTHEAST_ASIA_2016_2019)
fore_arima_SOUTHEAST_ASIA_2016_2019 = forecast::forecast(arima_model_SOUTHEAST_ASIA_2016_2019, h=12)
df_arima_SOUTHEAST_ASIA_2016_2019 = as.data.frame(fore_arima_SOUTHEAST_ASIA_2016_2019)
df_arima_SOUTHEAST_ASIA_2016_2019

#Plot the forecast chart out
plot(fore_arima_SOUTHEAST_ASIA_2016_2019, ylab="Total arrival")

###############################################
##################3) GREATER CHINA#############
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_GREATER_CHINA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="GREATER CHINA")

#Convert into timeseries data and plot the timeseries
dat_ts_GREATER_CHINA_2016_2019 <- ts(stb_2_GREATER_CHINA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_GREATER_CHINA_2016_2019 <- auto.arima(dat_ts_GREATER_CHINA_2016_2019)
fore_arima_GREATER_CHINA_2016_2019 = forecast::forecast(arima_model_GREATER_CHINA_2016_2019, h=12)
df_arima_GREATER_CHINA_2016_2019 = as.data.frame(fore_arima_GREATER_CHINA_2016_2019)
df_arima_GREATER_CHINA_2016_2019

#Plot the forecast chart out
plot(fore_arima_GREATER_CHINA_2016_2019, ylab="Total arrival")

###############################################
##################4) NORTH ASIA################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_NORTH_ASIA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="NORTH ASIA")

#Convert into timeseries data and plot the timeseries
dat_ts_NORTH_ASIA_2016_2019 <- ts(stb_2_NORTH_ASIA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_NORTH_ASIA_2016_2019 <- auto.arima(dat_ts_NORTH_ASIA_2016_2019)
fore_arima_NORTH_ASIA_2016_2019 = forecast::forecast(arima_model_NORTH_ASIA_2016_2019, h=12)
df_arima_NORTH_ASIA_2016_2019 = as.data.frame(fore_arima_NORTH_ASIA_2016_2019)
df_arima_NORTH_ASIA_2016_2019

#Plot the forecast chart out
plot(fore_arima_NORTH_ASIA_2016_2019, ylab="Total arrival")

###############################################
##################5) SOUTH ASIA################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_SOUTH_ASIA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="SOUTH ASIA")

#Convert into timeseries data and plot the timeseries
dat_ts_SOUTH_ASIA_2016_2019 <- ts(stb_2_SOUTH_ASIA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_SOUTH_ASIA_2016_2019 <- auto.arima(dat_ts_SOUTH_ASIA_2016_2019)
fore_arima_SOUTH_ASIA_2016_2019 = forecast::forecast(arima_model_SOUTH_ASIA_2016_2019, h=12)
df_arima_SOUTH_ASIA_2016_2019 = as.data.frame(fore_arima_SOUTH_ASIA_2016_2019)
df_arima_SOUTH_ASIA_2016_2019

#Plot the forecast chart out
plot(fore_arima_SOUTH_ASIA_2016_2019, ylab="Total arrival")

###############################################
##################6) WEST ASIA#################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_WEST_ASIA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="WEST ASIA")

#Convert into timeseries data and plot the timeseries
dat_ts_WEST_ASIA_2016_2019 <- ts(stb_2_WEST_ASIA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_WEST_ASIA_2016_2019 <- auto.arima(dat_ts_WEST_ASIA_2016_2019)
fore_arima_WEST_ASIA_2016_2019 = forecast::forecast(arima_model_WEST_ASIA_2016_2019, h=12)
df_arima_WEST_ASIA_2016_2019 = as.data.frame(fore_arima_WEST_ASIA_2016_2019)
df_arima_WEST_ASIA_2016_2019

#Plot the forecast chart out
plot(fore_arima_WEST_ASIA_2016_2019, ylab="Total arrival")

###############################################
##################7) EUROPE####################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_EUROPE_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="EUROPE")

#Convert into timeseries data and plot the timeseries
dat_ts_EUROPE_2016_2019 <- ts(stb_2_EUROPE_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_EUROPE_2016_2019 <- auto.arima(dat_ts_EUROPE_2016_2019)
fore_arima_EUROPE_2016_2019 = forecast::forecast(arima_model_EUROPE_2016_2019, h=12)
df_arima_EUROPE_2016_2019 = as.data.frame(fore_arima_EUROPE_2016_2019)
df_arima_EUROPE_2016_2019

#Plot the forecast chart out
plot(fore_arima_EUROPE_2016_2019, ylab="Total arrival")

###############################################
##################8) OCEANIA###################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_OCEANIA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="OCEANIA")

#Convert into timeseries data and plot the timeseries
dat_ts_OCEANIA_2016_2019 <- ts(stb_2_OCEANIA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_OCEANIA_2016_2019 <- auto.arima(dat_ts_OCEANIA_2016_2019)
fore_arima_OCEANIA_2016_2019 = forecast::forecast(arima_model_OCEANIA_2016_2019, h=12)
df_arima_OCEANIA_2016_2019 = as.data.frame(fore_arima_OCEANIA_2016_2019)
df_arima_OCEANIA_2016_2019

#Plot the forecast chart out
plot(fore_arima_OCEANIA_2016_2019, ylab="Total arrival")

###############################################
##################9) AFRICA####################
##Subset to keep only Jan-2016 to Dec-2019 records
stb_2_AFRICA_2016_2019 <- subset(stb_2_Region, as.numeric(format(stb_2_Region$month,'%Y'))>=2016 & as.numeric(format(stb_2_Region$month,'%Y'))<=2019 & stb_2_Region$region=="AFRICA")

#Convert into timeseries data and plot the timeseries
dat_ts_AFRICA_2016_2019 <- ts(stb_2_AFRICA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_AFRICA_2016_2019 <- auto.arima(dat_ts_AFRICA_2016_2019)
fore_arima_AFRICA_2016_2019 = forecast::forecast(arima_model_AFRICA_2016_2019, h=12)
df_arima_AFRICA_2016_2019 = as.data.frame(fore_arima_AFRICA_2016_2019)
df_arima_AFRICA_2016_2019

#Plot the forecast chart out
plot(fore_arima_AFRICA_2016_2019, ylab="Total arrival")

###################################################################################################
######## Country ##################################################################################
###################################################################################################

###############################################
##################1) Australia#################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Australia_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Australia")

#Convert into timeseries data and plot the timeseries
dat_ts_Australia_2016_2019 <- ts(stb_2_Australia_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Australia_2016_2019 <- auto.arima(dat_ts_Australia_2016_2019)
fore_arima_Australia_2016_2019 = forecast::forecast(arima_model_Australia_2016_2019, h=12)
df_arima_Australia_2016_2019 = as.data.frame(fore_arima_Australia_2016_2019)
df_arima_Australia_2016_2019

#Plot the forecast chart out
plot(fore_arima_Australia_2016_2019, ylab="Total arrival")

###############################################
##################2) Bangladesh################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Bangladesh_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Bangladesh")

#Convert into timeseries data and plot the timeseries
dat_ts_Bangladesh_2016_2019 <- ts(stb_2_Bangladesh_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Bangladesh_2016_2019 <- auto.arima(dat_ts_Bangladesh_2016_2019)
fore_arima_Bangladesh_2016_2019 = forecast::forecast(arima_model_Bangladesh_2016_2019, h=12)
df_arima_Bangladesh_2016_2019 = as.data.frame(fore_arima_Bangladesh_2016_2019)
df_arima_Bangladesh_2016_2019

#Plot the forecast chart out
plot(fore_arima_Bangladesh_2016_2019, ylab="Total arrival")

###############################################
##################3) Belgium and Luxembourg####
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Belgium_and_Luxembourg_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Belgium and Luxembourg")

#Convert into timeseries data and plot the timeseries
dat_ts_Belgium_and_Luxembourg_2016_2019 <- ts(stb_2_Belgium_and_Luxembourg_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Belgium_and_Luxembourg_2016_2019 <- auto.arima(dat_ts_Belgium_and_Luxembourg_2016_2019)
fore_arima_Belgium_and_Luxembourg_2016_2019 = forecast::forecast(arima_model_Belgium_and_Luxembourg_2016_2019, h=12)
df_arima_Belgium_and_Luxembourg_2016_2019 = as.data.frame(fore_arima_Belgium_and_Luxembourg_2016_2019)
df_arima_Belgium_and_Luxembourg_2016_2019

#Plot the forecast chart out
plot(fore_arima_Belgium_and_Luxembourg_2016_2019, ylab="Total arrival")

###############################################
##################4) Brunei Darussalam#########
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Brunei_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Brunei Darussalam")

#Convert into timeseries data and plot the timeseries
dat_ts_Brunei_2016_2019 <- ts(stb_2_Brunei_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Brunei_2016_2019 <- auto.arima(dat_ts_Brunei_2016_2019)
fore_arima_Brunei_2016_2019 = forecast::forecast(arima_model_Brunei_2016_2019, h=12)
df_arima_Brunei_2016_2019 = as.data.frame(fore_arima_Brunei_2016_2019)
df_arima_Brunei_2016_2019

#Plot the forecast chart out
plot(fore_arima_Brunei_2016_2019, ylab="Total arrival")

###############################################
##################5) Canada####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Canada_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Canada")

#Convert into timeseries data and plot the timeseries
dat_ts_Canada_2016_2019 <- ts(stb_2_Canada_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Canada_2016_2019 <- auto.arima(dat_ts_Canada_2016_2019)
fore_arima_Canada_2016_2019 = forecast::forecast(arima_model_Canada_2016_2019, h=12)
df_arima_Canada_2016_2019 = as.data.frame(fore_arima_Canada_2016_2019)
df_arima_Canada_2016_2019

#Plot the forecast chart out
plot(fore_arima_Canada_2016_2019, ylab="Total arrival")

###############################################
##################6) China#####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_China_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="China")

#Convert into timeseries data and plot the timeseries
dat_ts_China_2016_2019 <- ts(stb_2_China_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_China_2016_2019 <- auto.arima(dat_ts_China_2016_2019)
fore_arima_China_2016_2019 = forecast::forecast(arima_model_China_2016_2019, h=12)
df_arima_China_2016_2019 = as.data.frame(fore_arima_China_2016_2019)
df_arima_China_2016_2019

#Plot the forecast chart out
plot(fore_arima_China_2016_2019, ylab="Total arrival")

###############################################
##################7) Denmark###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Denmark_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Denmark")

#Convert into timeseries data and plot the timeseries
dat_ts_Denmark_2016_2019 <- ts(stb_2_Denmark_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Denmark_2016_2019 <- auto.arima(dat_ts_Denmark_2016_2019)
fore_arima_Denmark_2016_2019 = forecast::forecast(arima_model_Denmark_2016_2019, h=12)
df_arima_Denmark_2016_2019 = as.data.frame(fore_arima_Denmark_2016_2019)
df_arima_Denmark_2016_2019

#Plot the forecast chart out
plot(fore_arima_Denmark_2016_2019, ylab="Total arrival")

###############################################
##################8) Egypt#####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Egypt_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Egypt")

#Convert into timeseries data and plot the timeseries
dat_ts_Egypt_2016_2019 <- ts(stb_2_Egypt_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Egypt_2016_2019 <- auto.arima(dat_ts_Egypt_2016_2019)
fore_arima_Egypt_2016_2019 = forecast::forecast(arima_model_Egypt_2016_2019, h=12)
df_arima_Egypt_2016_2019 = as.data.frame(fore_arima_Egypt_2016_2019)
df_arima_Egypt_2016_2019

#Plot the forecast chart out
plot(fore_arima_Egypt_2016_2019, ylab="Total arrival")

###############################################
##################9) Finland##################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Finland_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Finland")

#Convert into timeseries data and plot the timeseries
dat_ts_Finland_2016_2019 <- ts(stb_2_Finland_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Finland_2016_2019 <- auto.arima(dat_ts_Finland_2016_2019)
fore_arima_Finland_2016_2019 = forecast::forecast(arima_model_Finland_2016_2019, h=12)
df_arima_Finland_2016_2019 = as.data.frame(fore_arima_Finland_2016_2019)
df_arima_Finland_2016_2019

#Plot the forecast chart out
plot(fore_arima_Finland_2016_2019, ylab="Total arrival")

###############################################
##################10) France##################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_France_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="France")

#Convert into timeseries data and plot the timeseries
dat_ts_France_2016_2019 <- ts(stb_2_France_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_France_2016_2019 <- auto.arima(dat_ts_France_2016_2019)
fore_arima_France_2016_2019 = forecast::forecast(arima_model_France_2016_2019, h=12)
df_arima_France_2016_2019 = as.data.frame(fore_arima_France_2016_2019)
df_arima_France_2016_2019

#Plot the forecast chart out
plot(fore_arima_France_2016_2019, ylab="Total arrival")

###############################################
##################11) Germany##################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Germany_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Germany")

#Convert into timeseries data and plot the timeseries
dat_ts_Germany_2016_2019 <- ts(stb_2_Germany_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Germany_2016_2019 <- auto.arima(dat_ts_Germany_2016_2019)
fore_arima_Germany_2016_2019 = forecast::forecast(arima_model_Germany_2016_2019, h=12)
df_arima_Germany_2016_2019 = as.data.frame(fore_arima_Germany_2016_2019)
df_arima_Germany_2016_2019

#Plot the forecast chart out
plot(fore_arima_Germany_2016_2019, ylab="Total arrival")

###############################################
##################12) Hong Kong SAR############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Hong_Kong_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Hong Kong SAR")

#Convert into timeseries data and plot the timeseries
dat_ts_Hong_Kong_2016_2019 <- ts(stb_2_Hong_Kong_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Hong_Kong_2016_2019 <- auto.arima(dat_ts_Hong_Kong_2016_2019)
fore_arima_Hong_Kong_2016_2019 = forecast::forecast(arima_model_Hong_Kong_2016_2019, h=12)
df_arima_Hong_Kong_2016_2019 = as.data.frame(fore_arima_Hong_Kong_2016_2019)
df_arima_Hong_Kong_2016_2019

#Plot the forecast chart out
plot(fore_arima_Hong_Kong_2016_2019, ylab="Total arrival")

###############################################
##################13) India####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_India_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="India")

#Convert into timeseries data and plot the timeseries
dat_ts_India_2016_2019 <- ts(stb_2_India_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_India_2016_2019 <- auto.arima(dat_ts_India_2016_2019)
fore_arima_India_2016_2019 = forecast::forecast(arima_model_India_2016_2019, h=12)
df_arima_India_2016_2019 = as.data.frame(fore_arima_India_2016_2019)
df_arima_India_2016_2019

#Plot the forecast chart out
plot(fore_arima_India_2016_2019, ylab="Total arrival")

###############################################
##################14) Indonesia################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Indonesia_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Indonesia")

#Convert into timeseries data and plot the timeseries
dat_ts_Indonesia_2016_2019 <- ts(stb_2_Indonesia_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Indonesia_2016_2019 <- auto.arima(dat_ts_Indonesia_2016_2019)
fore_arima_Indonesia_2016_2019 = forecast::forecast(arima_model_Indonesia_2016_2019, h=12)
df_arima_Indonesia_2016_2019 = as.data.frame(fore_arima_Indonesia_2016_2019)
df_arima_Indonesia_2016_2019

#Plot the forecast chart out
plot(fore_arima_Indonesia_2016_2019, ylab="Total arrival")

###############################################
##################15) Iran#####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Iran_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Iran")

#Convert into timeseries data and plot the timeseries
dat_ts_Iran_2016_2019 <- ts(stb_2_Iran_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Iran_2016_2019 <- auto.arima(dat_ts_Iran_2016_2019)
fore_arima_Iran_2016_2019 = forecast::forecast(arima_model_Iran_2016_2019, h=12)
df_arima_Iran_2016_2019 = as.data.frame(fore_arima_Iran_2016_2019)
df_arima_Iran_2016_2019

#Plot the forecast chart out
plot(fore_arima_Iran_2016_2019, ylab="Total arrival")

###############################################
##################16) Israel###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Israel_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Israel")

#Convert into timeseries data and plot the timeseries
dat_ts_Israel_2016_2019 <- ts(stb_2_Israel_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Israel_2016_2019 <- auto.arima(dat_ts_Israel_2016_2019)
fore_arima_Israel_2016_2019 = forecast::forecast(arima_model_Israel_2016_2019, h=12)
df_arima_Israel_2016_2019 = as.data.frame(fore_arima_Israel_2016_2019)
df_arima_Israel_2016_2019

#Plot the forecast chart out
plot(fore_arima_Israel_2016_2019, ylab="Total arrival")

###############################################
##################17) Italy####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Italy_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Italy")

#Convert into timeseries data and plot the timeseries
dat_ts_Italy_2016_2019 <- ts(stb_2_Italy_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Italy_2016_2019 <- auto.arima(dat_ts_Italy_2016_2019)
fore_arima_Italy_2016_2019 = forecast::forecast(arima_model_Italy_2016_2019, h=12)
df_arima_Italy_2016_2019 = as.data.frame(fore_arima_Italy_2016_2019)
df_arima_Italy_2016_2019

#Plot the forecast chart out
plot(fore_arima_Italy_2016_2019, ylab="Total arrival")

###############################################
##################18) Japan####################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Japan_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Japan")

#Convert into timeseries data and plot the timeseries
dat_ts_Japan_2016_2019 <- ts(stb_2_Japan_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Japan_2016_2019 <- auto.arima(dat_ts_Japan_2016_2019)
fore_arima_Japan_2016_2019 = forecast::forecast(arima_model_Japan_2016_2019, h=12)
df_arima_Japan_2016_2019 = as.data.frame(fore_arima_Japan_2016_2019)
df_arima_Japan_2016_2019

#Plot the forecast chart out
plot(fore_arima_Japan_2016_2019, ylab="Total arrival")

###############################################
##################19) Kuwait###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Kuwait_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Kuwait")

#Convert into timeseries data and plot the timeseries
dat_ts_Kuwait_2016_2019 <- ts(stb_2_Kuwait_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Kuwait_2016_2019 <- auto.arima(dat_ts_Kuwait_2016_2019)
fore_arima_Kuwait_2016_2019 = forecast::forecast(arima_model_Kuwait_2016_2019, h=12)
df_arima_Kuwait_2016_2019 = as.data.frame(fore_arima_Kuwait_2016_2019)
df_arima_Kuwait_2016_2019

#Plot the forecast chart out
plot(fore_arima_Kuwait_2016_2019, ylab="Total arrival")

###############################################
##################20) Malaysia#################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Malaysia_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Malaysia")

#Convert into timeseries data and plot the timeseries
dat_ts_Malaysia_2016_2019 <- ts(stb_2_Malaysia_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Malaysia_2016_2019 <- auto.arima(dat_ts_Malaysia_2016_2019)
fore_arima_Malaysia_2016_2019 = forecast::forecast(arima_model_Malaysia_2016_2019, h=12)
df_arima_Malaysia_2016_2019 = as.data.frame(fore_arima_Malaysia_2016_2019)
df_arima_Malaysia_2016_2019

#Plot the forecast chart out
plot(fore_arima_Malaysia_2016_2019, ylab="Total arrival")

###############################################
##################21) Mauritius################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Mauritius_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Mauritius")

#Convert into timeseries data and plot the timeseries
dat_ts_Mauritius_2016_2019 <- ts(stb_2_Mauritius_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Mauritius_2016_2019 <- auto.arima(dat_ts_Mauritius_2016_2019)
fore_arima_Mauritius_2016_2019 = forecast::forecast(arima_model_Mauritius_2016_2019, h=12)
df_arima_Mauritius_2016_2019 = as.data.frame(fore_arima_Mauritius_2016_2019)
df_arima_Mauritius_2016_2019

#Plot the forecast chart out
plot(fore_arima_Mauritius_2016_2019, ylab="Total arrival")

###############################################
##################22) Myanmar#################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Myanmar_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Myanmar")

#Convert into timeseries data and plot the timeseries
dat_ts_Myanmar_2016_2019 <- ts(stb_2_Myanmar_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Myanmar_2016_2019 <- auto.arima(dat_ts_Myanmar_2016_2019)
fore_arima_Myanmar_2016_2019 = forecast::forecast(arima_model_Myanmar_2016_2019, h=12)
df_arima_Myanmar_2016_2019 = as.data.frame(fore_arima_Myanmar_2016_2019)
df_arima_Myanmar_2016_2019

#Plot the forecast chart out
plot(fore_arima_Myanmar_2016_2019, ylab="Total arrival")

###############################################
##################23) Netherlands##############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Netherlands_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Netherlands")

#Convert into timeseries data and plot the timeseries
dat_ts_Netherlands_2016_2019 <- ts(stb_2_Netherlands_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Netherlands_2016_2019 <- auto.arima(dat_ts_Netherlands_2016_2019)
fore_arima_Netherlands_2016_2019 = forecast::forecast(arima_model_Netherlands_2016_2019, h=12)
df_arima_Netherlands_2016_2019 = as.data.frame(fore_arima_Netherlands_2016_2019)
df_arima_Netherlands_2016_2019

#Plot the forecast chart out
plot(fore_arima_Netherlands_2016_2019, ylab="Total arrival")

###############################################
##################24) New Zealand##############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_New_Zealand_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="New Zealand")

#Convert into timeseries data and plot the timeseries
dat_ts_New_Zealand_2016_2019 <- ts(stb_2_New_Zealand_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_New_Zealand_2016_2019 <- auto.arima(dat_ts_New_Zealand_2016_2019)
fore_arima_New_Zealand_2016_2019 = forecast::forecast(arima_model_New_Zealand_2016_2019, h=12)
df_arima_New_Zealand_2016_2019 = as.data.frame(fore_arima_New_Zealand_2016_2019)
df_arima_New_Zealand_2016_2019

#Plot the forecast chart out
plot(fore_arima_New_Zealand_2016_2019, ylab="Total arrival")

###############################################
##################25) Norway###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Norway_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Norway")

#Convert into timeseries data and plot the timeseries
dat_ts_Norway_2016_2019 <- ts(stb_2_Norway_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Norway_2016_2019 <- auto.arima(dat_ts_Norway_2016_2019)
fore_arima_Norway_2016_2019 = forecast::forecast(arima_model_Norway_2016_2019, h=12)
df_arima_Norway_2016_2019 = as.data.frame(fore_arima_Norway_2016_2019)
df_arima_Norway_2016_2019

#Plot the forecast chart out
plot(fore_arima_Norway_2016_2019, ylab="Total arrival")

###############################################
##################26) Pakistan#################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Pakistan_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Pakistan")

#Convert into timeseries data and plot the timeseries
dat_ts_Pakistan_2016_2019 <- ts(stb_2_Pakistan_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Pakistan_2016_2019 <- auto.arima(dat_ts_Pakistan_2016_2019)
fore_arima_Pakistan_2016_2019 = forecast::forecast(arima_model_Pakistan_2016_2019, h=12)
df_arima_Pakistan_2016_2019 = as.data.frame(fore_arima_Pakistan_2016_2019)
df_arima_Pakistan_2016_2019

#Plot the forecast chart out
plot(fore_arima_Pakistan_2016_2019, ylab="Total arrival")

###############################################
##################27) Philippines##############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Philippines_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Philippines")

#Convert into timeseries data and plot the timeseries
dat_ts_Philippines_2016_2019 <- ts(stb_2_Philippines_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Philippines_2016_2019 <- auto.arima(dat_ts_Philippines_2016_2019)
fore_arima_Philippines_2016_2019 = forecast::forecast(arima_model_Philippines_2016_2019, h=12)
df_arima_Philippines_2016_2019 = as.data.frame(fore_arima_Philippines_2016_2019)
df_arima_Philippines_2016_2019

#Plot the forecast chart out
plot(fore_arima_Philippines_2016_2019, ylab="Total arrival")

###############################################
##################28) Rep of Ireland###########
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Ireland_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Rep of Ireland")

#Convert into timeseries data and plot the timeseries
dat_ts_Ireland_2016_2019 <- ts(stb_2_Ireland_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Ireland_2016_2019 <- auto.arima(dat_ts_Ireland_2016_2019)
fore_arima_Ireland_2016_2019 = forecast::forecast(arima_model_Ireland_2016_2019, h=12)
df_arima_Ireland_2016_2019 = as.data.frame(fore_arima_Ireland_2016_2019)
df_arima_Ireland_2016_2019

#Plot the forecast chart out
plot(fore_arima_Ireland_2016_2019, ylab="Total arrival")

###############################################
##################29) Russian Federation#######
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Russian_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Russian Federation")

#Convert into timeseries data and plot the timeseries
dat_ts_Russian_2016_2019 <- ts(stb_2_Russian_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Russian_2016_2019 <- auto.arima(dat_ts_Russian_2016_2019)
fore_arima_Russian_2016_2019 = forecast::forecast(arima_model_Russian_2016_2019, h=12)
df_arima_Russian_2016_2019 = as.data.frame(fore_arima_Russian_2016_2019)
df_arima_Russian_2016_2019

#Plot the forecast chart out
plot(fore_arima_Russian_2016_2019, ylab="Total arrival")

###############################################
##################30) Saudi Arabia#############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Saudi_Arabia_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Saudi Arabia")

#Convert into timeseries data and plot the timeseries
dat_ts_Saudi_Arabia_2016_2019 <- ts(stb_2_Saudi_Arabia_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Saudi_Arabia_2016_2019 <- auto.arima(dat_ts_Saudi_Arabia_2016_2019)
fore_arima_Saudi_Arabia_2016_2019 = forecast::forecast(arima_model_Saudi_Arabia_2016_2019, h=12)
df_arima_Saudi_Arabia_2016_2019 = as.data.frame(fore_arima_Saudi_Arabia_2016_2019)
df_arima_Saudi_Arabia_2016_2019

#Plot the forecast chart out
plot(fore_arima_Saudi_Arabia_2016_2019, ylab="Total arrival")

###############################################
##################31) South Africa (Rep of)####
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_South_Africa_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="South Africa (Rep of)")

#Convert into timeseries data and plot the timeseries
dat_ts_South_Africa_2016_2019 <- ts(stb_2_South_Africa_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_South_Africa_2016_2019 <- auto.arima(dat_ts_South_Africa_2016_2019)
fore_arima_South_Africa_2016_2019 = forecast::forecast(arima_model_South_Africa_2016_2019, h=12)
df_arima_South_Africa_2016_2019 = as.data.frame(fore_arima_South_Africa_2016_2019)
df_arima_South_Africa_2016_2019

#Plot the forecast chart out
plot(fore_arima_South_Africa_2016_2019, ylab="Total arrival")

###############################################
##################32) South Korea##############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_South_Korea_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="South Korea")

#Convert into timeseries data and plot the timeseries
dat_ts_South_Korea_2016_2019 <- ts(stb_2_South_Korea_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_South_Korea_2016_2019 <- auto.arima(dat_ts_South_Korea_2016_2019)
fore_arima_South_Korea_2016_2019 = forecast::forecast(arima_model_South_Korea_2016_2019, h=12)
df_arima_South_Korea_2016_2019 = as.data.frame(fore_arima_South_Korea_2016_2019)
df_arima_South_Korea_2016_2019

#Plot the forecast chart out
plot(fore_arima_South_Korea_2016_2019, ylab="Total arrival")

###############################################
##################33) Spain###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Spain_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Spain")

#Convert into timeseries data and plot the timeseries
dat_ts_Spain_2016_2019 <- ts(stb_2_Spain_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Spain_2016_2019 <- auto.arima(dat_ts_Spain_2016_2019)
fore_arima_Spain_2016_2019 = forecast::forecast(arima_model_Spain_2016_2019, h=12)
df_arima_Spain_2016_2019 = as.data.frame(fore_arima_Spain_2016_2019)
df_arima_Spain_2016_2019

#Plot the forecast chart out
plot(fore_arima_Spain_2016_2019, ylab="Total arrival")

###############################################
##################34) Sri Lanka################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Sri_Lanka_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Sri Lanka")

#Convert into timeseries data and plot the timeseries
dat_ts_Sri_Lanka_2016_2019 <- ts(stb_2_Sri_Lanka_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Sri_Lanka_2016_2019 <- auto.arima(dat_ts_Sri_Lanka_2016_2019)
fore_arima_Sri_Lanka_2016_2019 = forecast::forecast(arima_model_Sri_Lanka_2016_2019, h=12)
df_arima_Sri_Lanka_2016_2019 = as.data.frame(fore_arima_Sri_Lanka_2016_2019)
df_arima_Sri_Lanka_2016_2019

#Plot the forecast chart out
plot(fore_arima_Sri_Lanka_2016_2019, ylab="Total arrival")

###############################################
##################35) Sweden###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Sweden_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Sweden")

#Convert into timeseries data and plot the timeseries
dat_ts_Sweden_2016_2019 <- ts(stb_2_Sweden_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Sweden_2016_2019 <- auto.arima(dat_ts_Sweden_2016_2019)
fore_arima_Sweden_2016_2019 = forecast::forecast(arima_model_Sweden_2016_2019, h=12)
df_arima_Sweden_2016_2019 = as.data.frame(fore_arima_Sweden_2016_2019)
df_arima_Sweden_2016_2019

#Plot the forecast chart out
plot(fore_arima_Sweden_2016_2019, ylab="Total arrival")

###############################################
##################36) Switzerland##############
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Switzerland_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Switzerland")

#Convert into timeseries data and plot the timeseries
dat_ts_Switzerland_2016_2019 <- ts(stb_2_Switzerland_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Switzerland_2016_2019 <- auto.arima(dat_ts_Switzerland_2016_2019)
fore_arima_Switzerland_2016_2019 = forecast::forecast(arima_model_Switzerland_2016_2019, h=12)
df_arima_Switzerland_2016_2019 = as.data.frame(fore_arima_Switzerland_2016_2019)
df_arima_Switzerland_2016_2019

#Plot the forecast chart out
plot(fore_arima_Switzerland_2016_2019, ylab="Total arrival")

###############################################
##################37) Taiwan###################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Taiwan_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Taiwan")

#Convert into timeseries data and plot the timeseries
dat_ts_Taiwan_2016_2019 <- ts(stb_2_Taiwan_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Taiwan_2016_2019 <- auto.arima(dat_ts_Taiwan_2016_2019)
fore_arima_Taiwan_2016_2019 = forecast::forecast(arima_model_Taiwan_2016_2019, h=12)
df_arima_Taiwan_2016_2019 = as.data.frame(fore_arima_Taiwan_2016_2019)
df_arima_Taiwan_2016_2019

#Plot the forecast chart out
plot(fore_arima_Taiwan_2016_2019, ylab="Total arrival")

###############################################
##################38) Thailand#################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_Thailand_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Thailand")

#Convert into timeseries data and plot the timeseries
dat_ts_Thailand_2016_2019 <- ts(stb_2_Thailand_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_Thailand_2016_2019 <- auto.arima(dat_ts_Thailand_2016_2019)
fore_arima_Thailand_2016_2019 = forecast::forecast(arima_model_Thailand_2016_2019, h=12)
df_arima_Thailand_2016_2019 = as.data.frame(fore_arima_Thailand_2016_2019)
df_arima_Thailand_2016_2019

#Plot the forecast chart out
plot(fore_arima_Thailand_2016_2019, ylab="Total arrival")

###############################################
##################39) UK#######################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_UK_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="UK")

#Convert into timeseries data and plot the timeseries
dat_ts_UK_2016_2019 <- ts(stb_2_UK_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_UK_2016_2019 <- auto.arima(dat_ts_UK_2016_2019)
fore_arima_UK_2016_2019 = forecast::forecast(arima_model_UK_2016_2019, h=12)
df_arima_UK_2016_2019 = as.data.frame(fore_arima_UK_2016_2019)
df_arima_UK_2016_2019

#Plot the forecast chart out
plot(fore_arima_UK_2016_2019, ylab="Total arrival")

###############################################
##################40) United Arab Emirates#####
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_UNITED_ARAB_EMIRATES_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="United Arab Emirates")

#Convert into timeseries data and plot the timeseries
dat_ts_UNITED_ARAB_EMIRATES_2016_2019 <- ts(stb_2_UNITED_ARAB_EMIRATES_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_UNITED_ARAB_EMIRATES_2016_2019 <- auto.arima(dat_ts_UNITED_ARAB_EMIRATES_2016_2019)
fore_arima_UNITED_ARAB_EMIRATES_2016_2019 = forecast::forecast(arima_model_UNITED_ARAB_EMIRATES_2016_2019, h=12)
df_arima_UNITED_ARAB_EMIRATES_2016_2019 = as.data.frame(fore_arima_UNITED_ARAB_EMIRATES_2016_2019)
df_arima_UNITED_ARAB_EMIRATES_2016_2019

#Plot the forecast chart out
plot(fore_arima_UNITED_ARAB_EMIRATES_2016_2019, ylab="Total arrival")

###############################################
##################41) USA######################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_USA_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="USA")

#Convert into timeseries data and plot the timeseries
dat_ts_USA_2016_2019 <- ts(stb_2_USA_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_USA_2016_2019 <- auto.arima(dat_ts_USA_2016_2019)
fore_arima_USA_2016_2019 = forecast::forecast(arima_model_USA_2016_2019, h=12)
df_arima_USA_2016_2019 = as.data.frame(fore_arima_USA_2016_2019)
df_arima_USA_2016_2019

#Plot the forecast chart out
plot(fore_arima_USA_2016_2019, ylab="Total arrival")

###############################################
##################42) Vietnam##################
##(Training data) Subset to keep only Jan-2016 to Dec-2019 records
stb_2_VIETNAM_2016_2019 <- subset(stb_2_Country, as.numeric(format(stb_2_Country$month,'%Y'))>=2016 & as.numeric(format(stb_2_Country$month,'%Y'))<=2019 & stb_2_Country$place_of_residence=="Vietnam")

#Convert into timeseries data and plot the timeseries
dat_ts_VIETNAM_2016_2019 <- ts(stb_2_VIETNAM_2016_2019[, 3], start = c(2016, 1), end = c(2019, 12), frequency = 12)

#Auto ARIMA method
arima_model_VIETNAM_2016_2019 <- auto.arima(dat_ts_VIETNAM_2016_2019)
fore_arima_VIETNAM_2016_2019 = forecast::forecast(arima_model_VIETNAM_2016_2019, h=12)
df_arima_VIETNAM_2016_2019 = as.data.frame(fore_arima_VIETNAM_2016_2019)
df_arima_VIETNAM_2016_2019

#Plot the forecast chart out
plot(fore_arima_VIETNAM_2016_2019, ylab="Total arrival")

#################################################################################################################################################


















