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
stb_2 <- read_excel("2.0.xlsx")
stb_4 <- read_excel("4.0.xlsx")

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

#### 3 Additional Breakdown Plots Below Map

head(stb_4_2015_2019)
library(reshape2)

### Gender 
stb_4_2015_2019_gender = stb_4_2015_2019[,1:5]
stb_4_2015_2019_gender_melt = melt(stb_4_2015_2019_gender,id=c('month','region','place_of_residence'))


### Age Bins
stb_4_2015_2019_age <- subset(stb_4_2015_2019, select = c(month,region,place_of_residence,age_14andbelow,age_15to19,age_20to24,age_25to34,age_35to44,age_45to54,age_55to64,age_65andabove,not_stated_age))
stb_4_2015_2019_age_melt = melt(stb_4_2015_2019_age,id=c('month','region','place_of_residence'))

### Visit Duration Bins PLot

stb_4_2015_2019_vdur = subset(stb_4_2015_2019, select = -c(male,female, not_stated_gender, average_age, age_14andbelow,age_15to19,age_20to24,age_25to34,age_35to44,age_45to54,age_55to64,age_65andabove,not_stated_age,average_duration,visitor_days))
stb_4_2015_2019_vdur_melt = melt(stb_4_2015_2019_vdur,id=c('month','region','place_of_residence'))



library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Test")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard",
                                          selectInput("inputTest", "Input Test",
                                                      choices = c("a", "b", "c", "d"), selected = NULL, multiple=TRUE,
                                                      width = '98%')))
  )
)

row1 <- fluidRow(
  box(
    title = "Arrival By Gender",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("arrivalsbygender", height = "300px")
  ),
  box(
    title = "Arrival By Age Groups",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("arrivalsbyage", height = "300px")
  )
)


row2 <- fluidRow(
  box(
    title = "Visit Duration",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("visitduration", height = "300px")
  )
)

body <- dashboardBody(row1, row2)

#completing the ui part with dashboardPage
ui <- dashboardPage(header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #creating the valueBoxOutput content
  
  output$arrivalsbygender <- renderPlot(
    ggplot(data = stb_4_2015_2019_gender_melt, aes(x="", y = value, fill = variable, group = variable)) +
      geom_bar(stat="identity") +
      coord_polar("y", start=0)
  )
  
  output$arrivalsbyage <- renderPlot(
    ggplot(data = stb_4_2015_2019_age_melt, aes(x="", y=value, fill=variable, group = variable)) +
      geom_bar(position = 'dodge', stat="identity" )  
  )
  
  output$visitduration <- renderPlot(
    ggplot(data = stb_4_2015_2019_vdur_melt, aes(x="", y=value, fill=variable, group = variable)) +
      geom_bar(position = 'dodge',stat="identity")  
  )
}

shinyApp(ui, server)
