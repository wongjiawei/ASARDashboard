setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(readxl)
library(DT)
library(shinyWidgets)
library(dplyr)
library(forecast)
library(ggplot2)
library(maps)
library(ggiraph)
library(reshape2)
library(rvest)
library(formattable)
library(magrittr)
library(cluster)
library(RColorBrewer)
library(scales)
library(tableHTML)
library(shinydashboard)
library(formattable)

list.of.packages <- c("shiny", "readxl","DT","shinyWidgets","dplyr","forecast","ggplot2","shinydashboard","formattable",
                      "maps","ggiraph","reshape2","rvest","formattable","magrittr","RColorBrewer","cluster","scales","tableHTML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

source('./mainFunctions.R')
stb_4 <- read_excel("./ShinyR/data/4.0.xlsx")
stb_2 <- read_excel("./ShinyR/data/2.0.xlsx")
transformList = transformData(stb_4, stb_2)
stb_4 = transformList$stb_4
stb_2 = transformList$stb_2

shiny::runApp("ShinyR")