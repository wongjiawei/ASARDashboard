setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(readxl)
library(DT)
library(shinyWidgets)

source('./mainFunctions.R')
stb_4 <- read_excel("./ShinyR/data/4.0.xlsx")
stb_2 <- read_excel("./ShinyR/data/2.0.xlsx")
transformList = transformData(stb_4, stb_2)
stb_4 = transformList$stb_4
stb_2 = transformList$stb_2


shiny::runApp("ShinyR")