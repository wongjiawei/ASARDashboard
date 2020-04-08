#Load Country Codes

library(ggiraph)
library(magrittr)
library(rvest)
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")

##Change ISO Codes for better understanding
old_names2 <- c("CAN", "IDN", "MYS", "PHL", "DEU", "NLD", "ZAF","GBR")
new_names2 <- c("CANADA", "INDO", "MSIA", "PHLPNES", "GERM","NETHLDS","S AFR","UK")
for (i in 1:length(old_names2)){
  iso_codes$ISO3[iso_codes$ISO3 == old_names2[i]] <-new_names2[i]
}
 

head(iso_codes)

##Load Data by Gender
library(readxl)
gender = read_excel("stb20172019.xlsx")
gendervars = c("Month","Place of Residence","Male","Female","Total")
gender = gender[gendervars]
head(gender)


##Load Data by Age
age = read_excel("stb20172019.xlsx")
age
agevars = c("Month","Place of Residence","14 & Below","15 - 19","20 - 24","25 - 34","35 - 44","45 - 54","55 - 64", "65 & Above", "Not Stated")
age = age[agevars]
head(age)


##Load World Map
library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

##Change age and gender country names to match iso_codes
old_names <- c("USA", "Vietnam", "Hong Kong SAR", "Taiwan","South Korea", "UK","South Africa (Rep of)")
new_names <- c("United States of America", "Viet Nam", "Hong Kong, SAR China", "Taiwan, Republic of China", "Korea (South)", "United Kingdom", "South Africa")

for (i in 1:length(old_names)){
  gender$`Place of Residence`[gender$`Place of Residence` == old_names[i]] <- new_names[i]
}
for (i in 1:length(old_names)){
  age$`Place of Residence`[age$`Place of Residence` == old_names[i]] <- new_names[i]
}

##Change worlddata country names to match iso_codes
old_names1 <- c("UK", "South Korea", "Taiwan", "USA", "Vietnam")
new_names1 <- c("United Kingdom", "Korea (South)", "Taiwan, Republic of China","United States of America", "Viet Nam")
for (i in 1:length(old_names1)){
  world_data$region[world_data$region == old_names1[i]] <- new_names1[i]
}
head(world_data)
head(gender)
unique(gender$ISO3)
head(age)

##Add ISO Codes to world data and age and gender
age['ISO3'] <- iso_codes$ISO3[match(age$`Place of Residence`, iso_codes$Country)]
gender['ISO3'] <- iso_codes$ISO3[match(gender$`Place of Residence`, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]


#library(dplyr)    


##Melt age and gender
#install.packages("reshape")
library(reshape2)

age_melt <- melt(age, id = c("Month", "ISO3", "Place of Residence"), 
                           variable.name = "Indicator", value.name = "Value")
age_melt$Value <- as.numeric(age_melt$Value)
head(age_melt)
gender_melt <- melt(gender, id = c("Month", "ISO3", "Place of Residence"), 
                 variable.name = "Indicator", value.name = "Value")
gender_melt$Value <- as.numeric(gender_melt$Value)
head(gender_melt)


# Merge data containing same column names with rbind() 

age_melt["DataType"] <- rep("Age", nrow(age_melt))
head(age_melt)
gender_melt["DataType"] <- rep("Gender", nrow(gender_melt))
df <- rbind(age_melt, gender_melt)
df[] <- lapply(df, as.character)
df$Value <- as.numeric(df$Value)
head(world_data)


##Define worldMaps function
worldMaps <- function(df, world_data, data_type, period, indicator){
  
# Function for setting the aesthetics of the plot
my_theme <- function () { 
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     legend.position = "right",
                     panel.border = element_blank(), 
                     strip.background = element_rect(fill = 'white', colour = 'white')
                    
                     )
}

# Select only the data that the user has selected to view
plotdf <- df[df$Indicator == indicator & df$DataType == data_type & df$Month == period,]
plotdf <- plotdf[!is.na(plotdf$ISO3), ]

# Add the data the user wants to see to the geographical world data
world_data['DataType'] <- rep(data_type, nrow(world_data))
world_data['Period'] <- rep(period, nrow(world_data))
world_data['Indicator'] <- rep(indicator, nrow(world_data))
world_data['Value'] <- plotdf$Value[match(world_data$ISO3, plotdf$ISO3)]

# Specify the plot for the world map

options(repr.plot.width=100, repr.plot.height=8)
library(RColorBrewer)
library(ggiraph)
g <- ggplot() + 
  geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'grey70', size = 0.1,
                           aes(x = long, y = lat, fill = Value, group = group, 
                               tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
  scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
  labs(fill = NULL, color = data_type, title = NULL, x = NULL, y = NULL) + 
  my_theme() 
head(world_data)

return(g)
}



##R Shiny App
library(shiny)
library(ggiraph)

# Define the UI
ui = fluidPage(
  
  # App title
  titlePanel("Age and Gender"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "data_type",
                  label = "Choose the type of data you want to see:",
                  choices = list("Age" = "Age", "Gender" = "Gender")),
      
      # Second input (choices depend on the choice for the first input)
      uiOutput("secondSelection"),
      
      # Third input (choices depend on the choice for the first and second input)
      uiOutput("thirdSelection")
      ,width = 3
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Hide errors
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      # Output: interactive world map
      girafeOutput("distPlot") ,width=9
      
    )
  )
)

# Define the server
server = function(input, output) {
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)), width_svg=10, zoom_max=10,width=1)
  })
  
  # Change the choices for the second selection on the basis of the input to the first selection
  output$secondSelection <- renderUI({
    choice_second <- as.list(unique(df$Month[which(df$DataType == input$data_type)]))
    selectInput(inputId = "period", choices = choice_second,
                label = "Choose the period for which you want to see the data:")
  })
  
  # Change the choices for the third selection on the basis of the input to the first and second selections
  output$thirdSelection <- renderUI({
    lab <- ifelse(input$data_type == "Age", "age group", "gender")
    choice_third <- as.list(unique(df$Indicator[df$DataType == input$data_type & df$Month == input$period]))
    selectInput(inputId = "indicator", choices = choice_third,
                label = paste0("Choose the type of ", lab, " you want to explore:"))
  })
}

# Run the App
shinyApp(ui = ui, server = server)

