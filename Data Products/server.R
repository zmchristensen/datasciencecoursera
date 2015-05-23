library(shiny)

# Plotting 
library(ggplot2)
# library(rCharts)
library(ggvis)
suppressPackageStartupMessages(library(googleVis))

# Data processing libraries
# library(data.table)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)

# Required by includeMarkdown
# library(markdown)

# It has to loaded to plot ggplot maps on shinyapps.io
# library(mapproj)
# library(maps)

# Load data
data <- read.csv("graph_data.csv")
data$Date <- mdy(data$Date)

dayOptions <- unique(data$Day)

shinyServer(
  function(input, output, session) {

    dayValue <- reactive({
      paste(input$days)
    })

    updateSliderInput(session, 
      "days",
      max = data[nrow(data),2])
    
    output$subtitle <- renderText({
      paste("Showing data from", data[nrow(data),1], "to", data[1,1])
    })
    
    output$day <- renderText({
      paste("<strong>Showing data from", 
            filter(data, Day == dayValue())[1,1],
            "</strong>")
    })
    
    output$table <- renderTable({
      d <- data %>% filter(Day == dayValue())
      d[,-c(1:2)]
      dcast(d, Country ~ Type)
    })
    
    output$map <- renderGvis({
      gvisGeoChart(data %>% filter(Day == dayValue()), 
                   locationvar = "Country", 
                   colorvar = "TotalValue", 
                   options = list(
                     width = 400, height = 400)
                   )      
    })
  } 
)