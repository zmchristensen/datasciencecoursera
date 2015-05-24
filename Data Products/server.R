library(shiny)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)
library(googleVis)

# Load data
data <- read.csv("graph_data.csv")
data$Date <- mdy(data$Date)

dayOptions <- unique(data$Day)

shinyServer(
  function(input, output, session) {

    dayValue <- reactive({
      # paste(input$days)
      paste(dayOptions[which.min(abs(dayOptions - input$days))])
    })
    
    newData <- reactive({
      data %>% filter(Day == dayValue())
    })
    
    updateSliderInput(session, 
      "days",
      max = data[nrow(data),2])
    
    output$subtitle <- renderText({
      paste("Showing data from", data[nrow(data),1], "to", data[1,1])
    })
    
    output$day <- renderText({
      paste("<strong>Showing data from", 
            (newData())[1,1],
            "</strong>")
    })
    
    output$table <- renderTable({
      newData()[,-c(1:2)]
      dcast(newData(), Country ~ Type)
    })
    
    output$map <- renderGvis({
      gvisGeoChart(newData() %>% filter(Type == "Cases"), 
                   locationvar = "Country", 
                   colorvar = "TotalValue",
                   options = list(
                     width = 400, height = 400)
                   )      
    })
    
    output$column <- renderGvis({
      gvisColumnChart(dcast(newData(), Country ~ Type))
    })
  } 
)