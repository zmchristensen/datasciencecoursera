library(shiny)
library(tm)
library(SnowballC)
library(RWeka)
library(reshape)
library(dplyr)
source(file = "prediction.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  trim.leading <- function (x)  sub("^\\s+", "", x)

  # Create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = "")
  }
  
  dir <- "nostop"
  types <- c("blogs", "news", "tweets")
  
  uni <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-unigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-unigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-unigram.RData", sep = ""))
  )
  updateProgress(detail = "Loaded unigrams")
  
  bi <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-bigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-bigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-bigram.RData", sep = ""))
  )
  updateProgress(detail = "Loaded bigrams")
  
  tri <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-trigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-trigram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-trigram.RData", sep = ""))
  )
  updateProgress(detail = "Loaded trigrams")
  
  tetra <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-tetragram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-tetragram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-tetragram.RData", sep = ""))
  )
  updateProgress(detail = "Loaded tetragrams")
  
  penta <- rbind(
    preprocessGram(paste(dir, "/", types[1], "-pentagram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[2], "-pentagram.RData", sep = "")),
    preprocessGram(paste(dir, "/", types[3], "-pentagram.RData", sep = ""))
  )  
  updateProgress(detail = "Loaded pentagrams")
  
  session$sendCustomMessage(type = "toggle", message = list())
  
  predictions <- reactive({
    text <- input$text
    results <- predict(text, uni, bi, tri, tetra, penta)
    results <- results[order(results$count, decreasing = TRUE),]
    
    df <- data.frame()
    if (nchar(text) > 0) {
      probabilities <- results$count / sum(results$count)
    
      df <- data.frame(results[,1], probabilities, row.names = c(), stringsAsFactors = FALSE)
      colnames(df) <- c("Word", "Probability")
    }
    
    session$sendCustomMessage(type = "hideButtons", message = list(count = nrow(df)))
    
    predictedProb <- 0
    if (nrow(df) > 0) {
      predictedProb <- predictedProb + round(df[1, "Probability"] * 100, digits = 0)
      session$sendCustomMessage(type = "updateButton", message = list(id = "first", label = df[[1, 1]], percentage = round(df[1, "Probability"] * 100, digits = 0)))
    }
    if (nrow(df) > 1) {
      predictedProb <- predictedProb + round(df[2, "Probability"] * 100, digits = 0)
      session$sendCustomMessage(type = "updateButton", message = list(id = "second", label = df[[2, 1]], percentage = round(df[2, "Probability"] * 100, digits = 0)))      
    }
    if (nrow(df) > 2) {
      predictedProb <- predictedProb + round(df[3, "Probability"] * 100, digits = 0)
      session$sendCustomMessage(type = "updateButton", message = list(id = "third", label = df[[3, 1]], percentage = round(df[3, "Probability"] * 100, digits = 0)))      
    }
    
    session$sendCustomMessage(type = "predictedProbability", message = list(predicted = predictedProb))
    
    df
  })
    
  
  output$table <- renderTable({
    predictions()
  })

  
  observeEvent(input$first, {
    n <- trim.leading(paste(input$text, predictions()[1,1], sep = " "))
    updateTextInput(session, inputId = "text", value = n)    
  })
    
  observeEvent(input$second, {
    n <- trim.leading(paste(input$text, predictions()[2,1], sep = " "))
    updateTextInput(session, inputId = "text", value = n)        
  })
  
  observeEvent(input$third, {
    n <- trim.leading(paste(input$text, predictions()[3,1], sep = " "))
    updateTextInput(session, inputId = "text", value = n)
  })
  
  outputOptions(output, "table", suspendWhenHidden=FALSE)
})