library(shiny)

source(file = "../prediction.R")

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
  
  callback <- function() {
    session$sendCustomMessage(type = "toggle", message = list())
  }
  
  # Compute the new data, and pass in the updateProgress function so
  # that it can update the progress indicator.
  loadNgrams(dir = "../nostop", updateProgress, callback)

  predictions <- reactive({
    text <- input$text
    results <- predict(text)
    results <- results[order(results$count, decreasing = TRUE),]
    
    df <- data.frame()
    if (nchar(text) > 0) {
      probabilities <- results$count / sum(results$count)
    
      df <- data.frame(results[,c(1:2)], probabilities, row.names = c(), stringsAsFactors = FALSE)
      colnames(df) <- c("Word", "Source", "Probability")
    }
    
    session$sendCustomMessage(type = "hideButtons", message = list(count = nrow(df)))
    
    if (nrow(df) > 0) {
      session$sendCustomMessage(type = "updateButton", message = list(id = "first", label = df[[1, 1]], percentage = df[1, "Probability"]))
    }
    if (nrow(df) > 1) {
      session$sendCustomMessage(type = "updateButton", message = list(id = "second", label = df[[2, 1]], percentage = df[[2, "Probability"]]))      
    }
    if (nrow(df) > 2) {
      session$sendCustomMessage(type = "updateButton", message = list(id = "third", label = df[[3, 1]], percentage = df[3, "Probability"]))      
    }
    
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