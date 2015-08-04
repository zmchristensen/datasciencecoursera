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
    progress$set(value = value, detail = detail)
  }
  
  # Compute the new data, and pass in the updateProgress function so
  # that it can update the progress indicator.
  loadNgrams(dir = "../nostop", updateProgress)

  predictions <- reactive({
    text <- input$text
    results <- predict(text)
    results <- results[order(results$count, decreasing = TRUE),]
    
    total <- sum(results$count)
    first <- results[1,]
    second <- results[2,]
    third <- results[3,]
    
##    setButtonText(first[[ncol(first) - 1]], second[[ncol(second) - 1]], third[[ncol(third) - 1]])
    
    if (nchar(text) > 0) {
      probabilities <- rep(0.24, 3)
      
      index <- ncol(results) - 1
      df <- data.frame(results[c(1:3),index], probabilities, row.names = c(), stringsAsFactors = FALSE)
      colnames(df) <- c("Word", "Probability")
    }
    
    session$sendCustomMessage(type = "updateButton", message = list(id = "first", label = df[[1, 1]]))
    session$sendCustomMessage(type = "updateButton", message = list(id = "second", label = df[[2, 1]]))
    session$sendCustomMessage(type = "updateButton", message = list(id = "third", label = df[[3, 1]]))
    
    df
  })
    
  
  output$table <- renderTable({
    predictions()
  })

  output$sentence <- renderText({
    input$text
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
  
})