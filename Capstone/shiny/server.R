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


  
  
  createTable <- function(text) {
    if (nchar(text) > 0) {
      words <- strsplit(text, split = " ")
      probabilities <- rep(0.24, length(words))
      
      df <- data.frame(words, probabilities, row.names = c(), stringsAsFactors = FALSE)
      colnames(df) <- c("Word", "Probability")
      df      
    }
  }
  
  output$table <- renderTable({
    ## createTable(input$text)
    predict(input$text)
  })

  output$sentence <- renderText({
    input$text
  })
  
  
  observeEvent(input$first, {
    n <- trim.leading(paste(input$text, "first", sep = " "))
    updateTextInput(session, inputId = "text", value = n)
  })
    
  observeEvent(input$second, {
    n <- trim.leading(paste(input$text, "second", sep = " "))
    updateTextInput(session, inputId = "text", value = n)
  })
  
  observeEvent(input$third, {
    n <- trim.leading(paste(input$text, "third", sep = " "))
    updateTextInput(session, inputId = "text", value = n)
  })
  
})