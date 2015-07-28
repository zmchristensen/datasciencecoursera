library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  trim.leading <- function (x)  sub("^\\s+", "", x)
  
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
    createTable(input$text)
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