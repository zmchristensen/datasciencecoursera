library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
  
  observeEvent(input$action, {
    updateTextInput(session, inputId = "text", value = paste(input$text, "new", sep = " "))
  })
    
})