library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  createTable <- function(text) {
    data.frame(strsplit(text, split = " "))
  }
  
  output$table <- renderTable({
    createTable(input$text)
  })

  output$sentence <- renderText({
    input$text
  })
    
})