library(shiny) 

shinyUI(fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "site.css")
    
  ),

  column(12,
         h2("2014 Ebola Outbreak: Cases and Deaths", class="well"),
         p("This Shiny app takes data from the recent Ebola outbreak and tracks it over time. 
           Use the slider to see the spread of the virus as a function of days since the initial infection.")
  ),
  column(6,
         div(class="well",
           sliderInput("days", "", min = 0,
                       max = 289, value = 0, step = 1,
                       pre = "Day ", animate = TRUE,
                       animationOptions(interval = 500, loop = TRUE, 
                          playButton = "Play", pauseButton = "Pause")
           )
         ),
         div(
           htmlOutput("day"),
           tableOutput("table") 
         )
  ),
  column(6,
         htmlOutput("column"),
         htmlOutput("map")         
  )
   
))