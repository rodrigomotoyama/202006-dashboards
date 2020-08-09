library(shiny)

ui <- fluidPage(
  "Um histograma",
  plotOutput("hista") #plot output requer renderplot
)

server <- function(input, output, session) {
  
  output$hista <- renderPlot({ #renderplot requer plotOutput
    hist(mtcars$mpg)
  })
  
}

shinyApp(ui, server)