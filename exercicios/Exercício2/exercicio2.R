library(shiny)
library(ggplot2)

column <- names(mtcars)

ui <- fluidPage(
  "Dispersão com dois inputs",
  selectInput(
    inputId = "variavelx",
    label = "Selecione a variável do eixo X",
    choices = column,
    selected = 'mpg'
  ),
  selectInput(
    inputId = "variavely",
    label = "Selecione a variável do eixo Y",
    choices = column,
    selected = 'cyl'
  ),
  plotOutput("points")
)

server <- function(input, output, session) {
  
  output$points <- renderPlot({
    ggplot(mtcars, aes(x = mtcars[,input$variavelx], 
                       y = mtcars[,input$variavely]))+
      geom_point()
    # points(x = mtcars[,input$variavelx], 
    #        y = mtcars[,input$variavely])
  })
  
}

shinyApp(ui, server)


# ggplot(mtcars, aes(x = mtcars[,'cyl'], y = 'mpg'))+
#   geom_point()
