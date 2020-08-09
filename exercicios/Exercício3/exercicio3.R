library(shiny)
library(ggplot2)
library(tidyverse)

diamonds

num_vars <- diamonds %>% 
  select_if(is.numeric) %>%
  names()

categorical_vars <- diamonds %>% 
  select_if(is.ordered) %>%
  names()

diamonds <- diamonds %>% data.frame()

ui <- fluidPage(
  "Box Plot",
  selectInput(
    inputId = "variavelx",
    label = "Selecione a variável do eixo X",
    choices = categorical_vars
  ),
  selectInput(
    inputId = "variavely",
    label = "Selecione a variável do eixo Y",
    choices = num_vars
  ),
  plotOutput("boxplot")
)

server <- function(input, output, session) {
  
  output$boxplot <- renderPlot({
    ggplot(diamonds, aes(x = diamonds[,input$variavelx], 
                       y = diamonds[,input$variavely]))+
      geom_boxplot()
  })
  
}

shinyApp(ui, server)