library(shiny)
library(ggplot2)
library(tidyverse)

# diamonds <- ggplot2::diamonds

# diamonds <- diamonds %>% 
#   mutate(carat = as.numeric(carat),
#          depth = as.numeric(depth),
#          table = as.numeric(table),
#          price = as.numeric(price),
#          x = as.numeric(x),
#          y = as.numeric(y),
#          z = as.numeric(z)) %>% 
#   tibble()

num_vars <- diamonds %>% 
            select_if(is.numeric) %>%
            names()
diamonds <- diamonds %>% data.frame()
# Não consegui fazer como o diamonds como tibble.

ui <- fluidPage(
  "Vários histogramas",
  selectInput(
    inputId = "variavel",
    label = "Selecione a variável numérica",
    choices = num_vars
  ),
  plotOutput("hist")
)

server <- function(input, output, session) {
  
  output$hist <- renderPlot({
    hist(diamonds[,input$variavel])
  })
  
}

shinyApp(ui, server)
  