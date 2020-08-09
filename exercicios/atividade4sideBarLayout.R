library(shiny)
library(tidyverse)
library(readr)

dados <- read_rds("dados/ssp.rds")
col_crime <- dados %>% 
  names() %>% 
  .[6:28]
col_muni <- dados %>% 
  count(municipio_nome) %>% 
  select(municipio_nome) %>% 
  c()

ui <- fluidPage(
  h1("Série temporal crimes"),
  selectInput(
    inputId = "crime",
    label = "Selecione o crime",
    choices = col_crime
  ),
  selectInput(
    inputId = "muni",
    label = "Selecione o municipio",
    choices = col_muni,
    multiple = T
  ),
  titlePanel("Período"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "periodo",
        label = "Selecione um período:",
        min = as.integer(min(dados$ano)),
        max = as.integer(max(dados$ano)),
        value = c(2010, 2013),
        step = 1
      )
    ),
    mainPanel(
      "main panel",
      plotOutput("bar")
    )
  )
)

server <- function(input, output, session) {
# dados <- read_rds("dados/ssp.rds")
# col_crime <- dados %>% 
#   names() %>% 
#   .[6:28]
# 
# dados_tidy <- dados %>% 
#   gather(tipo_crime, numero_casos, col_crime) %>% 
#   filter(numero_casos != 0)
# # dados_tidy %>% 
# #   filter(tipo_crime == input$crime,
# #          municipio_nome %in% input$muni,
# #          ano %in%  %>% 
#   # group_by(ano, municipio_nome) %>% 
#   # summarise(total_crimes = sum(numero_casos)) 
# 
# output$bar <- renderPlot({
#   dados_tidy %>% 
#     filter(tipo_crime == input$crime,
#            municipio_nome %in% input$muni,
#            ano %in% input$periodo[1]:input$periodo[2]) %>% 
#     group_by(ano, municipio_nome) %>% 
#     summarise(total_crimes = sum(numero_casos)) %>% 
#     ggplot(aes(x = ano, y = total_crimes, color = municipio_nome))+
#     geom_line()+
#     geom_point()+
#     scale_x_continuous(breaks = seq(input$periodo[1], input$periodo[2], 1))
# })

}

shinyApp(ui, server)
