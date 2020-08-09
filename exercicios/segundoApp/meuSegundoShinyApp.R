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
  "Série temporal crimes", #Como eu faço pra inserir o nome do municipio aqui?
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
  plotOutput("bar")
  )

server <- function(input, output, session) {
dados <- read_rds("dados/ssp.rds")
col_crime <- dados %>% 
  names() %>% 
  .[6:28]

dados_tidy <- dados %>% 
  gather(tipo_crime, numero_casos, col_crime) %>% 
  filter(numero_casos != 0)

output$bar <- renderPlot({
  dados_tidy %>% 
    filter(tipo_crime == input$crime,
           municipio_nome %in% input$muni) %>% 
    group_by(ano, municipio_nome) %>% 
    summarise(total_crimes = sum(numero_casos)) %>% 
    ggplot(aes(x = ano, y = total_crimes, color = municipio_nome))+
    geom_line()+
    geom_point()
})

}

shinyApp(ui, server)
