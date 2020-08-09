library(shiny)
library(tidyverse)
library(readr)
library(lubridate)

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
    selected = "São Paulo",
    multiple = T
  ),
  dateInput(
    inputId = "mes",
    label = "Selecione o mês"
  ),
  tableOutput(outputId = "tabela")
)

server <- function(input, output, session) {
  dados <- read_rds("dados/ssp.rds")
  col_crime <- dados %>% 
    names() %>% 
    .[6:28]
  
  dados_tidy <- dados %>% 
    gather(tipo_crime, numero_casos, col_crime) %>% 
    filter(numero_casos != 0) %>% 
    mutate(data = ymd(paste(as.character(mes), as.character(ano), sep = "-")))
  
  output$tabela <- renderTable({
    # browser()
    dados_tidy %>% 
      filter(tipo_crime == input$crime,
             municipio_nome %in% input$muni,
             data == input$mes
             ) %>% 
      group_by(data, municipio_nome) %>% 
      summarise(total_crimes = sum(numero_casos)) %>%
      data.frame()
  })
  
}

shinyApp(ui, server)
