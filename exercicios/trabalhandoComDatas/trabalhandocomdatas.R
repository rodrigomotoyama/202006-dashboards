library(nycflights13)
library(lubridate)

library(shiny)
library(tidyverse)
library(readr)


ui <- fluidPage(
"Série temporal crimes", #Como eu faço pra inserir o nome do municipio aqui?
dateInput(
  inputId = "data",
  label = "Escolha o dia",
  value = "2013-01-01"
),
"Tabela descritiva",
tableOutput(outputId = "tabela"),
"Número de voos por empresa",
plotOutput("bar")
  )

server <- function(input, output, session) {
  data("flights")
  flights <- flights %>% 
    mutate(arr_delay = 
             if_else(is.na(arr_delay), 
                     if_else(is.na(arr_time),
                             0,
                             as.double(sched_arr_time - arr_time)), 
                     arr_delay),
           dep_delay = 
             if_else(is.na(dep_delay), 
                     if_else(is.na(dep_time),
                             0,
                             as.double(sched_dep_time - dep_time)), 
                     dep_delay)) %>% 
    mutate(data = ymd(paste(as.character(year), 
                            as.character(month),
                            as.character(day), 
                            sep = "-"
    )))
  
  output$tabela <- renderTable({
    flights %>%
      filter(data == input$data) %>% 
      summarise(`Número de voos` = n(),
                `Atraso médio de partida` = mean(dep_delay),
                `Atraso médio de chegada` = mean(arr_delay)
                ) %>% 
      data.frame()
  })
  output$bar <- renderPlot({
    flights %>% 
      filter(data == input$data) %>% 
      group_by(carrier) %>% 
      count(carrier) %>%
      ggplot(aes(x = carrier, y = n, fill = carrier))+
      geom_col()+
      theme(legend.position="none")+
      xlab("Empresa")+ylab("Número de voos")
  })
  
}

shinyApp(ui, server)

