library(nycflights13)
library(lubridate)

library(shiny)
library(tidyverse)
library(readr)


ui <- fluidPage(
  "Selecione uma ou mais empresas", #Como eu faço pra inserir o nome do municipio aqui?
  checkboxGroupInput(
    inputId = "emp",
    label = "qq",
    choices = unique(flights$carrier),
    selected = "9E",
    inline = T
  ),
  "Número de voos por empresa ao longo de 2013",
  plotOutput(outputId = "line")
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
                            sep = "-")),
           data1 = (paste(as.character(year), 
                           as.character(month),
                           as.character(day), 
                                       sep = "-"))
    )
  
  output$line <- renderPlot({
    flights %>% 
      filter(year == 2013,
             # carrier %in% c("9E")
             carrier %in% input$emp
             ) %>% 
      group_by(data1, carrier) %>% 
      count(carrier) %>%
      ggplot(aes(x = data1, y = n, color = carrier, group = 1))+
      geom_line()+
      # theme(legend.position="none")+
      xlab("Data")+ylab("Número de voos")
  })
  
}

shinyApp(ui, server)

