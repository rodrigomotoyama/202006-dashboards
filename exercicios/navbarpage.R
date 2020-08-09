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
  
  ui <- navbarPage(
    title = "Shiny com navbarPage",
    tabPanel(
      "Número total de crimes no estado de São Paulo",
      selectInput(
        inputId = "crime",
        label = "Selecione o crime",
        choices = col_crime,
        multiple = T
      ),
      # selectInput(
      #   inputId = "muni",
      #   label = "Selecione o municipio",
      #   choices = col_muni,
      #   multiple = T
      # ),
      titlePanel("Período em anos"),
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
    ),
    navbarMenu(
      title = "Outros",
      tabPanel(
        title = "Serie temporal",
        selectInput(
          inputId = "crime_serie",
          label = "Selecione o crime",
          choices = col_crime
        ),
        selectInput(
          inputId = "muni_serie",
          label = "Selecione o municipio",
          choices = col_muni,
          multiple = T
        ),
        titlePanel("Período em anos"),
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              inputId = "periodo_serie",
              label = "Selecione um período:",
              min = as.integer(min(dados$ano)),
              max = as.integer(max(dados$ano)),
              value = c(2010, 2013),
              step = 1
            )
          ),
          mainPanel(
            "main panel",
            plotOutput("serie")
          )
        )
      )
    )
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
          filter(ano %in% input$periodo[1]:input$periodo[2],
                 tipo_crime %in% input$crime) %>% 
          group_by(tipo_crime) %>% 
          summarise(total_crimes = sum(numero_casos)) %>% 
        ggplot(aes(x = tipo_crime, y = total_crimes, fill = total_crimes))+
        geom_col()
    })
    output$serie <- renderPlot({
      dados_tidy %>%
        filter(tipo_crime == input$crime_serie,
               municipio_nome %in% input$muni_serie,
               ano %in% input$periodo_serie[1]:input$periodo_serie[2]) %>%
        group_by(ano, municipio_nome, tipo_crime) %>%
        summarise(total_crimes = sum(numero_casos)) %>%
        ggplot(aes(x = ano, y = total_crimes, color = municipio_nome))+
        geom_line()+
        geom_point()+
        scale_x_continuous(breaks = seq(input$periodo_serie[1], input$periodo_serie[2], 1))
    })
    
  }
  
  shinyApp(ui, server)
