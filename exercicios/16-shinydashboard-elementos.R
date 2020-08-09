library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "IMDB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("informações gerais", tabName = "info"),
      menuItem("Orçamentos", tabName = "orcamento"),
      menuItem("Receitas", tabName = "receitas")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "info",
        h1("informações gerais dos filmes"),
        infoBoxOutput(
          outputId = "num_filmes"
        ),
        infoBoxOutput(
          outputId = "num_diretores"
        ),
        infoBoxOutput(
          outputId = "num_atores"
        )
      ),
      tabItem(
        tabName = "orcamento",
        h1("Analisando orçamentos")
      ),
      tabItem(
        tabName = "receitas",
        h1("Analisando receitas")
      )
    )
  )
)

server <- function(input, output, session) {
  
  imdb <- read_rds("dados/imdb.rds")
  
  output$num_filmes <- renderInfoBox({
    infoBox(
      title = "Número de filmes",
      value = nrow(imdb),
      fill = T,
      color = "red",
      icon = icon("baseball-ball")
    )
  }
  )
  
}

shinyApp(ui, server)
















