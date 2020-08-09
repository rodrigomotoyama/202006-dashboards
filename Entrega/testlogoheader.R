library(shiny)
library(shinydashboard)

header <- dashboardHeader()
anchor <- tags$p(tags$img(src = 'MLBLogo.png', height='50', width='100'),
                 'project name')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: #002D72 }"))),
  anchor,
  class = 'name')

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = anchor,
                    titleWidth = 600
                    ),
    dashboardSidebar(),
    dashboardBody())
)

shinyApp(ui, server = function(input, output, session) {})