library(shiny)
library(shinydashboard)
library(janitor)
library(tidyverse)
library(fresh)

meu_tema <- create_theme(
  adminlte_color(
    light_blue = "#002D72"
  ),
  adminlte_sidebar(
    dark_bg = "#D50032"
  )
)
titulo <- tags$p(tags$img(src = 'MLBLogo.png', height='40', width='75'),
                 'Monitor MLB')


remove_pct <- function(x) {
  as.numeric(str_sub(x, end = -2))/100
}
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
{path1 <- map(1:3, ~str_c("dados/mlbBatting18712019-", .x, ".csv"))
batting_data <- map(path1, read_csv) %>% 
  reduce(full_join) %>% 
  clean_names() %>% 
  mutate_at(c("k_percent",
              "bb_percent",
              "ld_percent", "gb_percent", "fb_percent", 
              "iffb_percent","hr_fb",    #"ifh",
              "ifh_percent", "buh",    
              "buh_percent", "pull_percent","cent_percent","oppo_percent","soft_percent",
              "med_percent", "hard_percent"), remove_pct)}# batting data

{path2 <- map(1:5, ~str_c("dados/mlbPitching18712019-", .x, ".csv"))
pitching_data <- map(path2, read_csv) %>% 
  reduce(full_join) %>% 
  clean_names() 

pitching_data_post <- pitching_data %>% 
  select(!c(playerid, x_fip, tbf, wp, bk, k_bb_percent,
            era_2, fip_2, x_fip_2, e_f, siera, ra9_war,
            "bip_wins", "lob_wins", "fdp_wins", "rar", "dollars")) %>% 
  mutate_at(c("k_percent","lob_percent", "gb_percent", "hr_fb",
              "bb_percent",
              "ld_percent", "gb_percent", "fb_percent", 
              "iffb_percent","hr_fb", "pull_percent","cent_percent","oppo_percent","soft_percent",
              "med_percent", "hard_percent"), remove_pct)} #pitching data

{path3 <- "dados/mlbSalarios.csv"
mlb_salarios <- path3 %>%
  read_csv() %>%
  mutate(ano_inicial = years %>%
           str_split(" \\(|\\)")) %>%
  mutate(ano_inicial = ano_inicial[[1]][2],
         contract_year = years %>%
           str_sub(end = 2) %>%
           as.numeric())
mlb_salarios <- mlb_salarios %>%
  mutate(free_agent_year =
           as.numeric(ano_inicial) + as.numeric(contract_year)) %>%
  mutate_at(c("salary", "total_value", "avg_annual"),
            ~str_replace_all(.x, "\\$ |,", "")) %>%
  mutate_at(c("salary", "total_value", "avg_annual"),
            as.numeric)} #salarios mlb

{
  description_batting_stats <- read_csv("dados/battingStats.csv") %>% 
    clean_names() %>% 
    mutate_at(c("significado", "traducao_livre", "definicao"),
              ~firstup(.x))
  
} #description_batting_stats

{
  description_pitching_stats <- read_csv("dados/Pitching estats - Página1.csv") %>% 
    clean_names() %>% 
    mutate_at(c("significado", "traducao_livre", "definicao"),
              ~firstup(.x))
  
} #description_pitching_stats

ui <- dashboardPage(
  dashboardHeader(title = titulo,
                  titleWidth = 600),
  dashboardSidebar(
    # # Custom CSS to hide the default logout panel
    # tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    # 
    # # The dynamically-generated user panel
    # uiOutput("userpanel"),
    sidebarMenu(
      menuItem("Introdução", 
               tabName = "intro", 
               icon = icon("baseball-ball")),
      menuItem("Análise (1871~2019)", tabName = "estats",
               icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    use_theme(meu_tema),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
     # href = "http://www.mlbstatic.com/team-logos/league-on-dark/1.svg"
    ),
    tabItems(
      tabItem(
        tabName = "intro",
        h2("Introdução"),
        h4("Neste dashboard será possível navegar por visualização de séries temporais 
      datadas a partir de 1871 até 2019."
        ),
        h4("O objetivo deste dashboard é apresentar brevemente a evolução das estatísticas de beisebol 
    junto com o esporte com o passar dos anos, e porque algumas dessas estatísticas encontram-se
           ultrapassadas perante o cenário atual."),
        h4("Abaixo segue uma tabela com as definições das estatísticas. Eu adicionei alguns vídeos para ajudar
           no entendimento delas."),
        uiOutput("bat_pit1"),
        h3(textOutput("titulo_tabela")),
        DT::dataTableOutput("tabela")
      ),
      tabItem(
        tabName = "estats",
        h2("Estatísticas (1871~2019)"),
        fluidRow(
          box(
            # title = h4("Estatística de:"),
            uiOutput("bat_pit2"),
            width = 4,
            height = 110
          ),
          box(
            h4(uiOutput("intro_conclusão")),
            width = 8,
            height = 110
          )
        ),
          sidebarLayout( 
              box(
                # title = h4("Filtros"),
                sidebarPanel(
                  sliderInput(
                    "anos",
                    "Escolha uma janela de tempo:",
                    min = 1871,
                    max = 2019,
                    value = c(1871, 2013),
                    step = 1
                  ),
                  uiOutput("estat"),
                  width = 12
                ),
                width = 4
              ),
            mainPanel(
              box(
                h4(uiOutput("first_text")),
                width = "100px"
              ),
              box(
                plotly::plotlyOutput("plotFill1"),
                width = "100px"
              ),
              box(
                h4(uiOutput("second_text")),
                width = "100px"
              ),
              box(
                plotly::plotlyOutput("plotFill2"),
                width = "100px"
              )
            )
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # output$userpanel <- renderUI({
  #   # session$user is non-NULL only in authenticated sessions
  #   if (!is.null(session$user)) {
  #     sidebarUserPanel(
  #       span("Logged in as ", session$user),
  #       subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
  #   }
  # })

  
  output$bat_pit1 <- renderUI({
    
    tipo_estat <- c("Batedores", "Arremessadores")
    
    selectInput(
      inputId = "tipo_estat",
      label = h4("Estatística de:"),
      choices = tipo_estat,
      width = "25%"
    )
    
  })
  
  output$titulo_tabela <- renderText({
    req(input$tipo_estat)
    if (input$tipo_estat == "Batedores") {
      "Descrição de estatísticas de rebatedores"
    } else {
      "Descrição de estatísticas de arremessadores"
    }
  })
  
  output$tabela <- DT::renderDataTable({
    req(input$tipo_estat)
    if (input$tipo_estat == "Batedores") {
      req(input$tipo_estat)
      for (i in 1:length(description_batting_stats$definicao)) {
        if (!is.na(description_batting_stats$link1[[i]])) {
          description_batting_stats$definicao[[i]] <- 
            as.character(tags$a(href=description_batting_stats$link1[[i]], 
                              description_batting_stats$definicao[[i]]))
        }
      }
      description_batting_stats %>% 
        select(!link1)
    } else {
      req(input$tipo_estat)
      for (i in 1:length(description_pitching_stats$definicao)) {
        if (!is.na(description_pitching_stats$link1[[i]])) {
          description_pitching_stats$definicao[[i]] <- 
            as.character(tags$a(href=description_pitching_stats$link1[[i]], 
                                description_pitching_stats$definicao[[i]]))
        }
      }
      description_pitching_stats %>% 
        select(!link1)
    }
  }, escape = F)
  
  {battingDataByYear <- batting_data %>% 
      select(!c(bs_r, off, def, playerid, 
                iffb_percent, buh, buh_percent)) %>% 
      group_by(season) %>% 
      mutate(
        numero_de_jogadores = g > mean(g)
      ) %>% 
      mutate_at(c("g", "pa", "hr", "r", "rbi", "sb", "ab", "h",
                  "x1b", "x2b", "x3b", "bb", "ibb", "so", "hbp",
                  "sf", "sh", "gdp", "cs", "ifh", "numero_de_jogadores"), sum) %>%
      mutate_at(c("bb_percent", "k_percent", "babip", "avg", "obp",
                  "slg",  "w_oba", "w_rc", "war", "ld_percent", "gb_percent", "fb_percent",
                  "hr_fb",  "ifh_percent", "pull_percent",
                  "cent_percent","oppo_percent","soft_percent",
                  "med_percent", "hard_percent"), ~mean(.x, na.rm = T)) %>%
      mutate_at(c("bb_percent", "k_percent", "babip", "avg", "obp",
                  "slg",  "w_oba", "ld_percent", "gb_percent", "fb_percent",
                  "hr_fb", "ifh_percent", "pull_percent",
                  "cent_percent","oppo_percent","soft_percent",
                  "med_percent", "hard_percent"), ~.x*100) %>%
      group_by(season, bb_percent, k_percent, babip, avg, obp, 
               slg,  w_oba, w_rc, war, ld_percent, gb_percent, fb_percent, 
               hr_fb, ifh,  ifh_percent, pull_percent,
               cent_percent,oppo_percent,soft_percent,
               med_percent, hard_percent, g, pa, hr, r, rbi, sb, ab, h,
               x1b, x2b, x3b, bb, ibb, so, hbp,
               sf, sh, gdp, cs, numero_de_jogadores) %>%
      count() %>%
      ungroup() %>% 
      select(!n)} #batting data by year
  
  {pitchingDataByYear <- pitching_data_post %>% 
      group_by(season) %>% 
      mutate(ip_mean = mean(ip, na.rm = T),
             gs_mean = mean(gs, na.rm = T)) %>% 
      mutate_at(c("w",  "l", "sv", "gs", "ip", "cg", "sh_o", "hld", "bs", "h",           
                  "r", "er", "hr", "bb", "ibb", "hbp", "so", "rs", "balls", "strikes"), sum) %>%
      mutate_at(c("k_9", "bb_9", "hr_9", "babip", "lob_percent", "gb_percent", "hr_fb",
                  "war", "er", "k_bb", "k_percent", "bb_percent", "avg", "whip",        
                  "gb_fb", "ld_percent", "fb_percent", "rs_9", "pull_percent", "cent_percent",
                  "oppo_percent", "soft_percent", "med_percent", "hard_percent"
      ), ~mean(.x, na.rm = T)) %>% 
      mutate_at(c("g", "babip", "lob_percent", "gb_percent", "hr_fb",
                  "k_percent", "bb_percent", "avg",
                  "gb_fb", "ld_percent", "fb_percent", "pull_percent", "cent_percent",
                  "oppo_percent", "soft_percent", "med_percent", "hard_percent"
      ), ~.x*100) %>%
      mutate(ip_per_gs = ip_mean/gs_mean) %>% 
      group_by(season,
               k_9, bb_9, hr_9, babip, lob_percent, gb_percent, hr_fb,
               war, er, k_bb, k_percent, bb_percent, avg, whip,        
               gb_fb, ld_percent, fb_percent, rs_9, pull_percent, cent_percent,
               oppo_percent, soft_percent, med_percent, hard_percent,
               w,  l, sv, g, gs, ip, cg, sh_o, hld, bs, h,           
               r, er, hr, bb, ibb, hbp, so, rs, balls, strikes, ip_mean, ip_per_gs
      ) %>% 
      count() %>%
      ungroup() %>% 
      select((!n))
    } #pitching data by year
  
  output$bat_pit2 <- renderUI({
    
    tipo_estat <- c("Batedores", "Arremessadores")
    
    selectInput(
      inputId = "tipo_estat2",
      label = h4("Estatística de:"),
      choices = tipo_estat,
      width = "100%"
    )
    
  })
  
  output$intro_conclusão <- renderUI({
    # "As estatísticas de beisebol nos Estados Unidos existem desde 1871. 
    # Algumas foram criadas baseando-se em como o jogo funcionava na época - 
    # o que mudou muito, seja com o advento da tecnologia, evolução da biomecânica do esporte, 
    # ou em mudanças de regras como o número de jogos em uma temporada 
    # ou o número de jogadores em uma equipe."
    tags$p('As estatísticas de beisebol nos Estados Unidos existem desde 1871. 
    Algumas foram criadas baseando-se em como o jogo funcionava na época - como',
    tags$em('avg, wins'), 'e',  tags$em('loss.'), 
    'Porém, algumas delas ficaram ultrapassadas e veremos alguns dos motivos disso.')
  })
  
  output$estat <- renderUI({
    req(input$tipo_estat2)
    batting_estat_names <- battingDataByYear %>% 
      select(!c(season)) %>% 
      names() %>% 
      c("numero_de_jogadores")
    
    pitching_estat_names <- pitchingDataByYear %>% 
      select(!c(season)) %>% 
      names() %>% 
      c(., "ip_mean")
    
    if (input$tipo_estat2 == "Batedores") {
      selectInput(
        inputId = "estat_batting",
        label = "Selecione as estatísticas de rebatedores:",
        choices = batting_estat_names,
        multiple = T,
        selected = c("h", 'x1b', 'x2b', 'x3b', 'hr', 'avg', "ld_percent", "fb_hr", "fb_percent"
                     )
      )
    } else {
      selectInput(
        inputId = "estat_pitching",
        label = "Selecione as estatísticas de arremessadores:",
        choices = pitching_estat_names,
        multiple = T,
        selected = c('w', 'l','cg', 'gs', 'k_9', 'bb_9', 'ip_per_gs')
      )
    }
  })
  
  output$first_text <- renderUI({
    req(input$tipo_estat2)
    if (input$tipo_estat2 == "Batedores") {
      tags$p('Em 1876 surge o prêmio',  tags$em('batting title,'),
             'contemplado ao jogador com maior média de rebatida', tags$em('(avg).'), 
             'O prêmio condiz com o cenário da época, 
             onde o total de rebatidas de uma base', tags$em('(x1b)'), 
             'caminha próximo do total de rebatidas válidas', tags$em('(h)'), 
             'por temporada.')
      
      # HTML(as.character(p('My first paragraph, with some ',
      #   strong('bold'),
      #   ' text.')))
      # as.character(div(HTML("Old <em>Faithful Geyser</em> Data")))
    } else {
      tags$p('Estatísticas como', tags$em('win'), 'e', tags$em('loss'), 
             ', faziam sentido para arremessadores, 
            pois era muito comum os arremessadores iniciais 
            arremessassem o jogo inteiro, mas isso muda com o tempo, comparando as cuvas de
               jogos iniciados', tags$em('gs'), 'e jogos completos', tags$em('(cg)'), '.')
    }
  })
  
  output$plotFill1 = plotly::renderPlotly({
    
    req(input$estat_batting)
    req(input$tipo_estat)
    
    if (input$tipo_estat2 == "Batedores") {
      req(input$estat_batting)
      
      batting_data_classified <- battingDataByYear %>% 
        select(season, input$estat_batting) %>% 
        filter(season %in% input$anos[1]:input$anos[2]) %>% 
        pivot_longer(cols = input$estat_batting, names_to = "Estatística", values_to = "value") %>% 
        group_by(`Estatística`) %>% 
        mutate(
          classificador = mean(value, na.rm = T)>100
        ) %>% 
          ungroup()
      
      if (is.null(input$estat_batting)|
          mean(batting_data_classified$classificador, na.rm = T) == 0) {
        req(input$estat_batting)
        plot1 <- ggplot(data.frame(""))+
          ggtitle("Nenhuma variável de valor total (por temporada) maior que 100 selecionada")+
          theme(plot.title = element_text(size = 10.5))
      } else {
        req(input$estat_batting)
        plot1 <- batting_data_classified %>% 
          filter(classificador == T) %>%
          # plotly::plot_ly(x = ~season, y = ~value, color = ~stat, mode = 'lines')
          ggplot(aes(x = season, y = value, color = `Estatística`))+
          geom_line()+
          xlab(label = "Ano")+
          ylab(label = "Total (por temporada)")+
          ggtitle(paste("Rebatedores (", 
                        as.character(input$anos[1]),
                        " ~ ",
                        as.character(input$anos[2]),
                        ")", 
                        sep = ""))
      }
    } else {
      req(input$estat_pitching)
      pitching_data_classified <- pitchingDataByYear %>% 
        select(season, input$estat_pitching) %>% 
        filter(season %in% input$anos[1]:input$anos[2]) %>% 
        pivot_longer(cols = input$estat_pitching, names_to = "Estatística", values_to = "value") %>% 
        group_by(`Estatística`) %>% 
      mutate(
        classificador = mean(value, na.rm = T)>100
      ) %>% 
        ungroup()
      
      if ( 
          is.null(input$estat_pitching[1])|
          mean(pitching_data_classified$classificador, na.rm = T) == 0) {
        req(input$estat_pitching)
        plot1 <- ggplot(data.frame(""))+
          ggtitle("Nenhuma variável com valor total (por temporada) maior que 100 selecionada")+
          theme(plot.title = element_text(size = 10.5))
      } else {
        req(input$estat_pitching)
        plot1 <- pitching_data_classified %>% 
          filter(classificador == T) %>% 
          ggplot(aes(x = season, y = value, color = `Estatística`))+
          geom_line()+
          xlab(label = "Ano")+
          ylab(label = "Total (por temporada)")+
          ggtitle(paste("Arremessadores (", 
                        as.character(input$anos[1]),
                        " ~ ",
                        as.character(input$anos[2]),
                        ")", 
                        sep = ""))
      }
    }
    
    plotly::ggplotly(plot1)
  })
  
  output$second_text <- renderUI({
    req(input$tipo_estat2)
    if (input$tipo_estat2 == "Batedores") {
      tags$p('Com o surgimento de' , tags$em('Statcast'), 
      'em 2002 - um sistema de monitoramento por vídeo, ainda em fase de testes - 
        a MLB se torna capaz de capturar todos os movimentos em campo, seja da bola, 
      dos jogadores ou dos defensores. Medições mais refinadas surgem, 
      dando cada vez mais informações ao público e especialistas no esporte, 
      mas ainda não possuem a mesma fama de estatísticas clássicas', tags$em('(avg, rbis e hits,'),
      'por exemplo),', tags$a(href='https://www.instagram.com/p/CBZNGwPjNyF/', 
                              'que por vezes influenciam, mais do que deviam, 
                              em votações de premiações.')
      )
    } else {
      tags$p('Observando a evolução de entradas arremessadas por jogo iniciado',  tags$em('(ip_per_gs)'), '
               notamos um declínio constante, muito ligado com a desempenho do jogador e
               prevenção de lesões. Apesar de', tags$em('Win'), 'e', tags$em('Loss'), 
               'serem desatualizadas nos dias de hoje,
               ainda possuem uma grande influência em decisões de premiações, 
               mesmo que não possuam valor na 
               análise de desempenho de um arremessador.')
    }
  })
  
  output$plotFill2 = plotly::renderPlotly({
    
    req(input$estat_batting)
    req(input$tipo_estat)
    
    if (input$tipo_estat2 == "Batedores") {
      req(input$estat_batting)
      
      batting_data_classified <- battingDataByYear %>% 
        select(season, input$estat_batting) %>% 
        filter(season %in% input$anos[1]:input$anos[2]) %>% 
        pivot_longer(cols = input$estat_batting, names_to = "Estatística", values_to = "value") %>% 
        group_by(`Estatística`) %>% 
      mutate(
        classificador = mean(value, na.rm = T)>100
      ) %>% 
        ungroup()
      
      if (is.null(input$estat_batting)|
          mean(batting_data_classified$classificador, na.rm = T) == 1) {
        plot2 <- ggplot(data.frame(""))+
          ggtitle("Nenhuma variável de valor médio (por temporada) menor que 100 selecionada")+
          theme(plot.title = element_text(size = 10.5))
      } else {
        plot2 <- batting_data_classified %>% 
          filter(classificador == F) %>%
          ggplot(aes(x = season, y = value, color = `Estatística`))+
          geom_line()+
          ylab(label = "Value (%)")+
          xlab(label = "Ano")+
          ylab(label = "Média (por temporada)")+
          ggtitle(paste("Rebatedores (", 
                        as.character(input$anos[1]),
                        " ~ ",
                        as.character(input$anos[2]),
                        ")", 
                        sep = ""))
      }
      
    } 
    else {
      pitching_data_classified <- pitchingDataByYear %>% 
        select(season, input$estat_pitching) %>% 
        filter(season %in% input$anos[1]:input$anos[2]) %>% 
        pivot_longer(cols = input$estat_pitching, names_to = "Estatística", values_to = "value") %>% 
        group_by(`Estatística`) %>% 
      mutate(
        classificador = mean(value, na.rm = T)>100
      ) %>% 
        ungroup()
      
      if (is.null(input$estat_pitching)|
          mean(pitching_data_classified$classificador, na.rm = T) == 1) {
        plot2 <- ggplot(data.frame(""))+
          ggtitle("Nenhuma variável de valor médio (por temporada) menor que 100 selecionada")+
          theme(plot.title = element_text(size = 10.5))
      } else {
        plot2 <- pitching_data_classified %>% 
          filter(classificador == F) %>%
          ggplot(aes(x = season, y = value, color = `Estatística`))+
          geom_line()+
          ylab(label = "Value (<100)")+
          xlab(label = "Ano")+
          ylab(label = "Média (por temporada)")+
          ggtitle(paste("Arremessadores (", 
                        as.character(input$anos[1]),
                        " ~ ",
                        as.character(input$anos[2]),
                        ")", 
                        sep = ""))
      }
    }
    
    plotly::ggplotly(plot2)
  })
  
  
}

shinyApp(ui, server)









