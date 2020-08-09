
ui <- fluidPage(
    dataTableOutput('PM_output')
  )


server <- function(input, output, session) {
  require(DT)
  dat <- read.table(text="Col1     Col2                  Col3
          Google   '5 lines description'   www.google.com
          Yahoo    '5 lines description'   www.yahoo.com", header=T, strings=F)
  dat$Col3 <- sapply(dat$Col3, function(x) 
    toString(tags$a(href=paste0("http://", x), x)))
  
  output$PM_output <- renderDataTable(expr = datatable(dat, escape=FALSE),
                                      options = list(autoWidth = T))
}

shinyApp(ui, server)

test <- read_csv("Entrega/Planilha sem título - Página1.csv", )
test %>% DT::datatable()
data_table <- data %>% 
  filter(!is.na(pa), name %in% input$jogadorComp) %>% 
  group_by(name) %>% 
  summarise(
    PA = sum(pa),
    n = as.character(n()),
    H = sum(h),
    AVG = mean(avg) %>% round(3),
    OBP = mean(obp) %>% round(3),
    SLG = mean(slg) %>% round(3),
    OPS = mean(ops) %>% round(3),
    HR = sum(hr),
    Runs = sum(r),
    RBI = sum(rbi),
    SO = sum(so),
    `BB%` = paste(as.character(round(mean(bb_percent), 2), "%", sep = "")), 
    `K%` = paste(as.character(round(mean(k_percent), 2), "%", sep ="")),
    wOBA = mean(w_oba) %>% round(3),
    `wRC+` = mean(w_rc_2) %>% round(0),
    WAR = sum(war)
  ) %>% 
  mutate(
    name = paste(name, " (", n, ")", sep = ""),
  ) %>% 
  select(!n)

nomes <- data_table %>% 
  select(name) 
nomes2  <- c("stat", nomes$name)
data_table %>% 
  select(!name) %>% 
  t %>% 
  DT::datatable(colnames = nomes2,
                options = list(lengthMenu = c(5, 30, 50), pageLength = 15))