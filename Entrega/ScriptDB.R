library(tidyverse)
library(janitor)


remove_pct <- function(x) {
  as.numeric(str_sub(x, end = -2))/100
}
path1 <- map(1:3, ~str_c("Entrega/dados/mlbBatting18712019-", .x, ".csv"))
batting_data <- map(path1, read_csv) %>% 
  reduce(full_join) %>% 
  clean_names() %>% 
  mutate_at(c("k_percent",
              "bb_percent",
              "ld_percent", "gb_percent", "fb_percent", 
              "iffb_percent","hr_fb",    "ifh",     "ifh_percent", "buh",    
              "buh_percent", "pull_percent","cent_percent","oppo_percent","soft_percent",
              "med_percent", "hard_percent"), remove_pct)
battingDataByYear <- batting_data %>% 
  select(!c(bs_r, off, def, playerid, 
            iffb_percent, buh, buh_percent)) %>% 
  group_by(season) %>% 
  mutate_at(c("g", "pa", "hr", "r", "rbi", "sb", "ab", "h",
              "x1b", "x2b", "x3b", "bb", "ibb", "so", "hbp",
              "sf", "sh", "gdp", "cs"), sum) %>%
  mutate_at(c("bb_percent", "k_percent", "babip", "avg", "obp",
              "slg",  "w_oba", "w_rc", "war", "ld_percent", "gb_percent", "fb_percent",
              "hr_fb", "ifh",  "ifh_percent", "pull_percent",
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
           sf, sh, gdp, cs) %>% 
  count() %>% 
  ungroup()


battingDataByYear %>% 
  select(season, c("avg", "hr")) %>% 
  filter(season %in% 1871:1889) %>% 
  pivot_longer(cols = c("avg", "hr"), names_to = "stat", values_to = "value") %>% 
  group_by(stat) %>% %>% 
mutate(
  media = mean(value)
) %>% 
  ungroup()
  
# arquivos <- map(1:3, ~paste0("Entrega/dados/", "mlbBatting",.x, ".csv"))
# map2(map(path, read_csv), arquivos, write_csv)

# path1 <- map(1:3, ~str_c("Entrega/dados/mlbBatting18712019-", .x, ".csv"))
# batting_data <- map(path1, read_csv) %>% 
#   reduce(full_join) %>% 
#   clean_names() %>% 
#   mutate(bb_percent = as.numeric(str_sub(bb_percent, end = -2)),
#          k_percent= as.numeric(str_sub(k_percent, end = -2)))
# 
# path2 <- map(1:5, ~str_c("Entrega/dados/mlbPitching18712019-", .x, ".csv"))
# pitching_data <- map(path2, read_csv) %>% 
#   reduce(full_join) %>% 
#   clean_names() 
# 
# path3 <- "Entrega/dados/mlbSalarios.csv"
# mlb_salarios <- path3 %>% 
#   read_csv() %>% 
#   mutate(ano_inicial = years %>% 
#            str_split(" \\(|\\)")) %>% 
#   mutate(ano_inicial = ano_inicial[[1]][2],
#          contract_year = years %>% 
#            str_sub(end = 2) %>% 
#            as.numeric())
# mlb_salarios <- mlb_salarios %>% 
#   mutate(free_agent_year = 
#            as.numeric(ano_inicial) + as.numeric(contract_year)) %>% 
#   mutate_at(c("salary", "total_value", "avg_annual"), 
#             ~str_replace_all(.x, "\\$ |,", "")) %>% 
#   mutate_at(c("salary", "total_value", "avg_annual"),
#             as.numeric)
# 
# 
# 
# 
# 
# 
# 
# 
#   data %>% 
#   filter(name == "Barry Bonds") %>% 
#   head(15) %>% 
#   select(bb_percent)
# data %>% filter(!is.na(pa)) %>% select(k_percent) %>% 
#   summarise(qq = paste(as.character(round(mean(k_percent), 2)), "%", sep =""))
#   summarise(qq = mean(k_percent))
# 
#   tabela <- data %>% 
#   filter(!is.na(pa), name %in% c("Mike Trout", "Ichiro Suzuki")) %>% 
#   group_by(name) %>% 
#   summarise(
#     pa = sum(pa),
#     avg = mean(avg),
#     n = as.character(n())
#     ) %>% 
#   mutate(
#     name = paste(name, " (", n, ")", sep = "")
#   ) %>% 
#   select(!n)
# nomes <- tabela %>% 
#   select(name) 
# nomes2  <- c("stat", nomes$name)
# tabela %>% t %>% 
#   DT::datatable(colnames = nomes2)
# qq <- c('HR')
# 
# # batting_data %>% names() %>% tibble() %>% 
# qq %>% 
#   tibble() %>% 
#   mutate(
#     # path = file.path( '..', 'POC_R', file )
#     #       , path = stringr::str_replace_all( path, ' ', '%20')
#            link =  paste0('<a  target=_blank href=',
#                           'https://www.youtube.com/watch?v=RwHAjTgCKxE', '>', '</a>' ) ) %>%
#   DT::datatable()
# 
# 
# 
# dr = tibble( file = dir( file.path( '..', 'POC_R' ) ) ) %>%
#   mutate( path = file.path( '..', 'POC_R', file )
#           , path = stringr::str_replace_all( path, ' ', '%20')
#           , link =  paste0('<a  target=_blank href=', path, '>', file,'</a>' ) )
# DT::datatable( select(dr, link) , escape = F)
# 
# 
# 
# 
# 
# 
