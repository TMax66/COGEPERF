### questi codici una volta consolidati vanno le file "preparazione dati.R"
librerie()
library(odbc)
library(DBI)
library(openxlsx)
library(readxl)


con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                      Database = "IZSLER", Port = 1433)

source(here("app", "app_attività", "sql.R"))


dt  <- dbGetQuery(con, query)

dt <- readRDS(here( "app", "app_attività", "dt.RDS"))

strutture <- read_excel(here("data", "raw", "strutture.xlsx"))
























# 
# # correzione codifica lettere
# 
# dt <- dt %>% distinct() %>% 
#   mutate( stracc = recode(stracc, 
#                          "Bologna (Reparto chimico degli alimenti)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
#                          "Reparto Tecnologie Biologiche Applicate - Batteriologia Specializzata" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                          "Reparto Tecnologie Biologiche Applicate - Colture Cellulari"  = "LABORATORIO COLTURE CELLULARI, BIOBANCA",
#                          "Reparto Virologia - Laboratorio Proteomica" = "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE", 
#                          "Reparto Virologia (ME)" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                          "Sede Territoriale di Milano (IS)" = "SEDE TERRITORIALE DI MILANO"),
#          strpropr = recode(strpropr,
#                            "Bologna (Reparto chimico degli alimenti)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
#                            "Reparto Tecnologie Biologiche Applicate - Batteriologia Specializzata" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                            "Reparto Tecnologie Biologiche Applicate - Colture Cellulari"  = "LABORATORIO COLTURE CELLULARI, BIOBANCA",
#                            "Reparto Virologia - Laboratorio Proteomica" = "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE", 
#                            "Reparto Virologia (ME)" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                            "Sede Territoriale di Milano (IS)" = "SEDE TERRITORIALE DI MILANO"),
#          str_analisi = recode(str_analisi, 
#                               "Bologna (Reparto chimico degli alimenti)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
#                               "Reparto Tecnologie Biologiche Applicate - Batteriologia Specializzata" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                               "Reparto Tecnologie Biologiche Applicate - Colture Cellulari"  = "LABORATORIO COLTURE CELLULARI, BIOBANCA",
#                               "Reparto Virologia - Laboratorio Proteomica" = "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE", 
#                               "Reparto Virologia (ME)" = "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA",
#                               "Sede Territoriale di Milano (IS)" = "SEDE TERRITORIALE DI MILANO"))
#        
#    
# 
# saveRDS(dt, here("app", "app_attività", "dt2.RDS"))
# 
# dt2 <- readRDS(here( "app", "app_attività", "dt2.RDS"))
# 
# 
# conf <- dt2 %>% 
#   select(- gruppo_prove, - Tot_Eseguiti) %>% distinct() %>% 
#   mutate(stracc =  sapply(stracc, iconv, from = "latin1", to = "UTF-8", sub = ""), 
#          strpropr =  sapply(strpropr, iconv, from = "latin1", to = "UTF-8", sub = ""),
#          str_analisi =  sapply(str_analisi, iconv, from = "latin1", to = "UTF-8", sub = ""),
#          stracc = casefold(stracc, upper = TRUE),
#          strpropr = casefold(strpropr, upper = TRUE), 
#          str_analisi = casefold(str_analisi, upper = TRUE))
# 
# 
# 
# conf %>% distinct() %>% 
#   group_by(dtconf, stracc, settore, tipoconf, finalita ) %>% 
#   View()
# 
# 
# 
# # conf %>% 
# #   left_join(strutture %>% 
# #               select(Dipartimento, Reparto, Laboratorio), 
# #             by = c("stracc" = "Laboratorio"))%>%  
# 
#   
#   
# View(dt)
