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


conf <- dt %>% distinct() %>%
  mutate(repacc =  sapply(repacc, iconv, from = "latin1", to = "UTF-8", sub = ""),
         finalita =  sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         repacc = recode(repacc, 
                         "Bologna (Reparto chimico degli alimenti)" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                         "Reparto Chimica degli Alimenti e Mangimi" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                         "Reparto Tecnologie Biologiche Applicate - Batteriologia Specializzata" = "REPARTO VIROLOGIA",
                         "Reparto Tecnologie Biologiche Applicate - Colture Cellulari"  = "REPARTO VIROLOGIA",
                         "Reparto Tecnologie Biologiche Applicate" = "REPARTO VIROLOGIA",
                         "Reparto Virologia - Laboratorio Proteomica" = "REPARTO VIROLOGIA",
                         "Reparto Virologia (ME)" = "REPARTO VIROLOGIA",
                         "Sede Territoriale di Milano (IS)" = "SEDE TERRITORIALE DI LODI - MILANO", 
                         "Sede Territoriale di Bergamo"  =  "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                         "Sede Territoriale di Binago" =  "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                         "Sede Territoriale di Sondrio" =  "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                         "Sede Territoriale di Milano" =  "SEDE TERRITORIALE DI LODI - MILANO",
                         "Sede Territoriale di Lodi" =  "SEDE TERRITORIALE DI LODI - MILANO",
                         "Sede Territoriale di Pavia" = "SEDE TERRITORIALE DI PAVIA", 
                         "Sede Territoriale di Cremona" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",    
                         "Sede Territoriale di Mantova" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                         "Sede Territoriale di Brescia" = "SEDE TERRITORIALE DI BRESCIA",
                         "Sede Territoriale di Bologna" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                         "Sede Territoriale di Modena" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                         "Sede Territoriale di Ferrara" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                         "Sede Territoriale di Forlì" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                         "Sede Territoriale di Ravenna" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
                         "Sede Territoriale di Reggio Emilia"  = "SEDE TERRITORIALE DI REGGIO EMILIA",
                         "Sede Territoriale di Parma" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                         "Sede Territoriale di Piacenza" = "SEDE TERRITORIALE DI PIACENZA - PARMA"
                         ), 
         repacc = casefold(repacc, upper = TRUE))  %>%   
  # group_by(settore, repacc) %>% 
  # summarise(n = n()) %>% 
  left_join(strutture %>% 
              select(Dipartimento, Reparto),by = c("repacc" = "Reparto"))%>% 
  
  saveRDS( here( "app", "app_attività", "datiperapp.RDS"))




# datiperapp %>% 
#   mutate(anno = year(Data_Accettazione)) %>% 
#   filter(anno >=2021) %>%  
#   saveRDS( here( "app", "app_attività", "datiattività.RDS"))









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

# 
# 
# 
# conf %>% d  
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
