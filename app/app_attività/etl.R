### questi codici una volta consolidati vanno le file "preparazione dati.R"
librerie()
library(odbc)
library(DBI)
library(openxlsx)
library(readxl)


con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                      Database = "IZSLER", Port = 1433)

source(here("app", "app_attività", "sql.R"))

# query <- "SELECT
#   dbo.Conferimenti.Numero,
#   dbo_Anag_Finalita_Confer.Descrizione As finalitaconf,
#   dbo.Anag_Finalita.Descrizione As finprova,
#   dbo.Anag_Prove.Descrizione AS prova,
#   dbo.Anag_Tecniche.Descrizione As tecnica,
#   dbo.Anag_Metodi_di_Prova.Descrizione As mp
# FROM
# { oj dbo.Anag_Finalita  dbo_Anag_Finalita_Confer INNER JOIN dbo.Conferimenti_Finalita ON ( dbo.Conferimenti_Finalita.Finalita=dbo_Anag_Finalita_Confer.Codice )
#    INNER JOIN dbo.Conferimenti ON ( dbo.Conferimenti.Anno=dbo.Conferimenti_Finalita.Anno and dbo.Conferimenti.Numero=dbo.Conferimenti_Finalita.Numero )
#    LEFT OUTER JOIN dbo.Esami_Aggregati ON ( dbo.Conferimenti.Anno=dbo.Esami_Aggregati.Anno_Conferimento and dbo.Conferimenti.Numero=dbo.Esami_Aggregati.Numero_Conferimento )
#    LEFT OUTER JOIN dbo.Nomenclatore_MP ON ( dbo.Esami_Aggregati.Nomenclatore=dbo.Nomenclatore_MP.Codice )
#    LEFT OUTER JOIN dbo.Anag_Metodi_di_Prova ON ( dbo.Nomenclatore_MP.MP=dbo.Anag_Metodi_di_Prova.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore_Settori ON ( dbo.Nomenclatore_MP.Nomenclatore_Settore=dbo.Nomenclatore_Settori.Codice )
#    LEFT OUTER JOIN dbo.Nomenclatore ON ( dbo.Nomenclatore_Settori.Codice_Nomenclatore=dbo.Nomenclatore.Chiave )
#    LEFT OUTER JOIN dbo.Anag_Prove ON ( dbo.Nomenclatore.Codice_Prova=dbo.Anag_Prove.Codice )
#    LEFT OUTER JOIN dbo.Anag_Tecniche ON ( dbo.Nomenclatore.Codice_Tecnica=dbo.Anag_Tecniche.Codice )
#    LEFT OUTER JOIN dbo.Programmazione_Finalita ON ( dbo.Esami_Aggregati.Anno_Conferimento=dbo.Programmazione_Finalita.Anno_Conferimento and dbo.Esami_Aggregati.Numero_Conferimento=dbo.Programmazione_Finalita.Numero_Conferimento and dbo.Esami_Aggregati.Codice=dbo.Programmazione_Finalita.Codice )
#    LEFT OUTER JOIN dbo.Anag_Finalita ON ( dbo.Programmazione_Finalita.Finalita=dbo.Anag_Finalita.Codice )
#    INNER JOIN dbo.Anag_Registri ON ( dbo.Conferimenti.Registro=dbo.Anag_Registri.Codice )
#   }
# WHERE
#   dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  dbo.Esami_Aggregati.Esame_Altro_Ente = 0
#   AND  (
#   {fn year(dbo.Conferimenti.Data_Accettazione)}  =  2023
#   AND  dbo.Anag_Registri.Descrizione  =  'Sanità Animale'
#   AND  {fn month(dbo.Conferimenti.Data_Accettazione)}  =  5
#   )
# "
# 
# View(dt)
# 


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
