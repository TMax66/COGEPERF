library(tidyverse)
library(here)
library(gt)
library(readr)
library(readxl)
library(lubridate)
library(flextable)
# library(DBI)
# library(odbc)

cc <- readRDS(here("data", "processed","CC.rds"))
# ore <- readRDS(here("data", "processed","ore.rds"))
# anagCE <- read_excel("app/app_costiricavi/R/Nuovo Controllo di Gestione/anagCE.xlsx", 
#                      col_types = c("skip", "text", "skip", 
#                                    "text", "skip", "text", "skip", "text", 
#                                    "skip", "text", "skip", "text", "skip", 
#                                    "text", "skip", "text", "skip", "text", 
#                                    "skip", "skip", "skip", "skip", "skip", 
#                                    "skip", "skip"))
#  
# 
# 
# 
# cc %>% 
#   
#   left_join(anagCE %>% 
#               rename(Area = RG8) %>% 
#               mutate(Area = casefold(Area)), 
#             by = c("Area")) %>%  View()



# cc %>% 
#   group_by(Categoria, Classificazione, Classe, Area) %>% 
#   select(Categoria, Classificazione, Classe, Area) %>% 
#   unique() %>% 
#   write.xlsx(file = "anagrafeCE.xlsx")

cc %>% 
  mutate(RG7 = ifelse(Area %in% c("esami batteriologici", 
                                     "esami anatomo patologici",
                                     "esami biochimico clinici" , 
                                     "esami biologia molecolare", 
                                     "esami parassitologici", 
                                     "esami istologici", 
                                     "esami sierologici", 
                                     "esami chimici e tossicologici",
                                     "esami virologici", 
                                     "esami latte extra routine"
                                     ), "Prestazioni (analisi)",
                         ifelse(Area %in% c("prove di sperimentazione", 
                                            "parere tecnico", 
                                            "smaltimento carcasse",
                                            "preparazione campioni", 
                                            "sopralluoghi",
                                            "pareri tecnici a privati",
                                            "prove biologia cellulare",
                                            "altre prestazioni"), "Attività non analitiche",
                                
                                ifelse(Area %in% c("Contributi Regioni ASL altri", 
                                                   "Contributi statali",
                                                   "Contributi da privati"), Area, 
                                       
                                       ifelse(Classificazione == "Costo Produzione Interna", Classificazione, 
                                              
                                              ifelse(Area %in% c("dirigenti chimici e biologi",
                                                                 "dirigenti veterinari"), "Personale dirigente sanitario", 
                                                     ifelse(Area %in% c("personale sanitario comparto", 
                                                                        "personale tecnico comparto"), "Personale comparto tecnico sanitario",
                                                            
                                                            ifelse(Classe == "Materiale da ufficio", "Cancelleria", 
                                                                   ifelse(Area %in% c("pulizie e disinfestazione", 
                                                                                      "vigilanza", 
                                                                                      "giardinaggio", 
                                                                                      "contenitori smaltimenti rifiuti",
                                                                                      "materiale e accessori per pulizia"), "Pulizia, giardinaggio e vigilanza", 
                                                                          ifelse(Area %in% c("camici (noleggio e lavanderia)", 
                                                                                             "indumenti monouso e durevoli",
                                                                                             "rimborsi spese missione", 
                                                                                             "mensa", 
                                                                                             "voci residue", 
                                                                                             "iscrizioni a corsi e convegni"
                                                                                             ), "Altri costi del personale", 
                                                                                 ifelse(Area %in% c("dirigenti amministrativi",
                                                                                                    "personale amministrativo comparto",
                                                                                                    "dirigenti professionali", 
                                                                                                    "dirigenti tecnici"), "Personale tecnico-amministrativo" , 
                                                                                               ifelse(Area == "borsisti", "borsisti", 
                                                                                                      ifelse(Classe %in% c("Materiali per manutenzioni", 
                                                                                                                           "Riparazioni e manutenzioni"), "Manutenzioni e riparazioni",
                                                                                                             Classe )))))))))))),

         
         
         
         RG7 = factor(RG7, levels = c("Prestazioni (analisi)", 
                                                 "Vendite prodotti", 
                                                 "Ricavi da produzione",
                                                 "Attività non analitiche", 
                                                 "Altri proventi", 
                                                 "Contributi Regioni ASL altri",
                                                 "Contributi statali",
                                                 "Contributi da privati", 
                                                 "Materiali da laboratorio", 
                                                 "Costo Produzione Interna",
                                          "Personale dirigente sanitario",
                                          "Personale comparto tecnico sanitario", 
                                          "Personale tecnico-amministrativo",
                                          "borsisti",
                                          "Consulenze e coll. professionali",
                                          "Altri costi del personale", 
                                          "Prevenzione e sicurezza",
                                          "Presidi sanitari",
                                          "Stabulario",
                                          "Cancelleria",
                                          "Manutenzioni e riparazioni", 
                                          "Utenze",
                                          "Pulizia, giardinaggio e vigilanza", 
                                          "Trasporti",
                                          "Altri servizi",
                                          "Vari",
                                          "Ammortamenti"))) %>% 
  
  mutate(RG6 = ifelse(RG7 %in% c("Prestazioni (analisi)", 
                                       "Vendite prodotti", 
                                       "Ricavi da produzione",
                                       "Attività non analitiche", 
                                       "Altri proventi"), "Proventi per attività", 
                          ifelse(RG7 %in% c("Contributi Regioni ASL altri",
                                              "Contributi statali",
                                              "Contributi da privati"), "Contributi", 
                                 ifelse(RG7 %in% c("Materiali da laboratorio", 
                                                     "Costo Produzione Interna",
                                                     "Personale dirigente sanitario",
                                                     "Personale comparto tecnico sanitario"), "Costi attività sanitarie", 
                                        ifelse(RG7 %in% c("Personale tecnico-amministrativo",
                                                            "borsisti",
                                                            "Consulenze e coll. professionali",
                                                            "Altri costi del personale", 
                                                            "Prevenzione e sicurezza",
                                                            "Presidi sanitari",
                                                            "Stabulario",
                                                            "Cancelleria",
                                                            "Manutenzioni e riparazioni", 
                                                            "Utenze",
                                                            "Pulizia, giardinaggio e vigilanza", 
                                                            "Trasporti",
                                                            "Altri servizi",
                                                            "Vari"), "Costi di struttura", "Ammortamenti")))), 
         RG6 = ifelse(Classificazione == "Attività di ricerca", "Attività di ricerca", RG6))-> cc





 cc %>%  filter(ANNO == 2019, 
                Costi == "Ricavo",
                RG6 %in% c("Proventi per attività", 
                               "Contributi")) %>%  
  mutate(Ricavi = ifelse(Pagamento=="Pagamento", Fatturato,Tariffario),
         Ricavi = ifelse(RG7 == "Vendite prodotti", Fatturato, Ricavi),
         Ricavi = ifelse(Classe == "Ricavi da produzione",Tariffario, Ricavi ), 
         Ricavi = ifelse(RG7 %in% c("Contributi Regioni ASL altri",
                                      "Contributi statali",
                                      "Contributi da privati"),  Tariffario, Ricavi), 
         Ricavi = ifelse(RG7 == "Altri proventi", Tariffario, Ricavi), 
         RG5 = "Ricavi") %>%  
       
    group_by(RG5,  RG6, RG7 ) %>%  
    summarise(value = sum(Ricavi, na.rm = TRUE)) %>% 
   arrange(RG7) %>% 
   
   adorn_totals(where = "row", name = "TotRic")-> RICAVI
 
 
 
 cc %>% filter(ANNO == 2019, 
               Costi == "Costo", 
               RG6 == "Costi attività sanitarie" ) %>% 
   mutate(RG5 = "Costi sanitari") %>% 
   
   group_by(RG5, RG6, RG7 ) %>% 
   summarise(value = sum(Costo, na.rm = TRUE)) %>%
   arrange(RG7) %>%
   adorn_totals(where = "row", name = "TotCosti")  -> COSTI_SANITARI
  
 
 margine <- RICAVI %>% 
   rbind(COSTI_SANITARI) %>%  
   filter(RG5 %in% c("TotRic", "TotCosti")) %>% 
   pivot_wider(names_from = RG5, values_from = value) %>%  
   mutate(margine = TotRic-TotCosti)

  
 
 RICAVI %>% 
   rbind(COSTI_SANITARI) %>% 
   add_row(RG5 = "Margine di contribuzione", RG6 = "-", RG7 = "-", value = margine$margine) %>% 
   filter(!RG5 %in% c("TotRic", "TotCosti")) %>% 
   
   bind_rows( 
 
 
 
 cc %>% filter(ANNO == 2019, 
               Costi == "Costo", 
               RG6 == "Costi di struttura" ) %>% 
   mutate(RG5 = "Costi di Struttura") %>% 
  group_by(RG5, RG6, RG7 ) %>% 
  summarise(value = sum(Costo, na.rm = TRUE)) %>% 
    arrange(RG7) %>%
   adorn_totals(where = "row", name = "TotCostrutt")) -> CE 
  
 
 marginelordo <- CE %>% 
   filter(RG5 %in% c("Margine di contribuzione", "TotCostrutt")) %>% 
   pivot_wider(names_from = RG5, values_from = value) %>%  
   mutate(margine = `Margine di contribuzione`- TotCostrutt)
 
 
 CE %>% 
   add_row(RG5 = "Margine lordo", RG6 = "-", RG7 = "-", value = marginelordo$margine) %>% 
   filter(RG5 != "TotCostrutt" ) %>%  View()
 
 
 cc %>% filter(ANNO == 2019, 
               Costi == "Costo", 
               RG6 == "Ammortamenti" ) %>% 
   group_by(RG6, RG7) %>% 
   summarise(value = sum(Costo, na.rm = TRUE)) %>% 
   adorn_totals(where = "row") -> AMMORTAMENTI
 
 
 RICAVI %>% mutate(RG5 = "Ricavi") %>%  
   bind_rows(COSTI_SANITARI %>% 
               mutate(RG5 = "Costi sanitari")) %>% 
   bind_rows(COSTI_STRUTTURA %>% 
               mutate(RG5 = "Costi di struttura")) %>% 
   bind_rows(AMMORTAMENTI %>% 
               mutate(RG5 = "Ammortamenti")) %>% 
   mutate(RG5 = factor(RG5, levels = c("Ricavi", "Costi sanitari", 
                                           "Costi di struttura", "Ammortamenti")), 
          RG6 = factor(RG6, levels = c("Proventi per attività", 
                                            "Contributi", 
                                            "Costi attività sanitarie", 
                                            "Costi di struttura",
                                            "Ammortamenti"
                                            ))) %>% 
   group_by( RG5, RG6, RG7) %>% 
   summarise(value = sum(value)) -> CE
 
 
 fxt <- flextable(CE)
 
 fxt %>% 
   merge_v(j = ~ RG5 + RG6 )
 
 
 

 gt_tab <- gt(CE, groupname_col = c("RG5","RG6"))

 ## formatto la tabella----

gt_tab %>%

tab_header(
   title = "Conto Economico Gestionale per natura",
   subtitle = "2019") %>%
tab_source_note(
     source_note = md("_A cura del **COGEP**_")) %>% 
   tab_style(
     style = list(
       cell_fill("black"),
       cell_text(color = "white", weight = "bold")
     ),
     locations = cells_row_groups()
   ) %>% 
   tab_style(
     style = cell_text(color = "darkgrey", weight = "bold"),
     locations = cells_stub()
   )


   
   
 
 # rbind(tibble(
  #   Dipartimento = "Direzione Amministrativa",
  #   Categoria = "Proventi per attività",
  #   Classe = "Prestazioni",
  #   Tipologia_Costi = "Produzione",
  #   value = 0
  # ))






















# cc <- cc %>% 
#   filter(!Categoria %in% c("Contributi in conto esercizio", "Altri proventi")) %>% 
#   mutate(Tipologia_Costi = recode(Classe, 
#                               "Materiali da laboratorio" = "Costi Diretti", 
#                               "Presidi sanitari" = "Costi Diretti", 
#                               "Prodotti I.Z.S.L.E.R." = "Costi Diretti", 
#                               "Stabulario" = "Costi Diretti", 
#                               "Vestiario" = "Costi Diretti", 
#                               "Riparazioni e manutenzioni" = "Costi Diretti", 
#                               "Personale" = "Costi Diretti", 
#                               "Materiale da ufficio" = "Costi Indiretti",
#                               "Articoli pulizia" = "Costi Indiretti",
#                               "Materiali per manutenzioni" = "Costi Indiretti",
#                               "Prevenzione e sicurezza" = "Costi Indiretti",
#                               "Trasporti" = "Costi Indiretti",
#                               "Vari" = "Costi Indiretti",
#                               "Consulenze e coll. professionali" = "Costi Indiretti",
#                               "Riparazioni e manutenzioni" = "Costi Indiretti",
#                               "Utenze" = "Costi Indiretti",
#                               "Formazione" = "Costi Indiretti",
#                               "Altri servizi" = "Costi Indiretti", 
#                               "Ricavi da produzione" = "Produzione", 
#                               "Prestazioni" = "Produzione", 
#                               "Vendite prodotti" = "Produzione"
#                               )) 
# 
# 
# 
# ricavi <-  cc %>%  filter(ANNO == 2021, Costi == "Ricavo") %>% 
#   mutate(Ricavi = ifelse(Pagamento=="Pagamento", Fatturato,Tariffario), 
#          Ricavi = ifelse(Classe == "Vendite prodotti", Fatturato, Ricavi), 
#          Ricavi = ifelse(Classe == "Ricavi da produzione",Tariffario, Ricavi )) %>% 
#     group_by(Dipartimento, Categoria, Classe, Tipologia_Costi) %>%  
#     summarise(value = sum(Ricavi, na.rm = TRUE)) %>%  
#   rbind(tibble(
#     Dipartimento = "Direzione Amministrativa", 
#     Categoria = "Proventi per attività", 
#     Classe = "Prestazioni",
#     Tipologia_Costi = "Produzione", 
#     value = 0
#   ))
# 
# 
# 
#     #pivot_wider(names_from = "Dipartimento", values_from = "ricavi") %>%
#    # ungroup() %>% 
#   #mutate("Direzione Amministrativa" = rep(0, nrow(.)))  %>% View(
# 
# costiD <- cc %>%  filter(ANNO == 2022, Costi == "Costo", Tipologia_Costi == "Costi Diretti") %>%  
#   group_by(Dipartimento, Categoria, Classe, Tipologia_Costi) %>%  
#   summarise(value = sum(Costo, na.rm = TRUE))   
#  # pivot_wider(names_from = "Dipartimento", values_from = "costi") %>%
#  # ungroup() %>% 
#  # select (- "Non applicabile")  
# 
# 
# costiIND <- cc %>%  filter(ANNO == 2022, Costi == "Costo", Tipologia_Costi == "Costi Indiretti") %>%  
#   group_by(Dipartimento, Categoria, Classe, Tipologia_Costi) %>%  
#   summarise(value = sum(Costo, na.rm = TRUE))  
#   # pivot_wider(names_from = "Dipartimento", values_from = "costi") %>%
#   # ungroup() 
# 
# R1 <- ricavi %>% 
#   rbind(costiD) %>% 
#   rbind(costiIND) %>% 
#   pivot_wider(names_from = "Dipartimento", values_from = "value") %>%    
#   ungroup() %>% 
#     select(- Categoria) 
#   
# 
#  
#   
# 
#   # 
#   # 
#   # R1 %>% 
#   # group_by(Tipologia_Costi ) %>% 
#   #   summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
#   #   cbind( 
#   #     tibble(Classe = c("Totale Costi Diretti", "Totale Costi Indiretti", "Totale Valore della Produzione"))
#   #            ) %>% 
#   #   pivot_longer(cols = 2:11, names_to = "Dipartimento", values_to = "value") %>%  
#   #   group_by(Dipartimento) %>% 
#   #   mutate("I Margine" =  100*(value[Classe ==  "Totale Costi Diretti"]/ value[Classe == "Totale Valore della Produzione"])) %>% View()
#   #   
#   # 
#   #   
#   # rbind(R1) %>%  
#     
#  
#   
# # TABELLA PER TIPOLOGIA DI COSTI
# T <- R1 %>% select(-Tipologia_Costi,  -`Non applicabile` ) %>% 
#   gt(rowname_col = "Classe")
# 
#   T %>%
#   cols_move_to_end(
#     columns = 2 
#   ) %>% 
#   cols_move_to_start(
#     columns =  c(8:10)
#   ) %>%  
#   
#   tab_spanner(
#     label = "Direzione",
#     columns = 8:10
#   ) %>% 
#   tab_spanner(
#     label = "Dipartimenti tecnici",
#     columns =  4:7
#   ) %>% 
#   
#   tab_row_group(
#     label = "Costi indiretti",
#     rows = 11:20) %>%
#   tab_row_group(
#     label = "Costi diretti",
#     rows = 4:10) %>%
#   tab_row_group(
#     label = "Proventi per attività",
#     rows = 1:3) %>%
# 
#   summary_rows(
#     groups = "Proventi per attività",
#     formatter = fmt_currency,
#     currency = "EUR",
#     fns = list(
#     "Totale Valore della Produzione" = ~sum(., na.rm = TRUE)))
#      
#   
#   summary_rows(
#     groups = "Costi diretti",
#     formatter = fmt_currency,
#     currency = "EUR",
#     fns = list(
#       "Totale Costi diretti" = ~sum(., na.rm = TRUE))
#   ) %>% 
# 
#   summary_rows(
#     groups = "Costi indiretti",
#     formatter = fmt_currency,
#     currency = "EUR",
#     fns = list(
#       "Totale Costi indiretti" = ~sum(., na.rm = TRUE))
#   ) %>% 
#   
#   fmt_currency(
#     columns =  2:10,
#     currency = "EUR"
#   ) %>% 
#   
#   tab_options(
#     summary_row.background.color = "lightblue",
#     row_group.border.top.width = px(3),
#     row_group.border.top.color = "black",
#     row_group.border.bottom.color = "black"
#   ) 
# 
# ## IL CODICE QUI SOTTO CREA UNA TABELLA A PARTE CHE RIPORTA PER DIP I TOTALI E I MARGINI
# tx<-extract_summary(T)
# 
# Tx <- tx[["summary_df_data_list"]][["Costi indiretti"]] %>% 
#   rbind(
#     tx[["summary_df_data_list"]][["Costi diretti"]]
#   ) %>% 
#   rbind(
#     tx[["summary_df_data_list"]][["Proventi per attività"]]
#   ) %>% 
#   select(-Classe, -group_id)  
#   
# M <-Tx %>% summarise(across(where(is.numeric), mrg))  %>% 
#   mutate(rowname = "Primo Margine")  
#  
# Tx %>% 
#   rbind(M) %>% 
#   gt()
# 
# 
#   
#   
# mrg <- function(x){
#   
#   x[3]/x[2]
# }
#   
# 
# 
# 
# 
# 
# ###################################
# 
# coge <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02",
#                          Database = "DW_COGE", Port = 1433)
# 
# source(here("app", "appcostiricavi", "R", "Nuovo Controllo di Gestione", "sql.R"))
# 
# 
# cc <- coge %>% tbl(sql(dw_coge)) %>% as_tibble()
# saveRDS(cc, here("app", "appcostiricavi", "R", "Nuovo Controllo di Gestione","cg.RDS"))
# 
# 
# View(cc)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # cc %>% filter(ANNO == 2021) %>%
# # 
# #   mutate(Ricavi = ifelse(Pagamento=="Pagamento", Fatturato, Tariffario)) %>%
# #   group_by(Dipartimento, Categoria, Classe) %>%
# #   summarise(ricavi = sum(Ricavi, na.rm = TRUE)) %>%
# #   pivot_wider(names_from = "Dipartimento", values_from = "ricavi") %>%
# #   ungroup() %>% 
# # 
# #   rbind (
# #     cc %>% filter(ANNO == 2021) %>%
# #            mutate(Fatt_Tar = ifelse(Fatturato == 0, "T",
# #                                     ifelse(Fatturato == Tariffario, "T",
# #                                            ifelse( Fatturato < Tariffario & Fatturato != 0 | Fatturato > Tariffario,"F",  "T")))) %>%
# #            mutate(Ricavi = ifelse(Fatt_Tar=="T", Tariffario, Fatturato)) %>%
# #            group_by(Dipartimento, Categoria, Classe) %>%
# #            summarise(costi = sum(Costo, na.rm = TRUE)) %>%
# #            pivot_wider(names_from = "Dipartimento", values_from = "costi") %>% ungroup()
# #     )%>% View()
# # 
# #   