library(tidyverse)
library(here)
library(gt)

cc <- readRDS(here("data", "processed","CC.rds"))


cc <- cc %>% 
  filter(!Categoria %in% c("Contributi in conto esercizio", "Altri proventi")) %>% 
  mutate(Tipologia_Costi = recode(Classe, 
                              "Materiali da laboratorio" = "Costi Diretti", 
                              "Presidi sanitari" = "Costi Diretti", 
                              "Prodotti I.Z.S.L.E.R." = "Costi Diretti", 
                              "Stabulario" = "Costi Diretti", 
                              "Vestiario" = "Costi Diretti", 
                              "Riparazioni e manutenzioni" = "Costi Diretti", 
                              "Personale" = "Costi Diretti", 
                              "Materiale da ufficio" = "Costi Indiretti",
                              "Articoli pulizia" = "Costi Indiretti",
                              "Materiali per manutenzioni" = "Costi Indiretti",
                              "Prevenzione e sicurezza" = "Costi Indiretti",
                              "Trasporti" = "Costi Indiretti",
                              "Vari" = "Costi Indiretti",
                              "Consulenze e coll. professionali" = "Costi Indiretti",
                              "Riparazioni e manutenzioni" = "Costi Indiretti",
                              "Utenze" = "Costi Indiretti",
                              "Formazione" = "Costi Indiretti",
                              "Altri servizi" = "Costi Indiretti"
                              )) 



ricavi <-  cc %>%  filter(ANNO == 2021, Costi == "Ricavo") %>% 
  mutate(Ricavi = ifelse(Pagamento=="Pagamento", Fatturato,Tariffario), 
         Ricavi = ifelse(Classe == "Vendite prodotti", Fatturato, Ricavi), 
         Ricavi = ifelse(Classe == "Ricavi da produzione",Tariffario, Ricavi )) %>% 
    group_by(Dipartimento, Categoria, Classe) %>%  
    summarise(ricavi = sum(Ricavi, na.rm = TRUE)) %>% 
    pivot_wider(names_from = "Dipartimento", values_from = "ricavi") %>%
    ungroup() %>% 
  mutate("Direzione Amministrativa" = rep(0, nrow(.)))  

costiD <- cc %>%  filter(ANNO == 2021, Costi == "Costo", Tipologia_Costi == "Costi Diretti") %>%  
  group_by(Dipartimento, Categoria, Classe) %>%  
  summarise(costi = sum(Costo, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Dipartimento", values_from = "costi") %>%
  ungroup() %>% 
  select (- "Non applicabile")  


costiIND <- cc %>%  filter(ANNO == 2021, Costi == "Costo", Tipologia_Costi == "Costi Indiretti") %>%  
  group_by(Dipartimento, Categoria, Classe) %>%  
  summarise(costi = sum(Costo, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Dipartimento", values_from = "costi") %>%
  ungroup() 

R1 <- ricavi %>% 
  rbind(costiD) %>% 
  rbind(costiIND)


T <- R1 %>% select(- Categoria) %>% 
  gt(rowname_col = "Classe")

T %>%
  cols_move_to_start(
    columns =  8:10
  ) %>% 
  cols_move_to_end(
    columns = 2
  ) %>% 
  tab_spanner(
    label = "Direzione",
    columns = c("Direzione Generale", "Direzione Sanitaria", "Direzione Amministrativa" )
  ) %>% 
  tab_spanner(
    label = "Dipartimenti tecnici",
    columns =  4:7
  ) %>% 
  
  tab_row_group(
    label = "Costi indiretti",
    rows = 11:20) %>% 
  tab_row_group(
    label = "Costi diretti",
    rows = 4:10) %>% 
  tab_row_group(
    label = "Proventi per attività",
    rows = 1:3) %>% 
  
  summary_rows(
    groups = "Proventi per attività",
    formatter = fmt_currency,
    currency = "EUR",
    fns = list(
    "Totale Valore della Produzione" = ~sum(., na.rm = TRUE))
  ) %>% 
  
  summary_rows(
    groups = "Costi diretti",
    formatter = fmt_currency,
    currency = "EUR",
    fns = list(
      "Totale Costi diretti" = ~sum(., na.rm = TRUE))
  ) %>% 

  summary_rows(
    groups = "Costi indiretti",
    formatter = fmt_currency,
    currency = "EUR",
    fns = list(
      "Totale Costi indiretti" = ~sum(., na.rm = TRUE))
  ) %>% 
  
  fmt_currency(
    columns =  2:10,
    currency = "EUR"
  ) %>% 
  
  tab_options(
    summary_row.background.color = "lightblue",
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black"
  ) 
  
  



# cc %>% filter(ANNO == 2021) %>%
# 
#   mutate(Ricavi = ifelse(Pagamento=="Pagamento", Fatturato, Tariffario)) %>%
#   group_by(Dipartimento, Categoria, Classe) %>%
#   summarise(ricavi = sum(Ricavi, na.rm = TRUE)) %>%
#   pivot_wider(names_from = "Dipartimento", values_from = "ricavi") %>%
#   ungroup() %>% 
# 
#   rbind (
#     cc %>% filter(ANNO == 2021) %>%
#            mutate(Fatt_Tar = ifelse(Fatturato == 0, "T",
#                                     ifelse(Fatturato == Tariffario, "T",
#                                            ifelse( Fatturato < Tariffario & Fatturato != 0 | Fatturato > Tariffario,"F",  "T")))) %>%
#            mutate(Ricavi = ifelse(Fatt_Tar=="T", Tariffario, Fatturato)) %>%
#            group_by(Dipartimento, Categoria, Classe) %>%
#            summarise(costi = sum(Costo, na.rm = TRUE)) %>%
#            pivot_wider(names_from = "Dipartimento", values_from = "costi") %>% ungroup()
#     )%>% View()
# 
#   