library(tidyverse)
library(here)


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