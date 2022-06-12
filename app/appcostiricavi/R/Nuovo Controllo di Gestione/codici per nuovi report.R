library(tidyverse)
library(here)


cc <- readRDS(here("data", "processed","CC.rds"))


cc %>% filter(ANNO == 2021) %>% 
  mutate(Fatt_Tar = ifelse(Fatturato == 0, "T",
                           ifelse(Fatturato == Tariffario, "T", 
                                  ifelse( Fatturato < Tariffario & Fatturato != 0 | Fatturato > Tariffario,"F",  "T")))) %>%  
  mutate(Ricavi = ifelse(Fatt_Tar=="T", Tariffario, Fatturato)) %>% 
  group_by(Dipartimento, Categoria, Classe) %>% 
  summarise(ricavi = sum(Ricavi, na.rm = TRUE)) %>%  
  pivot_wider(names_from = "Dipartimento", values_from = "ricavi") %>% 
  ungroup() %>% 
  
  rbind (
    cc %>% filter(ANNO == 2021) %>% 
           mutate(Fatt_Tar = ifelse(Fatturato == 0, "T",
                                    ifelse(Fatturato == Tariffario, "T", 
                                           ifelse( Fatturato < Tariffario & Fatturato != 0 | Fatturato > Tariffario,"F",  "T")))) %>%  
           mutate(Ricavi = ifelse(Fatt_Tar=="T", Tariffario, Fatturato)) %>% 
           group_by(Dipartimento, Categoria, Classe) %>% 
           summarise(costi = sum(Costo, na.rm = TRUE)) %>%  
           pivot_wider(names_from = "Dipartimento", values_from = "costi") %>% ungroup()
  ) %>% View()
  
  