
library(tidyverse)
library(here)
 
ore <- readRDS(here("data", "processed", "ore.RDS"))
ricavi <- readRDS(here("data", "processed", "CC.rds"))


#fteq mensili
ore %>% 
  filter(ANNO == 2021) %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"), 
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%  
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(Mese, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*4.34), hworked/(38*4.34))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
  select(-hworked_, -FTE_)  %>% 
  mutate(FTET = FTE_Comparto+FTE_Dirigenza)






#ricavo totale mensile
ricavi %>% 
  filter(ANNO == 2021)