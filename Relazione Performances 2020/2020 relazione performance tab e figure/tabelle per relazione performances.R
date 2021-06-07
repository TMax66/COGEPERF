library("tidyverse")
library("here")
library("readxl")

esamiric <- readRDS(file =  here("programmazione", "NUOVA VERSIONE",  "shinyapp", "esamiricavi.rds"))
vpai <- readRDS(file =  here("programmazione", "NUOVA VERSIONE",  "shinyapp", "vpai.rds"))


esamiric %>% 
  group_by(Anno, Dipartimento) %>% 
  summarise("Esami" = sum(N.esami), 
            "Valorizzazione" = sum(Ricavi)) 
