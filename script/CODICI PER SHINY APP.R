library("tidyverse")
library("readxl")
#library("RColorBrewer")
# library("shiny")
# library("shinydashboard")
library("here")
# library("janitor")
# library("flextable")
# library("shinyBS")
# library("officer")
# library("DT")
# library("lubridate")
# library("fmsb")


#####carico i dati####
analisi <- readRDS(file = here( "data", "processed", "analisi.rds"))
costi <- readRDS(file = here(  "data", "processed", "costi.rds"))
vp <- readRDS(file = here(  "data", "processed", "vp.rds"))
ai <- readRDS(file = here(   "data", "processed", "ai.rds"))
ore <- readRDS(file = here(  "data", "processed", "orelavorate.rds"))


###orelavorate e FULL TIME Equivalenti per dip/rep/lab/


# ore %>%
#   filter(Dipartimento != "Non applicabile") %>% 
#   group_by(Anno, Dipartimento, Reparto, Laboratorio, Dirigente) %>%  
#   filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
#   summarise(hworked = sum(Ore, na.rm = T)) %>% 
#   mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
#   pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))
#  

analisi %>% 
  group_by(Anno, Dipartimento, Reparto) %>% 
  summarise(esami = sum(Determinazioni), 
            valore = sum(`A Tariffario`)) %>%
  left_join(
    (vp %>%
       group_by(Anno, Dipartimento, Reparto) %>% 
       summarise(nprodotti = sum(Numero), 
                 ricavovp = sum(Fatturato))

       ), by =c("Anno", "Dipartimento", "Reparto")
  ) %>% 
  left_join(
    (ai %>% 
       group_by(Anno, Dipartimento, Reparto) %>% 
       summarise( valoreai = sum(`A Tariffario`))
       ), by = c("Anno", "Dipartimento", "Reparto")
  ) %>% 
  filter(!is.na(Dipartimento)) 



###tabella IZSLER###



###tabella Dipartimenti###

###tabella Reparti###


###Centri di costo###






# analisi %>% 
#   group_by(Anno, Reparto,  `Centro Di Costo`) %>% 
# summarise(n.esami = sum(Determinazioni, na.rm = T), 
#           valore = sum ( `A Tariffario`, na.rm = T)) %>%  
#   pivot_wider(names_from = Anno, values_from = c("n.esami", "valore")) %>% View()