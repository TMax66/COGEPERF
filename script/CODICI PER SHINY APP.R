setwd("C:/Users/vito.tranquillo/Desktop/GitProjects/COGEPERF/script")
source("librerie.R")


#tabella IZSLER####
tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))



###tabella Dipartimenti###

###tabella Reparti###

###Centri di costo###






# analisi %>% 
#   group_by(Anno, Reparto,  `Centro Di Costo`) %>% 
# summarise(n.esami = sum(Determinazioni, na.rm = T), 
#           valore = sum ( `A Tariffario`, na.rm = T)) %>%  
#   pivot_wider(names_from = Anno, values_from = c("n.esami", "valore")) %>% View()