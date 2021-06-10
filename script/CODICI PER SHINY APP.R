setwd("C:/Users/vito.tranquillo/Desktop/GitProjects/COGEPERF/script")
source("librerie.R")


#tabella IZSLER####
tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))


 tabIZSLER%>% 
  rename("ricavi" = valore, "VP" = ricavovp, "AI" = valoreai) %>% 
  group_by(Anno, Dipartimento) %>% 
  summarise_at(c("esami", "ricavi",  "VP", "AI", "FTED", "FTEC","costi"), sum) %>% View()
  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_t,0) ) %>%
  select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


###tabella Dipartimenti###

###tabella Reparti###

###Centri di costo###






# analisi %>% 
#   group_by(Anno, Reparto,  `Centro Di Costo`) %>% 
# summarise(n.esami = sum(Determinazioni, na.rm = T), 
#           valore = sum ( `A Tariffario`, na.rm = T)) %>%  
#   pivot_wider(names_from = Anno, values_from = c("n.esami", "valore")) %>% View()