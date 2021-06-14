setwd("C:/Users/vito.tranquillo/Desktop/GitProjects/COGEPERF/script")
source("librerie.R")


#tabella IZSLER####
tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))


tabIZSLER %>% 
   filter(Anno == 2019) %>% 
  rename("ricavi" = valore, "VP" = ricavovp, "AI" = valoreai) %>% 
  group_by(Anno, Dipartimento) %>% 
  summarise_at(c("esami", "ricavi",  "VP", "AI", "FTED", "FTEC","costi"), sum, na.rm = T) %>% 
  mutate(RT = (ricavi+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row", name = "Totale") %>%
  mutate("R-FTE" = round(RT/FTE_T,0) )  %>% View()
  # select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
  #        "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")


tizsler <-  tabIZSLER %>%
                      rename("ricavi" = valore, "VP" = ricavovp, "AI" = valoreai) %>%
                      group_by(Anno, Dipartimento) %>%
                      summarise_at(c("esami", "ricavi",  "VP", "AI", "FTED", "FTEC","costi"), sum, na.rm = T) %>%
                      mutate(RT = (ricavi+VP+AI),
                             FTE_T = round((FTED+FTED),1)) %>%
                      arrange(desc(esami)) %>%
                      mutate("R-FTE" = round(RT/FTE_T,0) )


tizsler %>%
  filter(Anno == 2019) %>% 
  summarise(totes = sum(esami))


prj %>% 
  mutate("Stato" = ifelse(annofine < 2021, "Archiviato", "Attivo")) %>% 
  filter(Stato == "Attivo" & annoinizio <= 2021) %>% 
  summarise(n = nlevels(factor(Codice)))



###tabella Dipartimenti###

###tabella Reparti###

###Centri di costo###






# analisi %>% 
#   group_by(Anno, Reparto,  `Centro Di Costo`) %>% 
# summarise(n.esami = sum(Determinazioni, na.rm = T), 
#           valore = sum ( `A Tariffario`, na.rm = T)) %>%  
#   pivot_wider(names_from = Anno, values_from = c("n.esami", "valore")) %>% View()