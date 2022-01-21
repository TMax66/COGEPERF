


dtCostiT <-  dtanalisi %>% filter(CDC== "SEDE TERRITORIALE DI BOLOGNA" & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                       mutate(CumCosti = cumsum(Costi)) %>% 
                       ungroup () %>% 
                       arrange(Quarter) %>% 
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2, 
                              VarCumCosti = round((CumCosti/lag(CumCosti)-1)*100), 2) 


Tplot(dtCostiT, "CumCosti", "VarCumCosti")


dtanalisi %>%  
  filter(Costi== "Costo") %>% 
  group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
  summarise(costidett = sum(Costo, na.rm = TRUE),
  )  %>%  
  filter(CDC == "PROGETTI DI RICERCA" ) %>% 
  group_by(ANNO, Quarter,Classe) %>%  
  summarise(C = sum(costidett, na.rm=TRUE)) %>% 
  mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
  pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>% 
  group_by(ANNO, Classe) %>% 
  arrange(Classe) %>%  
  mutate(cs = cumsum(C)) %>% 
  ungroup() %>% 
  select(-ANNO, -Quarter, -C) %>%
  pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0)