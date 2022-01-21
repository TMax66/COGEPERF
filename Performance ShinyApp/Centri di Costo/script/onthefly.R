


dtCostiT <-  dtanalisi %>% filter(CDC== "SEDE TERRITORIALE DI BOLOGNA" & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                       mutate(CumCosti = cumsum(Costi)) %>% 
                       ungroup () %>% 
                       arrange(Quarter) %>% 
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2, 
                              VarCumCosti = round((CumCosti/lag(CumCosti)-1)*100), 2) 


Tplot(dtCostiT, "CumCosti", "VarCumCosti")
