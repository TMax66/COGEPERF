x <- dtanalisi %>% filter(CDC =="SEDE TERRITORIALE DI BOLOGNA" & Costi=="Ricavo") %>% 
                     group_by(CDC,  ANNO,  Quarter) %>% 
                     summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                               EUff = sum(AttUff, na.rm = TRUE), 
                               ENUff =sum(AttNUff, na.rm = TRUE), 
                               EPag = sum(AttPag, na.rm = TRUE), 
                               Egrat = sum(AttGrat, na.rm = TRUE), 
                               PI = sum(AI , na.rm = TRUE), 
                               Prodv = sum(VP, na.rm = TRUE)) %>% 
                     ungroup()






                    
                     mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
                            VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
                            VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
                            VarEPag = round((EPag/lag(EPag)-1)*100, 2),
                            VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
                            VarPI = round((PI/lag(PI)-1)*100, 2), 
                            VarProdv = round((Prodv/lag(Prodv)-1)*100, 2)
                     ) %>%  View()
dtanalisi$