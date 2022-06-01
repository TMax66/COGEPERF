prod<-ftep22 %>% 
  select(Dipartimento, Reparto, Struttura, FTED = AttivitaValorizzataPerproduzioni_FTED, FTEC =AttivitaValorizzataPerproduzioni_FTEC) %>% 
  distinct() %>% 
  summarise(FTED_si = sum(FTED), 
            FTEC_si = sum(FTEC), 
            FTED_tot = sum(FTED), 
            FTEC_tot = sum(FTEC))  
 

 
  
  

ftep22 %>% 
  filter(!str_detect(ObiettivoOperativo,"2.1.9.")) %>%
  summarise(FTED_si = sum(FTED[Valorizzato == "si"], na.rm = TRUE)+ prod$FTED_si,
            FTEC_si = sum(FTEC[Valorizzato == "si"], na.rm = TRUE)+ prod$FTEC_si,
            FTED_tot = sum(FTED, na.rm = TRUE)+ prod$FTED_tot, 
            FTEC_tot = sum(FTEC, na.rm = TRUE)+ prod$FTEC_tot) 





prodRep <- ftep22 %>% 
  select(Dipartimento, Reparto, Struttura, FTED = AttivitaValorizzataPerproduzioni_FTED, FTEC =AttivitaValorizzataPerproduzioni_FTEC) %>% 
  distinct() %>% 
  group_by(Dipartimento, Reparto) %>%
  summarise(FTED_si = sum(FTED), 
            FTEC_si = sum(FTEC), 
            FTED_tot = sum(FTED), 
            FTEC_tot = sum(FTEC)) %>% 
  filter(FTED_si != 0)


ftep22 %>% 
  filter(!str_detect(ObiettivoOperativo,"2.1.9.")) %>%  
  group_by(Dipartimento, Reparto) %>%  
  summarise(FTED_si = sum(FTED[Valorizzato == "si"], na.rm = TRUE),
            FTEC_si = sum(FTEC[Valorizzato == "si"], na.rm = TRUE),
            FTED_tot = sum(FTED, na.rm = TRUE), 
            FTEC_tot = sum(FTEC, na.rm = TRUE)) %>% 
  rbind(prodRep) %>%  
    group_by(Dipartimento, Reparto) %>%
    summarise(FTED_si = sum(FTED_si),
              FTEC_si = sum(FTEC_si),
              FTED_tot = sum(FTED_tot),
              FTEC_tot = sum(FTEC_tot)) 
  
  






 


