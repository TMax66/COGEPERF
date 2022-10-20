librerie()
dt <- TabellaGenerale


  dt %>% 
  filter(!Dipartimento %in% c("COSTI COMUNI E CENTRI CONTABILI" , 
                              "DIPARTIMENTO AMMINISTRATIVO", 
                              "DIREZIONE AMMINISTRATIVA", 
                              "DIREZIONE GENERALE", 
                              "NON APPLICABILE"), 
         ANNO >= 2021) %>% 
  group_by(ANNO, Reparto) %>% 
  summarise(attività = sum(TotPrestazioni, na.rm = TRUE), 
            orecomp = sum(hwcomp, na.rm = TRUE),
            oredir = sum(hwdir, na.rm = TRUE),
            ricprest = sum(TotTariff, na.rm = TRUE), 
            vp= sum(TotFattVP, na.rm = TRUE), 
            tai = sum(TAI, na.rm = TRUE),
            costi = sum(TotCost,na.rm = TRUE)) %>% 
  left_join(
    
    (pub %>% filter(OA >= 2021) %>% 
      group_by(OA,Reparto) %>% 
      count(NR) %>%  
      summarise(n = n())) , by= c("ANNO" = "OA", "Reparto")
  ) %>% 
    filter(!is.na(n), 
           attività > 0) %>%  
    
    mutate(Wattività = attività/orecomp, 
           Dattività = n/oredir) %>%  
    filter(ANNO == 2021) %>% 
    
    ggplot()+
    aes(x = scale(Dattività), 
        y = scale(Wattività))+ 
    geom_point()


