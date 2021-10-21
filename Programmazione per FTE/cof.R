library(here)
library(tidyverse)

##FTE
dtProg <- readRDS("datiSB.rds")

dtProg %>% 
  filter(Dipartimento != "Dipartimento Amministrativo") %>% 
  mutate(Dipartimento = recode(Dipartimento, 
                               "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" , 
                               "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia", 
                               "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale")
         ) %>% 
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
    group_by(Valorizzazione, Dipartimento) %>% 
    summarise(FTED = sum(FTED, na.rm = T), 
    FTEC = sum(FTEC, na.rm = T)) %>% 
   rowwise() %>% 
   mutate(FT = sum(FTED, FTEC)) %>% 
    group_by( Dipartimento) %>% 
     # filter(Valorizzazione == "si") %>%  
    mutate(FTp = prop.table(FT)) %>% 
  select(-FTED, -FTEC) %>%  
    group_by(Dipartimento, Valorizzazione) %>% 
      filter(Valorizzazione== "si") %>%  
 ungroup() %>% 
  select(Dipartimento, FTp) %>% 
left_join( 
( 
  tizsler %>% 
    mutate(Dipartimento =casefold(Dipartimento, upper = TRUE)) %>% 
      filter(Anno == 2020) %>% 
  select(Dipartimento, RT, FTE_T)   
), by = "Dipartimento") %>% 
  mutate(FTEprogrammato = FTE_T*FTp, 
         RFTE = RT/FTEprogrammato) %>%  
  saveRDS("FT.rds")
