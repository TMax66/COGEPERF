library(tidyverse)
library(here)
library(openxlsx)



# i dati originali provengono dal file obiettiviXSB.xlsx che si trova nella cartella
# \data\raw. Questi dati sono rielaborati dal codice FTEPROGRAMMATI.R che si trova 
# nella cartella \R\rcodevari e che restituisce l'output datiSB.rds che è inviato alla cartella \data\processed

dtProg <- readRDS(here("data", "processed", "datiSB.rds"))## questo vale per il 2021

ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% #conSB sta per connessione scheda budget  in quanto prende i dati dei fte programmati nelle schede budget
  mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
         Valorizzato = ifelse(Valorizzato != "no", "si", "no"),
         Dipartimento = recode(Dipartimento,
                               "DIPARTIMENTO TUTELA SALUTE ANIMALE" = "DIPARTIMENTO TUTELA E SALUTE ANIMALE")) %>% 
  mutate(Valorizzato = ifelse(Struttura == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" &
                                Indicatore == "Volume attività erogata per FTE  (Ricavo per FTE)", "si", Valorizzato))
saveRDS(ftep22, file = here("data", "processed", "ftep22.rds"))



# FTE PROGRAMMATI PER IZSLER----

prod<-ftep22 %>% # questo codice calcola gli fte allocati alle attività di produzione/vendita prodotti...(nle 2023 non ci saranno più)
  select(Dipartimento, Reparto, Struttura, FTED = AttivitaValorizzataPerproduzioni_FTED, FTEC =AttivitaValorizzataPerproduzioni_FTEC) %>% 
  distinct() %>% 
  summarise(FTED_si = sum(FTED), 
            FTEC_si = sum(FTEC), 
            FTED_tot = sum(FTED), 
            FTEC_tot = sum(FTEC))   


dtProg %>%
  group_by(Valorizzazione) %>%
  summarise(FTED = sum(FTED, na.rm = T),
            FTEC = sum(FTEC, na.rm = T)) %>%
  rowwise() %>%
  mutate(FT = sum(FTED, FTEC)) %>%
  ungroup() %>%
  mutate(FTp = 100*round(prop.table(FT), 1), 
         anno = rep(2021, nrow(.))) %>%
  filter(Valorizzazione == "si") %>%  
  select(anno, FTp)    %>%  


rbind(

ftep22 %>% 
  filter(!str_detect(ObiettivoOperativo,"2.1.9.")) %>%
  summarise(FTED_si = sum(FTED[Valorizzato == "si"], na.rm = TRUE)+ prod$FTED_si,
            FTEC_si = sum(FTEC[Valorizzato == "si"], na.rm = TRUE)+ prod$FTEC_si,
            FTED_tot = sum(FTED, na.rm = TRUE)+ prod$FTED_tot, 
            FTEC_tot = sum(FTEC, na.rm = TRUE)+ prod$FTEC_tot) %>% 
  mutate(FTE_si = FTED_si + FTEC_si,
         FTE_tot = FTED_tot + FTEC_tot) %>% 
  mutate(FTE_perc = 100*(FTE_si/FTE_tot), 
         anno = 2022) %>% 
  select(anno, FTp = FTE_perc) %>% 
  
  ungroup()    
) %>%  

saveRDS(here("data", "processed", "FTp.rds"))

# FTE PROGRAMMATI PER DIPARTIMENTO----
prodDip <- ftep22 %>% 
  select(Dipartimento, Reparto, Struttura, FTED = AttivitaValorizzataPerproduzioni_FTED, FTEC =AttivitaValorizzataPerproduzioni_FTEC) %>% 
  distinct() %>% 
  group_by(Dipartimento) %>%
  summarise(FTED_si = sum(FTED), 
            FTEC_si = sum(FTEC), 
            FTED_tot = sum(FTED), 
            FTEC_tot = sum(FTEC)) %>% 
  filter(FTED_si != 0) %>% 
  mutate(anno = 2022)




dtProg %>% 
  filter(Dipartimento != "Dipartimento Amministrativo") %>% 
  mutate(Dipartimento = recode(Dipartimento, 
                               "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" , 
                               "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia", 
                               "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale", 
  )
  ) %>% 
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%  
  group_by(Valorizzazione, Dipartimento) %>%
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(FT = sum(FTED, FTEC)) %>% 
  group_by(   Dipartimento) %>% 
  # filter(Valorizzazione == "si") %>%  
  mutate(FTp = round(100*prop.table(FT), 1)) %>%   
  select(-FTED, -FTEC) %>%  
  group_by(  Dipartimento, Valorizzazione) %>%  
  filter(Valorizzazione== "si") %>%  
  ungroup() %>%
  mutate(anno = rep(2021, nrow(.))) %>% 
  select(  anno, Dipartimento, FTp) %>% 
  ungroup() %>%  
  
  rbind( 
  
    ftep22 %>% 
      mutate(anno = rep(2022, nrow(.))) %>%  
      mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
             Valorizzato = ifelse(Valorizzato != "no", "si", "no")) %>% 
      filter(!str_detect(ObiettivoOperativo,"2.1.9.")) %>%  
      group_by(anno, Dipartimento) %>% 
      summarise(FTED_si = sum(FTED[Valorizzato == "si"], na.rm = TRUE), 
                FTEC_si = sum(FTEC[Valorizzato == "si"], na.rm = TRUE), 
                FTED_tot = sum(FTED, na.rm = TRUE), 
                FTEC_tot = sum(FTEC, na.rm = TRUE)) %>% 
      rbind(prodDip) %>%  
      group_by(Dipartimento, anno) %>% 
      summarise(FTED_si = sum(FTED_si), 
                FTEC_si = sum(FTEC_si), 
                FTED_tot = sum(FTED_tot), 
                FTEC_tot = sum(FTEC_tot)) %>%
  mutate(FTE_si = FTED_si + FTEC_si,
         FTE_tot = FTED_tot + FTEC_tot) %>% 
  mutate(FTE_perc = 100*(FTE_si/FTE_tot)) %>% 
  select(anno, Dipartimento, FTp = FTE_perc) %>%  
  filter(FTp > 0 ) %>% 
  ungroup()    
  
  ) %>%  


  saveRDS( here("data", "processed", "FTEPD.rds"))






# FTE PROGRAMMATI X REPARTO----


prodRep <- ftep22 %>% 
  select(Dipartimento, Reparto, Struttura, FTED = AttivitaValorizzataPerproduzioni_FTED, FTEC =AttivitaValorizzataPerproduzioni_FTEC) %>% 
  distinct() %>% 
  group_by(Dipartimento, Reparto) %>%
  summarise(FTED_si = sum(FTED), 
            FTEC_si = sum(FTEC), 
            FTED_tot = sum(FTED), 
            FTEC_tot = sum(FTEC)) %>% 
  filter(FTED_si != 0)




dtProg %>% 
  filter(Dipartimento != "Dipartimento Amministrativo") %>% 
  mutate(Dipartimento = recode(Dipartimento, 
                               "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" , 
                               "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia", 
                               "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale", 
  ), 
  Reparto = recode(Reparto, 
                   "STBO-FE-MO" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                   "STPR-PC" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                   "STFO-RA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
                   "STRE" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                   "STBG-BI-SO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                   "STLO-MI" = "SEDE TERRITORIALE DI LODI - MILANO", 
                   "STCR-MN" = "SEDE TERRITORIALE DI CREMONA - MANTOVA", 
                   "STPV" = "SEDE TERRITORIALE DI PAVIA", 
                   "STBS" = "SEDE TERRITORIALE DI BRESCIA",
                   "RPP" = "REPARTO PRODUZIONE PRIMARIA", 
                   "RCABO" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                   "RCA" = "REPARTO CONTROLLO ALIMENTI", 
                   "RCAM" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                   "RVIR" = "REPARTO VIROLOGIA", 
                   "RVVPB" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE", 
                   "RTBA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                   "RPCMB" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                   "AREG" = "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA", 
                   "GESTCENT" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
                   "SORVEPIDEM" = "SORVEGLIANZA EPIDEMIOLOGICA", 
                   "FORMAZIONE" = "FORMAZIONE E BIBLIOTECA"
  )
  ) %>%  
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
  group_by(Valorizzazione, Dipartimento, Reparto) %>%
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>%   
  rowwise() %>% 
  mutate(FT = sum(FTED, FTEC)) %>% 
  group_by( Dipartimento, Reparto) %>% 
  # filter(Valorizzazione == "si") %>%  
  mutate(FTp = round(100*prop.table(FT), 1)) %>%  
  #select(-FTED, -FTEC) %>%  
  group_by(Dipartimento,Reparto,  Valorizzazione) %>% 
  filter(Valorizzazione== "si") %>%  
  ungroup() %>% 
  mutate(anno = rep(2021, nrow(.))) %>% 
  select(anno, Dipartimento,Reparto, FTp) %>%  
  
  rbind(
    
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
              FTEC_tot = sum(FTEC_tot)) %>% 
      mutate(FTE_si = FTED_si + FTEC_si,
             FTE_tot = FTED_tot + FTEC_tot,   
             FTE_perc = 100*(FTE_si/FTE_tot)) %>%  
      ungroup() %>% 
      mutate( anno = rep(2022, nrow(.))) %>%  
      select(anno, Dipartimento, Reparto,  FTp = FTE_perc)  
      
  ) %>%  filter(FTp > 0) %>% 
  mutate(Reparto= ifelse(Reparto == "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" , "SEDE TERRITORIALE DI FORLÌ - RAVENNA", Reparto)) %>%  
saveRDS(here("data", "processed",  "FTEPREP.rds"))




