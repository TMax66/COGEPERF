# i dati originali provengono dal file obiettiviXSB.xlsx che si trova nella cartella
# \data\raw. Questi dati sono rielaborati dal codice FTEPROGRAMMATI.R che si trova 
# nella cartella \R\rcodevari e che restituisce l'output datiFSB.rds che è inviato alla cartella \data\processed

dtProg <- readRDS(here("data", "processed", "datiSB.rds"))
ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% 
  mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
         Valorizzato = ifelse(Valorizzato != "no", "si", "no"))  

                



  
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
  summarise(FTED_si = sum(FTED[Valorizzato == "si"], na.rm = TRUE),
            FTEC_si = sum(FTEC[Valorizzato == "si"], na.rm = TRUE),
            FTED_tot = sum(FTED, na.rm = TRUE), 
            FTEC_tot = sum(FTEC, na.rm = TRUE)) %>%
  mutate(FTE_si = FTED_si + FTEC_si,
         FTE_tot = FTED_tot + FTEC_tot) %>% 
  mutate(FTE_perc = 100*(FTE_si/FTE_tot), 
         anno = 2022) %>% 
  select(anno, FTp = FTE_perc) %>% 
  
  ungroup()  
) %>% 
  saveRDS(here("data", "processed", "FTp.rds"))









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
  mutate(FTE_si = FTED_si + FTEC_si,
         FTE_tot = FTED_tot + FTEC_tot) %>% 
  mutate(FTE_perc = 100*(FTE_si/FTE_tot)) %>% 
  select(anno, Dipartimento, FTp = FTE_perc) %>%  
  filter(FTp > 0 ) %>% 
  ungroup()    
  
  ) %>%  
  saveRDS(here("data", "processed", "FTEPD.rds"))


#saveRDS(here("data", "processed", "ftepDIP.RDS"))





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
      mutate(FTE_si = FTED_si + FTEC_si,
             FTE_tot = FTED_tot + FTEC_tot,   
             FTE_perc = 100*(FTE_si/FTE_tot)) %>%  
      ungroup() %>% 
      mutate( anno = rep(2022, nrow(.))) %>%  
      select(anno, Dipartimento, Reparto,  FTp = FTE_perc)  
      
  ) %>%  filter(FTp > 0) %>% 
  
  saveRDS(here("data", "processed", "FTEPREP.rds"))





# dtProg %>%
#   filter(Dipartimento != "Dipartimento Amministrativo") %>%
#   mutate(Dipartimento = recode(Dipartimento,
#                                "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" ,
#                                "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia",
#                                "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale",
#   ),
#   Reparto = recode(Reparto,
#                    "STBO-FE-MO" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
#                    "STPR-PC" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
#                    "STFO-RA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
#                    "STRE" = "SEDE TERRITORIALE DI REGGIO EMILIA",
#                    "STBG-BI-SO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
#                    "STLO-MI" = "SEDE TERRITORIALE DI LODI - MILANO",
#                    "STCR-MN" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
#                    "STPV" = "SEDE TERRITORIALE DI PAVIA",
#                    "STBS" = "SEDE TERRITORIALE DI BRESCIA",
#                    "RPP" = "REPARTO PRODUZIONE PRIMARIA",
#                    "RCABO" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
#                    "RCA" = "REPARTO CONTROLLO ALIMENTI",
#                    "RCAM" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
#                    "RVIR" = "REPARTO VIROLOGIA",
#                    "RVVPB" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
#                    "RTBA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
#                    "RPCMB" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
#                    "AREG" = "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA",
#                    "GESTCENT" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE",
#                    "SORVEPIDEM" = "SORVEGLIANZA EPIDEMIOLOGICA",
#                    "FORMAZIONE" = "FORMAZIONE E BIBLIOTECA"
#   )
#   ) %>%
#   mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%
#   group_by(Valorizzazione, Dipartimento, Reparto) %>%
#   summarise(FTED = sum(FTED, na.rm = T),
#             FTEC = sum(FTEC, na.rm = T)) %>%
#   rowwise() %>%
#   mutate(FT = sum(FTED, FTEC)) %>%
#   saveRDS(here("data", "processed", "ftepREPD.RDS"))

