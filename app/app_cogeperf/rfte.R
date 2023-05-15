ricaviCovid21 <- readRDS(here("data","processed", "ricavoCovid.RDS"))# questo dataset è generato dal codice


# 
# ftp22 <- readRDS(file = here("data", "processed",   "ftep22.rds")) # questo file è prodotto dal codice "codice per fte programmati.R" che si
# #trova nella cartella preparazione dati
# dtProg <- readRDS(here("data","processed", "datiSB.rds"))#
# 
# ftp <- dtProg %>% # FTE programmati per attività valorizzata nel 2021
#   filter(Dipartimento != "Dipartimento Amministrativo" & Valorizzazione == "si") %>%
#   mutate(Dipartimento = recode(Dipartimento,
#                                "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" ,
#                                "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia",
#                                "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale",
#   )
#   ) %>%
#   mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%
#   group_by( Dipartimento) %>%
#   summarise(FTED = sum(FTED, na.rm = T),
#             FTEC = sum(FTEC, na.rm = T)) %>%
#   rowwise() %>%
#   mutate(FT21 = sum(FTED, FTEC)) %>%
#   select(Dipartimento, FT21) %>%
#   ungroup()




dtmensili <- tabIZSLER %>% 
  filter(ANNO >=2022) %>% 
  
  rename("Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, "COSTI" = TotCost,   Anno = ANNO) %>%
  group_by(Dipartimento,Anno, MESE ) %>%
  summarise_at(c("Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  
  left_join(
    (ricaviCovid21 %>% 
       select(Dipartimento, anno, mese, ricOVID) %>% 
       group_by(Dipartimento, anno, mese) %>% 
       summarise(ricovid = sum(ricOVID, na.rm = TRUE))), 
    by = c("Dipartimento" = "Dipartimento", "Anno" = "anno", "MESE" = "mese")) %>%
  
    mutate(
    ricovid = ifelse(is.na(ricovid), 0, ricovid ), 
    RT = ifelse( Anno == 2022, (Valorizzazione+VP+AI)-ricovid, (Valorizzazione+VP+AI)),
    FTET = FTED+FTEC,
    Valorizzazionec = cumsum(Valorizzazione),
    VPc = cumsum(VP),
    AIc = cumsum(AI),
    COSTIc = cumsum(COSTI),
    RTc = cumsum(RT)) %>% 
    left_join(FTEPD %>% mutate(anno = as.numeric(anno),
                             Dipartimento = recode(Dipartimento,
                                                   "DIPARTIMENTO TUTELA SALUTE ANIMALE" = "DIPARTIMENTO TUTELA E SALUTE ANIMALE" )),
            by=c("Dipartimento",  "Anno" = "anno")) %>% 
   mutate(RFTE = RT/(FTET*(FTp/100)),
         # low= RFTE- 0.10*RFTE,
         # high = RFTE + 0.10*RFTE,
         # delta = high-low,
         rfte22 = ifelse(Anno==2022, RFTE, 0),
         # fteTD = 150.1*(FTp/100),
         # fteTC = 142.2*(FTp/100),
         RFTEc= RTc/(FTET*(FTp/100)),
         # lowc= RFTEc- 0.10*RFTEc,
         # highc = RFTEc + 0.10*RFTEc, 
         MESE = factor(MESE), 
         MESE = recode(MESE, 
                       '1' = "gen",
                       '2' = "feb",
                       '3' = "mar",
                       '4' = "apr",
                       '5' = "mag",
                       '6' = "giu",
                       '7' = "lug",
                       '8' = "ago",
                       '9' = "set",
                       '10' = "ott",
                       '11' = "nov",
                       '12' = "dic"
                       )) 
  
  



dtmensiliR <-  tabIZSLER %>%
  
  filter(ANNO >=2022) %>% 
  
  rename( "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI,
          "COSTI" = TotCost,   Anno = ANNO) %>%
  group_by(Dipartimento,Reparto,Anno, MESE ) %>%
  summarise_at(c( "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  
  left_join(
    (ricaviCovid21 %>% 
       mutate(Reparto = recode(Reparto, 
                               "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA"
                               
       )) %>% 
       select(Dipartimento, Reparto, anno, mese, ricOVID) %>% 
       group_by(Dipartimento, Reparto, anno, mese) %>% 
       summarise(ricovid = sum(ricOVID, na.rm = TRUE))), by = c("Dipartimento" = "Dipartimento", "Reparto" = "Reparto", "Anno" = "anno", "MESE" = "mese")) %>%   
  
  mutate(ricovid = ifelse(is.na(ricovid), 0, ricovid ),
         #RT = (Valorizzazione+VP+AI)-ricovid,
         RT = ifelse( Anno == 2022, (Valorizzazione+VP+AI)-ricovid, (Valorizzazione+VP+AI)),
         FTET = FTED+FTEC,
         RTc = cumsum(RT)) %>%
  # filter(Prestazionic >0) %>%
  
  left_join(
    (FTEPREP %>% 
       mutate(anno = as.numeric(anno),
                     Dipartimento = recode(Dipartimento,
                                           "DIPARTIMENTO TUTELA SALUTE ANIMALE" = "DIPARTIMENTO TUTELA E SALUTE ANIMALE" ),
         Reparto= recode(Reparto,
                         "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA")
         
       )),  by=c("Dipartimento","Reparto",  "Anno" = "anno")) %>%
  mutate(RFTE = RT/(FTET*(FTp/100)),
         #low= RFTE- 0.10*RFTE,
         # fteTD = 150.1*(FTp/100),
         # fteTC = 142.2*(FTp/100),
         rfte22 = ifelse(Anno==2022, RFTE, 0),
         RFTEc= RTc/(FTET*(FTp/100)),
         #lowc= RFTEc- 0.10*RFTEc,
         #highc = RFTEc + 0.10*RFTEc
         MESE = factor(MESE), 
         MESE = recode(MESE, 
                       '1' = "gen",
                       '2' = "feb",
                       '3' = "mar",
                       '4' = "apr",
                       '5' = "mag",
                       '6' = "giu",
                       '7' = "lug",
                       '8' = "ago",
                       '9' = "set",
                       '10' = "ott",
                       '11' = "nov",
                       '12' = "dic"
         )) 
         


#Dati per monitoraggio RFTE pagine dipartimenti----


# 
# 
