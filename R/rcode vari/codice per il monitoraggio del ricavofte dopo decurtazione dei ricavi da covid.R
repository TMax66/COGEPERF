library("tidyverse")
library("readxl")
library("lubridate")


tabIZSLER <- readRDS(file = here("data", "processed",   "TabellaGenerale.rds"))
ricaviCovid21 <- readRDS(here("data","processed", "ricavoCovid.RDS"))

dtmensili <- tabIZSLER %>%
  
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI,
          "COSTI" = TotCost,   Anno = ANNO) %>%
  group_by(Dipartimento,Anno, MESE ) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%   
  
  left_join(
    (ricaviCovid21 %>% 
       select(Dipartimento, anno, mese, ricOVID) %>% 
       group_by(Dipartimento, anno, mese) %>% 
       summarise(ricovid = sum(ricOVID, na.rm = TRUE))), by = c("Dipartimento" = "Dipartimento", "Anno" = "anno", "MESE" = "mese")) %>%  
   mutate(
     ricovid = ifelse(is.na(ricovid), 0, ricovid ), 
     RT = (Valorizzazione+VP+AI)-ricovid,
         FTET = FTED+FTEC,
         Prestazionic = cumsum(Prestazioni),
         Valorizzazionec = cumsum(Valorizzazione),
         VPc = cumsum(VP),
         AIc = cumsum(AI),
         COSTIc = cumsum(COSTI),
         RTc = cumsum(RT)) %>%
  filter(Prestazionic >0) %>%
  filter(Anno >=2021) %>% 
  left_join(FTEPD ,  by=c("Dipartimento",  "Anno" = "anno")) %>%  
  mutate(RFTE = RT/(FTET*(FTp/100)),
         low= RFTE- 0.10*RFTE,
         high = RFTE + 0.10*RFTE,
         delta = high-low,
         rfte22 = ifelse(Anno==2022, RFTE, 0),
         fteTD = 150.1*(FTp/100),
         fteTC = 142.2*(FTp/100),
         RFTEc= RTc/(FTET*(FTp/100)),
         lowc= RFTEc- 0.10*RFTEc,
         highc = RFTEc + 0.10*RFTEc
  ) %>%
  left_join(ftp, by=c("Dipartimento"))    




  
dtmensiliR <-  tabIZSLER %>%
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI,
          "COSTI" = TotCost,   Anno = ANNO) %>%
  group_by(Dipartimento,Reparto,Anno, MESE ) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
 
   left_join(
    (ricaviCovid21 %>% 
       mutate(Reparto = recode(Reparto, 
                               "SEDE TERRITORIALE DI MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA"
                               
                               )) %>% 
       select(Dipartimento, Reparto, anno, mese, ricOVID) %>% 
       group_by(Dipartimento, Reparto, anno, mese) %>% 
       summarise(ricovid = sum(ricOVID, na.rm = TRUE))), by = c("Dipartimento" = "Dipartimento", "Reparto" = "Reparto", "Anno" = "anno", "MESE" = "mese")) %>%   
  
  mutate(ricovid = ifelse(is.na(ricovid), 0, ricovid ),
    RT = (Valorizzazione+VP+AI)-ricovid,
         FTET = FTED+FTEC,
         Prestazionic = cumsum(Prestazioni),
         RTc = cumsum(RT)) %>%
  filter(Prestazionic >0) %>%
  filter(Anno >=2021) %>%
  left_join(
    (FTEPREP %>%
       mutate(
         Dipartimento = recode(Dipartimento,
                               "DIPARTIMENTO TUTELA SALUTE ANIMALE" = "DIPARTIMENTO TUTELA E SALUTE ANIMALE"),
         Reparto= recode(Reparto,
                         "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA")
         
       )),  by=c("Dipartimento","Reparto",  "Anno" = "anno")) %>%
  mutate(RFTE = RT/(FTET*(FTp/100)),
         low= RFTE- 0.10*RFTE,
         fteTD = 150.1*(FTp/100),
         fteTC = 142.2*(FTp/100),
         rfte22 = ifelse(Anno==2022, RFTE, 0),
         RFTEc= RTc/(FTET*(FTp/100)),
         lowc= RFTEc- 0.10*RFTEc,
         highc = RFTEc + 0.10*RFTEc) %>% 
  filter(MESE == 6) %>% 
select(Reparto, Anno, RFTEc) %>%  
  pivot_wider(names_from = "Anno", values_from = "RFTEc") %>% 
 
 mutate(gradoav = 100*(`2022`/(`2021`-(0.10*`2021`))) ) %>% 
  write.xlsx("rfte.xlsx")
  
  
  
  
  
  
  
  
  



#Reparti

dtmensiliR <-  tabIZSLER %>%
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI,
          "COSTI" = TotCost,   Anno = ANNO) %>%
  group_by(Dipartimento,Reparto,Anno, MESE ) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = FTED+FTEC,
         Prestazionic = cumsum(Prestazioni),
         RTc = cumsum(RT)) %>%
  filter(Prestazionic >0) %>%
  filter(Anno >=2021) %>%View()
