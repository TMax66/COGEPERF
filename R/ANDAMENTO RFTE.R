library(tidyverse)
library(here)


ore <- readRDS(here("data", "processed", "ore.RDS"))
ricavi <- readRDS(here("data", "processed", "CC.rds"))
ftep <- readRDS(here("data", "processed", "ftepDIP.RDS"))


#fteq mensili cumulati

mesi <- seq(1:12)
ftec <- 142.2 ## 142.2 DERIVA DA (36*47.4)/12  
fted <- 150.1 ## 150.1 DERIVA DA (38*47.4)/12

fteC <- mesi*ftec
fteD <- mesi*fted

fte <- ore %>% 
  filter(ANNO == 2021 &
           !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
                                "Dipartimento amministrativo", "Direzione Amministrativa", 
                                "Direzione Generale") &
           !is.na(Dirigente) & 
           !is.na(Ore)) %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%  
  group_by(ANNO, Dipartimento, Reparto, Dirigente, Mese) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  filter(hworked != 0) %>% 
  # mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*4.34), hworked/(38*4.34))) %>%
  pivot_wider(names_from = "Dirigente", values_from = c("hworked"), values_fill = 0) %>% 
  mutate(hwcomp = cumsum(Comparto), 
         hwdir = cumsum(Dirigenza),   
         FTEC = hwcomp/fteC, 
         FTED = hwdir/fteD, 
         FTET = FTEC+FTED) %>% ungroup() %>%
  select(-ANNO, Dipartimento, Reparto,  MESE =Mese, FTET, -hwcomp, -hwdir, -FTEC, -FTED, 
         -Comparto, -Dirigenza)
  



# ricavi mensili cumulati


Ricfte <- ricavi %>% 
  filter(ANNO == 2021 & Costi== "Ricavo") %>% 
  # filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
  rowwise() %>% 
  mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>%   
  filter(TotRic >0) %>% 
  group_by(Dipartimento, Reparto,  MESE) %>%  
  summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
  mutate(Ricavi = cumsum(TRic)) %>% select(-TRic) %>% 
  left_join(
    fte, by=c("Dipartimento", "Reparto",  "MESE")
  ) %>% 
  mutate(RFTE = Ricavi/FTET) 


Ricfte %>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_line(group = 1)+
  facet_wrap(Dipartimento ~ Reparto, scales = "free")
  



band <- Ricfte %>% 
  mutate(low= RFTE- 0.10*RFTE, 
         high = RFTE + 0.10*RFTE)


band %>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_ribbon(aes(ymin = low, ymax = high), fill = "grey70", alpha= 0.5) +
  facet_wrap(Dipartimento ~ Reparto, scales = "free")


##fte mensili

ftem <- ore %>% 
  filter(ANNO == 2021 &
           !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
                                "Dipartimento amministrativo", "Direzione Amministrativa", 
                                "Direzione Generale") &
           !is.na(Dirigente) & 
           !is.na(Ore)) %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%  
  group_by(ANNO, Dipartimento, Reparto, Dirigente, Mese) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  filter(hworked != 0) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(142.2), hworked/(150.1))) %>% 
  select(-hworked) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("FTE"), values_fill = 0) %>% 
  mutate( FTET = Comparto+Dirigenza)%>% ungroup() %>%  
  select(-ANNO, Dipartimento, Reparto,  MESE =Mese, FTET, -Comparto, -Dirigenza)



Ricftem<- ricavi %>% 
  filter(ANNO == 2021 & Costi== "Ricavo") %>% 
  # filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
  rowwise() %>% 
  mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>%   
  filter(TotRic >0) %>% 
  group_by(Dipartimento, Reparto,  MESE) %>%  
  summarise(TRic= sum(TotRic, na.rm = TRUE)) %>%   
  left_join(
    ftem, by=c("Dipartimento", "Reparto",  "MESE")) %>%  
  mutate(RFTE = TRic/FTET)


Ricftem%>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_line(group = 1)+
  facet_wrap(Dipartimento ~ Reparto, scales = "free")


bandm <- Ricftem %>% 
  mutate(low= RFTE- 0.10*RFTE, 
         high = RFTE + 0.10*RFTE, 
         delta = high-low)


bandm %>% 
 # filter(Reparto == "SEDE TERRITORIALE DI FORLÌ - RAVENNA") %>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_line(group = 1, color = "Blue", lty= 2, alpha= 0.5)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.3) +
  
  facet_wrap(Dipartimento ~ Reparto, scales = "free")
