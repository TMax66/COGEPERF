library(tidyverse)
library(here)
 
dtmensili <- tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost,   Anno = ANNO) %>%   
  group_by(Dipartimento,Anno, MESE ) %>% 
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  mutate(RT = (Valorizzazione+VP+AI),
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
         fakep = RFTE+rnorm(12, 0, 0.10*RFTE), 
         fteTD = 150.1*(FTp/100), 
         fteTC = 142.2*(FTp/100))


  
  
  
 rfte<- function(dip){  
  dtmensili %>% 
     filter(Dipartimento == dip) %>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_line(group = 1)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.5) +
  geom_point(aes(x=MESE, y=fakep), color="red") 
 }


fte <- function(dip){   
  dtmensili %>% 
    filter(Dipartimento == dip) %>% 
  ggplot()+
  aes(x = MESE, y = FTET)+
  geom_line(group = 1)+
  geom_point()#+
  # facet_wrap(Dipartimento ~ ., scales = "free")
}


rt<- function(dip){ 
  
  dtmensili %>% 
    filter(Dipartimento == dip) %>% 
  ggplot()+
  aes(x = MESE, y = RT)+
  geom_line(group = 1)+
  geom_point()
}

library(patchwork)

  
rfte("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")/rt("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")/fte("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")
rfte("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")/rt("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")/fte("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")
rfte("DIPARTIMENTO SICUREZZA ALIMENTARE")/rt("DIPARTIMENTO SICUREZZA ALIMENTARE")/fte("DIPARTIMENTO SICUREZZA ALIMENTARE")
rfte("DIPARTIMENTO TUTELA E SALUTE ANIMALE")/rt("DIPARTIMENTO TUTELA E SALUTE ANIMALE")/fte("DIPARTIMENTO TUTELA E SALUTE ANIMALE")




tizsler %>% 
  filter(Anno >=2021) %>% 
  left_join(FTEPD ,  by=c("Dipartimento",  "Anno" = "anno")) %>%  
  mutate(RFTE = RT/(FTET*(FTp/100)), 
         low= RFTE- 0.10*RFTE, 
         high = RFTE + 0.10*RFTE, 
         delta = high-low, 
         fakep = RFTE+rnorm(12, 0, 0.10*RFTE)) %>%   
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_line(group = 1)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.5) +
  geom_point(aes(x=MESE, y=fakep), color="red")+
  facet_wrap(Dipartimento ~ ., scales = "free")
  


tizsler %>% 
  filter(Anno >=2021) %>% 
  left_join(FTEPD ,  by=c("Dipartimento",  "Anno" = "anno")) %>% 
  ggplot()+
  aes(x = MESE, y = FTET)+
  geom_line(group = 1)+
  geom_point()+
  facet_wrap(Dipartimento ~ ., scales = "free")















# 
# 
# 
# 
# band <- Ricfte %>% 
#   mutate(low= RFTE- 0.10*RFTE, 
#          high = RFTE + 0.10*RFTE)
# 
# 
# band %>% 
#   ggplot()+
#   aes(x = MESE, y = RFTE)+
#   geom_ribbon(aes(ymin = low, ymax = high), fill = "grey70", alpha= 0.5) +
#   facet_wrap(Dipartimento ~ Reparto, scales = "free")
# 
# 
# ##fte mensili
# 
# ftem <- ore %>% 
#   filter(ANNO == 2021 &
#            !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
#                                 "Dipartimento amministrativo", "Direzione Amministrativa", 
#                                 "Direzione Generale") &
#            !is.na(Dirigente) & 
#            !is.na(Ore)) %>% 
#   mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
#          Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%  
#   group_by(ANNO, Dipartimento, Reparto, Dirigente, Mese) %>% 
#   summarise(hworked = sum(Ore, na.rm = T)) %>%  
#   filter(hworked != 0) %>% 
#   mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(142.2), hworked/(150.1))) %>% 
#   select(-hworked) %>% 
#   pivot_wider(names_from = "Dirigente", values_from = c("FTE"), values_fill = 0) %>% 
#   mutate( FTET = Comparto+Dirigenza)%>% ungroup() %>%  
#   select(-ANNO, Dipartimento, Reparto,  MESE =Mese, FTET, -Comparto, -Dirigenza)
# 
# 
# 
# Ricftem<- ricavi %>% 
#   filter(ANNO == 2021 & Costi== "Ricavo") %>% 
#   # filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
#   rowwise() %>% 
#   mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>%   
#   filter(TotRic >0) %>% 
#   group_by(Dipartimento, Reparto,  MESE) %>%  
#   summarise(TRic= sum(TotRic, na.rm = TRUE)) %>%   
#   left_join(
#     ftem, by=c("Dipartimento", "Reparto",  "MESE")) %>%  
#   mutate(RFTE = TRic/FTET)
# 
# 
# Ricftem%>% 
#   ggplot()+
#   aes(x = MESE, y = RFTE)+
#   geom_line(group = 1)+
#   facet_wrap(Dipartimento ~ Reparto, scales = "free")
# 
# 
# bandm <- Ricftem %>% 
#   mutate(low= RFTE- 0.10*RFTE, 
#          high = RFTE + 0.10*RFTE, 
#          delta = high-low)
# 
# 
# bandm %>% 
#  # filter(Reparto == "SEDE TERRITORIALE DI FORLÌ - RAVENNA") %>% 
#   ggplot()+
#   aes(x = MESE, y = RFTE)+
#   geom_line(group = 1, color = "Blue", lty= 2, alpha= 0.5)+
#   geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.3) +
#   
#   facet_wrap(Dipartimento ~ Reparto, scales = "free")
