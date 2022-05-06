library(tidyverse)
library(here)
 

tabIZSLER <- readRDS(file = here("data", "processed",   "TabellaGenerale.rds"))
FTEPD <- readRDS(file = here("data", "processed",   "FTEPD.rds"))
conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                        Database = "ObiettiviStrategici2022", Port = 1433)
query <- "SELECT Pesatura, Valorizzato, Anno, Target, PercFTED, FTED, PercFTEC, FTEC, CentroReferenza_PercFTED, CentroReferenza_FTED, CentroReferenza_PercFTEC, CentroReferenza_FTEC, AttivitaRoutinaria_PercFTED, AttivitaRoutinaria_FTED, AttivitaRoutinaria_PercFTEC, AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_PercFTED, AttivitaValorizzataPerproduzioni_FTED, AttivitaValorizzataPerproduzioni_PercFTEC, AttivitaValorizzataPerproduzioni_FTEC, AreaStrategica, ObiettivoGenerale, ObiettivoOperativo, Indicatore, CriteriAttivita, Dipartimento, Reparto, Struttura, TipologiaIndicatore FROM vSchedaBudget"

ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% #conSB sta per connessione scheda budget  in quanto prende i dati dei fte programmati nelle schede budget
  mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
         Valorizzato = ifelse(Valorizzato != "no", "si", "no"),
         Dipartimento = recode(Dipartimento,
                               "DIPARTIMENTO TUTELA SALUTE ANIMALE" = "DIPARTIMENTO TUTELA E SALUTE ANIMALE")) 

ftep22r<-ftep22 %>% 
  filter(Valorizzato == "si") %>% 
  group_by(Dipartimento, Reparto) %>% 
  summarise(ftetot= sum(FTEC)+sum(FTED))

ftep22D <- ftep22 %>% 
  filter(Valorizzato == "si") %>% 
  group_by(Dipartimento) %>% 
  summarise(ftetot= sum(FTEC)+sum(FTED))


dtProg <- readRDS(here("data", "processed", "datiSB.rds"))

ftp <- dtProg %>% 
  filter(Dipartimento != "Dipartimento Amministrativo" & Valorizzazione == "si") %>% 
  mutate(Dipartimento = recode(Dipartimento, 
                               "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" , 
                               "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia", 
                               "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale", 
  )
  ) %>% 
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%  
  group_by( Dipartimento) %>%
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(FT21 = sum(FTED, FTEC)) %>% 
  #group_by(   Dipartimento) %>% 
  # filter(Valorizzazione == "si") %>%  
  # mutate(FTp = round(100*prop.table(FT), 1)) %>%   
  # select(-FTED, -FTEC) %>%  
  # group_by(  Dipartimento, Valorizzazione) %>%  
  # filter(Valorizzazione== "si") %>%  
  # ungroup() %>%
  # mutate(anno = rep(2021, nrow(.))) %>% 
  select(Dipartimento, FT21) %>% 
  ungroup()   
  
    
    
    
  
  
  
  

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
         fteTC = 142.2*(FTp/100)) %>% 
  left_join(ftp, by=c("Dipartimento"))  


  
  
  
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
  ggplot()+aes(x = MESE, y = FTET)+
  #aes(x = MESE, y = FTET*(FTp/100))+
  geom_line(group = 1)+
  geom_point()+
    #geom_line(aes(x = MESE, y = FT21), color = "red")
  geom_line(aes(x = MESE, y = FT21+(FT21*(ft))), color = "red")
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



FTEPREP <- readRDS(file = here("data", "processed",   "FTEPREP.rds"))
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
  filter(Anno >=2021) %>% 
  left_join(
    (FTEPREP %>% 
       mutate(Reparto= ifelse(Reparto == "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" , "SEDE TERRITORIALE DI FORLÌ - RAVENNA", Reparto)
    )),  by=c("Dipartimento","Reparto",  "Anno" = "anno")) %>%  
  mutate(RFTE = RT/(FTET*(FTp/100)), 
         low= RFTE- 0.10*RFTE, 
         high = RFTE + 0.10*RFTE, 
         delta = high-low, 
         fteTD = 150.1*(FTp/100), 
         fteTC = 142.2*(FTp/100))
   

rfteR<- function(dip){  
  dtmensiliR %>% 
    filter(Dipartimento == dip) %>% 
    ggplot()+
    aes(x = MESE, y = RFTE)+
    geom_line(group = 1)+
    geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.5) +
   # geom_point(aes(x=MESE, y=fakep), color="red") +
    facet_wrap(Reparto ~., scales = "free" )
}



fteR <- function(dip){   
  dtmensiliR %>% 
    filter(Dipartimento == dip) %>% 
    ggplot()+
    aes(x = MESE, y = FTET)+
    geom_line(group = 1)+
    geom_point()+
  facet_wrap(Reparto ~., scales = "free" )
}



rtR<- function(dip){ 
  
  dtmensiliR %>% 
    filter(Dipartimento == dip) %>% 
    ggplot()+
    aes(x = MESE, y = RT)+
    geom_line(group = 1)+
    geom_point()+
    facet_wrap(Reparto ~., scales = "free" )
}



##variazione fteerogato rispetto al disponibile---


varfte <- tabIZSLER %>% 
  rename(Anno = ANNO) %>% 
  filter(Anno >= 2021) %>% 
  group_by(Dipartimento,Anno, MESE ) %>% 
  summarise_at(c("FTED", "FTEC"), sum, na.rm = T) %>% 
  mutate(FTET = FTED+FTEC) %>% 
  
  left_join(
    (dtProg %>% 
       filter(Dipartimento != "Dipartimento Amministrativo") %>% 
       mutate(Dipartimento = recode(Dipartimento, 
                                    "Area Territoriale Emilia Romagna" = "Dipartimento Area Territoriale Emilia Romagna" , 
                                    "Area Territoriale Lombardia" = "Dipartimento Area Territoriale Lombardia", 
                                    "Dipartimento Tutela Salute Animale" = "Dipartimento tutela e salute animale", 
       )
       ) %>% 
       mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%  
       group_by( Dipartimento) %>%
       summarise(FTED = sum(FTED, na.rm = T), 
                 FTEC = sum(FTEC, na.rm = T)) %>% 
       rowwise() %>% 
       mutate(FT21 = sum(FTED, FTEC)) %>% 
       select(-FTED, -FTEC)), by = c("Dipartimento")) %>%  
  filter(!is.na(FT21)) 



varfte %>% 
  ggplot()+
  aes(x=MESE, y=FT21)+
  geom_line(group = 1)+
  geom_point(aes(x=MESE, y=FTET))+
  geom_line(aes(x=MESE, y=FTET))+
  ylim(0, max(varfte$FT21+3))+
  facet_wrap(Dipartimento~., scales = "free")















# conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
#                         Database = "ObiettiviStrategici2022", Port = 1433)
# 
# query <- "SELECT Pesatura, Valorizzato, Anno, Target, PercFTED, FTED, PercFTEC, FTEC, CentroReferenza_PercFTED, CentroReferenza_FTED, CentroReferenza_PercFTEC, CentroReferenza_FTEC, AttivitaRoutinaria_PercFTED, AttivitaRoutinaria_FTED, AttivitaRoutinaria_PercFTEC, AttivitaRoutinaria_FTEC, AttivitaValorizzataPerproduzioni_PercFTED, AttivitaValorizzataPerproduzioni_FTED, AttivitaValorizzataPerproduzioni_PercFTEC, AttivitaValorizzataPerproduzioni_FTEC, AreaStrategica, ObiettivoGenerale, ObiettivoOperativo, Indicatore, CriteriAttivita, Dipartimento, Reparto, Struttura, TipologiaIndicatore FROM vSchedaBudget"
# 
# ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% #conSB sta per connessione scheda budget  in quanto prende i dati dei fte programmati nelle schede budget
#   mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
#          Valorizzato = ifelse(Valorizzato != "no", "si", "no")) 
# 
# ftep22r<-ftep22 %>% 
#   filter(Valorizzato == "si") %>% 
#   group_by(Dipartimento, Reparto) %>% 
#   summarise(ftetot= sum(FTEC)+sum(FTED))
# 
# ftep22D <- ftep22 %>% 
#   filter(Valorizzato == "si") %>% 
#   group_by(Dipartimento) %>% 
#   summarise(ftetot= sum(FTEC)+sum(FTED))















































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
