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
         fteTC = 142.2*(FTp/100),
         RFTEc= RTc/(FTET*(FTp/100)), 
         lowc= RFTEc- 0.10*RFTEc, 
         highc = RFTEc + 0.10*RFTEc, 
         deltac = high-low, 
         fakepc = RFTEc+rnorm(12, 0, 0.10*RFTEc), 
         ) %>% 
  left_join(ftp, by=c("Dipartimento"))  

dtmensili %>% 
  filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>% 
ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_point()+
  geom_line(group = 1)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha = 0.5) +
  geom_point(aes(x=MESE, y=fakep), color="red") +
  #facet_wrap(Dipartimento~., ncol = 1, scales = "free")+
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_bw()


dtmensili %>% 
  ggplot()+
  aes(x = MESE, y = RFTEc)+
  geom_point()+
  geom_line(group = 1)+
  geom_ribbon(aes(ymin = lowc, ymax = RFTEc), fill = "grey70", alpha= 0.5) +
  geom_point(aes(x=MESE, y=fakepc), color="red") +
  facet_wrap(Dipartimento~., ncol = 1, scales = "free")+
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_bw()




  
  
 rfte<- function(dip){  
   dtmensili %>% 
     filter(Dipartimento == dip) %>% 
     ggplot()+
     aes(x = MESE, y = RFTE)+
     geom_point()+
     geom_line(group = 1)+
     geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha = 0.5) +
     #geom_point(aes(x=MESE, y=fakep), color="red") +
     scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
     theme_bw()
 }


fte <- function(dip){   
  dtmensili %>% 
    filter(Dipartimento == dip) %>% 
  ggplot()+aes(x = MESE, y = FTET)+
  #aes(x = MESE, y = FTET*(FTp/100))+
  geom_line(group = 1)+
  geom_point()+  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
    #geom_line(aes(x = MESE, y = FT21), color = "red")
  #geom_line(aes(x = MESE, y = FT21+(FT21*(ft))), color = "red")
  # facet_wrap(Dipartimento ~ ., scales = "free")
}


rt<- function(dip){ 
  
  dtmensili %>% 
    filter(Dipartimento == dip) %>% 
  ggplot()+
  aes(x = MESE, y = RT)+
  geom_line(group = 1)+
  geom_point()+
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
}

library(patchwork)

  
rfte("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")/rt("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")/fte("DIPARTIMENTO AREA TERRITORIALE LOMBARDIA")+
  plot_annotation('DIPARTIMENTO AREA TERRITORIALE LOMBARDIA')

rfte("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")/rt("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")/fte("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA")+
  plot_annotation('DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA')

rfte("DIPARTIMENTO SICUREZZA ALIMENTARE")/rt("DIPARTIMENTO SICUREZZA ALIMENTARE")/fte("DIPARTIMENTO SICUREZZA ALIMENTARE")+
  plot_annotation('DIPARTIMENTO SICUREZZA ALIMENTARE')

rfte("DIPARTIMENTO TUTELA E SALUTE ANIMALE")/rt("DIPARTIMENTO TUTELA E SALUTE ANIMALE")/fte("DIPARTIMENTO TUTELA E SALUTE ANIMALE")+
  plot_annotation('DIPARTIMENTO TUTELA E SALUTE ANIMALE')



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
   

rfteR<- function(rep){  
  dtmensiliR %>% 
    filter(Reparto == rep) %>% 
    ggplot()+
    aes(x = MESE, y = RFTE)+
    geom_point()+
    geom_line(group = 1)+
    geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha= 0.5) +
   # geom_point(aes(x=MESE, y=fakep), color="red") +
   # facet_wrap(Reparto ~., scales = "free" )+
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
}



fteR <- function(rep){   
  dtmensiliR %>% 
    filter(Reparto == rep) %>% 
    ggplot()+
    aes(x = MESE, y = FTET)+
    geom_line(group = 1)+
    geom_point()+
  # facet_wrap(Reparto ~., scales = "free" )
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
}



rtR<- function(rep){ 
  
  dtmensiliR %>% 
    filter(Reparto == rep) %>% 
    ggplot()+
    aes(x = MESE, y = RT)+
    geom_line(group = 1)+
    geom_point()+
    #facet_wrap(Reparto ~., scales = "free" )
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
}


rfteR("ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA")/
  rtR("ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA")/
  fteR("ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA")+
  plot_annotation('ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA')

rfteR("SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA")/
  rtR("SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA")/
  fteR("SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA")+
  plot_annotation('SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA')

rfteR("SEDE TERRITORIALE DI FORLÌ - RAVENNA")/
  rtR("SEDE TERRITORIALE DI FORLÌ - RAVENNA")/
  fteR("SEDE TERRITORIALE DI FORLÌ - RAVENNA")+
  plot_annotation('SEDE TERRITORIALE DI FORLÌ - RAVENNA')

rfteR("SEDE TERRITORIALE DI PIACENZA - PARMA")/
  rtR("SEDE TERRITORIALE DI PIACENZA - PARMA")/
  fteR("SEDE TERRITORIALE DI PIACENZA - PARMA")+
  plot_annotation('SEDE TERRITORIALE DI PIACENZA - PARMA')

rfteR("SEDE TERRITORIALE DI REGGIO EMILIA")/
  rtR("SEDE TERRITORIALE DI REGGIO EMILIA")/
  fteR("SEDE TERRITORIALE DI REGGIO EMILIA")+
  plot_annotation('SEDE TERRITORIALE DI REGGIO EMILIA')



##variazione fteerogato rispetto al disponibile---

###x dip
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
  aes(x=MESE, y=FTET)+
  geom_line(group = 1)+
  geom_point(aes(x=MESE, y=FTET))+
  geom_line(aes(x=MESE, y=FT21), col = "red", lty = 2)+
  ylim(0, max(varfte$FT21+3))+
  labs(y="FTET")+
  facet_wrap(Dipartimento~., scales = "free")+
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()

##xrep

varfteR <- tabIZSLER %>% 
  filter(Dipartimento != "Dipartimento Amministrativo") %>% 
  rename(Anno = ANNO) %>% 
  filter(Anno >= 2021) %>% 
  group_by(Dipartimento,Reparto, Anno, MESE ) %>% 
  summarise_at(c("FTED", "FTEC"), sum, na.rm = T) %>% 
  mutate(FTET = FTED+FTEC) %>% 
  left_join(   
  
  (dtProg %>% 
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
  group_by( Dipartimento, Reparto) %>%
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(FT21 = sum(FTED, FTEC))), by = c("Dipartimento", "Reparto") ) %>%  
  mutate(varFTE = FTET-FT21) %>% 
  filter(!Reparto == "COSTI COMUNI LOMBARDIA")  %>%  
  group_by(Dipartimento, Reparto) %>%
  summarise(varM = mean(varFTE))

varfteR %>% 
  filter(Dipartimento == "DIREZIONE SANITARIA") %>%  
  ggplot()+
  aes(x=MESE, y=FTET)+
  geom_point()+
  geom_line()+
  geom_line(aes(x=MESE, y=FT21), color = "blue", lty=2)+
  ylim(0,max(20))+
  facet_wrap(Reparto~., scales = "free")+
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_bw()+
  labs(title= "STRUTTURE IN STAFF ALLA DIREZIONE SANITARIA")
  

 
 

varfteR %>% 
  filter(!is.na(varM)) %>% 
  ggplot()+
  aes(x=varM, y = Reparto, label = round(varM, 1) )+
  geom_point(size = 9, col="steelblue", alpha = 0.5)+
  geom_text()+
  geom_segment(aes(x=0, xend=varM, y=Reparto, yend=Reparto), col= "darkgrey")+
  labs(x= "media annuale delle differenze tra FTE disponibili e FTE erogati")+
  geom_vline(xintercept = 0, col = "red", lty=2)+
  theme_bw()
  
  #facet_wrap(Dipartimento~., scales = "free")
 



##eccedenze orarie mensili ----

#ore di lavoro previste per il 2021 e per mese---
library(readxl)
festivi <- read_excel("data/raw/festivi.xlsx")

library(lubridate)
f <- festivi %>%
  mutate(g = wday(data, label= TRUE), 
         Mese = month(data)) %>% 
  filter(!g %in% c("sab", "dom")) %>% 
  group_by(Mese) %>% 
  count()


hC_21 <- tibble(
  giorni = seq(dmy("1/1/2021"), dmy("31/12/2021"), by = "day")
) %>% 
  mutate(gg=wday(giorni, label = TRUE), 
         Mese=month(giorni)) %>% 
  filter(!gg %in% c("sab", "dom")) %>% 
  group_by(Mese) %>% 
  count() %>%  
  left_join(f, by="Mese") %>%
  mutate(n.y = ifelse(is.na(n.y), 0, n.y),
         glav=n.x-n.y, 
         Comparto = 7.2*glav, 
         Dirigenza = 7.6*glav) %>% 
  ungroup() %>%
  select(Mese, Comparto, Dirigenza) %>% 
  pivot_longer(cols = 2:3, names_to = "Dirigente", values_to = "h") 

ore <- readRDS(here("data", "processed", "ore.rds"))
names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")

oreW <- ore %>% 
  
  filter( !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
                               "Dipartimento amministrativo", "Direzione Amministrativa", 
                               "Direzione Generale") &
            #!Reparto %in% c("GESTIONE CENTRALIZZATA DELLE RICHIESTE")&
            !is.na(Dirigente) & 
            !is.na(Ore)) %>% 
  filter( !str_detect(Laboratorio, "Costi") & ANNO == 2021) %>%  
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>% 
  left_join(hC_21, by=c("Dirigente", "Mese")) %>%  
  group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente, Mese) %>%  
  summarise(h = Percentuale*h,
            hcontratto = sum(h, na.rm = TRUE), 
            hworked = sum(Ore, na.rm = TRUE)) %>%  View()
  filter(hworked != 0) %>%  
  ungroup() %>% 
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
  filter(!Reparto == "COSTI COMUNI LOMBARDIA") %>% 
  select(-ANNO)# %>% 
  # pivot_wider(names_from = "Dirigente", values_from = c("hworked", "hcontratto")) %>% 
  # mutate(hWtot = hworked_Comparto+hworked_Dirigenza,
  #        hCtot = hcontratto_Comparto+hcontratto_Dirigenza)  


oreW %>% filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA") %>%  
  ggplot()+
    aes(x=Mese, y = hcontratto)+
    geom_point()+
    geom_line()+
    geom_point(aes(x=Mese, y = hworked), col="blue")+
    geom_line(aes(x=Mese, y = hworked), col="blue")+
    facet_wrap(Laboratorio~Dirigente, scales = "free")





#ore di lavoro previste per struttura e per contratto

#library(readxl)
# dati22 <- read_excel(here("data", "raw", "presenze2022.xlsx"))
# 
# CC <- readRDS(here("data", "processed", "CC.rds"))
# 
# 
# oreC <- dati22 %>% 
#   mutate(peror = as.numeric(`Perc Orario`)) %>%  
#   left_join(  
#     (CC %>% 
#        select(Dipartimento, Reparto, Laboratorio, CDC, CodiceCDC) %>% 
#        unique() ) , by = c("CODICE_CDC" = "CodiceCDC")) %>%   
#   
#   # mutate(Laboratorio = recode(Laboratorio, 
#   #                             "LABORATORIO DI CONTROLLO DI PRODOTTI BIOLOGICI, FARMACEUTICI E CONVALIDA DI PROCESSI PRODUTTIVI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
#   #                             "LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
#   #                             "LABORATORIO ANALISI GENOMICHE, LABORATORIO DIAGNOSTICA MOLECOLARE, OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
#   #                             "LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
#   #                             "LABORATORIO DI PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA", 
#   #                             "LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA, MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA", 
#   #                             "LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
#   #                             "LABORATORIO CONTAMINANTI AMBIENTALI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
#   #                             "LABORATORIO MANGIMI E TOSSICOLOGIA" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI")) %>%  
#   mutate(Dirigente = ifelse(Dirigente == "N", "Comparto", "Dirigenza"), 
#          Dipartimento = casefold(Dipartimento, upper= TRUE)) %>%  
#   filter(!Dipartimento %in% c("DIPARTIMENTO AMMINISTRATIVO" , "COSTI COMUNI LOMBARDIA", "DIREZIONE GENERALE")) %>% 
#   select(Dipartimento, Reparto, Laboratorio, Dirigente,peror) %>%  
# 
#   left_join(
    


# hC_21, by="Dirigente") %>%    
#     mutate(hcontr  = h*(peror/100)) %>% 
#   group_by(Dipartimento, Reparto, Laboratorio, Dirigente, Mese) %>% 
#   summarise(hcontr= sum(hcontr))   

  
  
  
 







  # select(Dipartimento, REPARTO, Laboratorio, Dirigente,  Mese, hcontr) %>%  
  # group_by(Dipartimento, REPARTO, Laboratorio, Dirigente) %>%
  # summarise(hcontr=sum(hcontr, na.rm = TRUE)) %>%
  # pivot_wider(names_from = Dirigente, values_from = hcontr) %>%
  # mutate(htot=N+S) %>%
  # rename("Comparto" = N, "Dirigenza" = S, "Reparto"= REPARTO) %>%
  # mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%
  # filter(!Dipartimento == "DIPARTIMENTO AMMINISTRATIVO") %>%
  # ungroup() %>%
  # select(Laboratorio, Comparto, Dirigenza)










# ore erogate nel 2021


  

  



# oreW %>% 
#   left_join(
#     oreC, by=c("Laboratorio")
#   ) %>% 
#   mutate(eccC = Comparto.x-Comparto.y, 
#          eccD = Dirigenza.x-Dirigenza.y) %>% 
#   filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>% 
#   ggplot()+
#   aes(x=Mese, y = eccC)+
#   geom_point()+
#   geom_line()+
#   geom_point(aes(x=Mese, y = eccD), col="blue")+
#   geom_line(aes(x=Mese, y = eccD), col="blue")+
#   facet_wrap(Laboratorio~., scales = "free")
# 
# 
# 
# 
# 
# 
# 
# 
#   
# 
# 
# ore <- readRDS(here("data", "processed", "ore.rds"))
# names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")
# 
# oreW <- ore %>% 
#    
#   filter( !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
#                                "Dipartimento amministrativo", "Direzione Amministrativa", 
#                                "Direzione Generale") &
#             #!Reparto %in% c("GESTIONE CENTRALIZZATA DELLE RICHIESTE")&
#             !is.na(Dirigente) & 
#             !is.na(Ore)) %>% 
#   filter( !str_detect(Laboratorio, "Costi")) %>%  
#   mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
#          Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%   
#   group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente, Mese) %>% 
#   summarise(hworked = sum(Ore, na.rm = T)) %>%  
#   filter(hworked != 0) %>%  
#   pivot_wider(names_from = "Dirigente", values_from = c("hworked"), values_fill = 0) %>%  
#   ungroup() %>% 
#   filter(ANNO == 2021) %>%  
#   select(Dipartimento,Reparto,Laboratorio,Mese,Comparto,Dirigenza) %>% 
#   mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) 
#  
#   
# 
# 
# oreW %>% 
# left_join(
#   oreC, by=c("Laboratorio")
# ) %>% 
#   mutate(eccC = Comparto.x-Comparto.y, 
#          eccD = Dirigenza.x-Dirigenza.y) %>% 
#   filter(Dipartimento == "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA") %>% 
#   ggplot()+
#   aes(x=Mese, y = eccC)+
#   geom_point()+
#   geom_line()+
#   geom_point(aes(x=Mese, y = eccD), col="blue")+
#   geom_line(aes(x=Mese, y = eccD), col="blue")+
#   facet_wrap(Laboratorio~., scales = "free")
#   



         
          

  # summarise(mC = mean(Comparto), 
  #           mD = mean(Dirigenza), 
  #           sC = sum(Comparto), 
  #           sD = sum(Dirigenza), 
  #           GL = sum(glav))

















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
