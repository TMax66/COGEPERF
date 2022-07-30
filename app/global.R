library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")
library("DT")
library("lubridate")
library("fmsb")
library("ECharts2Shiny")
library("formattable")
library("fmsb")
library(openxlsx)
library(patchwork)
library(shinyWidgets)

 

#Carico i dati----
#tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))#-tabella complessiva izsler esami prodotti orelav##

tabIZSLER <- readRDS(file = here("data", "processed",   "TabellaGenerale.rds"))#-tabella complessiva izsler esami prodotti orelav##
GCR <- readRDS(file = here("data", "processed", "GCR.rds"))#-dati da gestione centralizzata della richiesta

#inserisco i dati relativi al numero di conferimenti e valorizzazione della GCR


tabIZSLER <- tabIZSLER %>% 
  left_join(
(GCR %>% 
  select(Reparto, Anno, MESE, n.conf, Valore)), by = c("Reparto", "ANNO" = "Anno", "MESE")) %>% 
  mutate_at(vars(TotPrestazioni),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,n.conf, TotPrestazioni ))) %>%  
            
  mutate_at(vars(TotTariff),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,Valore, TotTariff ))) %>%  
  select(-n.conf, Valore)             

tabIZSLER <- tabIZSLER %>% 
  filter(!Dipartimento %in% c("Non applicabile", 
                              "Costi Comuni e Centri contabili", 
                              "Dipartimento amministrativo", 
                              "Direzione Generale", 
                              "Direzione Amministrativa",
                              "Costi Comuni e Centri contabili"
                               )) %>% 
  filter(!Reparto %in% c("COSTI COMUNI LOMBARDIA", "DIREZIONE SANITARIA", "FORMAZIONE E BIBLIOTECA")) 
  



FTp <- readRDS(here("data", "processed",   "FTp.RDS"))
FTEPD <- readRDS( here("data", "processed",  "FTEPD.RDS"))
FTEPREP <- readRDS(here("data", "processed",  "FTEPREP.RDS"))

FTEPREP <- FTEPREP %>% 
 mutate(Reparto= ifelse(Reparto == "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" , "SEDE TERRITORIALE DI FORLÌ - RAVENNA", Reparto))



prj <- readRDS(file = here("data", "processed",  "prj.rds"))#-tabella progetti di ricerca con strutture


pub <- readRDS(file = here("data", "processed",  "pub.rds"))#-tabella pubblicazioni


#perf <- readRDS( "performance.rds"))




# #Carico funzioni----
ValueBOX <- function(dt, Variabile, Variabile2 = NULL, Titolo, colore, icona){ 
  
  if(is.null(Variabile2)){    
    
    valore <- sum(dt[, Variabile])  
    
  } else {
    valore <- round(sum(dt[, Variabile]/sum(dt[, Variabile2])),2)
  }
  
  valueBox(prettyNum(valore, big.mark = ".", decimal.mark = ","), Titolo, icon = icon(icona, verify_fa = FALSE), color = colore)
}

#TABELLA IZSLER aggregato per dipartimenti con FTE mensile (già cumulato)----

tizsler <-  tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost,   Anno = ANNO) %>%   
  group_by(Dipartimento,Anno, MESE ) %>% 
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = FTED+FTEC,
         Prestazioni = cumsum(Prestazioni), 
         Valorizzazione = cumsum(Valorizzazione), 
         VP = cumsum(VP), 
         AI = cumsum(AI), 
         COSTI = cumsum(COSTI), 
         RT = cumsum(RT)) %>%   
  filter(Prestazioni >0) 



#Tabella pubblicazioni----

pub <- pub %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))  



# #Dati per monitoraggio RFTE homepage----

ricaviCovid21 <- readRDS(here("data","processed", "ricavoCovid.RDS"))



ftp22 <- readRDS(file = here("data", "processed",   "ftep22.rds")) # questo file è prodotto dal codice "codice per fte programmati.R" che si
                                                                   #trova nella cartella preparazione dati
dtProg <- readRDS(here("data","processed", "datiSB.rds"))#

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
  select(Dipartimento, FT21) %>%
  ungroup()

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
         highc = RFTEc + 0.10*RFTEc)


#Dati per monitoraggio RFTE pagine dipartimenti----


# 
# 
