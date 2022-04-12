library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)

#CONNESSIONI AI DATABASE-------
#### dati   ore lavorate dal personale izsler----
conOre <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)
### dati accettazioni effettuate dalla gestione centralizzata----
conAcc <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it", 
                         Database = "IZSLER", Port = 1433)

### dati da dbase performance berenice per il 2021 -----
conPerf <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                      Database = "ObiettiviStrategiciV2018", Port = 1433)

### dati da dbase performance berenice per il 2022 ----
conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                        Database = "ObiettiviStrategici2022", Port = 1433)

source(here("R","sql.R"))


#PREPARAZIONE DATI PER DASHBOARD PERFORMANCES----

cc <- conOre %>% tbl(sql(queryCoge)) %>% as_tibble() 

# cc <-  cc %>% 
#        #filter(!idClassificazione %in% c("-5", "-13","-11", "-6"))
       # filter(!idClassificazione %in% c("-5"))# escludo i controlli interni

###TABELLE-----
T1 <- cc %>% #tabella con prestazioni (tariffato, fatturato) e costi
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE), 
            Tariffato = sum(Tariffario, na.rm=TRUE), 
            Fatturato = sum (Fatturato, na.rm = TRUE), 
            costi = sum(Costo, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotPrestazioni = sum(Prestazioni), 
            TotCost = sum(costi), 
            TotTariff = sum(Tariffato)) 

T2 <- cc %>% filter(Classe== "Vendite prodotti") %>% ###vendita prodotti
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  mutate(Fatturato = ifelse(Fatturato == 0, Tariffario, Fatturato )) %>% 
  summarise(NVP = sum(Numero, na.rm = TRUE), 
            FattVP = sum(Fatturato, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNVP = sum(NVP), 
            TotFattVP = sum(FattVP))


T3 <- cc %>% filter(Classe== "Ricavi da produzione") %>% ###attività interna
  group_by(ANNO, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(NumAI = sum(Numero, na.rm = TRUE), 
            TarAI = sum(Tariffario, na.rm = TRUE)) %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNAI = sum(NumAI), 
            TAI = sum(TarAI)) 


ore <- conOre %>% tbl(sql(queryOre)) %>% as_tibble()  ### FTEq
names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")
fte <- ore %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>% 
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*47.4), hworked/(38*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>%  
  select(-hworked_, -FTE_)  

##TABELLA GENERALE----
T1 %>% ##attività costi e fte
  left_join(T2, by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>%  
  left_join(T3, by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  left_join(fte,by=c("ANNO", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
  saveRDS(., file = here("data", "processed",  "TabellaGenerale.rds"))



## TABELLA GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA----
acc <- conAcc%>% tbl(sql(queryAcc)) %>% as_tibble() 

accV <- acc %>% 
  mutate(tipoprove = ifelse(Prova=="Prova Chimica", "Prova Chimica", 
                            ifelse(Prova== "Prova Sierologica", "Prova Sierologica", 
                                   ifelse(Prova == "Parere Tecnico", "Parere Tecnico", "Prova Diagnostica/Alimenti")))) %>%
  mutate(Valorizzazione = ifelse(tipoprove == "Prova Chimica", 3.70, 
                                 ifelse(tipoprove == "Prova Sierologica", 0.20,
                                        ifelse(tipoprove == "Prova Diagnostica/Alimenti", 0.72, 0))))%>% 
  group_by(Nconf) %>% 
  mutate(Valore = sum(Valorizzazione) ) %>% 
  select(-Valorizzazione, -Finalita) %>% 
  distinct(Nconf, .keep_all = TRUE) %>% 
  mutate(Valore =  0.07*(Valore)+Valore ) %>% 
  group_by(dtreg, PC) %>% 
  summarise(n.conf = n(), 
            Valore = sum(Valore),
            ncamp = sum(NrCampioni, na.rm = TRUE)) %>% 
  mutate(Anno = year(dtreg)) %>%  
  group_by(Anno) %>% 
  summarise(n.conf = sum(n.conf), 
            Valore = sum(Valore)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE")  %>%  
saveRDS(here("data", "processed", "GCR.rds"))




##DATI DA PROGETTI DI RICERCA----

prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2021.xlsx"))




anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018)%>%
  mutate(Nome = gsub("\\s.*$", "", Nome) )
 # distinct(Matricola, .keep_all = TRUE)

prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine),
         Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%   
  
  saveRDS(., file = here( "data", "processed",  "prj.rds"))



##DATI DA PUBBLICAZIONI####

# pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
# pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
# pubblicazioni$AU <- gsub(",.*$", "", pubblicazioni$AU)
# pubblicazioni %>% filter(OA >= 2019) %>%
#   mutate(Cognome = recode(AU,
#                           "COSCIANI_CUNICO" = "COSCIANI CUNICO",
#   )) %>%
#   left_join(anag, by = c("Cognome" = "Cognome")) %>%
#   filter(Dirigente == "S") %>%  
#   mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
  

pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
pubblicazioni$AU <- str_remove(pubblicazioni$AU, " ")
pubblicazioni$AU <- gsub("_", " ", pubblicazioni$AU)
pubblicazioni$Nome <- str_extract( pubblicazioni$AU, ",.*$")
pubblicazioni$Nome <- str_remove(pubblicazioni$Nome, ",")
pubblicazioni$Nome <- gsub("\\s.*$", "", pubblicazioni$Nome)
pubblicazioni$Cognome <- gsub(",.*$", "", pubblicazioni$AU)

pubblicazioni %>% filter(OA >= 2019) %>%
  left_join(anag, by = c("Cognome" = "Cognome", "Nome" = "Nome", "OA" = "ANNO")) %>%  
  # filter(Dirigente == "S") %>%  
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
saveRDS(., file = here( "data", "processed",  "pub.rds"))

##DATI DA DBASE PERFORMANCE (OBIETTIVI, INDICATORI, TARGET, RISULTATO, FTEQ PROGRAMMATI)-----
source(here("R", "codici performance 2021.R"))## dati delle performance 2021

### dati performance 2022----




### DATI FTEQ programmati ----

## 2021
source( here("R", "codici per fte programmati 2021.R"))

##DATI FTEQ programmati 2022--- #

ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% 
mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
       Valorizzato = ifelse(Valorizzato != "no", "si", "no"))




#PREPARAZIONE DATI PER APPLICATIVO COSTI-RICAVI----

cc %>% 
  mutate(ClassAnalisi = recode(idClassificazione, 
                               `-1` = "Ufficiale a Pagamento", 
                               `-3` = "Ufficiale a Pagamento", 
                               `-8` = "Non Ufficiale a Pagamento", 
                               `-9` = "Non Ufficiale a Pagamento", 
                               `-4` = "Ufficiale Gratuito", 
                               `-5` = "Ufficiale Gratuito", 
                               `-7` = "Ufficiale Gratuito", 
                               `-11` = "Ufficiale Gratuito", 
                               `-6`  = "Non Ufficiale Gratuito", 
                               `-10` = "Non Ufficiale Gratuito", 
                               `-13` = "Non Ufficiale Gratuito" ,  
                              .default = NA_character_),
         Pagamento = recode(idClassificazione, 
                            `-1` = "Pagamento", 
                            `-3` = "Pagamento", 
                            `-8` = "Pagamento", 
                            `-9` = "Pagamento", 
                            `-4` = "Gratuito", 
                            `-5` = "Gratuito", 
                            `-7` = "Gratuito", 
                            `-11` = "Gratuito", 
                             `-6`  = "Gratuito", 
                            `-10` = "Gratuito", 
                             `-13` = "Gratuito" ,  
                           .default = NA_character_), 
         Uff = recode (idClassificazione, 
                       `-1` = "Ufficiale", 
                       `-3` = "Ufficiale", 
                       `-8` = "Non Ufficiale", 
                       `-9` = "Non Ufficiale", 
                       `-4` = "Ufficiale", 
                       `-5` = "Ufficiale", 
                       `-7` = "Ufficiale", 
                        `-11` = "Ufficiale", 
                        `-6`  = "Non Ufficiale", 
                       `-10` = "Non Ufficiale", 
                        `-13` = "Non Ufficiale", 
                      .default = NA_character_), 
         Quarter = factor(paste("Q",TRIMESTRE)),
         TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato,
                       ifelse(ClassAnalisi == "Ufficiale Gratuito",  Tariffario, 0)),
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato,
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", Tariffario, 0)),
         TGratuito = ifelse(Pagamento == "Gratuito", Tariffario,0), 
         TPagamento = ifelse(Pagamento == "Pagamento", Fatturato,0), 
         TVP = ifelse(Classe == "Vendite prodotti", Fatturato, 0), 
         TAI = ifelse(Classe == "Ricavi da produzione interna", Tariffario, 0), 
         AttUff = ifelse(Uff== "Ufficiale", Determinazioni, 0 ), 
         AttNUff = ifelse(Uff== "Non Ufficiale", Determinazioni, 0 ), 
         AttGrat = ifelse(Pagamento== "Gratuito", Determinazioni, 0 ), 
         AttPag = ifelse(Pagamento == "Pagamento", Determinazioni, 0), 
         VP = ifelse(Classe == "Vendite prodotti", Numero, 0), 
         AI = ifelse(Classe == "Ricavi da produzione interna", Numero, 0)) %>% 
  mutate(CDC = ifelse(CodiceCDC == 5502, "LABORATORIO CONTAMINANTI AMBIENTALI-(Bologna)", CDC)) %>%  
  saveRDS(here("data", "processed", "CC.rds"))


