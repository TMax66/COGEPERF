library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#DATI ORE LAVORATE DA DBASE PERSONALE_COGE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)
query <- "SELECT
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Personale_V2020.CDC,
  dbo.Personale_V2020.Anno,
  dbo.Personale_V2020.Matricola,
  dbo.Personale_V2020.Ore,
  dbo.Personale_V2020.SmartWorking,
  dbo.Personale_V2020.Dirigente,
  dbo.Personale_V2020.Contratto,
  dbo.Personale_V2020.Percentuale,
  dbo.Personale_V2020.Mese,
  dbo.Personale_V2020.FineRapporto,
  dbo.Personale_V2020.Nome,
  dbo.Personale_V2020.Cognome
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)
  
WHERE
  dbo.Personale_V2020.Anno  >=  2019"
ore <- con %>% tbl(sql(query)) %>% as_tibble()  


#DATI DA COGE----
cc <- read_delim(here("data", "raw", "coge1921.txt"), 
                 "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                              grouping_mark = "."), trim_ws = TRUE)

names(cc)[5:9] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")

#PREPARAZIONE TABELLE-----
T1 <- cc %>% #tabella con prestazioni (tariffato, fatturato) e costi
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE), 
            Tariffato = sum(`A Tariffario`, na.rm=TRUE), 
            Fatturato = sum (Fatturato, na.rm = TRUE), 
            Costi = sum(Costo, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotPrestazioni = sum(Prestazioni), 
            TotCost = sum(Costi), 
            TotTariff = sum(Tariffato)) 

T2 <- cc %>% filter(Classe== "Vendite prodotti") %>% ###vendita prodotti
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  mutate(Fatturato = ifelse(Fatturato == 0,`A Tariffario`, Fatturato )) %>% 
  summarise(NVP = sum(Numero, na.rm = TRUE), 
            FattVP = sum(Fatturato, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNVP = sum(NVP), 
            TotFattVP = sum(FattVP))


T3 <- cc %>% filter(Classe== "Ricavi da produzione") %>% ###attività interna
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(NumAI = sum(Numero, na.rm = TRUE), 
            TarAI = sum(`A Tariffario`, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNAI = sum(NumAI), 
            TAI = sum(TarAI)) 

ore <- con %>% tbl(sql(query)) %>% as_tibble()  ### FTEq
names(ore)[1:5] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")
fte <- ore %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
  select(-hworked_, -FTE_)  

#TABELLA GENERALE----
T1 %>% ##attività costi e fte
  left_join(T2, by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>%  
  left_join(T3, by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  left_join(fte,by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  saveRDS(., file = here("data", "processed",  "TabellaGenerale.rds"))



##Dati da Gestione centralizzata----

acc <- read_delim("C:/Users/vito.tranquillo/Desktop/Git Projects/COGEPERF/data/raw/postazioni.txt", 
                  "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

names(acc) <- c("nconf", "strpropr", "settore", "finalità", "pagamento", 
                "dtprel", "dtreg", "dtacc", "dtrdp", "IstRDP",  "pc", "gruppoprova")



#preparazione dati-----
acc %>% filter(gruppoprova!= "Parere Tecnico") %>% 
  mutate(tipoprove = ifelse(gruppoprova=="Prova Chimica", "Prova Chimica", 
                            ifelse(gruppoprova== "Prova Sierologica", "Prova Sierologica", "Prova Diagnostica/Alimenti"))) %>%  
  select(-gruppoprova) %>% 
  group_by(dtreg, nconf, pc, settore ) %>% 
  pivot_wider(names_from = "tipoprove", values_from = "tipoprove") %>% 
  mutate(`Prova Chimica` = ifelse(`Prova Chimica`!= "NULL", 2.46, 0), 
         `Prova Diagnostica/Alimenti` = ifelse(`Prova Diagnostica/Alimenti` != "NULL", 0.72, 0),
         `Prova Sierologica` = ifelse(`Prova Sierologica` != "NULL", 0.20, 0)) %>%  
  rowwise() %>% 
  mutate(valore= sum(`Prova Chimica` ,`Prova Diagnostica/Alimenti`, `Prova Sierologica`), 
         valore = 0.07*(valore)+valore) %>%
  ungroup() %>%  
  group_by(dtreg, pc) %>% 
  summarise(n.conf = n(), 
            valore = sum(valore)) %>% 
  mutate(Anno = year(dtreg)) %>%  
  group_by(Anno) %>% 
  summarise(n.conf = sum(n.conf), 
            valore = sum(valore)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE") %>% 
  saveRDS(here("data", "processed", "GCR.rds"))




#DATI DA PROGETTI DI RICERCA----

prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2020.xlsx"))

anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018) %>% 
  distinct(Matricola, .keep_all = TRUE)

prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine)) %>%  
  saveRDS(., file = here( "data", "processed",  "prj.rds"))



#DATI DA PUBBLICAZIONI####

pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
pubblicazioni$AU <- gsub(",.*$", "", pubblicazioni$AU)
pubblicazioni %>% filter(OA >= 2019) %>%
  mutate(Cognome = recode(AU,
                          "COSCIANI_CUNICO" = "COSCIANI CUNICO",
  )) %>%
  left_join(anag, by = c("Cognome" = "Cognome")) %>%
  filter(Dirigente == "S") %>%  
  saveRDS(., file = here( "data", "processed",  "pub.rds"))




#DATI DA DBASE PERFORMANCE (OBIETTIVI, INDICATORI, TARGET, RISULTATO, FTEQ PROGRAMMATI)####


##Programmazione 2021 FTE----
source(here("R",  "FTEPROGRAMMATI.R"))



