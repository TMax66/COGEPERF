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

### dati da dbase performance berenice -----
conPerf <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                      Database = "ObiettiviStrategiciV2018", Port = 1433)

source(here("R","sql.R"))


#PREPARAZIONE DATI PER DASHBOARD PERFORMANCES----

cc <- conOre %>% tbl(sql(queryCoge)) %>% as_tibble() 

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


##DATI FTEQ programmati----

# i dati originali provengono dal file obiettiviXSB.xlsx che si trova nella cartella
# \data\raw. Questi dati sono rielaborati dal codice FTEPROGRAMMATI.R che si trova 
# nella cartella \R\rcodevari e che restituisce l'output datiFSB.rds che è inviato alla cartella \data\processed

dtProg <- readRDS(here("data", "processed", "datiSB.rds"))




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
  group_by( Dipartimento) %>% 
  # filter(Valorizzazione == "si") %>%  
  mutate(FTp = round(100*prop.table(FT), 1)) %>%   
  select(-FTED, -FTEC) %>%  
  group_by(Dipartimento, Valorizzazione) %>% 
  filter(Valorizzazione== "si") %>%  
  ungroup() %>%
  select(Dipartimento, FTp) %>% 
  saveRDS(here("data", "processed", "ftepDIP.RDS"))









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
  select(Dipartimento,Reparto, Valorizzazione, FTp) %>%  
  saveRDS(here("data", "processed", "ftepREP.RDS"))





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
  saveRDS(here("data", "processed", "ftepREPD.RDS"))



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


prj %>% filter(MatrRS == 0 | is.na(MatrRS)) %>% 
  write.xlsx( file="dati mancanti.xlsx")


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





perf <- conPerf %>% tbl(sql(queryPERF)) %>% as_tibble()

strutture <- read_excel(here("data", "raw", "strutture.xlsx"))


dip <- c("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", "DIPARTIMENTO SICUREZZA ALIMENTARE",
         "DIPARTIMENTO TUTELA SALUTE ANIMALE", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA",
         "DIPARTIMENTO AMMINISTRATIVO", "CONTROLLO DI GESTIONE")


##ricodificare le strutture

dt <- perf %>%
  filter(!StrutturaAssegnataria %in% dip & TipoObiettivo == "Operativo" ) %>%
  mutate(Struttura = recode(StrutturaAssegnataria,
                            "S.T. PIACENZA E PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                            "REP. CHIM. DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                            "REP. CHIMICO ALIMENTI BOLOGNA" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                            "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA",
                            "S.T. BOLOGNA, FERRARA E MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                            "S.T. REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA",
                            "REP. VIROLOGIA" = "REPARTO VIROLOGIA",
                            "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                            "S.T. BERGAMO, SONDRIO E BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                            "S.T. BRESCIA" = "SEDE TERRITORIALE DI BRESCIA",
                            "S.T. CREMONA, MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                            "S.T. FORLI' E RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                            "S.T. LODI E MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                            "S.T. PAVIA" = "SEDE TERRITORIALE DI PAVIA",
                            "U.O. PROVV. ECONOMATO E VENDITE" = "UO PROVVEDITORATO ECONOMATO E VENDITE",
                            "SERVIZIO ASSICURAZIONE QUALITA" = "SERVIZIO ASSICURAZIONE QUALITA'",
                            "U.O. AFFARI GENERALI E LEGALI" = "U.O. AFFARI GENERALI E LEGALI",
                            "U.O. TECNICO PATRIMONIALE" = "UO TECNICO PATRIMONIALE",
                            "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE",
                            "U.O. GESTIONE SERVIZI CONTABILI" = "U.O. GESTIONE SERVIZI CONTABILI",
                            "PROGRAMMAZIONE DEI SERVIZI TECNICI E CONTROLLO DI GESTIONE" = "Programmazione dei servizi tecnici e controllo di gestione",
                            "FORMAZIONE" =  "FORMAZIONE E BIBLIOTECA",
                            "SISTEMI INFORMATIVI" = "Programmazione dei servizi tecnici e controllo di gestione",
                            "SEGRETERIA DIREZIONALE" = "DIREZIONE GENERALE",
                            "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE")

  )

dt %>% rename( Reparto = Struttura ) %>%
  left_join(

    (strutture %>% select(Dipartimento, Reparto) %>%
       unique())


    ,  by = c("Reparto")) %>%  

  saveRDS(here("data", "processed", "performance.RDS"))
 


##Programmazione 2021 FTE--
#source(here("R",  "FTEPROGRAMMATI.R"))


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
                               `-13` = "Non Ufficiale Gratuito" ,  .default = NA_character_),
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
                            `-13` = "Gratuito" ,  .default = NA_character_), 
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
                       `-13` = "Non Ufficiale", .default = NA_character_), 
         
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


