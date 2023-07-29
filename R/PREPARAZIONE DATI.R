

pkg() # <- funzone in .Rprofile
library(DBI)
library(odbc)

#CONNESSIONI AI DATABASE-------


conOre <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02",
                         Database = "DW_COGE", Port = 1433)


# dati accettazioni effettuate dalla gestione centralizzata----
conAcc <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                         Database = "DarwinSqlSE", Port = 1433)



# ### dati da dbase performance berenice per il 2021 -----
# conPerf <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
#                       Database = "ObiettiviStrategiciV2018", Port = 1433)
# 
# ### dati da dbase performance berenice per il 2022 ----
# conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
#                         Database = "ObiettiviStrategici2022", Port = 1433)
# 


source(here("R","sql.R"))



#PREPARAZIONE DATI PER DASHBOARD PERFORMANCES----

cc <- conOre %>% tbl(sql(queryCoge)) %>% as_tibble()

# cc <-  cc %>% 
#        #filter(!idClassificazione %in% c("-5", "-13","-11", "-6"))
       # filter(!idClassificazione %in% c("-5"))# escludo i controlli interni

###TABELLE-----

T1 <- cc %>% #tabella con prestazioni (tariffato, fatturato) e costi
  group_by(ANNO, MESE,Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE), 
            Tariffato = sum(Tariffario, na.rm=TRUE), 
            Fatturato = sum (Fatturato, na.rm = TRUE), 
            costi = sum(Costo, na.rm = TRUE)) %>% 
  group_by(ANNO, MESE,  Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotPrestazioni = sum(Prestazioni), 
            TotCost = sum(costi), 
            TotTariff = sum(Tariffato)) 

T2 <- cc %>% filter(Classe == "Vendite prodotti") %>% ###vendita prodotti
  group_by(ANNO,MESE,  Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  mutate(Fatturato = ifelse(Fatturato == 0, Tariffario, Fatturato )) %>% 
  summarise(
            FattVP = sum(Fatturato, na.rm = TRUE)) %>% 
  group_by(ANNO, MESE, Dipartimento, Reparto, Laboratorio) %>% 
  summarise( TotFattVP = sum(FattVP))


T3 <- cc %>% filter(Classe== "Ricavi da produzione") %>% ###attività interna
  group_by(ANNO, MESE,Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise( 
            TarAI = sum(Tariffario, na.rm = TRUE)) %>% 
  group_by(ANNO, MESE, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(
            TAI = sum(TarAI)) 


ore <- conOre %>% tbl(sql(queryOre)) %>% as_tibble()  ### FTEq
#ore <- readRDS("ore.rds")

names(ore)[1:6] <- c("Dipartimento", "Reparto", "Laboratorio", "CDC", "CodiceCC", "ANNO")



ore <- ore %>% 
  filter(Ruolo != "RICERCA SANITARIA E SUPPORTO RIC. SAN.")#escludo i piramidati
  

saveRDS(ore, here("data", "processed",  "ore.RDS"))



mesi <- seq(1:12)
ftec <- 142.2 ## 142.2 DERIVA DA (36*47.4)/12  (138 se 36*46)
fted <- 150.1 ## 150.1 DERIVA DA (38*47.4)/12  (145 se 38*46)

ftec23 <- 138
fted23 <- 145



# FTE mensili cumulati (progressivi)
fteC <- mesi*ftec 
fteD <- mesi*fted
fteC23 <- mesi*ftec23
fteD23 <- mesi*fted23

ftedf <- data.frame(mesi, fteC, fteD, fteC23, fteD23)

fte <-  ore %>% 
  filter( !Dipartimento %in% c("Non applicabile", "Costi Comuni e Centri contabili", 
                                "Dipartimento amministrativo", "Direzione Amministrativa", 
                                "Direzione Generale") &
           #!Reparto %in% c("GESTIONE CENTRALIZZATA DELLE RICHIESTE")&
           !is.na(Dirigente) & 
           !is.na(Ore)) %>% 
  filter( !str_detect(Laboratorio, "Costi")) %>%  
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza"),
         Ore = ifelse(Ore == SmartWorking, Ore, Ore+SmartWorking)) %>%   
  group_by(ANNO, Dipartimento, Reparto, Laboratorio, Dirigente, Mese) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>%  
  filter(hworked != 0) %>% 
  # mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(36*4.34), hworked/(38*4.34))) %>%
  pivot_wider(names_from = "Dirigente", values_from = c("hworked"), values_fill = 0) %>% 
  left_join(
    ftedf, by=c("Mese" = "mesi")
  ) %>% 
  mutate(hwcomp = cumsum(Comparto), 
         hwdir = cumsum(Dirigenza),
         FTEC = ifelse(ANNO < 2023, hwcomp/fteC, hwcomp/fteC23), #rispetto al precedente codice nel 2023 tengo conto del corretto calcolo delle settimane lavorative che è 46 e non 47.4
         FTED = ifelse(ANNO <2023, hwdir/fteD, hwdir/fteD23), 
         #FTEC =  hwcomp/fteC,
         #FTED = hwdir/fteD, 
         FTET = FTEC+FTED
         ) %>% ungroup() %>% 
  rename(MESE = Mese)

##TABELLA GENERALE----
T1 %>%  ##attività costi e fte 
  left_join(T2, by=c("ANNO", "MESE", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  left_join(T3, by=c("ANNO","MESE", "Dipartimento", "Reparto", "Laboratorio")) %>%   
  left_join(fte,by=c("ANNO", "MESE",  "Dipartimento", "Reparto", "Laboratorio")) %>%  
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE),
         TotFattVP = ifelse(is.na(TotFattVP), 0, TotFattVP), 
         TAI = ifelse(is.na(TAI), 0, TAI), 
         TotTariff = TotTariff - (TotFattVP + TAI), 
         Rep2 = recode(Reparto,  "REPARTO PRODUZIONE PRIMARIA" = "RPP", # questa ricodifica è molto lenta prima si trovava nel codice server 
                       #per costurire il dataset da cui originava il trenplot dei dipartimenti.... ora l'ho spostata qui in modo che
                       #quando gira l'app shiny non rallenta molto
                          "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM",
                          "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB",
                          "REPARTO CONTROLLO ALIMENTI" = "RCA",
                          "REPARTO VIROLOGIA" = "RVIR",
                          "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB",
                          "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB",
                          "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA",
                          "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN",
                          "SEDE TERRITORIALE DI BRESCIA" = "BS",
                          "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO",
                          "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI",
                          "SEDE TERRITORIALE DI PAVIA" = "PV",
                          "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE",
                          "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA",
                          "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR",
                          "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE",
                          "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "GCR",
                          "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "AREG",
                          "FORMAZIONE E BIBLIOTECA" = "FORMAZIONE",
                          "SORVEGLIANZA EPIDEMIOLOGICA" = "SORVEPID" )) %>% 

saveRDS(.,here("data", "processed", "TabellaGenerale.rds"))


###ATTENZIONE CANCELLA QUESTO CODICE (sotto) DOPO IL PROSSIMO AGGIORNAMENTO A OTTOBRE!!!!!!

# Tabella <- readRDS(here("data", "processed", "TabellaGenerale.rds"))
# 
# 
# Tabella %>% 
#   mutate(Rep2 = recode(Reparto,  "REPARTO PRODUZIONE PRIMARIA" = "RPP",
#                 "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM",
#                 "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB",
#                 "REPARTO CONTROLLO ALIMENTI" = "RCA",
#                 "REPARTO VIROLOGIA" = "RVIR",
#                 "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB",
#                 "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB",
#                 "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA",
#                 "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN",
#                 "SEDE TERRITORIALE DI BRESCIA" = "BS",
#                 "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO",
#                 "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI",
#                 "SEDE TERRITORIALE DI PAVIA" = "PV",
#                 "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE",
#                 "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA",
#                 "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR",
#                 "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE",
#                 "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "GCR",
#                 "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "AREG",
#                 "FORMAZIONE E BIBLIOTECA" = "FORMAZIONE",
#                 "SORVEGLIANZA EPIDEMIOLOGICA" = "SORVEPID" )) %>% 
#   saveRDS(.,here("data", "processed", "TabellaGenerale.rds"))

  

## TABELLA GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA----


source(here("R",  "sqlCGR.R"))

acc <- conAcc%>% tbl(sql(queryAcc)) %>% as_tibble() 


acc[,"FinalitaConferimento"] <- sapply(acc[, "FinalitaConferimento"], iconv, from = "latin1", to = "UTF-8", sub = "")
acc[,"strutturaaccettante"] <- sapply(acc[, "strutturaaccettante"], iconv, from = "latin1", to = "UTF-8", sub = "")
acc[,"comune"] <- sapply(acc[, "comune"], iconv, from = "latin1", to = "UTF-8", sub = "")



acc <- acc %>%
  
  
  mutate(tipoprove = ifelse(gProva=="Prova Chimica", "Prova Chimica",
                            ifelse(gProva== "Prova Sierologica", "Prova Sierologica",
                                   ifelse(gProva == "Parere Tecnico", "Parere Tecnico", "Prova Diagnostica/Alimenti")))) %>%
  mutate(Valorizzazione = ifelse(tipoprove == "Prova Chimica", 3.70,
                                 ifelse(tipoprove == "Prova Sierologica", 0.20,
                                        ifelse(tipoprove == "Prova Diagnostica/Alimenti", 0.72, 0))))%>%   
  
  group_by(Anno, Conferimento) %>% 
  mutate(Valore = sum(Valorizzazione) ) %>%  
  select(-Valorizzazione) %>% 
  
  mutate(Valore =  0.07*(Valore)+Valore ) %>%  
  distinct(Conferimento, .keep_all = TRUE) %>% 
  mutate(Anno = year(Data_Registrazione),
         MESE = month(Data_Registrazione)) %>%
  group_by(Anno, MESE) %>% 
  summarise(n.conf = n(), 
            Valore = sum(Valore, na.rm = TRUE)) %>% 
  tibble(Dipartimento = "Direzione sanitaria", Reparto = "GESTIONE CENTRALIZZATA DELLE RICHIESTE",
         Laboratorio = "	GESTIONE CENTRALIZZATA DELLE RICHIESTE")   

     
accatt <- conAcc%>% tbl(sql(queryAtt)) %>% as_tibble()

accatt<-accatt %>% 
  mutate(valore = ifelse(TipoPrestazione == "Gestione materiale sanitario conferito da corrieri esterni", 3.23, 
                         ifelse(TipoPrestazione == "Gestione smistamento campioni in Sede" , 129.60, 0)),
         Anno = year(Data_Attività), 
         year = as.numeric(as.character(Anno)),
         MESE = month(Data_Attività), 
         month = as.numeric(as.character(MESE)))  %>% 
  group_by(Anno, MESE) %>% 
  summarise(TotAtt = sum(quantita), 
            Valorizzazione = sum(valore*quantita)) 
    

acc %>% 
  left_join(accatt, by= c("Anno","MESE")) %>% 
  mutate(TotAtt = ifelse(is.na(TotAtt), 0, TotAtt), 
         Valorizzazione = ifelse(is.na(Valorizzazione), 0, Valorizzazione), 
         n.conf = n.conf+TotAtt,
         Valore = Valore+Valorizzazione) %>%
  select(-TotAtt, -Valorizzazione) %>% 
  saveRDS(here("data", "processed", "GCR.rds"))

##DATI DA PROGETTI DI RICERCA----


prj <- readRDS(here("data", "processed", "prj2x.RDS"))##<- deriva dal codice del file codice per accesso dbase progetti.R

ore <- readRDS(here("data", "processed", "ore.RDS"))
anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018)%>%
  mutate(Nome = gsub("\\s.*$", "", Nome) ) %>% 
  distinct(ANNO,Matricola, .keep_all = TRUE) %>% 
  saveRDS(here("data", "processed", "anag.RDS"))

 


prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine),
         Dipartimento = casefold(Dipartimento, upper = TRUE)) %>%  
  saveRDS(., here("data", "processed", "prj.rds"))



##DATI DA PUBBLICAZIONI####

 
anag <- readRDS(here("data", "processed", "anag.RDS"))

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
  mutate(Dipartimento = casefold(Dipartimento, upper = TRUE)) %>% 
saveRDS(., file = here("data", "processed",   "pub.rds"))




### DATI FTEQ programmati ----

## 2021 e 2022
#questa stringa esegue le istruzioni che ci sono nel file "codici per fte programmati.R" e 
#salva nella cartella /data/processed i file che servono i calcoli degli FTE programmati da cui si ottiene il RFTEprog
#source( here( "R","codici per fte programmati.R"))

# ATTENZIONE I DATI VANNO GENERATI SOLO ALL'INIZIO DELL'ANNO DOPO CHE SONO STATE PRODOTTE LE SCHEDE BUDGET CON
#L'ALLOCAZIONE DEGLI FTE ALLE STRUTTURE E AGLI OBIETTIVI

# ##DATI FTEQ programmati 2022--- #
# 
# ftep22 <- tbl(conSB, sql(query)) %>% as_tibble() %>% 
# mutate(Pesatura = ifelse(Pesatura != "no", "si", "no"), 
#        Valorizzato = ifelse(Valorizzato != "no", "si", "no"))

## QUINDI IN FASE DI AGGIORNAMENTO DEL DASHBOARD NON E' NECESSARIO ESEGUIRE NESSUN CODICE PERCHE' I FILE SONO GIA' PRESENTI NELLA
## CARTELLA DATI DOVE ANDRA'ì A LEGGERE IL CODICE GLOBAL DELL'APPLICATIVO SHINY




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
         TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Tariffario,
                       ifelse(ClassAnalisi == "Ufficiale Gratuito",  Tariffario, 0)),
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Tariffario,
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", Tariffario, 0)),
         TGratuito = ifelse(Pagamento == "Gratuito", Tariffario,0),
         TPagamento = ifelse(Pagamento == "Pagamento", Fatturato,0),
         TVP = ifelse(Classe == "Vendite prodotti", Tariffario, 0), 
         TAI = ifelse(Classe == "Ricavi da produzione", Tariffario, 0), 
         AttUff = ifelse(Uff== "Ufficiale", Determinazioni, 0 ), 
         AttNUff = ifelse(Uff== "Non Ufficiale", Determinazioni, 0 ), 
         AttGrat = ifelse(Pagamento== "Gratuito", Determinazioni, 0 ), 
         AttPag = ifelse(Pagamento == "Pagamento", Determinazioni, 0), 
         AltriProv = ifelse(Classe == "Altri proventi", Tariffario, 0)) %>% 
        
  mutate(CDC = ifelse(CodiceCDC == 5502, "LABORATORIO CONTAMINANTI AMBIENTALI-(Bologna)", 
                      ifelse(CodiceCDC == 5311, "PIACENZA-LABORATORIO LATTE", 
                      ifelse(CodiceCDC == 5312, "SEDE TERRITORIALE DI PIACENZA", CDC)))) %>%  
  saveRDS( here("data", "processed", "CC.rds"))


