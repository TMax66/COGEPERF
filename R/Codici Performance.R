library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#DATI PERFORMANCE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2", 
                      Database = "ObiettiviStrategiciV2018", Port = 1433)

query <- "SELECT  
Avanzamento,  
Valore, 
Anno,
TipoObiettivo, 
Periodo, 
MacroArea,Obiettivo, 
Azione,
Indicatore, 
StrutturaAssegnataria

FROM ObiettiviStrategiciV2018.dbo.v_EstrazioneObiettivi
WHERE Anno > 2020"

perf <- con %>% tbl(sql(query)) %>% as_tibble() 

##Dati strutture

strutture <- read_xlsx("strutture.xlsx")

dip <- c("DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", "DIPARTIMENTO SICUREZZA ALIMENTARE", 
               "DIPARTIMENTO TUTELA SALUTE ANIMALE", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA", 
               "DIPARTIMENTO AMMINISTRATIVO", "CONTROLLO DI GESTIONE")


##ricordificare le strutture

perf <- perf %>% 
  filter(!StrutturaAssegnataria %in% dip ) %>% 
  mutate(Struttura = recode(StrutturaAssegnataria, 
                            "S.T. PIACENZA E PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                            "REP. CHIM. DEGLI ALIMENTI E MANGIMI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                            "REP. CHIMICO ALIMENTI BOLOGNA" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                            "REP. PRODUZIONE PRIMARIA" = "REPARTO PRODUZIONE PRIMARIA", 
                            "S.T. BOLOGNA, FERRARA E MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                            "S.T. REGGIO EMILIA" = "SEDE TERRITORIALE DI REGGIO EMILIA", 
                            "REP. VIROLOGIA" = "	REPARTO VIROLOGIA", 
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
                            "FORMAZIONE E BIBLIOTECA" = "FORMAZIONE E BIBLIOTECA"))
  



##Analisi 
perf %>% 
 filter(Periodo == 2 ) %>%View()
  group_by(Periodo, Obiettivo, Indicatore,  StrutturaAssegnataria) %>% 
  summarise(MediaAvanzamento = mean(Avanzamento, na.rm=T)) %>% View()
