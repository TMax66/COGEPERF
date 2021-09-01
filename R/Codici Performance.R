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

##Analisi 
perf %>% 
  filter(!StrutturaAssegnataria %in% dip & Periodo == 2 ) %>%View()
  group_by(Periodo, Obiettivo, Indicatore,  StrutturaAssegnataria) %>% 
  summarise(MediaAvanzamento = mean(Avanzamento, na.rm=T)) %>% View()
