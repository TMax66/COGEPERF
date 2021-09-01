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

query <- "SELECT  Avanzamento,  Valore, Anno,TipoObiettivo, Periodo, MacroArea,Obiettivo, Azione,Indicatore, 
StrutturaAssegnataria

FROM ObiettiviStrategiciV2018.dbo.v_EstrazioneObiettivi
WHERE Anno > 2020"

perf <- con %>% tbl(sql(query)) %>% as_tibble() 

 

