library(tidyverse)
library(readr)
library(tidyverse)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)

#CONNESSIONI AI DATABASE-------


con<- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02",
                         Database = "DW_COGE", Port = 1433)
query <- "SELECT
dbo.CDC_MOVIMENTI_BO.Settore AS Settore,
dbo.IZS_Livello0.Livello0 AS Dipartimento,  
dbo.IZS_Dipartimenti.DIPARTIMENTO As Reparto, 
dbo.IZS_Reparti.REPARTO As Lab,
sum(dbo.CDC_MOVIMENTI_BO.Determinazioni) AS Determinazioni,
sum(dbo.CDC_MOVIMENTI_BO.Nominale) AS Tariffario
FROM
  dbo.IZS_ANNI INNER JOIN dbo.CDC_MOVIMENTI_BO ON (dbo.IZS_ANNI.ANNO=dbo.CDC_MOVIMENTI_BO.ANNO)
   INNER JOIN dbo.IZS_Classificazioni ON (dbo.IZS_Classificazioni.idClassificazione=dbo.CDC_MOVIMENTI_BO.IdClassificazione)
   INNER JOIN dbo.IZS_Riclassificazione ON (dbo.IZS_Riclassificazione.idClassificazione=dbo.IZS_Classificazioni.idRiclassifica)
   INNER JOIN dbo.IZS_CDC ON (dbo.IZS_CDC.CODICE_CDC=dbo.CDC_MOVIMENTI_BO.CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_Reparti.CODICE_REPARTO=dbo.IZS_CDC.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO=dbo.IZS_Reparti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Livello0.CODICE_Livello0=dbo.IZS_Dipartimenti.Codice_Livello0)
  
WHERE
  (
   dbo.IZS_ANNI.ANNO  IN  ( 2022  )
   AND
   dbo.IZS_Riclassificazione.Ufficialita  =  'Ufficiale'
  )
GROUP BY
  dbo.CDC_MOVIMENTI_BO.Settore, 
  dbo.IZS_Livello0.Livello0, 
  dbo.IZS_Dipartimenti.DIPARTIMENTO, 
  dbo.IZS_Reparti.REPARTO"



 


dt <- con %>% tbl(sql(query)) %>% as_tibble()

library(openxlsx)

dt %>% filter(Dipartimento %in% c("Dipartimento area territoriale Lombardia",
                                  "Dipartimento sicurezza alimentare",
                                  "Dipartimento tutela e salute animale"), 
              Settore == "A") %>% 
  group_by(Dipartimento) %>% 
summarise(Analisi = sum(Determinazioni), 
          Valorizzazione = sum(Tariffario)) %>% 
  write.xlsx(file = "B.xlsx")
  