library(DBI)
library(odbc)
library(tidyverse)
library(dbplyr)
library(writexl)
library(here)



con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)
DBI::dbListTables(con)### elenca tutte le tabelle del database
#con %>% tbl("nome tabella") ## estrae il contentuo di una tabella del db


####tabella ore lavorate###------------------------------------------------------
query <- "SELECT
  dbo.IZS_Livello0.Livello0,
  dbo.IZS_Dipartimenti.DIPARTIMENTO,
  dbo.IZS_Reparti.REPARTO,
  dbo.IZS_CDC.CENTRO_DI_COSTO,
  dbo.Personale_V2020.Anno,
  dbo.Personale_V2020.Matricola,
  dbo.Personale_V2020.Ore,
  dbo.Personale_V2020.SmartWorking,
  dbo.Personale_V2020.Dirigente,
  dbo.Personale_V2020.Contratto,
  dbo.Personale_V2020.Percentuale,
  dbo.Personale_V2020.Mese,
  dbo.Personale_V2020.FineRapporto
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)
  
WHERE
  dbo.Personale_V2020.Anno  >=  2019
"
con %>% tbl(sql(query)) %>% as_tibble() %>% 
saveRDS(., file = here("COGES", "data", "processed",  "orelavorate.rds"))


## tabella conversione strutture
# replabdip <- read.csv("C:/Users/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/replabdip.txt", sep="")
# 
# write_xlsx(replabdip, path = "tabella.xlsx")





