library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)

conM <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it", 
                         Database = "AnagrafePersonale", 
                         # UID      = rstudioapi::askForPassword("Database user"),
                         # PWD      = rstudioapi::askForPassword("Database password")
                      
                         UID = "LettoreAnagrafe",
                         PWD = "Dn22052015",
                         Port = 1433)

queryAnag <- "SELECT DISTINCT Matricola, Nome, Cognome, TermineRapporto, DIRIGENTE
              FROM MovimentiAnagrafici
              WHERE (TermineRapporto >= '2015-12-31')"


matricole <- conM %>% tbl(sql(queryAnag)) %>% as_tibble() 
