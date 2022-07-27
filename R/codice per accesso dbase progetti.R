library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)
library(openxlsx)


con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it",
                      Database = "ProgettiAccordi")
source(here("R", "sqlprj.R"))
prj <- con %>% tbl(sql(queryPA)) %>% as_tibble() 
saveRDS(prj, here("data", "processed", "prjxx.RDS"))

prjuo <- con %>% tbl(sql(queryUO)) %>% as_tibble() 
saveRDS(prj, here("data", "processed", "prjuo.RDS"))

prjRicercatori <- con %>% tbl(sql(queryRic)) %>% as_tibble() 

matricole <- read_excel(here("data", "raw", "MatricoleSigmaGRU.xlsx")) 

matricole <- prjRicercatori %>% 
  left_join(matricole %>% 
              select(MatricolaSigma, Matricola, Nome, Cognome) , by = c("Matricola" = "MatricolaSigma"))
saveRDS(matricole, here("data", "processed", "matricole.RDS"))



prjx <-  prj %>% 
  left_join( prjRicercatori, by=c("MatrRespScientifico" = "Matricola")) %>% 
  select(Codice, CodIDIzsler, DataInizio, DataFine, Descrizione, Tipologia, MatrRespScientifico, 
         Cognome, Nome, FinCompApprovato) %>%  View()
  
  
  
  left_join(
    prjuo,  by = c("Codice" = "CodProgetto")) %>% 
  left_join(prjRicercatori, by=c("MatrRespScientifico" = "Matricola", "MatricolaRespScientUO" = "Matricola")) %>%  View()
  select(Codice, CodIDIzsler,  DataInizio,  DataFine, Descrizione,Tipologia,MatrRespScientifico,   RespScientifico,  DescrUO, MatricolaRespScientUO, QuotaPersContratto )  
  