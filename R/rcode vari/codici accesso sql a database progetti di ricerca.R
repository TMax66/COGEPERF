library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it", 
                       Database = "ProgettiAccordi")

query <- "SELECT        
          Codice, 
          CodIDIzsler, 
          Tipologia, 
          Descrizione As Titolo, 
          RespScientifico, 
          MatrRespScientifico, 
          Pubblicazioni, 
          NumeroPubblicazioni, 
          DataScadenzaRelazioneFinale, 
          DataScadenzaRelazioneIntermedia, 
          DataInvioRelazioneFinale, 
          DataInvioRelazioneIntermedia, 
          InviatoSollecitoRelazioneFinale, 
          InviatoSollecitoRelazioneIntermedia, 
          Prolungato, 
          InviatoSollecitoRelazioneScadenza, 
          ProrogatoCoronavirus, 
          RelazioneIntermediaDaInviare, 
          DataInizio, 
          DataFine, 
          Anno, 
          StatoRelazioneIntermedia.Descrizione AS RelIntermedia
FROM            ProgettiAccordi INNER JOIN
                         StatoRelazioneIntermedia ON ProgettiAccordi.Codice = StatoRelazioneIntermedia.Codice
          
FROM      ProgettiAccordi"


Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q<-Query(tabella = "'ProgettiAccordi'")

myfun <- function(con, q, tabella)
{   
  
  column.types <- dbGetQuery(con, q)
  
  ct <- column.types %>%
    mutate(cml = case_when(
      is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
      CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
      TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
    )
    ) %>%
    arrange(cml) %>%
    pull(COLUMN_NAME)
  fields <- paste(ct, collapse=", ")
  query <- paste("SELECT", fields, paste("FROM", tabella))
  return(query)
}

query <- myfun(con=con, q=q, tabella = "ProgettiAccordi")


prj <- con %>% tbl(sql(query)) %>% as_tibble() 


# estrazione dati per rendicontazione obiettivi

prj %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine), 
         annoIntermedia = year(DataScadenzaRelazioneIntermedia)) %>% 
  filter(annofine == 2022 |
           annoIntermedia == 2022) %>%
  select(Codice, Tipologia, MatrRespScientifico, DataInizio, DataFine, 
         RelIntermedia, DataScadenzaRelazioneFinale, DataScadenzaRelazioneIntermedia, Prolungato, 
         InviatoSollecitoRelazioneFinale, InviatoSollecitoRelazioneIntermedia, RelazioneIntermediaDaInviare) %>%  View()
