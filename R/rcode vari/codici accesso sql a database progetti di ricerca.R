library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)
library(openxlsx)


# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod03.izsler.it", 
#                        Database = "ProgettiAccordi")
# 
# Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
#   paste(fixed, tabella)
# }
# 
# q<-Query(tabella = "'ProgettiAccordi'")
# 
# myfun <- function(con, q, tabella)
# {   
#   
#   column.types <- dbGetQuery(con, q)
#   
#   ct <- column.types %>%
#     mutate(cml = case_when(
#       is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
#       CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
#       TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
#     )
#     ) %>%
#     arrange(cml) %>%
#     pull(COLUMN_NAME)
#   fields <- paste(ct, collapse=", ")
#   query <- paste("SELECT", fields, paste("FROM", tabella))
#   return(query)
# }
# 
# query <- myfun(con=con, q=q, tabella = "ProgettiAccordi")
# 
# 
# prj <- con %>% tbl(sql(query)) %>% as_tibble() 


# estrazione dati per rendicontazione obiettivo  % di utilizzo del budget
#estraggo i progetti che si concludono nel 2022

prj <- readRDS(here("R", "rcode vari", "prj.RDS"))


prj <- prj %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine))%>% 
  filter(annofine == 2022,  
         # !is.na(MatrRespScientifico), 
         !is.na(DataScadenzaRelazioneFinale)) %>%
  select(Codice,CodIDIzsler, Tipologia, Descrizione, RespScientifico, MatrRespScientifico, DataInizio, DataFine, 
         Stato, StatoRelazioneIntermedia , DataScadenzaRelazioneFinale, DataScadenzaRelazioneIntermedia, Prolungato, 
         InviatoSollecitoRelazioneFinale, InviatoSollecitoRelazioneIntermedia, RelazioneIntermediaDaInviare, 
         FinCompApprovato, Prolungato) %>%  
  mutate(
    Stato = recode(Stato,
                   `1` = "annullato", 
                   `2` = "inviato", 
                   `3` = "in attesa", 
                   `4` = "in corso", 
                   `5` = "terminato", 
                   `6` = "terminato non richiesta relazione"), 
    StatoRelazioneIntermedia = recode(StatoRelazioneIntermedia, 
                                      `0` = "non impostato", 
                                      `1` = "non prevista", 
                                      `2` = "non inviata", 
                                      `3` = "inviata", 
                                      `4` = "approvata", 
                                      `5` = "non approvata"),
    Tipologia = recode(Tipologia,
                       `0` = "Autofinanziato",
                       `1` = "Finalizzato", 
                       `2` = "Corrente", 
                       `3` = "Altro tipo", 
                       `4` = "Regionali", 
                       `5` = "Europeo", 
                       `6` = "CCM",
                       `7` = "Istituzionale")
  )
   
##importo i dati dello speso dei progetti di ricerca

prjspeso <- read_excel(here("R", "rcode vari", "PRWEB_movimenti.xls"))

prjspeso <- prjspeso %>% 
  rename( CodIDIzsler = Progetto) %>% 
  group_by(CodIDIzsler) %>% 
  filter(`Data Registrazione` <= "2022-06-30") %>% 
  group_by(CodIDIzsler) %>% 
  summarise(speso = sum(Importo))
 

prj %>% 
  select(CodIDIzsler,RespScientifico, Descrizione,  DataInizio, DataFine, FinCompApprovato) %>% 
  left_join(prjspeso, by = "CodIDIzsler") %>% 
  mutate("budget consumato" = 100*(speso/FinCompApprovato)) %>%  
  write.xlsx(file = "budget consumato.xlsx")
  
  
