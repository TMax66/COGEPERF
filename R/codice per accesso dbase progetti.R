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
saveRDS(prjuo, here("data", "processed", "prjuo.RDS"))
prj <- readRDS(here("data", "processed", "prjxx.RDS"))
prjuo <- readRDS(here("data", "processed", "prjuo.RDS"))


##verifica duplicati nel codice progetti###

# prj$duplicati <- duplicated(prj$Codice)
# 
# 
# codicidoppi <- prj %>% filter(duplicati == TRUE) %>% 
#   select(Codice)
# 
# prj %>% filter(Codice %in% codicidoppi$Codice) %>%  View()
  


prj22 <- prj %>% filter(Tipo_P_A == "A") %>% 
 select(Codice, CodIDIzsler, DataInizio, DataFine,  Descrizione, Tipologia, MatrRespScientifico, 
        RespScientifico, FinCompApprovato) %>%   
  left_join(
    prjuo %>% 
      mutate(budgetUO = QuotaSpeseGenerali+QuotaSpeseCoordinamento+
               QuotaApparecchiature+QuotaReagenti+
               QuotaMissioniConv+QuotaPersContratto+QuotaPersStrutturato) %>% 
      select(CodProgetto, NumUOProgetto, RespScientUO, MatricolaRespScientUO, budgetUO,DescrUO ),   by = c("Codice" = "CodProgetto")) %>% 
  rename(MatrRSUO = MatricolaRespScientUO, 
         BudgetUO = budgetUO) 


saveRDS(prj22, here("data", "processed", "prj22.RDS"))


