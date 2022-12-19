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
  


prj22 <- prj %>% filter(Tipo_P_A == "P") %>% 
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




#estrazione per quiantificazione finanziamenti alla ricerca per tipologia di progetto
prj22 <- prj %>% filter(Tipo_P_A == "P") %>% 
  select(Codice, CodIDIzsler, DataInizio, DataFine, Anno,  Descrizione, Tipologia, MatrRespScientifico, 
         RespScientifico, FinCompApprovato) %>% 
  left_join(
    prjuo %>% 
      mutate(budgetUO = QuotaSpeseGenerali+QuotaSpeseCoordinamento+
               QuotaApparecchiature+QuotaReagenti+
               QuotaMissioniConv+QuotaPersContratto+QuotaPersStrutturato) %>% 
      select(CodProgetto, NumUOProgetto, RespScientUO, MatricolaRespScientUO, budgetUO,DescrUO ),   by = c("Codice" = "CodProgetto")) %>% 
  rename(MatrRSUO = MatricolaRespScientUO, 
         BudgetUO = budgetUO) %>% 
  filter(Anno >=2019) %>% 
 
  mutate(Tipologia = case_when(Tipologia == 0 ~ "Autofinanziato",
                               Tipologia == 1 ~ "Finalizzato",
                               Tipologia == 2 ~ "Corrente",
                               Tipologia == 3 ~ "Altro tipo",
                               Tipologia == 4 ~ "Regionali",
                               Tipologia == 5 ~ "Europeo",
                               Tipologia == 6 ~ "CCM",
                               Tipologia == 7 ~ "Istituzionale")) %>%
  group_by(Anno) %>% 
  summarise(finanziamento = sum(BudgetUO, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Anno", values_from = "finanziamento" ) %>%  View()




# estrazione per controllo rispetto scadenze di presentazione relazioni intermedie/finali

# prj %>% filter(Tipo_P_A == "P") %>% 
#   select(Codice, CodIDIzsler, RespScientifico, DescrizioneBreve,  DataInizio, DataFine,  DataScadenzaRelazioneFinale, 
#          DataScadenzaRelazioneIntermedia, Descrizione, Tipologia, MatrRespScientifico, 
#          RespScientifico, FinCompApprovato) %>% 
#   mutate(annoint = year(DataScadenzaRelazioneIntermedia), 
#          annofin = year(DataScadenzaRelazioneFinale)) %>% 
#   filter(annoint %in% c("2019", "2020")) %>%  
#   write.xlsx(file = "prjIntermedi2019-2020.xls")
# 
# prj %>% filter(Tipo_P_A == "P") %>% 
#   select(Codice, CodIDIzsler, RespScientifico, DescrizioneBreve,  DataInizio, DataFine,  DataScadenzaRelazioneFinale, 
#          DataScadenzaRelazioneIntermedia, Descrizione, Tipologia, MatrRespScientifico, 
#          RespScientifico, FinCompApprovato) %>% 
#   mutate(annoint = year(DataScadenzaRelazioneIntermedia), 
#          annofin = year(DataScadenzaRelazioneFinale)) %>% 
#   filter(annofin %in% c("2019", "2020")) %>%  
#   write.xlsx(file = "prjfinali2019-2020.xls")
