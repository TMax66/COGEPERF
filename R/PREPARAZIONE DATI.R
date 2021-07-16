library("tidyverse")
library("here")
library("readxl")
library("DBI")
library("odbc")
 

#DATI ORE LAVORATE DA DBASE PERSONALE_COGE####
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbtest02", 
                      Database = "DW_COGE_DEV", Port = 1433)

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
  dbo.Personale_V2020.FineRapporto,
  dbo.Personale_V2020.Nome,
  dbo.Personale_V2020.Cognome
FROM
  dbo.Personale_V2020 INNER JOIN dbo.IZS_CDC ON (dbo.Personale_V2020.CDC=dbo.IZS_CDC.CODICE_CDC)
   INNER JOIN dbo.IZS_Reparti ON (dbo.IZS_CDC.CODICE_REPARTO=dbo.IZS_Reparti.CODICE_REPARTO)
   INNER JOIN dbo.IZS_Dipartimenti ON (dbo.IZS_Reparti.CODICE_DIPARTIMENTO=dbo.IZS_Dipartimenti.CODICE_DIPARTIMENTO)
   INNER JOIN dbo.IZS_Livello0 ON (dbo.IZS_Dipartimenti.Codice_Livello0=dbo.IZS_Livello0.CODICE_Livello0)
  
WHERE
  dbo.Personale_V2020.Anno  >=  2019



"
ore <- con %>% tbl(sql(query)) %>% as_tibble()  
  # saveRDS(., file = here("data", "processed",  "orelavorate.rds"))


# tabstr <- readRDS(file = here("data", "processed",  "orelavorate.rds") )

strutture <- ore %>% 
  select("Dipartimento" = Livello0, "Reparto" = DIPARTIMENTO, 
         "Laboratorio" = REPARTO, CENTRO_DI_COSTO) %>% 
  unique()
  #saveRDS(., file = here("COGES", "data", "processed",  "strutture.rds"))

ore1 <- ore %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% 
  select(-Livello0, -DIPARTIMENTO, -REPARTO) %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>%
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio, Dirigente) %>%  
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  
  #saveRDS(., file = here(  "data", "processed",  "orelavorate.rds"))
  

#DATI DA CONTROLLO DI GESTIONE####

##ANALISI e Ricavi####
analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "analisi1921.xls"))
# il file analisi1921.xls deriva da una query eseguita in business object in sai-manager##

analisi <- analisi %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  select(-Reparto) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO"))
  #saveRDS(., file = here(  "data", "processed",  "analisi.rds"))
  


##COSTI####

costi <- read_excel(sheet = "Report 1", here( "data", "raw",  "costi1921.xls"))
# il file costi1921.xls deriva da una query eseguita in business object in sai-manager##

costi <- costi %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  select(-Reparto) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO"))  
  #saveRDS(., file = here( "data", "processed",  "costi.rds"))

##VENDITA PRODOTTI####

VP <- read_excel(sheet = "Report 1", here( "data", "raw",  "VP1921.xls"))
  # il file VP1921.xls  deriva da una query eseguita in business object in sai-manager##
  
vp <- VP %>% rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) 
  #saveRDS(., file = here( "data", "processed",  "vp.rds"))

##ATTIVITA' INTERNA####

AI <- read_excel(sheet = "Report 1", here( "data", "raw",  "AI1921.xls"))
  # il file AI1921.xls deriva da una query eseguita in business object in sai-manager##

ai <- AI %>% 
  rename("CENTRO_DI_COSTO" =`Centro Di Costo`) %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) 
  # saveRDS(., file = here( "data", "processed",  "ai.rds"))




#TABELLA PRINCIPALE####
analisi %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(esami = sum(Determinazioni), 
            valore = sum(`A Tariffario`)) %>%  
  left_join(
    (vp %>%
       group_by(Anno, Dipartimento, Reparto,Laboratorio ) %>% 
       summarise(nprodotti = sum(Numero), 
                 ricavovp = sum(Fatturato))   
     
    ), by =c("Anno", "Dipartimento", "Reparto", "Laboratorio")
  ) %>% 
  left_join(
    (ai %>% 
       group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
       summarise( valoreai = sum(`A Tariffario`))
    ), by = c("Anno", "Dipartimento", "Reparto", "Laboratorio")
  ) %>% 
  left_join(
    (costi %>% 
       group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
       summarise(costi = sum(Costo))
    ), by = c("Anno", "Dipartimento", "Reparto", "Laboratorio")
  ) %>% 
  left_join(
    (ore1 %>% 
       group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
       summarise(FTED = sum(FTE_Dirigenza, na.rm = T), 
                 FTEC = sum (FTE_Comparto, na.rm = T))
    ), by = c("Anno", "Dipartimento", "Reparto", "Laboratorio")
  ) %>% 
  filter(!is.na(Dipartimento)) %>% rowwise() %>% 
  mutate(totricavi = sum(valore, ricavovp, valoreai, na.rm = T)) %>%  
  saveRDS(., file = here( "data", "processed",  "TABELLA.rds"))


#DATI DA PROGETTI DI RICERCA####

prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2020.xlsx"))

anag <- ore %>% 
  left_join(strutture, by = c("CENTRO_DI_COSTO")) %>% 
  select(-Anno, -Ore, -Mese, -Livello0, -DIPARTIMENTO, -REPARTO) %>%  
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018) %>% 
  distinct(Matricola, .keep_all = TRUE)

prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine)) %>% View()
  saveRDS(., file = here( "data", "processed",  "prj.rds"))
  

#DATI DA PUBBLICAZIONI####

pubblicazioni <- read_excel(here("data", "raw", "pubblicazioni.xlsx"))
pubblicazioni$AU <- str_to_upper(pubblicazioni$AU)
pubblicazioni$AU <- gsub(",.*$", "", pubblicazioni$AU)
pubblicazioni %>% filter(OA >= 2019) %>%
  mutate(Cognome = recode(AU,
                          "COSCIANI_CUNICO" = "COSCIANI CUNICO",
  )) %>%
  left_join(anag, by = c("Cognome" = "Cognome")) %>%
  filter(Dirigente == "S") %>%  
  saveRDS(., file = here( "data", "processed",  "pub.rds"))




#DATI DA DBASE PERFORMANCE (OBIETTIVI, INDICATORI, TARGET, RISULTATO, FTEQ PROGRAMMATI)####


##Programmazione 2021 FTE----
source(here("R",  "FTEPROGRAMMATI.R"))



