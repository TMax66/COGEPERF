library(tidyverse)
library(readr)
library(here)

cc <- read_delim(here("data", "raw", "coge1921.txt"), 
                 "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                              grouping_mark = "."), trim_ws = TRUE)

names(cc)[5:9] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")


T1 <- cc %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE), 
            Tariffato = sum(`A Tariffario`, na.rm=TRUE), 
            Fatturato = sum (Fatturato, na.rm = TRUE), 
            Costi = sum(Costo, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotPrestazioni = sum(Prestazioni), 
            TotCost = sum(Costi), 
            TotTariff = sum(Tariffato)) 

T2 <- cc %>% filter(Classe== "Vendite prodotti") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  mutate(Fatturato = ifelse(Fatturato == 0,`A Tariffario`, Fatturato )) %>% 
  summarise(NVP = sum(Numero, na.rm = TRUE), 
            FattVP = sum(Fatturato, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNVP = sum(NVP), 
            TotFattVP = sum(FattVP))


T3 <- cc %>% filter(Classe== "Ricavi da produzione") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio,  Categoria, Classe, Area, Classificazione) %>% 
  summarise(NumAI = sum(Numero, na.rm = TRUE), 
            TarAI = sum(`A Tariffario`, na.rm = TRUE)) %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio) %>% 
  summarise(TotNAI = sum(NumAI), 
            TAI = sum(TarAI)) 

ore <- con %>% tbl(sql(query)) %>% as_tibble()  
names(ore)[1:5] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")
fte <- ore %>% 
  mutate(Dirigente = recode(Dirigente, N = "Comparto", S = "Dirigenza")) %>% 
  filter(Dipartimento != "Non applicabile") %>% 
  group_by(Anno, Dipartimento, Reparto, Laboratorio, Dirigente) %>%   
  filter(!is.na(Dirigente) & !is.na(Ore)) %>% 
  summarise(hworked = sum(Ore, na.rm = T)) %>% 
  mutate(FTE = ifelse(Dirigente == "Comparto", hworked/(38*47.4), hworked/(36*47.4))) %>% 
  pivot_wider(names_from = "Dirigente", values_from = c("hworked", "FTE"))  %>% 
  select(-hworked_, -FTE_)  

 


T1 %>% 
  left_join(T2, by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>%  
  left_join(T3, by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>% 
  left_join(fte,by=c("Anno", "Dipartimento", "Reparto", "Laboratorio")) %>% 
saveRDS(., file = here("data", "processed",  "TabellaGenerale.rds"))
   



prj <- read_excel(sheet = "PRJ", here("data", "raw", "prj2020.xlsx"))

anag <- ore %>% 
  mutate(annoraplav = year(FineRapporto)) %>% 
  filter(annoraplav > 2018) %>% 
  distinct(Matricola, .keep_all = TRUE)

prj %>%
  left_join(anag, by = c("MatrRSUO" = "Matricola")) %>% 
  mutate(annoinizio = year(DataInizio), 
         annofine = year(DataFine)) %>%  
saveRDS(., file = here( "data", "processed",  "prj.rds"))



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





x <- dtanalisi %>% 
filter(Classe == "Prestazioni" & `Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO") %>%  
group_by(Anno, Area) %>% 
  summarise("n.prestazioni" = sum(Determinazioni, na.rm=TRUE)) %>% View()


 




# analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "analisi1921.xls"))
# 
# analisi %>% 
#   filter(Anno == 2019  & Determinazioni >0  ) %>%
#   group_by( Livello0,  Classe, Area, Classificazione) %>% 
#   summarise(Prestazioni = sum(Determinazioni, na.rm = TRUE)) %>%  
#   group_by(Livello0) %>%  
#   summarise(TotPrestazioni = sum(Prestazioni)) %>% 
#   summarise(sum(TotPrestazioni))
