library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")
library("DT")
library("lubridate")
library("fmsb")


#Carico i dati----
tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))#-tabella complessiva izsler esami prodotti orelav##
prj <- readRDS(file = here( "data", "processed", "prj.rds"))#-tabella progetti di ricerca con strutture
pub <- readRDS(file = here( "data", "processed", "pub.rds"))#-tabella pubblicazioni


#Carico funzioni----

diprep <- function(Anno, Dipartimento)
{
  tabIZSLER %>% 
    filter(Anno == Anno & Dipartimento == Dipartimento) %>% 
    rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
            "COSTI" = costi) %>%
    group_by(Reparto) %>%
    summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
    mutate(RT = (VALORE+VP+AI),
           FTE_T = round((FTED+FTED),1)) %>%
    arrange(desc(ANALISI)) %>%
    mutate("R-FTE" = round(RT/FTE_T,0), 
           "C-FTE" = round(COSTI/FTE_T, 0)) %>% 
    select(-FTED, -FTEC) 
  
}
# 
# ###IZSLER######_________________________________________________________________
# 

#TABELLA IZSLER aggregato per dipartimenti con FTE----
tizsler <-  tabIZSLER %>%  
  rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
          "COSTI" = costi) %>%
  group_by(Anno, Dipartimento) %>%
  summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (VALORE+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(ANALISI)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0), 
         "C-FTE" = round(COSTI/FTE_T, 0)) %>% 
  select(-FTED, -FTEC)



#TABELLA DIPARTIMENTO da aggregare per reparto e anno con FTE----








#Tabella pubblicazioni----
pub <- pub %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))

