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
# library("summaryBox")
library("ECharts2Shiny")
library("formattable")
library("fmsb")
#library(flexdashboard)
# library("dashboardthemes")

#

#Carico i dati----
#tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))#-tabella complessiva izsler esami prodotti orelav##

tabIZSLER <- readRDS(file = here("data", "processed",   "TabellaGenerale.rds"))#-tabella complessiva izsler esami prodotti orelav##
GCR <- readRDS(file = here("data", "processed",   "GCR.rds"))#-dati da gestione centralizzata della richiesta

#inserisco i dati relativi al numero di conferimenti e valorizzazione della GCR


tabIZSLER <- tabIZSLER %>% 
  left_join(
(GCR %>% 
  select(Reparto, Anno, MESE, n.conf, Valore)), by = c("Reparto", "ANNO" = "Anno", "MESE")) %>% 
  mutate_at(vars(TotPrestazioni),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,n.conf, TotPrestazioni ))) %>%  
            
  mutate_at(vars(TotTariff),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,Valore, TotTariff ))) %>%  
  select(-n.conf, Valore)             

tabIZSLER <- tabIZSLER %>% 
  filter(!Dipartimento %in% c("Non applicabile", 
                              "Costi Comuni e Centri contabili", 
                              "Dipartimento amministrativo", 
                              "Direzione Generale", 
                              "Direzione Amministrativa",
                              "Costi Comuni e Centri contabili"
                               )) %>% 
  filter(!Reparto %in% c("COSTI COMUNI LOMBARDIA", "DIREZIONE SANITARIA", "FORMAZIONE E BIBLIOTECA")) 
  



FTp <- readRDS(here("data", "processed",   "FTp.RDS"))
FTEPD <- readRDS( here("data", "processed",  "FTEPD.RDS"))
FTEPREP <- readRDS(here("data", "processed",  "FTEPREP.RDS"))

FTEPREP <- FTEPREP %>% 
 mutate(Reparto= ifelse(Reparto == "SEDE TERRITORIALE DI FORLÃŒ - RAVENNA" , "SEDE TERRITORIALE DI FORLÌ - RAVENNA", Reparto))



prj <- readRDS(file = here("data", "processed",  "prj.rds"))#-tabella progetti di ricerca con strutture


pub <- readRDS(file = here("data", "processed",  "pub.rds"))#-tabella pubblicazioni


#perf <- readRDS( "performance.RDS"))




# #Carico funzioni----
ValueBOX <- function(dt, Variabile, Variabile2 = NULL, Titolo, colore, icona){ 
  
  if(is.null(Variabile2)){    
    
    valore <- sum(dt[, Variabile])  
    
  } else {
    valore <- round(sum(dt[, Variabile]/sum(dt[, Variabile2])),2)
  }
  
  valueBox(prettyNum(valore, big.mark = ".", decimal.mark = ","), Titolo, icon = icon(icona, verify_fa = FALSE), color = colore)
}

#TABELLA IZSLER aggregato per dipartimenti con FTE mensile (già cumulato)----

tizsler <-  tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost,   Anno = ANNO) %>%   
  group_by(Dipartimento,Anno, MESE ) %>% 
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = FTED+FTEC,
         Prestazioni = cumsum(Prestazioni), 
         Valorizzazione = cumsum(Valorizzazione), 
         VP = cumsum(VP), 
         AI = cumsum(AI), 
         COSTI = cumsum(COSTI), 
         RT = cumsum(RT)) %>%   
  filter(Prestazioni >0) 



#Tabella pubblicazioni----

pub <- pub %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))  

