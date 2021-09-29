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
library("summaryBox")
library(C3)
#library(flexdashboard)
# library("dashboardthemes")


#Carico i dati----
#tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))#-tabella complessiva izsler esami prodotti orelav##

tabIZSLER <- readRDS(file = here( "data", "processed", "TabellaGenerale.rds"))#-tabella complessiva izsler esami prodotti orelav##
GCR <- readRDS(file = here("data", "processed", "GCR.rds"))#-dati da gestione centralizzata della richiesta

#inserisco i dati relativi al numero di conferimenti e valorizzazione della GCR
tabIZSLER <- tabIZSLER %>%
  mutate_at(vars(TotPrestazioni),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,GCR$n.conf, TotPrestazioni )))
tabIZSLER <- tabIZSLER %>%
  mutate_at(vars(TotTariff),
            funs(ifelse( Laboratorio == "GESTIONE CENTRALIZZATA DELLE RICHIESTE" & ANNO >= 2021,GCR$Valore, TotTariff )))

prj <- readRDS(file = here( "data", "processed", "prj.rds"))#-tabella progetti di ricerca con strutture
pub <- readRDS(file = here( "data", "processed", "pub.rds"))#-tabella pubblicazioni


perf <- readRDS(here("data", "processed", "performance.RDS"))









# #Carico funzioni----
ValueBOX <- function(dt, Variabile, Variabile2 = NULL, Titolo, colore, icona){ 
  
  if(is.null(Variabile2)){    
    
    valore <- sum(dt[, Variabile])  
    
  } else {
    valore <- round(sum(dt[, Variabile]/sum(dt[, Variabile2])),2)
  }
  
  valueBox(prettyNum(valore, big.mark = ".", decimal.mark = ","), Titolo, icon = icon(icona), color = colore)
}

#TABELLA IZSLER aggregato per dipartimenti con FTE----

tizsler <-  tabIZSLER %>% 
  rename( "ANALISI" = TotPrestazioni, "VALORE" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%   
  group_by(Anno, Dipartimento) %>% 
  summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%  
  mutate(RT = (VALORE+VP+AI),
         FTE_T = round((FTED+FTEC),1)) %>%
  arrange(desc(ANALISI)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0), 
         "C-FTE" = round(COSTI/FTE_T, 0), 
         "ROI" = round(RT/COSTI, 2)) %>% 
  select(-FTED, -FTEC)  %>%
  filter(ANALISI >0)  






# tizsler <-  tabIZSLER %>%  
#   rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
#           "COSTI" = costi) %>%
#   group_by(Anno, Dipartimento) %>%
#   summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
#   mutate(RT = (VALORE+VP+AI),
#          FTE_T = round((FTED+FTEC),1)) %>%
#   arrange(desc(ANALISI)) %>%
#   mutate("R-FTE" = round(RT/FTE_T,0), 
#          "C-FTE" = round(COSTI/FTE_T, 0), 
#          "ROI" = round(RT/COSTI, 2)) %>% 
#   select(-FTED, -FTEC)


#Tabella pubblicazioni----

pub <- pub %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))

