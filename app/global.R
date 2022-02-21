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

tabIZSLER <- tabIZSLER %>% 
  filter(!Dipartimento %in% c("Non applicabile", 
                              "Costi Comuni e Centri contabili", 
                              "Dipartimento amministrativo", 
                              "Direzione Generale", 
                              "Direzione Amministrativa",
                              "Costi Comuni e Centri contabili"
                               )) %>% 
  filter(!Reparto %in% c("COSTI COMUNI LOMBARDIA", "DIREZIONE SANITARIA")) 
  

#####calcolo dei ftep----
dtProg <- readRDS(here("data", "processed", "datiFSB.rds"))

#questo codice serve per calcolare la % di FTE progrmmati per attività isti di tutto l'izsler
FTp <- dtProg %>% 
  group_by(Valorizzazione) %>% 
  summarise(FTED = sum(FTED, na.rm = T), 
            FTEC = sum(FTEC, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(FT = sum(FTED, FTEC)) %>% 
  ungroup() %>% 
  mutate(FTp = round(prop.table(FT), 1)) %>%  
  filter(Valorizzazione == "si") %>%
  select(FTp)  

ftepDIP <- readRDS(here("data", "processed", "ftepDIP.RDS"))
ftepREP <- readRDS(here("data", "processed", "ftepREP.RDS"))
ftepREP <- ftepREP %>% 
  rename( valorizz = Valorizzazione) %>% 
  mutate(Reparto = casefold(Reparto, upper = TRUE))

ftepREPD <- readRDS(here("data", "processed", "ftepREPD.RDS"))#questi dati servono per calcolare il rfte per dipartimento nei valuebox
ftepREPD <- ftepREPD %>%
  rename( valorizz = Valorizzazione)



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

# ValueBox2 <- function(Dipartimento, Titolo, colore ){
#   
#   valueBox(
#     paste0((perf %>% 
#     filter(Dipartimento == Dipartimento) %>% 
#     filter(Periodo == 4 & Avanzamento != 0 ) %>%
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2))),"%"),
#  
#    Titolo, color = colore)
#   
# }





 


#TABELLA IZSLER aggregato per dipartimenti con FTE----

tizsler <-  tabIZSLER %>% 
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%   
  group_by(Anno, Dipartimento) %>% 
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>% 
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = (FTED+FTEC)) %>%   
  arrange(desc(Prestazioni)) %>%  
  #mutate("R-FTE" = round(RT/FTE_T,0)) %>%  
  filter(Prestazioni >0)  


#Tabella pubblicazioni----

pub <- pub %>% 
  mutate(articoliif = ifelse(Congr == "IF ; Int" | Congr == "IF",  "IF", NA), 
         INT = ifelse(Congr == "IF ; Int" | Congr == "Int",  "Int", NA ), 
         NAZ = ifelse(Congr == "Naz", "Naz", NA), 
         Oth = ifelse(Congr == "Others" , "Others", NA), 
         IF = as.numeric(IF))  

