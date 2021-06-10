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


 
tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))
# 
# ###IZSLER######_________________________________________________________________
# 
 
tizsler <-  tabIZSLER %>%
  rename("ricavi" = valore, "VP" = ricavovp, "AI" = valoreai) %>%
  group_by(Anno, Dipartimento) %>%
  summarise_at(c("esami", "ricavi",  "VP", "AI", "FTED", "FTEC","costi"), sum, na.rm = T) %>%
  mutate(RT = (ricavi+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(esami)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0) )