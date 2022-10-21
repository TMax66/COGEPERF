library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)
library(here)
library(hrbrthemes)
library(patchwork)
library(readr)
library(formattable)
library(sparkline)
library(rpivotTable)
library(shinyjs)
#library(rvest)
library(shinycssloaders)
#Carica dati----
#library(writexl)

#dtanalisi <-  readRDS(file = here( "data", "CC.rds"))
dtanalisi <- readRDS(here("data", "processed","CC.RDS"))
 
source(here("app", "appcostiricavi", "R", "utils.R"))
#source(here("R", "utils.R"))


#controlli----
ccnotecnici <- c(
  "1000", "1100", "1200", "1300", "1400", "2100", "3000", 
  "3100", "7100", "7200", "7300", "7410", "7420", "2000", "3210", "3220", "3400", "3230")
ccomuni <- c("4CCC", "5CCC", "6CCC", "CC2C", 
             "41CC", "43CC", "44CC", "45CC", "550C", "C300", "CC1C", "CCCC", "Z300", 
             "531C")
ccescludere <- c("M101", "N301", "T004", "V401")

produzione <- dtanalisi %>% select(CodiceCDC, CDC) %>% 
  distinct() %>%  
  filter(!CodiceCDC %in% c(ccnotecnici, ccomuni, ccescludere)) %>% 
  select(CDC)

gestionale <- dtanalisi %>% select(CodiceCDC, CDC) %>% 
  distinct() %>%  
  filter(CodiceCDC %in% c(ccnotecnici)) %>% 
  select(CDC)

cscomuni <- dtanalisi %>% select(CodiceCDC, CDC) %>% 
  distinct() %>%  
  filter(CodiceCDC %in% c(ccomuni)) %>% 
  select(CDC)





 