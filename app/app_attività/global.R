librerie()
library(tidyverse)
library(here)
library(shiny)
library(zoo)



dt <- readRDS(here("app", "app_attività","datiperapp.RDS"))

source(here("app", "app_attività", "funzioni", "plot.R"))


conf <- dt %>% distinct() %>% 
  group_by(Data_Accettazione, Dipartimento, repacc, settore, tipoconf, tipoprel) %>% 
  summarise(conferimenti = n())

 





# funzioni

# myPanel <- function(title, value) {
#   tabPanel(
#   title = title, 
#   value = value, 
#   
#  
#     tabsetPanel("Sanità Animale"),
#     tabsetPanel("Alimenti Uomo"),
#     tabsetPanel("Alimenti Zootecnici")
# 
#     
#   )
#   )
# }
  
 