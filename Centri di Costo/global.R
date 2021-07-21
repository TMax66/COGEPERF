library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinythemes)
library(DT)
library(here)
library(hrbrthemes)
library(patchwork)


analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "newanalisi1921.xls"))

names(analisi)[1:4] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo")

dtanalisi <- analisi %>% 
  mutate(ClassAnalisi = recode(`Cod. classificazione`, 
                               `-1` = "Ufficiale a Pagamento", 
                               `-3` = "Ufficiale a Pagamento", 
                               `-8` = "Non Ufficiale a Pagamento", 
                               `-9` = "Non Ufficiale a Pagamento", 
                               `-4` = "Ufficiale Gratuito", 
                               `-5` = "Ufficiale Gratuito", 
                               `-7` = "Ufficiale Gratuito", 
                               `-11` = "Ufficiale Gratuito", 
                               `-6`  = "Non Ufficiale Gratuito", 
                               `-10` = "Non Ufficiale Gratuito", 
                               `-13` = "Non Ufficiale Gratuito" 
  ), 
  Pagamento = recode(`Cod. classificazione`, 
                     `-1` = "Pagamento", 
                     `-3` = "Pagamento", 
                     `-8` = "Pagamento", 
                     `-9` = "Pagamento", 
                     `-4` = "Gratuito", 
                     `-5` = "Gratuito", 
                     `-7` = "Gratuito", 
                     `-11` = "Gratuito", 
                     `-6`  = "Gratuito", 
                     `-10` = "Gratuito", 
                     `-13` = "Gratuito" 
  ), 
  
  
  Quarter = factor(paste(`N. Trimestre`,"-", "Trim")))

#funzioni----

to_be <- function(df, Pagamento){
  if(Pagamento == "Pagamento")
    dplyr::filter(df, Parametro %in% c("Fatturato","VarFatt"))
  else filter(df, Parametro %in% c("Valorizzato","VarVal"))
}