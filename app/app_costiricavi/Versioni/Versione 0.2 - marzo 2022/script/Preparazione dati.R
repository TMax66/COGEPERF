library("tidyverse")
library("here")
library("readxl")
# library("DBI")
# library("odbc")

analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "newanalisi1921.xls"))
names(analisi)[1:4] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo")
analisi <- analisi %>% 
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
                               `-13` = "NonUfficiale Gratuito" 
  )) 


