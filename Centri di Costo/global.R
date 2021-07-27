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

cc <- read_delim(here("data", "raw", "coge1921.txt"), 
                 "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                              grouping_mark = "."), trim_ws = TRUE)

names(cc)[5:9] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")


dtanalisi <- cc %>% 
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
                               `-13` = "Non Ufficiale Gratuito" ,  .default = NA_character_),
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
                            `-13` = "Gratuito" ,  .default = NA_character_), 
         Uff = recode (`Cod. classificazione`, 
                       `-1` = "Ufficiale", 
                       `-3` = "Ufficiale", 
                       `-8` = "Non Ufficiale", 
                       `-9` = "Non Ufficiale", 
                       `-4` = "Ufficiale", 
                       `-5` = "Ufficiale", 
                       `-7` = "Ufficiale", 
                       `-11` = "Ufficiale", 
                       `-6`  = "Non Ufficiale", 
                       `-10` = "Non Ufficiale", 
                       `-13` = "Non Ufficiale", .default = NA_character_), 
         
         Quarter = factor(paste(`N. Trimestre`,"-", "Trim")),
         TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato,
                       ifelse(ClassAnalisi == "Ufficiale Gratuito", `A Tariffario`, 0)),
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato,
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", `A Tariffario`, 0)),
         TGratuito = ifelse(Pagamento == "Pagamento", Fatturato,
                            ifelse(Pagamento == "Gratuito",`A Tariffario`, 0 )))



# analisi <- read_excel(sheet = "Report 1", here(  "data", "raw",  "analisi1921.xls"))
# 
# names(analisi)[1:4] <- c("Dipartimento", "Reparto", "Laboratorio", "Centro di Costo", "CodCC")
# 
# dtanalisi <- analisi %>% 
#   mutate(ClassAnalisi = recode(`Cod. classificazione`, 
#                                `-1` = "Ufficiale a Pagamento", 
#                                `-3` = "Ufficiale a Pagamento", 
#                                `-8` = "Non Ufficiale a Pagamento", 
#                                `-9` = "Non Ufficiale a Pagamento", 
#                                `-4` = "Ufficiale Gratuito", 
#                                `-5` = "Ufficiale Gratuito", 
#                                `-7` = "Ufficiale Gratuito", 
#                                `-11` = "Ufficiale Gratuito", 
#                                `-6`  = "Non Ufficiale Gratuito", 
#                                `-10` = "Non Ufficiale Gratuito", 
#                                `-13` = "Non Ufficiale Gratuito" 
#   ), 
#   Pagamento = recode(`Cod. classificazione`, 
#                      `-1` = "Pagamento", 
#                      `-3` = "Pagamento", 
#                      `-8` = "Pagamento", 
#                      `-9` = "Pagamento", 
#                      `-4` = "Gratuito", 
#                      `-5` = "Gratuito", 
#                      `-7` = "Gratuito", 
#                      `-11` = "Gratuito", 
#                      `-6`  = "Gratuito", 
#                      `-10` = "Gratuito", 
#                      `-13` = "Gratuito" 
#   ), 
#   Uff = recode (`Cod. classificazione`, 
#                 `-1` = "Ufficiale", 
#                 `-3` = "Ufficiale", 
#                 `-8` = "Non Ufficiale", 
#                 `-9` = "Non Ufficiale", 
#                 `-4` = "Ufficiale", 
#                 `-5` = "Ufficiale", 
#                 `-7` = "Ufficiale", 
#                 `-11` = "Ufficiale", 
#                 `-6`  = "Non Ufficiale", 
#                 `-10` = "Non Ufficiale", 
#                 `-13` = "Non Ufficiale" ), 
#   
#   
#   Quarter = factor(paste(`N. Trimestre`,"-", "Trim")), 
#   TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato, 
#                        ifelse(ClassAnalisi == "Ufficiale Gratuito", `A Tariffario`, 0)), 
#   TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato, 
#                           ifelse(ClassAnalisi == "Non Ufficiale Gratuito", `A Tariffario`, 0)), 
#   TGratuito = ifelse(Pagamento == "Pagamento", Fatturato, 
#                      ifelse(Pagamento == "Gratuito",`A Tariffario`, 0 )))  

#funzioni----

to_be <- function(df, Pagamento){
  if(Pagamento == "Pagamento")
    dplyr::filter(df, Parametro %in% c("Fatturato","VarFatt"))
  else filter(df, Parametro %in% c("Tariffato","VarVal"))
}