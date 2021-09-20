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

# cc <- read_delim(here("data", "raw", "coge1921.txt"), 
#                  "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
#                                                               grouping_mark = "."), trim_ws = TRUE)

dtanalisi <-  readRDS(file = here( "data", "processed", "CC.rds"))


#names(cc)[c(5:7, 20:21)] <- c("Dipartimento", "Reparto", "Laboratorio", "CodCC", "Centro di Costo")


# dtanalisi <- cc %>% 
#   mutate(ClassAnalisi = recode(idClassificazione, 
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
#                                `-13` = "Non Ufficiale Gratuito" ,  .default = NA_character_),
#          Pagamento = recode(`Cod. classificazione`, 
#                             `-1` = "Pagamento", 
#                             `-3` = "Pagamento", 
#                             `-8` = "Pagamento", 
#                             `-9` = "Pagamento", 
#                             `-4` = "Gratuito", 
#                             `-5` = "Gratuito", 
#                             `-7` = "Gratuito", 
#                             `-11` = "Gratuito", 
#                             `-6`  = "Gratuito", 
#                             `-10` = "Gratuito", 
#                             `-13` = "Gratuito" ,  .default = NA_character_), 
#          Uff = recode (`Cod. classificazione`, 
#                        `-1` = "Ufficiale", 
#                        `-3` = "Ufficiale", 
#                        `-8` = "Non Ufficiale", 
#                        `-9` = "Non Ufficiale", 
#                        `-4` = "Ufficiale", 
#                        `-5` = "Ufficiale", 
#                        `-7` = "Ufficiale", 
#                        `-11` = "Ufficiale", 
#                        `-6`  = "Non Ufficiale", 
#                        `-10` = "Non Ufficiale", 
#                        `-13` = "Non Ufficiale", .default = NA_character_), 
#          
#          Quarter = factor(paste("Q",`N. Trimestre`)),
#          TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato,
#                        ifelse(ClassAnalisi == "Ufficiale Gratuito", `A Tariffario`, 0)),
#          TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato,
#                           ifelse(ClassAnalisi == "Non Ufficiale Gratuito", `A Tariffario`, 0)),
#          TGratuito = ifelse(Pagamento == "Gratuito", `A Tariffario`,0), 
#          TPagamento = ifelse(Pagamento == "Pagamento", Fatturato,0), 
#          TVP = ifelse(Classe == "Vendite prodotti", Fatturato, 0), 
#          TAI = ifelse(Classe == "Ricavi da produzione interna", `A Tariffario`, 0), 
#          AttUff = ifelse(Uff== "Ufficiale", Determinazioni, 0 ), 
#          AttNUff = ifelse(Uff== "Non Ufficiale", Determinazioni, 0 ), 
#          AttGrat = ifelse(Pagamento== "Gratuito", Determinazioni, 0 ), 
#          AttPag = ifelse(Pagamento == "Pagamento", Determinazioni, 0), 
#          VP = ifelse(Classe == "Vendite prodotti", Numero, 0), 
#          AI = ifelse(Classe == "Ricavi da produzione interna", Numero, 0))
# 
# 

#oggetto UI----

# parametri <- tabsetPanel(
#   id = "params", 
#   type = "hidden"
#   
# )

#funzioni----

# to_be <- function(df, Pagamento){
#   if(Pagamento == "Pagamento")
#     dplyr::filter(df, Parametro %in% c("Fatturato","VarFatt"))
#   else filter(df, Parametro %in% c("Tariffato","VarVal"))
# }

Tplot <- function(df, y_par, y_par2, euro)
{    
  p1 <- ggplot(df)+ 
    aes(
      y = .data[[y_par]],
      x = .data[["Quarter"]],  
      label=paste(as.character(.data[[y_par]]), euro))+
    geom_line(group = 1, aes(color = ANNO == max(ANNO)), size= 1.1,  )+
    geom_label(size = 4.5, aes(color = ANNO == max(ANNO)))+
    scale_color_manual(values = c("grey", "blue"), guide = "none") +
    facet_grid(~ANNO, switch = "x", scales = "free")+
    geom_hline(yintercept = 0, size = 0.5)+
    labs(y = "", x = " ",
         title = "")+
    theme_ipsum_rc()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15),
          
          strip.text.x = element_text(size = 18))
  
  p2 <- ggplot(df)+ 
    aes(
      y = .data[[y_par2]],
      x = .data[["Quarter"]],  
      label=paste(as.character(.data[[y_par2]]), "%"))+
    geom_line(group = 1, aes(color = ANNO == max(ANNO)), size= 1.1,  )+
    geom_label(size = 4.5, aes(color = ANNO == max(ANNO)))+
    scale_color_manual(values = c("grey", "blue"), guide = "none") +
    facet_grid(~ANNO, switch = "x", scales = "free")+
    geom_hline(yintercept = 0, size = 0.5)+
    labs(y = "", x = " ",
         title = "")+
    theme_ipsum_rc()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15),
          
          strip.text.x = element_text(size = 18))
  
  p1|p2
  
  
}


 