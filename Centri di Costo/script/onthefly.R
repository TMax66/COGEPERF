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
cc <- read_delim("data/raw/coge1921.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

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

















cc %>% 
  mutate(TUff = ifelse(ClassAnalisi == "Ufficiale a Pagamento", Fatturato, 
                       ifelse(ClassAnalisi == "Ufficiale Gratuito", `A Tariffario`, 0)), 
         TNonUff = ifelse(ClassAnalisi == "Non Ufficiale a Pagamento", Fatturato, 
                          ifelse(ClassAnalisi == "Non Ufficiale Gratuito", `A Tariffario`, 0))) %>% View()
  
  
  
  
                 summarise(Tariffato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
                           Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% View()
                 mutate(VarVal = round((Tariffato/lag(Tariffato) - 1) * 100, 2 ), 
                        VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
                 filter(`Centro di Costo` == input$CC) %>% 
                 pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>% 
                 to_be(Pagamento = input$paga) %>%  
                 filter(Pagamento == input$paga & Uff == input$uff) %>%  
                 pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
                 data.frame()






                 lab <- reactive(   
                   analisi %>% 
                     filter(`Centro di Costo`== input$CC) %>% 
                     select(Laboratorio, `Codice Livello 3 Centro di Costo` ) %>% 
                     data.frame())


                 sidebarPanel(
                   h4(textOutput("struttura")),
                   
                   selectInput("CC", "Seleziona il Centro di Costo", 
                               choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))),
                   selectInput("uff", "Ufficiale/Non Ufficiale", 
                               choices = c("","Ufficiale", "Non Ufficiale")), 
                   selectInput("paga", "Gratuita/Pagamento", 
                               choices = c("", "Gratuita", "Pagamento"))
                   
                 ),














dt <- dtanalisi %>% 
                   filter(`Costo o Ricavo`=="Ricavo") %>%  
  group_by(`Centro di Costo`, Pagamento, ClassAnalisi,Uff, Anno,  Quarter) %>% 
                   summarise(Tariffato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
                             Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% 
                   mutate(VarVal = round((Tariffato/lag(Tariffato) - 1) * 100, 2 ), 
                          VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
  filter(`Centro di Costo` == "SEDE TERRITORIALE DI BERGAMO") %>%
  pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>%  View()
  to_be(Pagamento = "Pagamento") %>%  
  filter(Pagamento == "Pagamento" & Uff == "Ufficiale") %>%  
  pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
  data.frame()  


  ggplot(dt, 
         aes(y = dt[,7], x = Quarter,  label=as.character(dt[,7])))+
    geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+ 
    geom_label(size = 4, aes(color = Anno == max(Anno)))+
    scale_color_manual(values = c("grey", "blue"), guide = "none") +
    
    facet_grid(~Anno, switch = "x", scales = "free")+
     
    
    geom_hline(yintercept = 0, size = 0.5)+
    labs(y = "", x = " ", 
         title = paste( "Andamento trimestrale del", names(dt[7]), "in €"
         ))+
    theme_ipsum_rc()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15), 
          
          strip.text.x = element_text(size = 18))

  
  l <- analisi %>% 
    filter(`Centro di Costo`== "LABORATORIO LATTE") %>% 
    select(Laboratorio) %>% data.frame()   
    
    
    
 
   

 

  