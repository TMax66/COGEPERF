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

dtanalisi <-  readRDS(file = here( "data", "processed", "CC.rds"))
#dtanalisi <- readRDS(here("Performance ShinyApp", "Centri di Costo","CC.rds"))
 
 


# dtanalisi %>% 
#   select(Dipartimento, Reparto, Laboratorio) %>% 
#   distinct() %>% 
#   write_xlsx(path= "strutture.xlsx")



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


## Funzione per fare i grafici-----
Tplot <- function(df, y_par, y_par2)
{    
  ggplot(df)+ 
    aes(
      y = .data[[y_par]],
      x = .data[["ANNO"]])+  
    
    geom_ribbon(aes(ymin = 0, ymax = (.data[[y_par]])+0.1*(.data[[y_par]])), alpha=0.05)+
    geom_point(aes(y = .data[[y_par]])) +
    geom_line(aes(y = .data[[y_par]]))+
    facet_wrap(facets = ~Quarter, nrow=1, scales = "free")+
    scale_x_continuous(breaks = unique(df$ANNO), expand=c(0.16, 0))+
    geom_text(data = dplyr::filter(df, ANNO == 2020), aes(label = sprintf('%+0.1f%%',.data[[y_par2]])), 
              x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
    geom_text(data = dplyr::filter(df, ANNO == 2021), aes(label = sprintf('%+0.1f%%', .data[[y_par2]])), 
              x = 2020.5, y = 0, vjust = -1, fontface = 'bold', size=5)+
    geom_text(aes(label = sprintf('%0.1f',.data[[y_par]]), y = .data[[y_par]]), vjust = -1, size=3.5)+
    labs(y = "", x = " ",
         title = "")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15),
          
          strip.text.x = element_text(size = 15))
}


## Funzioni per tabelle----


AC <- function(CC = input$CC ){ 
  dtanalisi %>%  
    dplyr::filter(Costi== "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    dplyr::filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    left_join(  
      
      (dtanalisi %>% 
         dplyr::filter(Costi == "Ricavo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                   N_Num = sum(Numero, na.rm = TRUE), 
                   S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                   S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
         dplyr::filter(CDC == CC & Classe == "Prestazioni") %>% 
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
}

AC2 <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi== "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    group_by(ANNO, Area) %>% 
    arrange(Area) %>% 
    mutate(cs = cumsum(N)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -N) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
}


AU <- function(CC = input$CC, Uff){
  
  dtanalisi %>% 
  filter(Uff == Uff & Costi == "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    left_join(  
      
      (dtanalisi %>% 
         filter(Uff == Uff & Costi == "Ricavo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                   N_Num = sum(Numero, na.rm = TRUE), 
                   S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                   S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
         filter(CDC == CC & Classe == "Prestazioni") %>% 
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
  
  
}


AU2 <- function(CC = input$CC, Uff){
  dtanalisi %>%  
    filter(Uff == Uff & Costi == "Ricavo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
    summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
              N_Num = sum(Numero, na.rm = TRUE), 
              S_Tariffa = sum(Tariffario, na.rm = TRUE), 
              S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>% 
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    group_by(ANNO, Area) %>% 
    arrange(Area) %>% 
    mutate(cs = cumsum(N)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -N) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
}



RC <- function(CC = input$CC){
 dtanalisi %>%  
    filter(Costi== "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    rowwise() %>% 
    mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
    summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TRic, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    #rename(., "Prestazione" = Area) %>% 
    left_join(  
      
      (dtanalisi %>% 
         filter(Costi == "Ricavo") %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
         rowwise() %>% 
         mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
         filter(CDC == CC & Classe == "Prestazioni") %>%  
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(TRic, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% 
    
    rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
  
  
  
  
}

RC2 <- function(CC = input$CC)
{
  dtanalisi %>%  
    filter(Costi== "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    rowwise() %>% 
    mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
    summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TRic, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    group_by(ANNO, Area) %>% 
    arrange(Area) %>% 
    mutate(cs = cumsum(N)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -N ) %>% 
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
  
}

plotRC2 <-function(CC = input$CC)
{
  dtanalisi %>% filter(CDC == CC & Costi=="Ricavo") %>% 
    rowwise() %>% 
    mutate(totRic = round(sum(TUff, TNonUff, na.rm = T),2)) %>% ungroup() %>%  
    group_by(CDC,  ANNO,  Quarter) %>% 
    summarise(TotRic = sum(totRic, na.rm= TRUE)) %>%  
    arrange(Quarter) %>%
    #group_by(CDC,  ANNO,  Quarter) %>%
    mutate(CumTotRic = cumsum(TotRic)) %>% 
    ungroup() %>% 
    mutate(VarTot = round((TotRic/lag(TotRic)-1)*100, 2),
           VarCumTotRic = round((CumTotRic/lag(CumTotRic)-1)*100, 2))
  
}





RUf <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi == "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(TUff= sum(TUff, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TUff, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
    
    left_join(  
      (dtanalisi %>%  
         filter(Costi== "Ricavo") %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
         # rowwise() %>% 
         # mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
         summarise(TUff= sum(TUff, na.rm = TRUE)) %>% 
         filter(CDC == CC & Classe == "Prestazioni") %>%  
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(TUff, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% 
    
    rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
  
  
}
# 
RUf2 <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi == "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(TUff= sum(TUff, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TUff, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    group_by(ANNO, Area) %>% 
    arrange(Area) %>% 
    mutate(cs = cumsum(N)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -N) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
  
  
}

RNUf <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi== "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(TNonUff= sum(TNonUff, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TNonUff, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    select(-ANNO, -Quarter) %>% 
    pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   

    left_join(  
      (dtanalisi %>%  
         filter(Costi== "Ricavo") %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
         
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         summarise(TNonUff= sum(TNonUff, na.rm = TRUE)) %>% 
         filter(CDC == CC & Classe == "Prestazioni") %>%  
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(TNonUff, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Area) %>%
         summarise(trend = spk_chr(N, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% 
    
    rename("Prestazioni" = Area) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
  
  
  
  
}
#
RNUf2 <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi== "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(TNonUff= sum(TNonUff, na.rm = TRUE)) %>% 
    filter(CDC == CC & Classe == "Prestazioni") %>%  
    group_by(ANNO, Quarter, Area) %>% 
    summarise(N = sum(TNonUff, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
    group_by(ANNO, Area) %>% 
    arrange(Area) %>% 
    mutate(cs = cumsum(N)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -N) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
  
}



















#OLD STUFF----

# cc <- read_delim(here("data", "raw", "coge1921.txt"), 
#                  "\t", escape_double = FALSE, locale = locale(decimal_mark = ",", 
#                                                               grouping_mark = "."), trim_ws = TRUE)


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

#oggetto UI--

# parametri <- tabsetPanel(
#   id = "params", 
#   type = "hidden"
#   
# )

#funzioni--

# to_be <- function(df, Pagamento){
#   if(Pagamento == "Pagamento")
#     dplyr::filter(df, Parametro %in% c("Fatturato","VarFatt"))
#   else filter(df, Parametro %in% c("Tariffato","VarVal"))
# }

# Tplot <- function(df, y_par, y_par2, euro)
# {    
#   p1 <- ggplot(df)+ 
#     aes(
#       y = .data[[y_par]],
#       x = .data[["Quarter"]],  
#       label=paste(as.character(.data[[y_par]]), euro))+
#     geom_line(group = 1, aes(color = ANNO == max(ANNO)), size= 1.1,  )+
#     geom_label(size = 4.5, aes(color = ANNO == max(ANNO)))+
#     scale_color_manual(values = c("grey", "blue"), guide = "none") +
#     facet_grid(~ANNO, switch = "x", scales = "free")+
#     geom_hline(yintercept = 0, size = 0.5)+
#     labs(y = "", x = " ",
#          title = "")+
#     theme_ipsum_rc()+
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           
#           axis.text.y = element_blank(),
#           axis.text.x = element_text(size = 15),
#           
#           strip.text.x = element_text(size = 18))
#   
#   # p2 <- ggplot(df)+ 
#   #   aes(
#   #     y = .data[[y_par2]],
#   #     x = .data[["Quarter"]],  
#   #     label=paste(as.character(.data[[y_par2]]), "%"))+
#   #   geom_line(group = 1, aes(color = ANNO == max(ANNO)), size= 1.1,  )+
#   #   geom_label(size = 4.5, aes(color = ANNO == max(ANNO)))+
#   #   scale_color_manual(values = c("grey", "blue"), guide = "none") +
#   #   facet_grid(~ANNO, switch = "x", scales = "free")+
#   #   geom_hline(yintercept = 0, size = 0.5)+
#   #   labs(y = "", x = " ",
#   #        title = "")+
#   #   theme_ipsum_rc()+
#   #   theme(panel.grid.major = element_blank(),
#   #         panel.grid.minor = element_blank(),
#   #         
#   #         axis.text.y = element_blank(),
#   #         axis.text.x = element_text(size = 15),
#   #         
#   #         strip.text.x = element_text(size = 18))
#   
#   p1
#   #|p2
#   
#   
# }




 