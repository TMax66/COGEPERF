dtanalisi %>% filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO" & `Costo o Ricavo`=="Costo") %>% 
  group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
  summarise(Costi = sum(Costo, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(VarCosti = round((Costi/lag(Costi)-1)*100,2)) %>% View()



























library(tidyverse)
library(readr)
library(here)
 
library(formattable)
library(sparkline)
 


dtanalisi %>%  
  filter(`Costo o Ricavo`== "Ricavo") %>% 
  filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
  rowwise() %>% 
  mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
  group_by(Anno, Quarter, Dipartimento, Reparto, Laboratorio, `Centro di Costo`,ClassAnalisi, Classe, Area) %>% 
  summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
  filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO" & Classe == "Prestazioni") %>%  
  group_by(Anno, Quarter, Area) %>% 
  summarise(N = sum(TRic, na.rm=TRUE)) %>% 
  mutate(YQ = paste(Anno, "-", Quarter)) %>% ungroup() %>% 
  select(-Anno, -Quarter) %>% 
  pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
  #rename(., "Prestazione" = Area) %>% 
    left_join(  

 (dtanalisi %>% 
    filter(`Costo o Ricavo`== "Ricavo") %>% 
    filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
    rowwise() %>% 
    mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
    group_by(Anno, Quarter, Dipartimento, Reparto, Laboratorio, `Centro di Costo`,ClassAnalisi, Classe, Area) %>% 
    summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
    filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO" & Classe == "Prestazioni") %>%  
  group_by(Anno, Quarter, Area) %>% 
  summarise(N = sum(TRic, na.rm=TRUE)) %>% 
  mutate(YQ = paste(Anno, "-", Quarter)) %>%
  select(-Anno, -Quarter) %>% 
  group_by(Area) %>%
  summarise(trend = spk_chr(N, type= "line", options =
                              list(paging = FALSE)))
 )) %>% 

formattable()  %>% 
  as.htmlwidget() %>% 
  spk_add_deps()
 

      
   
  
# mutate(m=round(rowMeans(cbind( `2019_Q 1`, `2019_Q 2`, `2019_Q 3`, `2019_Q 4`, 
#                            `2020_Q 1`, `2020_Q 2`, `2020_Q 3`, `2020_Q 4`,
#                            `2021_Q 1`, `2021_Q 2`), na.rm = TRUE),2), 
#          trend = round((`2019_Q 1`- `2020_Q 4`)/`2019_Q 1`*100, 2) ) %>%
#   #select(Prestazione, 2:9, trend, 10:11) %>% 
#   formattable(list(trend = improvement_formatter)) %>% 
#   # as.datatable()
#   



# customGreen0 = "#DeF7E9"
# customGreen = "#71CA97"
# customRed = "#ff7f7f"
# 
# 
# 
# improvement_formatter <- formatter("span", 
#                                    style = x ~ style(font.weight = "bold", 
#                                                      color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
#                                    x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
# )
