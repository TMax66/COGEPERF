

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





df <- dtanalisi %>% filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Costi=="Ricavo") %>% 
  group_by(CDC,  ANNO,  Quarter) %>% 
  summarise(Ufficiali = sum(TUff, na.rm = T), 
            NonUfficiali = sum(TNonUff, na.rm = T), 
            Gratuiti = sum(TGratuito, na.rm = T),
            Pagamento = sum(TPagamento, na.rm = T), 
            Vprod = sum(TVP, na.rm= T), 
            AI = sum(TAI, na.rm = T)) %>%
  mutate(CumUff = cumsum(Ufficiali),
         CumNonUff = cumsum(NonUfficiali), 
         CumGrat = cumsum(Gratuiti), 
         CumPag = cumsum(Pagamento), 
         CumVprod = cumsum(Vprod), 
         CumAI = cumsum(AI)) %>% 
  rowwise() %>% 
  mutate(TotRic = round(sum(Ufficiali, NonUfficiali, na.rm = T),2)) %>% 
  ungroup %>% 
  mutate(CumTotRic = cumsum(TotRic)) %>% View()
  ungroup() %>% 
  
  arrange(Quarter) %>% 
  mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
         VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
         VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
         VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
         VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
         VarAI = round((AI/lag(AI)-1)*100,2), 
         VarTot = round((TotRic/lag(TotRic)-1)*100, 2),
         
         VarCumUff = round((CumUff/lag(CumUff)-1)*100, 2), 
         VarCumNonUff = round((CumNonUff/lag(CumNonUff)-1)*100, 2), 
         VarCumGrat = round((CumGrat/lag(CumGrat)-1)*100, 2), 
         VarCumPag = round((CumPag/lag(CumPag)-1)*100, 2), 
         VarCumVprod = round((CumVprod/lag(CumVprod)-1)*100, 2), 
         VarCumAI = round((CumAI/lag(CumAI)-1)*100, 2), 
         VarCumTotRic = round((CumTotRic/lag(CumTotRic)-1)*100, 2) )  






Tplot(df,"CumTotRic","VarCumTotRic"  )







































































dtAtt <-  dtanalisi %>% filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Costi=="Ricavo") %>% 
                     group_by(CDC,  ANNO,  Quarter) %>% 
                     summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                               EUff = sum(AttUff, na.rm = TRUE), 
                               ENUff =sum(AttNUff, na.rm = TRUE), 
                               EPag = sum(AttPag, na.rm = TRUE), 
                               Egrat = sum(AttGrat, na.rm = TRUE), 
                               PI = sum(AI , na.rm = TRUE), 
                               Prodv = sum(VP, na.rm = TRUE)) %>% 
                     mutate(CumEsami = cumsum(N.Esami), 
                            CumEUff = cumsum(EUff), 
                            CumENUff = cumsum(ENUff), 
                            CumEPag = cumsum(EPag), 
                            CumEgrat = cumsum(Egrat), 
                            CumPI = cumsum(PI), 
                            CumProdv = cumsum(Prodv)) %>% 
  ungroup() %>% 
  arrange(Quarter) %>%  
  mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
         VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
         VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
         VarEPag = round((EPag/lag(EPag)-1)*100, 2),
         VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
         VarPI = round((PI/lag(PI)-1)*100, 2), 
         VarProdv = round((Prodv/lag(Prodv)-1)*100, 2), 
         VarCumEs = round((CumEsami/lag(CumEsami)-1)*100,2), 
         VarCumEUff = round((CumEUff/lag(CumEUff)-1)*100,2), 
         VarCumENUff = round((CumENUff/lag(CumENUff)-1)*100,2), 
         VarCumEPag = round((CumEPag/lag(CumEPag)-1)*100,2),
         VarCumEgrat = round((CumEgrat/lag(CumEgrat)-1)*100,2),
         VarCumPI = round((CumPI/lag(CumPI)-1)*100,2),
         VarCumProdv = round((CumProdv/lag(CumProdv)-1)*100,2)
  )  %>% View()


  
  dtT <- dtanalisi %>% filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Costi=="Ricavo") %>% 
                    group_by(CDC,  ANNO,  Quarter) %>% 
                    summarise(Ufficiali = sum(TUff, na.rm = T), 
                              NonUfficiali = sum(TNonUff, na.rm = T), 
                              Gratuiti = sum(TGratuito, na.rm = T),
                              Pagamento = sum(TPagamento, na.rm = T), 
                              Vprod = sum(TVP, na.rm= T), 
                              AI = sum(TAI, na.rm = T)) %>%
                    mutate(CumUff = cumsum(Ufficiali),
                           CumNonUff = cumsum(NonUfficiali), 
                           CumGrat = cumsum(Gratuiti), 
                           CumPag = cumsum(Pagamento), 
                           CumVprod = cumsum(Vprod), 
                           CumAI = cumsum(AI)) %>% 
                   rowwise() %>% 
                   mutate(TotRic = round(sum(Ufficiali, NonUfficiali, na.rm = T),2)) %>%
                   ungroup %>% 
                   mutate(CumTotRic = cumsum(TotRic)) %>% 
                   ungroup() %>% 
                   
                   arrange(Quarter) %>% 
                   mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
                           VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
                           VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
                           VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
                           VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
                           VarAI = round((AI/lag(AI)-1)*100,2), 
                           VarTot = round((TotRic/lag(TotRic)-1)*100, 2),
                           
                           VarCumUff = round((CumUff/lag(CumUff)-1)*100, 2), 
                           VarCumNonUff = round((CumNonUff/lag(CumNonUff)-1)*100, 2), 
                           VarCumGrat = round((CumGrat/lag(CumGrat)-1)*100, 2), 
                           VarCumPag = round((CumPag/lag(CumPag)-1)*100, 2), 
                           VarCumVprod = round((CumVprod/lag(CumVprod)-1)*100, 2), 
                           VarCumAI = round((CumAI/lag(CumAI)-1)*100, 2), 
                           VarCumTotRic = round((CumTotRic/lag(CumTotRic)-1)*100, 2) )  

       

  dtCostiT <- dtanalisi %>% filter(CDC== "SEDE TERRITORIALE DI BERGAMO" & Costi=="Costo") %>% 
                         group_by(CDC,  ANNO,  Quarter) %>% 
                         summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                         mutate(CumCosti = cumsum(Costi)) %>% 
                         ungroup () %>% 
                         arrange(Quarter) %>% 
                         mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2, 
                                VarCumCosti = round((CumCosti/lag(CumCosti)-1)*100), 2) 














































dtanalisi %>%  
  filter(`Costo o Ricavo`== "Costo") %>% 
  group_by(Anno, Quarter, Dipartimento, Reparto, Laboratorio, `Centro di Costo`,ClassAnalisi, Classe, Area) %>% 

  summarise(costidett = sum(Costo, na.rm = TRUE),
             )  %>%  
  filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO"  ) %>% 
  group_by(Anno, Quarter,Classe) %>%  
  summarise(C = sum(costidett, na.rm=TRUE)) %>% 
  mutate(YQ = paste(Anno, "-", Quarter)) %>%  ungroup() %>% 
  select(-Anno, -Quarter) %>%  
  pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
 
  left_join(  
    
    (dtanalisi %>% 
       filter(`Costo o Ricavo`== "Costo") %>% 
       group_by(Anno, Quarter, Dipartimento, Reparto, Laboratorio, `Centro di Costo`,ClassAnalisi, Classe, Area) %>% 
       #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
       summarise(costidett = sum(Costo, na.rm = TRUE),
       )  %>%  
       filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO"  ) %>% 
       group_by(Anno, Quarter, Classe) %>% 
       summarise(C = sum(costidett, na.rm=TRUE)) %>% 
       mutate(YQ = paste(Anno, "-", Quarter)) %>%
       select(-Anno, -Quarter) %>% 
       group_by(Classe) %>%
       summarise(trend = spk_chr(C, type= "line", options =
                                   list(paging = FALSE)))
    )) %>% rename("Tipologia Costi" = Classe) %>% 
  
  format_table()  %>% 
  htmltools::HTML() %>% 
  div() %>% 
  spk_add_deps()





































dtCostiT <- dtanalisi %>% filter(`Centro di Costo`== "SEDE TERRITORIALE DI BERGAMO" & `Costo o Ricavo`=="Costo") %>% 
  group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
  summarise(Costi = sum(Costo, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(VarCosti = round((Costi/lag(Costi)-1)*100,2))  
  

Tplot(dtCostiT, "Costi", "VarCosti", euro="€")



























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
