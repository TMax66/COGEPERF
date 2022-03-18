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


AU <- function(CC = input$CC, tipo){
  
  dtanalisi %>% 
    filter(Uff == tipo & Costi == "Ricavo") %>% 
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
         filter(Uff == tipo & Costi == "Ricavo") %>% 
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


AU2 <- function(CC = input$CC, tipo){
  dtanalisi %>%  
    filter(Uff == tipo & Costi == "Ricavo") %>% 
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



CostiN <- function(CC = input$CC){
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == CC  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
    select(-ANNO, -Quarter) %>%  
    pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
    
    left_join(  
      
      (dtanalisi %>% 
         filter(Costi == "Costo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         summarise(costidett = sum(Costo, na.rm = TRUE),
         )  %>%  
         filter(CDC == CC ) %>% 
         group_by(ANNO, Quarter, Classe) %>% 
         summarise(C = sum(costidett, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Classe) %>%
         summarise(trend = spk_chr(C, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Tipologia Costi" = Classe) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
}


CostiP <- function(CC = input$CC)
{
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == CC  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>%  
    group_by(ANNO, Classe) %>% 
    arrange(Classe) %>% 
    mutate(cs = cumsum(C)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -C) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
  
}

CostigestN <- function(CC = input$CC2){
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == CC  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
    select(-ANNO, -Quarter) %>%  
    pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
    
    left_join(  
      
      (dtanalisi %>% 
         filter(Costi == "Costo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(costidett = sum(Costo, na.rm = TRUE),
         )  %>%  
         filter(CDC == CC ) %>% 
         group_by(ANNO, Quarter, Classe) %>% 
         summarise(C = sum(costidett, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Classe) %>%
         summarise(trend = spk_chr(C, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Tipologia Costi" = Classe) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
}


CostigestP <- function(CC = input$CC2){
  
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == CC  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>%  
    group_by(ANNO, Classe) %>% 
    arrange(Classe) %>% 
    mutate(cs = cumsum(C)) %>% 
    ungroup() %>% 
    select(-ANNO, -Quarter, -C) %>%
    pivot_wider( names_from = YQ,  values_from = cs, values_fill = 0) %>% 
    format_table() %>% 
    htmltools::HTML() 
  
}

