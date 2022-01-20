dtAtt <- dtanalisi %>% filter(CDC == "SEDE TERRITORIALE DI BERGAMO" & Costi=="Ricavo") %>% 
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
                     )
                   



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





