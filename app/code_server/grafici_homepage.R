## grafico coordinate polari Dipartimento-----

plot_dt <- reactive(tdip() %>% ungroup() %>%
                      mutate(Dipartimento = recode(Dipartimento,  "DIPARTIMENTO SICUREZZA ALIMENTARE"  = "DSA",
                                                   "DIPARTIMENTO TUTELA E SALUTE ANIMALE" = "DTSA",
                                                   "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA" = "ATLOMB",
                                                   "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA" = "ATER")) %>%
                      mutate(Prestazioni = round(100*(Prestazioni/sum(Prestazioni)), 1),
                             "RT" = round(100*(RT/sum(RT)),1),
                             "FTET" = round(100*(FTET/ sum(FTET)), 1),
                             "FTED" = round(100*(FTED/ sum(FTED)), 1),
                             "FTEC" = round(100*(FTEC/ sum(FTEC)), 1),
                             Costi = round(100*(COSTI/sum(COSTI)), 1)) %>%
                      select(Dipartimento, Prestazioni, RT, FTED, FTEC, FTET, Costi) %>%
                      pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>%
                      mutate(KPI = factor(KPI, levels = c("Prestazioni", "RT", "FTED", "FTEC", "FTET", "Costi" ))) %>%
                      filter(Dipartimento != "DIREZIONE SANITARIA")
)
# 
# 
# 



output$tbd <- renderPlot(
  
  
  
  if(input$ind == "Dipartimento")
    
  {
    
    ggplot(plot_dt(),  aes(
      x = KPI,
      y = valore,
      fill = KPI
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~Dipartimento, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(),
            strip.text.x = element_text(size = 15, colour = "blue"),
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "")
    
  }
  
  else
    
  {
    plot_dt() %>%
      mutate(Dipartimento = recode(Dipartimento,  "DIPARTIMENTO SICUREZZA ALIMENTARE"  = "DSA",
                                   "DIPARTIMENTO TUTELA E SALUTE ANIMALE" = "DTSA",
                                   "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA" = "ATLOMB",
                                   "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA" = "ATER")) %>%
      ggplot(aes(
        x = Dipartimento,
        y = valore,
        fill = Dipartimento
      )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(),
            strip.text.x = element_text(size = 15, colour = "blue"),
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "")
    
  }, bg = "transparent")
# 
# 
# ##TrendPlot----
# 

Year <- reactive(year(Sys.Date()))

trend <- reactive(
  tizsler %>%
    filter(Dipartimento != "DIREZIONE SANITARIA") %>%
    mutate(Dipartimento = recode(Dipartimento,  "DIPARTIMENTO SICUREZZA ALIMENTARE"  = "DSA",
                                 "DIPARTIMENTO TUTELA E SALUTE ANIMALE" = "DTSA",
                                 "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA" = "ATLOMB",
                                 "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA" = "ATER")) %>%
    filter(MESE == max(MESE)) %>% 
    pivot_longer(!c(Anno,MESE, Dipartimento), names_to = "KPI", values_to = "valore") %>% 
    filter(KPI == "Prestazioni") %>%
    group_by(Dipartimento) %>%
    arrange(Dipartimento, Anno) %>%
    filter(Anno != Year()) %>% 
    mutate(Var = round((valore/lag(valore)-1)*100, 2))
)
# 
# 
output$ptrend <- renderPlot({
  req(input$kpi)
  ggplot(trend())+
    aes(
      y = .data[["valore"]],
      x = .data[["Anno"]])+
    geom_ribbon(aes(ymin = 0, ymax = (.data[["valore"]])+0.1*(.data[["valore"]])), alpha=0.0)+
    geom_point(aes(y = .data[["valore"]]), size= 10, color = "blue", alpha = 0.2) +
    geom_line(aes(y = .data[["valore"]]))+
    facet_wrap(facets = ~Dipartimento, nrow=1, scales = "free")+
    scale_x_continuous(breaks = unique(trend()$Anno), expand=c(0.16, 0))+
    geom_text(data = dplyr::filter(trend(), Anno == 2020), aes(label = sprintf('%+0.1f%%',.data[["Var"]])),
              x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=8)+
    geom_text(data = dplyr::filter(trend(), Anno == 2021), aes(label = sprintf('%+0.1f%%', .data[["Var"]])),
              x = 2020.5, y = 0, vjust = -1, fontface = 'bold', size=8)+
    geom_text(aes(label = sprintf('%0.1f',.data[["valore"]]), y = .data[["valore"]]), vjust = -1,
              size=6, color="blue", fontface = "bold")+
    labs(y = "", x = " ",
         title = "")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20, color = "blue"),
          axis.line.x = element_line(),
          
          strip.text.x = element_text(size = 15, color = "blue"),
          panel.background= element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          
    )
  
}, bg="transparent"
)