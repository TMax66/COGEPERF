## grafico coordinate polari Reparti----


plot_dt2 <- reactive(tdiprep() %>% ungroup() %>%
                       mutate(Reparto = recode(Reparto,  "REPARTO PRODUZIONE PRIMARIA" = "RPP",
                                               "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM",
                                               "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB",
                                               "REPARTO CONTROLLO ALIMENTI" = "RCA",
                                               "REPARTO VIROLOGIA" = "RVIR",
                                               "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB",
                                               "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB",
                                               "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA",
                                               "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN",
                                               "SEDE TERRITORIALE DI BRESCIA" = "BS",
                                               "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO",
                                               "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI",
                                               "SEDE TERRITORIALE DI PAVIA" = "PV",
                                               "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE",
                                               "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA",
                                               "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR",
                                               "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE",
                                               "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "GCR",
                                               "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "AREG",
                                               "FORMAZIONE E BIBLIOTECA" = "FORMAZIONE",
                                               "SORVEGLIANZA EPIDEMIOLOGICA" = "SORVEPID" )) %>%
                       mutate(Prestazioni = round(100*(Prestazioni/sum(Prestazioni)), 1),
                              "RT" = round(100*(RT/sum(RT)),1),
                              "FTET" = round(100*(FTET/ sum(FTET)), 1),
                              "FTED" = round(100*(FTED/ sum(FTED)), 1),
                              "FTEC" = round(100*(FTEC/ sum(FTEC)), 1),
                              Costi = round(100*(COSTI/sum(COSTI)), 1)) %>%
                       select(Reparto, Prestazioni, RT, FTED, FTEC, FTET, Costi) %>%
                       pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>%
                       mutate(KPI = factor(KPI, levels = c("Prestazioni", "RT", "FTED", "FTEC", "FTET", "Costi" ))) #%>%
                     #filter(Dipartimento != "DIREZIONE SANITARIA")
)
# 
# 
# 
output$tbd2<- renderPlot(
  
  if(input$ind2 == "Reparto")
    
  {
    
    ggplot(plot_dt2(),  aes(
      x = KPI,
      y = valore,
      fill = KPI
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
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
    plot_dt2() %>%
      ggplot(aes(
        x = Reparto,
        y = valore,
        fill = Reparto
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
# ## TrendPlot Reparti----
# 
trendRep <- reactive(
  
  tabIZSLER %>%
    mutate(Reparto = recode(Reparto,  "REPARTO PRODUZIONE PRIMARIA" = "RPP",
                            "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM",
                            "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB",
                            "REPARTO CONTROLLO ALIMENTI" = "RCA",
                            "REPARTO VIROLOGIA" = "RVIR",
                            "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB",
                            "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB",
                            "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA",
                            "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN",
                            "SEDE TERRITORIALE DI BRESCIA" = "BS",
                            "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO",
                            "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI",
                            "SEDE TERRITORIALE DI PAVIA" = "PV",
                            "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE",
                            "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA",
                            "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR",
                            "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE",
                            "GESTIONE CENTRALIZZATA DELLE RICHIESTE" = "GCR",
                            "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA" = "AREG",
                            "FORMAZIONE E BIBLIOTECA" = "FORMAZIONE",
                            "SORVEGLIANZA EPIDEMIOLOGICA" = "SORVEPID" )) %>%
    
    rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, 
            "VP" = TotFattVP, "AI" = TAI,"COSTI" = TotCost) %>%  
    filter( Dipartimento == input$dip) %>%    
    group_by(Reparto, ANNO, MESE) %>%   
    summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%  
    mutate(RT = (Valorizzazione+VP+AI),
           FTET = FTED+FTEC,
           Prestazioni = cumsum(Prestazioni), 
           Valorizzazione = cumsum(Valorizzazione), 
           VP = cumsum(VP), 
           AI = cumsum(AI), 
           COSTI = cumsum(COSTI), 
           RT = cumsum(RT)) %>%
    filter(Prestazioni >0) %>% 
    filter(MESE == max(MESE)) %>%  
    pivot_longer(!c(ANNO,Reparto), names_to = "KPI", values_to = "valore") %>%
    filter(KPI == input$kpi2) %>%
    group_by(Reparto) %>%
    arrange(Reparto, ANNO) %>%
    rename(Anno = ANNO) %>% 
    filter(Anno != Year()) %>% 
    mutate(Var = round((valore/lag(valore)-1)*100, 2)) 
)
# 
output$ptrendRep <- renderPlot({
  req(input$kpi2)
  ggplot(trendRep())+
    aes(
      y = .data[["valore"]],
      x = .data[["Anno"]])+
    geom_ribbon(aes(ymin = 0, ymax = (.data[["valore"]])+0.1*(.data[["valore"]])), alpha=0.0)+
    geom_point(aes(y = .data[["valore"]]), size= 10, color = "blue", alpha = 0.2) +
    geom_line(aes(y = .data[["valore"]]))+
    facet_wrap(facets = ~Reparto, nrow=1, scales = "free")+
    scale_x_continuous(breaks = unique(trendRep()$Anno), expand=c(0.16, 0))+
    geom_text(data = dplyr::filter(trendRep(), Anno == 2020), aes(label = sprintf('%+0.1f%%',.data[["Var"]])),
              x = 2019.5, y = 0, vjust = -1, fontface = 'bold', size=8)+
    geom_text(data = dplyr::filter(trendRep(), Anno == 2021), aes(label = sprintf('%+0.1f%%', .data[["Var"]])),
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