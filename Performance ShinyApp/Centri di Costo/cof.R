tb2 <- reactive({tdsa %>% 
    filter(Reparto != "Totale") %>% 
    mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
           "RA" = round(100*(RA/sum(RA)),1), 
           "FTED" = round(100*(FTED/sum(FTED)),1), 
           "FTEC" = round(100*(FTEC/sum(FTEC)),1),
           "RVP" =round(100*(RVP/sum(RVP)),1), 
           "RAI" = round(100*(RAI/sum(RAI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTET/ sum(FTET)), 1) 
           # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
    ) %>% 
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT") %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
})

output$tbd2 <- renderPlot( 
  
  if(input$ind2 == "Reparto")
    
  {  
    
    ggplot(tb2(),  aes( 
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
            strip.text.x = element_text(size = 12, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "") 
    
  }
  
  else
    
  {
    
    tb2() %>% 
      mutate(Reparto = recode(Reparto, "REPARTO PRODUZIONE PRIMARIA" = "RPP", 
                              "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM", 
                              "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB", 
                              "REPARTO CONTROLLO ALIMENTI" = "RCA")) %>% 
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


