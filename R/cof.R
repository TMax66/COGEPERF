


tdiprep <- reactive(
  (tabIZSLER %>% 
     filter(Anno == input$anno2 & Dipartimento == input$dip) %>% 
     rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
             "COSTI" = costi) %>%
     group_by(Reparto) %>%
     summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
     mutate(RT = (VALORE+VP+AI),
            FTE_T = round((FTED+FTEC),1)) %>%
     arrange(desc(ANALISI)) %>%
     mutate("R-FTE" = round(RT/FTE_T,0), 
            "C-FTE" = round(COSTI/FTE_T, 0), 
            "ROI" = round(RT/COSTI, 2)) %>% 
     select(-FTED, -FTEC)) %>% 
    left_join(
      (pubsdip() %>%
         filter(articoliif == "IF") %>%
         count(Reparto, NR) %>%
         group_by(Reparto) %>%  
         count(NR) %>%
         summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>%    
    left_join(
      (prdip() %>%
         group_by(Reparto) %>%
         summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
      ), by = "Reparto")
)












