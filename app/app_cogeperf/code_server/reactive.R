output$year <- renderText(input$anno)
output$dipa <- renderText(input$dip)
output$dipa2 <- renderText(input$dip)

IZSLER <- reactive( tizsler %>%
                      filter(Anno == input$anno &
                               MESE == max(MESE)))

pr <- reactive(prj %>% 
                 mutate("Stato" = ifelse(annofine < input$anno, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= input$anno))

prdip <- reactive(
  prj %>% 
    mutate("Stato" = ifelse(annofine < input$anno2, "Archiviato", "Attivo")) %>% 
    filter(Stato == "Attivo" & annoinizio <= input$anno2 & Dipartimento == input$dip))



pubs <- reactive(pub %>% 
                   filter(OA == input$anno))


pubsdip <- reactive(pub %>% 
                      filter(OA == input$anno2 & Dipartimento == input$dip))


FTEPDx <- reactive(FTEPD %>% filter(anno == input$anno))



tdip <- reactive(#questo codice prepara la tabella complessiva dei dipartimenti
  
  if(input$anno >= 2021){   
    
    IZSLER() %>%
      left_join(
        (pubs() %>%
           filter(articoliif == "IF") %>%
           # count(Dipartimento, NR) %>%
           group_by(Dipartimento) %>%  
           # count(NR) %>%
           summarise("Pubblicazioni" = nlevels(factor(NR)), 
                     "Impact Factor" = sum(IF, na.rm = TRUE))), by = "Dipartimento") %>%    
      left_join(
        (pr() %>%
           group_by(Dipartimento) %>%
           summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
        ),  by = "Dipartimento" ) %>% 
      left_join(#aggiungola tabella con i fte programmati per dipartimento
        #ftepDIP, 
        FTEPDx(),  by="Dipartimento"
      ) %>% 
      mutate(RFTE = RT/(FTET*(FTp/100)))#< questo calcola il ricavo fte usando solo la % di fte allocata alle attività istituzionali
  } else
    
    
  {   IZSLER() %>%
      left_join(
        (pubs() %>%
           filter(articoliif == "IF") %>%
           # count(Dipartimento, NR) %>%
           group_by(Dipartimento) %>%  
           # count(NR) %>%
           summarise("Pubblicazioni" = nlevels(factor(NR)), 
                     "Impact Factor" = sum(IF, na.rm = TRUE))), by = "Dipartimento") %>%    
      left_join(
        (pr() %>%
           group_by(Dipartimento) %>%
           summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
        ),  by = "Dipartimento" ) %>% 
      
      mutate(RFTE = RT/(FTET))
  }
  
)

FTEPREPx <- reactive(FTEPREP %>% filter(anno == input$anno2))
#questo codice prepara la tabella dei singoli dipartimenti con i dati dei reparti
tdiprep <- reactive(
  
  if(input$anno2 >= 2021) {  
    
    (tabIZSLER %>% 
       rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, 
               "VP" = TotFattVP, "AI" = TAI,"COSTI" = TotCost) %>%  
       filter(ANNO == input$anno2 & Dipartimento == input$dip) %>%    
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
       filter(MESE == max(MESE))
    ) %>% 
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
        ), by = "Reparto") %>% 
      left_join(
        FTEPREPx(), by = "Reparto"
      ) %>% 
      mutate(RFTE = RT/(FTET*(FTp/100)))
    
  } else {
    (tabIZSLER %>% 
       rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, 
               "VP" = TotFattVP, "AI" = TAI,"COSTI" = TotCost) %>%  
       filter(ANNO == input$anno2 & Dipartimento == input$dip) %>%    
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
       filter(MESE == max(MESE))
    ) %>% 
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
        ), by = "Reparto") %>% 
      
      mutate(RFTE = RT/(FTET))
    
  }
)
