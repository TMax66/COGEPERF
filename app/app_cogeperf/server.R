server <- function(input, output, session) { 

options(shiny.reactlog=TRUE)
  
 

 
## reactive----
#source(file = "code_server/reactive.r", local = TRUE)
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
             filter(articoliif == "IF"|is.na(articoliif)) %>%
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
             filter(articoliif == "IF"|is.na(articoliif)) %>%
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
             filter(articoliif == "IF"|is.na(articoliif)) %>%
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
             filter(articoliif == "IF"|is.na(articoliif)) %>%
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
  

## valuebox----
#source(file = "code_server/valuebox.r", local = TRUE)
  # VALUEBOX HOMEPAGE----
  
  
  output$esami <- renderValueBox(
    ValueBOX(IZSLER(), "Prestazioni", Titolo = "N. Prestazioni", colore = "blue", icona = "flask")
  )
  output$ricavi <- renderValueBox(
    ValueBOX(IZSLER(), "Valorizzazione", Titolo = "Valorizzazione da Tariffario", colore = "blue", icona = "euro")
  )
  output$venprod <- renderValueBox(
    ValueBOX(IZSLER(), "VP", Titolo = "Vendita Prodotti", colore = "blue", icona = "euro")
  )
  
  output$attint <- renderValueBox(
    ValueBOX(IZSLER(), "AI", Titolo = "Attività Interna", colore = "blue", icona = "euro")
  )
  
  output$rictot <- renderValueBox(
    ValueBOX(IZSLER(), "RT", Titolo = "Ricavi Totali", colore = "blue", icona = "euro")
  )
  
  # output$RFTE <- renderValueBox(
  #   ValueBOX(IZSLER(), Variabile = "RT", Variabile2 = "FTE_T",  Titolo = "Ricavo per Full Time Equivalente", colore = "blue", icona = "euro")
  # )
  
  
  ftp <-  reactive({
    FTp %>% 
      add_row(anno = 2020, FTp = 100) %>% 
      add_row(anno = 2019, FTp = 100) %>% 
      filter(anno == input$anno)})
  
  rfteDip <- reactive(tdip() %>% ungroup() %>%
                        summarise(rt=sum(RT),
                                  ft=sum(FTET))%>% 
                        bind_cols(ftp()) %>%  
                        mutate(rfte=rt/(ft*FTp/100)) %>%   
                        select(rfte) %>%
                        unlist()
  )
  
  
  # ftp <- reactive(FTp %>%
  # filter(anno == input$anno))
  # 
  # rfteDip <- reactive(tdip() %>% ungroup() %>%
  #                       summarise(rt=sum(RT),
  #                                 ft=sum(FTET))%>%
  #                       bind_cols(ftp()) %>%
  #                       mutate(rfte=rt/(ft*(FTp/100))) %>%
  #                       
  #                       
  #                       select(rfte) %>%
  #                       unlist()
  # )
  
  output$RFTE <- renderValueBox(
    valueBox( prettyNum(rfteDip(), big.mark = ".", decimal.mark = ",") ,
              subtitle = "Ricavo Full Time Equivalente", color = "blue",
              icon = icon("euro")
    )
  )
  # 
  output$Costi <- renderValueBox(
    ValueBOX(IZSLER(), Variabile = "COSTI", Titolo = "Costi", colore = "blue", icona = "euro")
  )
  
  
  
  output$PR <- renderValueBox({
    valueBox(
      (  pr() %>%
           summarise(n = nlevels(factor(Codice)))
      ), "Progetti di ricerca in corso (scarica elenco) ", icon = icon("user-graduate"), color = "light-blue")
  })
  # 
  output$IF <- renderValueBox({
    valueBox(
      (pubs() %>%
         filter(articoliif == "IF"|is.na(articoliif)) %>%
         group_by(NR) %>%
         count(NR) %>%
         select(NR) %>%
         nrow()),  "Articoli pubblicati su riviste peer-review con IF (scarica elenco)", icon = icon("book"), color = "light-blue")
  })
  
  # output$Int <- renderValueBox({
  #   valueBox(
  #     (pubs() %>%
  #        filter(articoliif == "Int") %>%
  #        group_by(NR) %>%
  #        count(NR) %>%
  #        select(NR) %>%
  #        nrow()
  #     ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
  # })
  
  
  # VALUEBOX PAGINE DIPARTIMENTI----
  
  # ##ValueBOX Dipartimento/Reparto----
  # 
  # 
  output$esamidip <- renderValueBox(
    ValueBOX(tdiprep(), "Prestazioni", Titolo = "N.Prestazioni", colore = "blue", icona = "flask")
  )
  # 
  output$ricavidip <- renderValueBox(
    ValueBOX(tdiprep(), "Valorizzazione", Titolo = "Valorizzazioni da Tariffario", colore = "blue", icona = "euro")
  )
  
  output$venproddip <- renderValueBox(
    ValueBOX(tdiprep(), "VP", Titolo = "Vendita Prodotti", colore = "blue", icona = "euro")
  )
  # 
  output$attintdip <- renderValueBox(
    ValueBOX(tdiprep(), "AI", Titolo = "Attività Interna", colore = "blue", icona = "euro")
  )
  # 
  output$rictotdip <- renderValueBox(
    ValueBOX(tdiprep(), "RT", Titolo = "Ricavi Totali", colore = "blue", icona = "euro")
  )
  
  
  # rftedip <- reactive({
  #     tizsler %>%
  #     left_join(#aggiungola tabella con i fte programmati per dipartimento
  #       FTEPREP, by="Dipartimento"
  #     ) %>%
  #     mutate(RFTE = RT/(FTET*(FTp/100))) %>%
  #     filter(Dipartimento == input$dip & Anno == input$anno2) })
  
  
  rftedip <-  reactive({
    
    if(input$anno2 >= 2021)
    {   
      tizsler %>% 
        left_join(#aggiungola tabella con i fte programmati per dipartimento
          (FTEPD %>% 
             filter(anno == input$anno2)), by="Dipartimento"
        ) %>% 
        mutate(RFTE = RT/(FTET*(FTp/100))) %>% 
        filter(Dipartimento == input$dip & Anno == input$anno2 & MESE == max(MESE))
    } else {    
      
      tizsler %>% 
        mutate(RFTE = RT/(FTET)) %>% 
        filter(Dipartimento == input$dip & Anno == input$anno2 & MESE == max(MESE))
    }
  })
  
  
  
  
  
  output$RFTEdip <- renderValueBox(
    
    ValueBOX(rftedip(), Variabile = "RFTE", Titolo =  "Ricavo Full Time Equivalente", colore = "blue", icona = "euro")
  )
  
  output$Costidip <- renderValueBox(
    ValueBOX(tdiprep(), Variabile = "COSTI",   Titolo = "Costi", colore = "blue", icona = "euro")
  )
  
  
  output$PRdip <- renderValueBox({
    valueBox(
      (  prdip() %>%
           summarise(n = nlevels(factor(Codice)))
      ), "Progetti di ricerca in corso (scarica elenco) ", icon = icon("user-graduate"), color = "light-blue")
  })
  
  output$IFdip <- renderValueBox({
    valueBox(
      (pubsdip() %>%
         filter(articoliif == "IF"|is.na(articoliif)) %>%
         group_by(NR) %>%
         count(NR) %>%
         select(NR) %>%
         nrow()),  "Articoli pubblicati su riviste peer-review con IF (scarica elenco)", icon = icon("book"), color = "light-blue")
  })
  
  
  
  
  
  

#tabella HomePage####

output$t <- renderUI({
    border <- officer::fp_border()
   flextable(tdip(),
      col_keys = c("Dipartimento", "Prestazioni", "Valorizzazione",
                   "VP", "AI",  "RT","COSTI", "FTED", "FTEC","FTET", "FTp", "RFTE",
                   "Pubblicazioni", "Progetti di Ricerca")
      ) %>%
      theme_booktabs() %>%
      color(i = 1, color = "blue", part = "header") %>%
      bold( part = "header") %>%
      fontsize(size=15) %>%
      fontsize(part = "header", size = 15) %>%
      line_spacing(space = 2.5) %>%
      autofit() %>%
      colformat_double(j = c( "Valorizzazione", "VP", "AI", "COSTI",  "RT",  "RFTE"), big.mark = ".", decimal.mark = ",", prefix = "€", digits = 0) %>%
      colformat_double(j= c("Prestazioni", "FTET"), big.mark = ".", decimal.mark = "," ,  digits = 2) %>%
      colformat_double(j= c("FTp"), big.mark = ".", decimal.mark = "," ,  digits = 0, suffix = "%") %>%

     footnote(i=1, j=2:12,
              value = as_paragraph(
                c("Attività analitica/Altre Prestazioni",
                  "Valorizzazione da Tariffario",
                  "Fatturato da Vendita Prodotti",
                  "Valorizzazione dell'Attività Interna",
                  "Ricavo Totale",
                  "Costi complessivi",
                  "Full Time Equivalenti Dirigenza",
                  "Full Time Equivalenti Comparto",
                  "Full Time Equivalenti Totale",
                  "Full Time Equivalenti Programmati per l'attività analitica",
                  "Ricavo per Full Equivalenti Programmati")
              ),
              ref_symbols = c("a","b","c","d","e","f","g","h", "i", "l", "m"),
              part = "header", inline = T) %>%
     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
     htmltools_value()

})
# grafici Homepage----
#source(file = "code_server/grafici_homepage.r", local = TRUE)
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
      filter(KPI == input$kpi) %>%
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
      geom_text(data = dplyr::filter(trend(), Anno == 2022), aes(label = sprintf('%+0.1f%%', .data[["Var"]])),
                x = 2021.5, y = 0, vjust = -1, fontface = 'bold', size=8)+
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
  
  
  
  
# tabelle modali home page----
#source(file = "code_server/tabmodalihomePage.r", local = TRUE)
  Prj <- reactive({
    pr() %>%
      group_by(CodIDIzsler, Tipologia, DataInizio, DataFine, Descrizione, RespScientifico) %>%
      summarise(Budget = sum(BudgetUO)) %>%
      select(-Budget)#, nUO = n()) # %>%
    #   ungroup() %>%
    #     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>%
    #    # summarise(Durata = as.numeric(DataFine-DataInizio),
    #              # R = as.numeric(date("2019-12-31")-date(DataInizio)),
    #               #Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
    #               #Realizzazione = paste(round(Realizzazione, 0), "%") )%>%
    #     mutate(DataInizio = as.character(DataInizio),
    #            DataFine = as.character(DataFine)) #%>%
    #     #arrange(Realizzazione) %>%
    #    # select(-R, -Durata)
    #
  })
  # 
  output$projr <- renderDT(Prj(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                           extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                 paging = TRUE,autoWidth = TRUE,
                                                                 buttons = c('excel')))
  # ##tabella pubblicazioni IZSLER----
  #  
  paper <- reactive({
    pubs() %>%
      select(NR, Autori = "CAU" , `TITOLO RIVISTA`= "JO","TITOLO" = `TI-INGLESE`,  "IF" ) %>%
      unique() %>%
      arrange(desc(IF))
  })
  # 
  output$articoli <- renderDT(server = FALSE,{
    datatable(paper(), class = 'cell-border stripe', rownames=FALSE,
              extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                    paging = TRUE,autoWidth = TRUE,
                                                    buttons = c('excel')))
  })

##tabella Dipartimento/Reparto----

output$tr <- renderUI({
  border <- officer::fp_border()
  flextable(tdiprep(), 
            
            col_keys = c("Reparto", "Prestazioni", "Valorizzazione", "VP", "AI", "RT",  "COSTI",
                         "FTED",  "FTEC",  "FTET",  "FTp",  "RFTE", "Pubblicazioni", "Progetti di Ricerca")
  ) %>%  
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>%
    bold( part = "header") %>%
    fontsize(size=15) %>%
    fontsize(part = "header", size = 15) %>%
    line_spacing(space = 2.5) %>% 
    autofit() %>%
    colformat_num(j = c( "Valorizzazione", "VP", "AI", "COSTI",  "RT", "RFTE" ), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
    colformat_num(j= c("Prestazioni", "FTET"), big.mark = ".", decimal.mark = "," , digits = 2) %>% 
    colformat_num(j= c("FTp" ), big.mark = ".", decimal.mark = ",", digits = 2, suffix = "%") %>% 
    
    footnote(i=1, j=2:12,
             value = as_paragraph(
               c("Attività analitica/Altre Prestazioni",
                 "Valorizzazione da Tariffario",
                 "Fatturato da Vendita Prodotti",
                 "Valorizzazione dell'Attività Interna",
                 "Ricavo Totale",
                 "Costi complessivi",
                 "Full Time Equivalenti Dirigenza",
                 "Full Time Equivalenti Comparto",
                 "Full Time Equivalenti Totale",
                 "Full Time Equivalenti Programmati per l'attività analitica",
                 "Ricavo per Full Equivalenti Programmati")
             ),
             ref_symbols = c("a","b","c","d","e","f","g","h", "i", "l", "m"),
             part = "header", inline = T) %>%
    fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
    
    
    htmltools_value() 
            

})

#grafici pagine dipartimenti----
#source(file = "code_server/grafici_paginedipartimenti.r", local = TRUE)
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
      geom_text(data = dplyr::filter(trendRep(), Anno == 2022), aes(label = sprintf('%+0.1f%%', .data[["Var"]])),
                x = 2021.5, y = 0, vjust = -1, fontface = 'bold', size=8)+
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
  
  
# tabelle modali pagine dipartimenti----
#source(file = "code_server/tabmodalidipartimenti.r", local = TRUE)
  Prjdip <- reactive({
    prdip() %>%
      group_by(CodIDIzsler, Tipologia, DataInizio, DataFine, Descrizione, RespScientifico) %>%
      summarise(Budget = sum(BudgetUO)) %>%
      select(-Budget)#, nUO = n()) #%>%
    #   ungroup() %>%
    #     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>%
    #    # summarise(Durata = as.numeric(DataFine-DataInizio),
    #              # R = as.numeric(date("2019-12-31")-date(DataInizio)),
    #               #Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
    #               #Realizzazione = paste(round(Realizzazione, 0), "%") )%>%
    #     mutate(DataInizio = as.character(DataInizio),
    #            DataFine = as.character(DataFine)) #%>%
    #     #arrange(Realizzazione) %>%
    #    # select(-R, -Durata)
    #
  })
  # 
  output$projrep <- renderDT(Prjdip(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                             extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                   paging = TRUE,autoWidth = TRUE,
                                                                   buttons = c('excel')))
  # 
  # ##tabella modale pubblicazioni dip----
  # 
  paper2 <- reactive({
    pubsdip() %>%
      select(  Autori = "CAU" , `TITOLO RIVISTA`= "JO","TITOLO" = `TI-INGLESE`,  "IF" ) %>%
      unique() %>%
      arrange(desc(IF))
  })
  # 
  output$articolidip <- renderDT(paper2(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                       paging = TRUE,autoWidth = TRUE,
                                                                       buttons = c('excel')))
  # 
  # 
  
  
# Report rfte dipartimenti e reparti----

  ### Dipartimento----
  
  #### andamento mensile----
  p1 <- reactive({dtmensili%>% 
      mutate(MESE = as.factor(MESE),
             Anno = as.factor(Anno)) %>% 
      filter(Dipartimento == input$dip) %>% 
      ggplot()+
      aes(x = MESE, y = RFTE, group = Anno)+
      geom_point(aes(color = Anno), size = 4)+
      geom_line( alpha = 0.5) +
      scale_colour_manual(values=c("2022"="gray", "2023"="red"))+
      theme_bw()+
      theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        axis.title.y = element_text("RFTE", size = 18),
        axis.title.x = element_text("MESE", size = 18),
        axis.text.y = element_text(size = 15, color = "blue"),
        axis.text.x = element_text(size = 15, color = "blue"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
      )
  })
  
  
  
  #### andamento cumulato----
  p2<-reactive({dtmensili%>% 
      mutate(MESE = as.factor(MESE),
             Anno = as.factor(Anno)) %>% 
      filter(Dipartimento == input$dip) %>% 
      ggplot()+
      aes(x = MESE, y = RFTEc, group = Anno)+
      geom_point(aes(color = Anno), size = 4)+
      geom_line( alpha = 0.5) +
      scale_colour_manual(values=c("2022"="gray", "2023"="red"))+
      theme_bw()+
      theme(
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title.y = element_text("RFTE", size = 18),
        axis.title.x = element_text("MESE", size = 18),
        axis.text.y = element_text(size = 15, color = "blue"),
        axis.text.x = element_text(size = 15, color = "blue"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        # panel.background= element_blank(),
        # plot.background = element_blank()
      )
  })
  
  
  output$rftedip1 <- renderPlot(
    p1() #, bg="transparent"
  )
  
  output$rftedip2 <- renderPlot(
    p2()#, bg="transparent"
  )

  #Strutture Complesse----
  #### andamento mensile----
  P3 <- reactive({dtmensiliR %>%
      filter(Dipartimento == input$dip) %>%
      ggplot()+
      aes(x = MESE, y = RFTE)+
      geom_point()+
      geom_line(group = 1, alpha = 0.3)+
      #geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha = 0.3) +
      # geom_point(data = rfte23pointR(), aes(x=MESE, y=RFTE), color="red") +
      # geom_line(data = rfte23pointR(), aes(x=MESE, y=RFTE), color="red", lty=3) +
      scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
      facet_wrap(Reparto~., scales = "free")+
      theme_bw()+
      theme(strip.text = element_text(size = 6.5, color = "blue"))
  })
  #### andamento cumulato----
  P4 <- reactive({dtmensiliR%>% 
      filter(Dipartimento == input$dip & Anno == 2022) %>% 
      ggplot()+
      aes(x = MESE, y = RFTEc)+
      geom_point()+
      geom_line(group = 1, alpha =0.5)+
      #geom_ribbon(aes(ymin = lowc, ymax = RFTEc), fill = "grey70", alpha = 0.3) +
      geom_point(data = rfte23pointR(), aes(x=MESE, y=RFTEc), color="red") +
      geom_line(data = rfte23pointR(), aes(x=MESE, y=RFTEc), color="red", lty=3)+
      scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
      facet_wrap(Reparto~., scales = "free")+
      theme_bw()+
      theme(strip.text = element_text(size = 6.5, color = "blue"))
  })
  
  Prep <- reactive(P3()+P4())
  
  output$rfterep <- renderPlot(
    Prep(), bg = "transparent"
  )
  
  
  
  ##dati del 2023
  # rfte23point <- reactive({
  #   dtmensili %>%
  #     select(Dipartimento, Anno, MESE, RT, FTET,FTp, RFTE, RFTEc) %>%  
  #     filter(Dipartimento == input$dip & Anno == 2023)
  # })
  
  ##tabella
  # tRFTEdipa <- reactive({
  #   dtmensili %>% 
  #     filter(Dipartimento == input$dip) %>%
  #     ungroup() %>% 
  #     mutate(FTETp = round(FTET*(FTp/100),1),
  #            FTp = round(FTp,1)) %>% 
  #     select(Anno, MESE, RT, FTET,FTp,FTETp, RFTE, RFTEc)
  # })
  
  
  # dip <- reactive({input$dip})
  

  
  ##grafici RT e FTE
  
  # p3<-reactive({dtmensili%>% 
  #     filter(Dipartimento == input$dip & Anno == 2022) %>% 
  #     ggplot()+
  #     aes(x = MESE, y = RT)+
  #     geom_point(alpha = 0.3)+
  #     geom_line(group = 1, alpha = 0.3)+
  #     geom_point(data = rfte23point(), aes(x=MESE, y=RT), color="red") +
  #     geom_line(data = rfte23point(), aes(x=MESE, y=RT), color="red", lty=3) +
  #     scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #     theme_bw()
  # })
  # 
  # p4<-reactive({dtmensili%>% 
  #     filter(Dipartimento == input$dip & Anno == 2022) %>% 
  #     ggplot()+
  #     aes(x = MESE, y = FTET*(FTp/100))+
  #     labs(y = "FTET programmati per gli obiettivi valorizzati")+
  #     geom_point(alpha = 0.3)+
  #     geom_line(group = 1, alpha = 0.3)+
  #     geom_point(data = rfte23point(), aes(x=MESE, y = FTET*(FTp/100)), color="red") +
  #     geom_line(data = rfte23point(), aes(x=MESE, y = FTET*(FTp/100)), color="red", lty=3) +
  #     scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #     theme_bw()
  # })
  # 
  # P2 <-reactive({ p3()/p4()})
  # 
  
  
  
  
  
  
  
  # output$report <- downloadHandler(
  #   filename = "report.html",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(Dipartimento = input$dip)
  #     
  #     
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  
  
  
  
  ##grafici RT e FTE
  
  # P5<-reactive({dtmensiliR%>% 
  #     filter(Dipartimento == input$dip & Anno == 2022) %>% 
  #     ggplot()+
  #     aes(x = MESE, y = RT)+
  #     geom_point(alpha = 0.3)+
  #     geom_line(group = 1, alpha = 0.3)+
  #     geom_point(data = rfte23pointR(), aes(x=MESE, y=RT), color="red") +
  #     geom_line(data = rfte23pointR(), aes(x=MESE, y=RT), color="red", lty=3) +
  #     scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #     facet_wrap(Reparto~., scales = "free")+
  #     theme_bw()+
  #     theme(strip.text = element_text(size = 6.5, color = "blue"))
  # })
  # 
  # P6<-reactive({dtmensiliR%>% 
  #     filter(Dipartimento == input$dip & Anno == 2022) %>% 
  #     ggplot()+
  #     aes(x = MESE, y = FTET*(FTp/100))+
  #     labs(y = "FTET programmati per gli obiettivi valorizzati")+
  #     geom_point(alpha = 0.3)+
  #     geom_line(group = 1, alpha = 0.3)+
  #     geom_point(data = rfte23pointR(), aes(x=MESE, y = FTET*(FTp/100)), color="red") +
  #     geom_line(data = rfte23pointR(), aes(x=MESE, y = FTET*(FTp/100)), color="red", lty=3) +
  #     scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  #     facet_wrap(Reparto~., scales = "free")+
  #     theme_bw()+
  #     theme(strip.text = element_text(size = 6.5, color = "blue"))
  # })
  # 
  # 
  
  
  # # Codici per la produzione del report in Excel
  # output$download_excel <- downloadHandler(
  #   filename = function() {
  #     "RFTE.xlsx"
  #   },
  #   content = function(file) {
  #     wb <- createWorkbook()
  #     
  #     addWorksheet(wb, sheetName = "Info")
  #     addWorksheet(wb, sheetName = "Dipartimento")
  #     addWorksheet(wb, sheetName = "Reparti")
  #     
  #     #Cover
  #     
  #     showGridLines(
  #       wb,
  #       sheet = 1,
  #       showGridLines = FALSE
  #     )
  #     
  #     writeData(
  #       wb, sheet = 1,
  #       
  #       c(dip(), 
  #         "", 
  #         "Monitoraggio del Ricavo Full Time Equivalente", 
  #         "", 
  #         "Ricavi totali (RT)",
  #         "I RT per ogni dipartimento sono calcolati a partire dai dati forniti dal datawerehouse DW_COGEP", 
  #         "sommando  l'attivita' valorizzata a tariffario, i ricavi dalla Vendita dei prodotti (VP) e dall'attivita' interna (AI).",
  #         "",
  #         "FTE", 
  #         "I FTE sono calcolati come il rapporto tra le ore complessivamente erogate dal personale di comparto e dirigenza", 
  #         "e le ore previste per un full time equivalente sulla base del contratto (36h settimanli per il comparto e 38 per la dirigenza)", 
  #         "nelle tabelle sono riportati  i FTE totali (FTET) dirigenza+comparto effettivamente erogati nel periodo anno/mese corrispondente ( il dato deriva dalle timbrature del personale)",
  #         "",
  #         "FTE programmato", 
  #         "In fase di programmazione viene assegnata una % dei FTE disponibili a inizio anno alle attivita' istituzionali", 
  #         "dette anche valorizzate. Questa percentuale rappresenta il FTE programmato che viene utilizzata per il calcolo del RFTE",
  #         "in tabella e' riportato come FTp e rappresenta una %", 
  #         "", 
  #         "FTETp",
  #         "questo dato riportato in tabella e' calcolato moltiplicando i FTET per la % di FTp per le attivita' valorizzate", 
  #         "Ricavo Full Time Equivalente (RFTE)", 
  #         "RFTE e' un indicatore di performance che tiene conto dei volumi di attivita' valorizzata e delle risorse a disposizione per la loro realizzazione", 
  #         "misurate come FTE. Il  RFTE viene assegnato a tutti i Dipartimenti che erogano attivita' valorizzate ( attività analitiche, vendita prodotti,ecc) come obiettivo di efficienza", 
  #         "Target: mantenimento del RFTE al 31/12/2022",
  #         "RFTE e' calcolato come RT/FTETp", 
  #         "Il RFTE del 2022 è calcolato detraendo i ricavi dovuti all'attività COVID dal Ricavo Totale.",
  #         " Tale detrazione non viene applicata per il 2023",
  #         "",
  #         "Monitoraggio", 
  #         "Per ogni dipartimento e per ogni struttura complessa o reparto", 
  #         "viene calcolato il RFTE mensile e cumulato ottenuto nel corso del 2022", 
  #         "Graficamente viene rappresentato sia l'andamento  cumulato che quello mensile", 
  #       
  #         "I dati dell'andamento del RFTE del 2023 sono riportati in rosso e vengono aggiornati ad ogni trimestre in occasione dell'aggiornamento", 
  #         "del data warehouse del controllo di gestione da parte dei Sistemi Informativi",
  #         "mentre l'andamento cumulato del 2022 che fa da riferimento è riportato in nero", 
  #         
  # 
  #         "", 
  #         "Ogni grafico e' accompagnato dalla tabella che riporta i dati utilizzati per la sua realizzazione",
  #         "I grafici sono inseriti nei diversi fogli del workbook come immagini e quindi non possono essere modificati"
  #       ), 
  #       startRow = 1, 
  #       startCol = 1
  #     )
  #     
  #     #Dipartimento sheet
  #     
  #     writeData(
  #       wb,
  #       sheet = 2,
  #       tRFTEdipa(),
  #       startRow = 1,
  #       startCol = 1
  #       
  #     )
  #     
  #     
  #     png("p11.png")
  #     print(P())
  #     dev.off()
  #     # png("p12.png")
  #     # print(P2())
  #     # dev.off()
  #     
  #     insertImage(wb, "Dipartimento", "p11.png",  startCol = "J",  width = 5, height = 5)
  #    # insertImage(wb, "Dipartimento", "p12.png",  startCol = "Q",  width = 5, height = 5)
  #     
  #     #Reparti sheet
  #     
  #     writeData(
  #       wb,
  #       sheet = 3,
  #       tRFTErep(),
  #       startRow = 1,
  #       startCol = 1
  #       
  #     )
  #     
  #     png("p13.png", width = 650)
  #     print(P3())
  #     dev.off()
  #     
  #     png("p14.png", width = 650)
  #     print(P4())
  #     dev.off()
  #     
  #     # png("p15.png", width = 650)
  #     # print(P5())
  #     # dev.off()
  #     # 
  #     # png("p16.png", width = 650)
  #     # print(P6())
  #     # dev.off()
  #     
  #     
  #     insertImage(wb, "Reparti", "p13.png",  startCol = "K",width = 7, height = 5)
  #     insertImage(wb, "Reparti", "p14.png",  startCol = "T",width = 7, height = 5)
  #     # insertImage(wb, "Reparti", "p15.png",  startCol = "K",startRow = 27,width = 7, height = 5)
  #     # insertImage(wb, "Reparti", "p16.png",  startCol = "T",startRow = 27,width = 7, height = 5)
  #     
  #     
  #     
  #     
  #     saveWorkbook(wb, file, overwrite = T)
  #     
  #     unlink(c(P(),  P3(), P4())) #, P5(), P6()))P2(),
  #     
  #   }
  # )
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
}

