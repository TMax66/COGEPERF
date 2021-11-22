server <- function(input, output, session) { 

options(shiny.reactlog=TRUE)
  
##codici reactive di preparazione ----

IZSLER <- reactive( tizsler %>% 
                         filter(Anno == input$anno))

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



tdip <- reactive(
  IZSLER() %>%
  left_join(
    (pubs() %>%
       filter(articoliif == "IF") %>%
      # count(Dipartimento, NR) %>%
       group_by(Dipartimento) %>%  
      # count(NR) %>%
       summarise("Pubblicazioni" = nlevels(factor(NR)))), by = "Dipartimento") %>%    
  left_join(
    (pr() %>%
       group_by(Dipartimento) %>%
       summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
    ),  by = "Dipartimento" ))


tdiprep <- reactive(
  (tabIZSLER %>% 
     rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
                         "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%
     filter(Anno == input$anno2 & Dipartimento == input$dip) %>% 
     
     group_by(Reparto) %>%
     summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
     mutate(RT = (Valorizzazione+VP+AI),
            FTE_T = round((FTED+FTEC),1)) %>%
     arrange(desc(Prestazioni)) %>%
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





# tdiprep <- reactive(
#   (tabIZSLER %>% 
#     filter(Anno == input$anno2 & Dipartimento == input$dip) %>% 
#     rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
#             "COSTI" = costi) %>%
#     group_by(Reparto) %>%
#     summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
#     mutate(RT = (VALORE+VP+AI),
#            FTE_T = round((FTED+FTEC),1)) %>%
#     arrange(desc(ANALISI)) %>%
#     mutate("R-FTE" = round(RT/FTE_T,0), 
#            "C-FTE" = round(COSTI/FTE_T, 0), 
#            "ROI" = round(RT/COSTI, 2)) %>% 
#     select(-FTED, -FTEC)) %>% 
#     left_join(
#       (pubsdip() %>%
#          filter(articoliif == "IF") %>%
#          count(Reparto, NR) %>%
#          group_by(Reparto) %>%  
#          count(NR) %>%
#          summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>%    
#     left_join(
#       (prdip() %>%
#          group_by(Reparto) %>%
#          summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
#       ), by = "Reparto")
# )



output$year <- renderText(input$anno)
output$dipa <- renderText(input$dip)
output$dipa2 <- renderText(input$dip)

#ValueBoxDipartimenti----
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

output$RFTE <- renderValueBox(
  ValueBOX(IZSLER(), Variabile = "RT", Variabile2 = "FTE_T",  Titolo = "Ricavo per Full Time Equivalente", colore = "blue", icona = "euro")
)

output$Costi <- renderValueBox(
  ValueBOX(IZSLER(), Variabile = "COSTI",   Titolo = "Costi totali", colore = "blue", icona = "euro")
)

# output$costifte <- renderValueBox(
#   ValueBOX(IZSLER(), Variabile = "COSTI", Variabile2 = "FTE_T",  Titolo = "Costi per Full Time Equivalente", colore = "blue", icona = "euro")
# )

# roit <- reactive(
#   IZSLER() %>%
#     summarise(roi = round((sum(RT)/sum(COSTI)), 2)) %>%
#     select(roi))
# output$roi <- renderValueBox({
#   valueBox(prettyNum(roit(), big.mark = "." , decimal.mark = ","), "ROI",
#            color = ifelse(roit() >= 1, "green", "red")
#   )
# })

output$PR <- renderValueBox({
  valueBox(
    (  pr() %>% 
         summarise(n = nlevels(factor(Codice)))
    ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
})

output$IF <- renderValueBox({
    valueBox(
      (pubs() %>%
         filter(articoliif == "IF") %>%
         group_by(NR) %>%
         count(NR) %>%
         select(NR) %>%
         nrow()),  "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
  })

# output$Int <- renderValueBox({
#       valueBox(
#         (pubs() %>%
#            filter(articoliif == "Int") %>%
#            group_by(NR) %>%
#            count(NR) %>%
#            select(NR) %>%
#            nrow()
#           ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
#     })


##tabella complessiva DIPARTIMENTI####

output$t <- renderUI({
    border <- officer::fp_border()
   flextable(tdip(),
      col_keys = c("Dipartimento", "Prestazioni", "Valorizzazione", "VP", "AI",  "RT","COSTI", "FTE_T", "R-FTE", "Pubblicazioni", "Progetti di Ricerca")
      ) %>%  
      theme_booktabs() %>% 
      color(i = 1, color = "blue", part = "header") %>%
      bold( part = "header") %>%
      fontsize(size=15) %>%
      fontsize(part = "header", size = 15) %>%
      line_spacing(space = 2.5) %>% 
      autofit() %>%
      colformat_double(j = c( "Valorizzazione", "VP", "AI", "COSTI",  "RT", "R-FTE"), big.mark = ".", decimal.mark = ",", prefix = "€", digits = 0) %>%
      colformat_double(j= c("Prestazioni", "FTE_T"), big.mark = ".", decimal.mark = "," ,  digits = 0) %>% 
     # bg( i = ~ ROI >= 1, 
     #    j = ~ ROI, 
     #    bg="green") %>% 
     # bg( i = ~ ROI < 1, 
     #     j = ~ ROI, 
     #     bg="red") %>% 
      htmltools_value() 
      
})




##ValueBOX Diaprtimento/Reparto----


output$esamidip <- renderValueBox(
  ValueBOX(tdiprep(), "Prestazioni", Titolo = "N.Prestazioni", colore = "blue", icona = "flask")
)

output$ricavidip <- renderValueBox(
  ValueBOX(tdiprep(), "Valorizzazione", Titolo = "Valorizzazioni da Tariffario", colore = "blue", icona = "euro")
)
output$venproddip <- renderValueBox(
  ValueBOX(tdiprep(), "VP", Titolo = "Vendita Prodotti", colore = "blue", icona = "euro")
)

output$attintdip <- renderValueBox(
  ValueBOX(tdiprep(), "AI", Titolo = "Attività Interna", colore = "blue", icona = "euro")
)

output$rictotdip <- renderValueBox(
  ValueBOX(tdiprep(), "RT", Titolo = "Ricavi Totali", colore = "blue", icona = "euro")
)

output$RFTEdip <- renderValueBox(
  ValueBOX(tdiprep(), Variabile = "RT", Variabile2 = "FTE_T",  Titolo = "Ricavo per Full Time Equivalente", colore = "blue", icona = "euro")
)

output$Costidip <- renderValueBox(
  ValueBOX(tdiprep(), Variabile = "COSTI",   Titolo = "Costi totali", colore = "blue", icona = "euro")
)

# output$costiftedip <- renderValueBox(
#   ValueBOX(tdiprep(), Variabile = "COSTI", Variabile2 = "FTE_T",  Titolo = "Costi per Full Time Equivalente", colore = "blue", icona = "euro")
# )

# roitdip <- reactive(
#   tdiprep() %>%
#     summarise(roi = round((sum(RT)/sum(COSTI)), 2)) %>%
#     select(roi))
# output$roidip <- renderValueBox({
#   valueBox(prettyNum(roitdip(), big.mark = "." , decimal.mark = ","), "ROI",
#            color = ifelse(roitdip() >= 1, "green", "red")
#   )
# })

output$PRdip <- renderValueBox({
  valueBox(
    (  prdip() %>% 
         summarise(n = nlevels(factor(Codice)))
    ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
})

output$IFdip <- renderValueBox({
  valueBox(
    (pubsdip() %>%
       filter(articoliif == "IF") %>%
       group_by(NR) %>%
       count(NR) %>%
       select(NR) %>%
       nrow()),  "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
})

# output$Intdip <- renderValueBox({
#   valueBox(
#     (pubsdip() %>%
#        filter(articoliif == "Int") %>%
#        group_by(NR) %>%
#        count(NR) %>%
#        select(NR) %>%
#        nrow()
#     ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
# })








##tabella Dipartimento/Reparto####

 
output$tr <- renderUI({
  border <- officer::fp_border()
  flextable(tdiprep(), 
            
            col_keys = c("Reparto", "Prestazioni", "Valorizzazione", "VP", "AI", "RT",  "COSTI",  "FTE_T",  "R-FTE", "Pubblicazioni", "Progetti di Ricerca")
  ) %>%  
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>%
    bold( part = "header") %>%
    fontsize(size=15) %>%
    fontsize(part = "header", size = 15) %>%
    line_spacing(space = 2.5) %>% 
    autofit() %>%
    colformat_num(j = c( "Valorizzazione", "VP", "AI", "COSTI",  "RT", "R-FTE"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
    colformat_num(j= c("Prestazioni"), big.mark = ".", decimal.mark = "," ) %>% 
    # bg( i = ~ ROI >= 1, 
    #     j = ~ ROI, 
    #     bg="green") %>% 
    # bg( i = ~ ROI < 1, 
    #     j = ~ ROI, 
    #     bg="red") %>% 
    htmltools_value() 
            

})

##tabelle modali DIPA------ 
Prj <- reactive({
  pr() %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>%
    summarise(Budget = sum(Budget), nUO = n()) #%>%
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

output$projr <- renderDataTable(Prj(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                              extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                  paging = TRUE,autoWidth = TRUE,
                                                              buttons = c('excel')))



##tabelle modali DIPA/REP----

Prjdip <- reactive({
  prdip() %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>%
    summarise(Budget = sum(Budget), nUO = n()) #%>%
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

output$projrep <- renderDataTable(Prjdip(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                                extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                      paging = TRUE,autoWidth = TRUE,
                                                                      buttons = c('excel')))







# Performance----

renderGauge(div_id = "perfIZSLER", theme =  "shine",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2))
  
  
), gauge_name = "")


renderGauge(div_id = "dirgen", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Direzione Generale")
), gauge_name = "")


renderGauge(div_id = "dirsan", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Direzione Sanitaria")
), gauge_name = "")


renderGauge(div_id = "dipamm", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Dipartimento amministrativo")
), gauge_name = "")


renderGauge(div_id = "dipTSA", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Dipartimento tutela e salute animale")
), gauge_name = "")


renderGauge(div_id = "dipSA", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Dipartimento sicurezza alimentare")
), gauge_name = "")


renderGauge(div_id = "LOMB", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Dipartimento area territoriale Lombardia")
), gauge_name = "")


renderGauge(div_id = "EMR", theme =  "london",  rate = (
  
  perf %>%
    filter(Periodo == 2 & Avanzamento != 0 ) %>% 
    group_by(Dipartimento) %>% 
    summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
    filter(Dipartimento == "Dipartimento area territoriale Emilia Romagna")
), gauge_name = "")



###Aree performance----

# Area <-  perf %>%
#   mutate(MacroArea = factor(MacroArea)) %>%
#   group_by(MacroArea) %>%
#   summarise(mediana =  round(median(Avanzamento, na.rm = T),2),
#             media = round(mean(Avanzamento,na.rm  = T),2),
#             n = n()) %>%  
#   mutate(mediana = percent(mediana),
#          mediana = as.character(mediana),
#          media = percent(media),
#          media = as.character(media)) %>%  
#   #pivot_wider(names_from = "Dipartimento", values_from = "mediana", values_fill = " ") %>%  View()
#   arrange(MacroArea) %>%
#   mutate(MacroArea = as.character(MacroArea)) %>%
#   mutate(MacroArea = gsub("\\d+", "", MacroArea),
#          MacroArea = gsub("\"", "", MacroArea))  %>%
#   kbl( ) %>%
#   kable_styling() %>%
#   kable_paper(bootstrap_options = "striped", full_width = F)


### Obiettivi----

### Azioni----

### Indicatori----








}
     
      
      # 
      
      # color(j= "R/FTET", color = "red", part = "all") %>%
      # color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
      # color(j= "Progetti di Ricerca", color = "red", part = "all") %>%
      # vline(j= "RT", border = border, part = "all") %>%
      # footnote(i=1, j=3:10,
      #          value = as_paragraph(
      #            c("Full Time Equivalenti Dirigenza",
      #              "Full Time Equivalenti Comparto",
      #              "Full Time Equivalenti Totale",
      #              "Ricavo da Analisi",
      #              "Ricavo Vendita Prodotti",
      #              "Ricavo Attività Interna",
      #              "Ricavo Totale",
      #              "Ricavo per Full Equivalenti Totale")
      #          ),
      #          ref_symbols = c("a","b","c","d","e","f","g","h"),
      #          part = "header", inline = T) %>%
      # fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
      # htmltools_value()
  

 #}

