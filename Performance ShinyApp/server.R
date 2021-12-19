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



tdip <- reactive(#questo codice prepara la tabella complessiva dei dipartimenti
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
      ftepDIP, by="Dipartimento"
    ) %>% 
    mutate(RFTE = RT/(FTET*(FTp/100)))#< questo calcola il ricavo fte usando solo la % di fte allocata alle attività istituzionali
  )


tdiprep <- reactive(#questo codice prepara la tabella dei singoli dipartimenti con i dati dei reparti
  (tabIZSLER %>% 
     rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
                         "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%
     filter(Anno == input$anno2 & Dipartimento == input$dip) %>% 
     
     group_by(Reparto) %>%
     summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
     mutate(RT = (Valorizzazione+VP+AI),
            FTET = round((FTED+FTEC),2)) %>%
     arrange(desc(Prestazioni)) %>%
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
      ), by = "Reparto") %>% 
    left_join(
      ftepREP, by = "Reparto"
    ) %>% 
    mutate(RFTE = RT/(FTET*(FTp/100)))
)

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

# output$RFTE <- renderValueBox(
#   ValueBOX(IZSLER(), Variabile = "RT", Variabile2 = "FTE_T",  Titolo = "Ricavo per Full Time Equivalente", colore = "blue", icona = "euro")
# )


ftp <- reactive(FTp)

rfteDip <- reactive(tdip() %>% ungroup() %>% 
                      summarise(rt=sum(RT),
                                ft=sum(FTET))%>% 
                      bind_cols(ftp()) %>% 
                      mutate(rfte=rt/(ft*FTp)) %>% 
                      select(rfte) %>% 
                      unlist()
)  
                 
output$RFTE <- renderValueBox(
  valueBox( prettyNum(rfteDip(), big.mark = ".", decimal.mark = ",") , 
            subtitle = "Ricavo Full Time Equivalente", color = "blue", 
            icon = icon("euro")
  )
)

output$Costi <- renderValueBox(
  ValueBOX(IZSLER(), Variabile = "COSTI", Titolo = "Costi totali", colore = "blue", icona = "euro")
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
     # bg( i = ~ ROI >= 1, 
     #    j = ~ ROI, 
     #    bg="green") %>% 
     # bg( i = ~ ROI < 1, 
     #     j = ~ ROI, 
     #     bg="red") %>% 
     # 
     footnote(i=1, j=2:12,
              value = as_paragraph(
                c("Attività analitica",
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


##TrendPlot----

trend <- reactive(
  tizsler %>% 
    filter(Dipartimento != "DIREZIONE SANITARIA") %>% 
    mutate(Dipartimento = recode(Dipartimento,  "DIPARTIMENTO SICUREZZA ALIMENTARE"  = "DSA", 
                                 "DIPARTIMENTO TUTELA E SALUTE ANIMALE" = "DTSA", 
                                 "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA" = "ATLOMB", 
                                 "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA" = "ATER")) %>% 
  mutate(FTET = FTED+FTEC) %>%
  pivot_longer(!c(Anno,Dipartimento), names_to = "KPI", values_to = "valore") %>%   
  filter(KPI == input$kpi) %>%  
  group_by(Dipartimento) %>% 
  arrange(Dipartimento, Anno) %>% 
  mutate(Var = round((valore/lag(valore)-1)*100, 2)) 
)


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

##ValueBOX Dipartimento/Reparto----


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


ftePrep <- reactive(ftepREPD %>% 
                      filter(Dipartimento == input$dip) %>% 
                      group_by(valorizz) %>% 
                      summarise(ft= sum(FT)) %>%
                      mutate(FTp = round(prop.table(ft), 1)) %>%
                      filter(valorizz=="si") %>% 
                      select(FTp)
)


rfteDipr <- reactive(tdiprep() %>%ungroup() %>% 
                      summarise(rt=sum(RT),
                                ft=sum(FTET))%>% 
                      bind_cols(ftePrep()) %>% 
                      mutate(rfte=rt/(ft*FTp)) %>% 
                      select(rfte) %>% 
                      unlist()
)  



output$RFTEdip <- renderValueBox(
  valueBox(prettyNum(rfteDipr(), big.mark = ".", decimal.mark = ","),
           subtitle = "Ricavo Full Time Equivalente",color = "blue", 
           icon = icon("euro"))
)

# output$RFTEdip <- renderValueBox(
#   ValueBOX(tdiprep(), Variabile = "RT", Variabile2 = "FTE_T",  Titolo = "Ricavo per Full Time Equivalente", colore = "blue", icona = "euro")
# )

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
               c("Attività analitica",
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


## TrendPlot Reparti----

trendRep <- reactive(tabIZSLER %>% 
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
  rename( "Prestazioni" = TotPrestazioni, "Valorizzazione" = TotTariff, "VP" = TotFattVP, "AI" = TAI, 
          "COSTI" = TotCost, "FTED" = FTE_Dirigenza, "FTEC"= FTE_Comparto, Anno = ANNO) %>%
  filter( Dipartimento == input$dip) %>% 
  
  group_by(Anno,Reparto) %>%
  summarise_at(c("Prestazioni", "Valorizzazione",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (Valorizzazione+VP+AI),
         FTET = round((FTED+FTEC),2)) %>%   
  pivot_longer(!c(Anno,Reparto), names_to = "KPI", values_to = "valore") %>%
  filter(KPI == input$kpi2) %>%  
  group_by(Reparto) %>% 
  arrange(Reparto, Anno) %>% 
  mutate(Var = round((valore/lag(valore)-1)*100, 2)) 
)

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

###tabella modale progetti di ricerca----
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



###tabella modale pubblicazioni----
# paper <- reactive({
#   
#   pubs %>% filter(IF == "IF") %>% 
#     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese, "IF" = impf) %>%
#     unique() %>% 
#     arrange(desc(IF))
# })
# 
# output$articoli <- renderDataTable(paper(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                    extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                          paging = TRUE,autoWidth = TRUE,
#                                                                          buttons = c('excel')))







# Performance----

pPerf <- reactive(perf %>%
  filter(Periodo == 2 & Avanzamento != 0 ) %>%
  mutate(MacroArea = factor(MacroArea)) %>%
  group_by(MacroArea) %>%
  summarise(mediana =  100*round(median(Avanzamento, na.rm = T),2),
            media = 100*round(mean(Avanzamento, na.rm = T),2),
            n = n()) %>%
  mutate(target = 100) %>%
  mutate(MacroArea = as.character(MacroArea)) %>%
  mutate(MacroArea = gsub("\\d+", "", MacroArea),
         MacroArea = gsub("\"", "", MacroArea))
)

output$pltArea <- renderPlot({  
  ggplot(pPerf())+
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75, 90, 100)),
    color = "lightgrey"
  )+
  geom_col(
    aes(x = reorder(str_wrap(MacroArea, 1), media),
        y = media,
        fill = media
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .6
  )+

  geom_point(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media
    ),
    size = 3, color = "gray12"
  )+

  geom_segment(
    aes(
      x =  reorder(str_wrap(MacroArea, 1), media),
      y = 0,
      xend = reorder(str_wrap(MacroArea, 1), media),
      yend = 100
    ),
    linetype = "dashed",
    color = "gray12"
  )+
  coord_polar()+

  scale_y_continuous(
    limits = c(-20,110),
    expand = c(0, 0)

  ) +
  geom_text(
    aes(
      x = reorder(str_wrap(MacroArea, 1), media),
      y = media-10,
      label = paste0(media, "%")),
    color = "black",
    size=9)+

  annotate(
    x = 0.5,
    y = 30,
    label = "25%",
    geom = "text",
    color = "red",
    family = "Bell MT", 
    size= 9
  )  +
  annotate(
    x = 0.5,
    y = 55,
    label = "50%",
    geom = "text",
    color = "red",
    family = "Bell MT",
    size= 9
  )  +

  annotate(
    x = 0.5,
    y = 80,
    label = "75%",
    geom = "text",
    color = "red",
    family = "Bell MT",
    size= 9
  )  +

  annotate(
    x = 0.5,
    y = 110,
    label = "100%",
    geom = "text",
    color = "red",
    family = "Bell MT",
    size= 9
  )  +

  #scale_fill_gradientn(colours = gray.colors(7))+

  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 16),
    # Move the legend to the bottom
    legend.position = "blank",
  )+

  # Customize general theme
  theme(

    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),

    # Customize the text in the title, subtitle, and caption
    #plot.title = element_text(face = "bold", size = 18),
    #plot.subtitle = element_text(size = 14, hjust = 0.05),
   # plot.caption = element_text(size = 10, hjust = .5),

    # Make the background white and remove extra grid lines
    # panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
}, bg = "transparent")



##tabella Area x Dip----

tArea <- reactive(perf %>%
                    filter(Periodo == 2 & Avanzamento!=0  ) %>%
                    mutate(MacroArea = factor(MacroArea)) %>%
                    group_by(Dipartimento,  MacroArea) %>%
                    summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>%
                    mutate(media = percent(media),
                           media = as.character(media)) %>%
                    pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%
                    select("MacroArea","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale",
                           "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
                           "Dipartimento area territoriale Emilia Romagna",
                           "Dipartimento amministrativo") %>%
                    arrange(MacroArea) %>%
                    mutate(MacroArea = as.character(MacroArea)) %>%
                    mutate(MacroArea = gsub("\\d+", "", MacroArea),
                           MacroArea = gsub("\"", "", MacroArea))  %>%
                    rename("Macro Area" = "MacroArea")  )




output$AreaDip <- renderUI({
  border <- officer::fp_border()
  flextable(tArea() ,
            col_keys = c("Macro Area", "Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale",
                         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
                         "Dipartimento area territoriale Emilia Romagna",
                         "Dipartimento amministrativo")
  ) %>%  
    theme_box() %>% 
    color(i = 1, color = "blue", part = "header") %>%
    bold( part = "header") %>%
    fontsize(size=15) %>%
    fontsize(part = "header", size = 15) %>%
    line_spacing(space = 2.5) %>% 
    autofit() %>%
    colformat_double(j= c("Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale",
                          "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
                          "Dipartimento area territoriale Emilia Romagna",
                          "Dipartimento amministrativo"), decimal.mark = "," , digits = 0, suffix = "%") %>% 
    htmltools_value() 
  
})
 


















 tArea <- reactive(perf %>%
  filter(Periodo == 2 & Avanzamento!=0  ) %>%
  mutate(MacroArea = factor(MacroArea)) %>%
  group_by(Dipartimento,  MacroArea) %>%
  summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>%
  mutate(media = percent(media),
         media = as.character(media)) %>%
  pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%
  select("MacroArea","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale",
         "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
         "Dipartimento area territoriale Emilia Romagna",
         "Dipartimento amministrativo") %>%
  arrange(MacroArea) %>%
  mutate(MacroArea = as.character(MacroArea)) %>%
  mutate(MacroArea = gsub("\\d+", "", MacroArea),
         MacroArea = gsub("\"", "", MacroArea))  %>%
  rename("Macro Area" = "MacroArea")  )
  
 



















}




# renderGauge(div_id = "perfIZSLER", theme =  "shine",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2))
#   
#   
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "dirgen", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Direzione Generale")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "dirsan", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Direzione Sanitaria")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "dipamm", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Dipartimento amministrativo")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "dipTSA", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Dipartimento tutela e salute animale")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "dipSA", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Dipartimento sicurezza alimentare")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "LOMB", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Dipartimento area territoriale Lombardia")
# ), gauge_name = "")
# 
# 
# renderGauge(div_id = "EMR", theme =  "london",  rate = (
#   
#   perf %>%
#     filter(Periodo == 4 & Avanzamento != 0 ) %>% 
#     group_by(Dipartimento) %>% 
#     summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2)) %>% 
#     filter(Dipartimento == "Dipartimento area territoriale Emilia Romagna")
# ), gauge_name = "")













     
     