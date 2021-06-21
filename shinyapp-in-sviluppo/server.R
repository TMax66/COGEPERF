server <- function(input, output, session) { 


#IZSLER#####
##codici reactive di preparazione ----
IZSLER <- reactive( tizsler %>% 
                         filter(Anno == input$anno))

pr <- reactive(prj %>% 
                 mutate("Stato" = ifelse(annofine < input$anno, "Archiviato", "Attivo")) %>% 
                 filter(Stato == "Attivo" & annoinizio <= input$anno))

pubs <- reactive(pub %>% 
                  filter(OA == input$anno))



#value boxes######  
es <- reactive(
        IZSLER() %>%
         summarise(totes = sum(esami)) %>% 
          select(totes))
output$esami <- renderValueBox({
    valueBox(prettyNum(es(), big.mark = "."), "N. esami",  icon = icon("flask"),
      color = "blue"
    )
  })


ric <- reactive(
  IZSLER() %>%
    summarise(totric = round(sum(ricavi), 0)) %>% 
    select(totric))
output$ricavi <- renderValueBox({
  valueBox(prettyNum(ric(), big.mark = "." ), "Ricavi",  icon = icon("euro"),
           color = "blue"
  )
})

vp <- reactive(
  IZSLER() %>%
    summarise(totvp = round(sum(VP), 0)) %>% 
    select(totvp))
output$venprod <- renderValueBox({
  valueBox(prettyNum(vp(), big.mark = "." ), "Vendita Prodotti",  icon = icon("euro"),
           color = "blue"
  )
})

ai <- reactive(
  IZSLER() %>%
    summarise(totai = round(sum(AI), 0))%>% 
    select(totai))
output$attint <- renderValueBox({
  valueBox(prettyNum(ai(), big.mark = "." ), "Attività Interna",  icon = icon("euro"),
           color = "blue"
  )
})

rt <- reactive(
  IZSLER() %>%
    summarise(rt = round(sum(RT), 0)) %>% 
    select(rt))
output$rictot <- renderValueBox({
  valueBox(prettyNum(rt(), big.mark = "." ), "Ricavi Totali",  icon = icon("euro"),
           color = "blue"
  )
})


rtfte <- reactive(
  IZSLER() %>%
    summarise(rtf = round((sum(RT)/sum(FTE_T)), 0)) %>% 
    select(rtf))
output$RFTE <- renderValueBox({
  valueBox(prettyNum(rtfte(), big.mark = "." ), "Ricavo per Full Time Equivalente",  icon = icon("euro"),
           color = "blue"
  )
})



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


#   
# ra <- reactive(tizsler %>% 
#   filter(Dipartimento == "Totale") %>% 
#   select("RA"))
# 
#   output$ra <- renderValueBox({
#     valueBox(prettyNum(ra(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
#       color = "aqua"
#     )
#   })
# 
#   vp <- reactive(tizsler %>% 
#                    filter(Dipartimento == "Totale") %>% 
#                    select("RVP"))
#   
#   output$vp <- renderValueBox({
#     valueBox(prettyNum(vp(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
#              color = "aqua"
#     )
#   })
#   
#   
#   ai <- reactive(tizsler %>% 
#                    filter(Dipartimento == "Totale") %>% 
#                    select("RAI"))
#   
#   output$ai <- renderValueBox({
#     valueBox(prettyNum(ai(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
#              color = "aqua"
#     )
#   })
#   
#   rt <- reactive(tizsler %>% 
#                    filter(Dipartimento == "Totale") %>% 
#                    select("RT"))
#   
#   output$rt <- renderValueBox({
#     valueBox(prettyNum(rt(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
#              color = "aqua"
#     )
#   })
#   
#   rfte <- reactive(tizsler %>% 
#                    filter(Dipartimento == "Totale") %>% 
#                    select("R/FTET"))
#   
#   output$rfte <- renderValueBox({
#     valueBox(prettyNum(rfte(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
#              color = "aqua"
#     )
#   })
# 
# 
#   output$IF <- renderValueBox({
#     valueBox(
#       (ricerca %>% 
#          filter(IF == "IF") %>% 
#          group_by(nr) %>% 
#          count(nr) %>% 
#          select(nr) %>% 
#          nrow()),  "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
#   })
#   
#   
#   output$Int <- renderValueBox({
#     valueBox(
#       (ricerca %>% 
#          filter(INT == "Int") %>% 
#          group_by(nr) %>% 
#          count(nr) %>% 
#          select(nr) %>% 
#          nrow()
#         # ric() %>% 
#         #  filter(tipologia == "Int") %>% 
#         #  select(n.articoli)
#         ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
#   })
#   
#   
#   output$PR <- renderValueBox({
#     valueBox(
#       (  pr %>% 
#            summarise(n = nlevels(factor(Codice)))
#       ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
#   })
# 
# ###tabella x dipartimenti####
# 
#   output$t <- renderUI({
#     border <- officer::fp_border()
#     flextable(
#     #   (tizsler %>%
#     #      filter(Dipartimento != "Totale") %>% 
#     #      left_join(
#     #        (ricerca %>%
#     #           filter(IF == IF) %>%
#     #           count(Dipartimento, nr) %>%
#     #           group_by(Dipartimento) %>%
#     #           count(nr) %>%
#     #           summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>% 
#     #      left_join(
#     #        (pr %>% 
#     #          group_by(Dipartimento) %>% 
#     #          summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
#     #          filter(!is.na(Dipartimento))),  by = "Dipartimento")
#     # 
#     # )
#     Tizsler
#     )%>%
#       theme_booktabs() %>%
#       color(i = 1, color = "blue", part = "header") %>%
#       bold( part = "header") %>%
#       fontsize(size=15) %>%
#       fontsize(part = "header", size = 15) %>%
#       line_spacing(space = 2.5) %>%
#       colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
#       autofit() %>%
#       color(j= "R/FTET", color = "red", part = "all") %>%
#       color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
#       color(j= "Progetti di Ricerca", color = "red", part = "all") %>% 
#       vline(j= "RT", border = border, part = "all") %>%
#       footnote(i=1, j=3:10,
#                value = as_paragraph(
#                  c("Full Time Equivalenti Dirigenza",
#                    "Full Time Equivalenti Comparto",
#                    "Full Time Equivalenti Totale",
#                    "Ricavo da Analisi",
#                    "Ricavo Vendita Prodotti",
#                    "Ricavo Attività Interna",
#                    "Ricavo Totale",
#                    "Ricavo per Full Equivalenti Totale")
#                ),
#                ref_symbols = c("a","b","c","d","e","f","g","h"),
#                part = "header", inline = T) %>%
#       fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
#       htmltools_value()
#   })
#   
#   
#   
#   
#   
#   
#   
#   
#   
# 
# ####grafici benchmarking#########################################
# 
# tb <- reactive({tizsler %>% 
#   filter(Dipartimento != "Totale") %>% 
#   mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
#          "RA" = round(100*(RA/sum(RA)),1), 
#          "FTED" = round(100*(FTED/sum(FTED)),1), 
#          "FTEC" = round(100*(FTEC/sum(FTEC)),1),
#          "RVP" =round(100*(RVP/sum(RVP)),1), 
#          "RAI" = round(100*(RAI/sum(RAI)), 1),
#          "RT" = round(100*(RT/sum(RT)),1),
#          "FTET" = round(100*(FTET/ sum(FTET)), 1)
#          # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
#   ) %>% 
#   select(Dipartimento, Esami, "FTED", "FTEC", "FTET",   "RT") %>% 
#   pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
#   mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT")))
#   })
# 
# 
# 
# output$tbd <- renderPlot( 
#   
#   if(input$ind == "Dipartimento")
#     
#   {
#   
#    ggplot(tb(),  aes( 
#     x = KPI, 
#     y = valore, 
#     fill = KPI
#   )) + geom_col(width = 0.9, color = "black")+
#   coord_polar(theta = "x")+ facet_wrap(~Dipartimento, nrow = 1)+
#   scale_fill_brewer(palette = "Blues")+
#   geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#     theme(legend.position = "blank",
#           panel.background= element_blank(),
#           plot.background = element_blank(), 
#           strip.text.x = element_text(size = 15, colour = "blue"), 
#           axis.text.x = element_text(size = 10, color = "black"))+
#     labs(x = "", y = "") 
#  
# }
# 
# else
#   
# {
#    tb() %>% 
#     mutate(Dipartimento = recode(Dipartimento, "Dipartimento Sicurezza Alimentare" = "DSA", 
#                                  "Dipartimento Tutela e  Salute Animale" = "DTSA", 
#                                  "Area Territoriale Lombardia" = "ATLOMB", 
#                                  "Area Territoriale Emilia Romagna" = "ATER")) %>% 
#     ggplot(aes( 
#       x = Dipartimento, 
#       y = valore, 
#       fill = Dipartimento
#     )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 15, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "")
# 
# }, bg = "transparent")
# 
# ####tabelle modali pubblicazioni e convegni####################################################
# 
paper <- reactive({
  pubs() %>% filter(articoliif == "IF") %>%
    select("Autori" = CAU, "Journal" = datoBibl, "Titolo" = TI, "IF" = IF) %>%
    unique() %>%
    arrange(desc(IF))
})

output$articoli <- renderDataTable(paper(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                                   extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                          paging = TRUE,autoWidth = TRUE,
                                                                         buttons = c('excel')))


# ###tabella modale convegni internazionali
# 
# Cint <- reactive({
#   ricerca %>% filter(INT == "Int") %>% 
#     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
#     unique()
#   
# })
# 
# output$convegni <- renderDataTable(Cint(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                    extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                           paging = TRUE,autoWidth = TRUE,
#                                                                          buttons = c('excel')))
# 
Prj <- reactive({
  pr() %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>%
    summarise(Budget = sum(Budget), nUO = n()) %>%
    ungroup() %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>%
    # summarise(Durata = as.numeric(DataFine-DataInizio),
    #           R = as.numeric(date("2019-12-31")-date(DataInizio)),
    #           Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
    #           Realizzazione = paste(round(Realizzazione, 0), "%") )%>%
    mutate(DataInizio = as.character(DataInizio),
           DataFine = as.character(DataFine)) #%>%
    #arrange(Realizzazione) %>%
    #select(-R, -Durata)

})
# 
output$projr <- renderDataTable(Prj(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
                                extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
                                                                      paging = TRUE,autoWidth = TRUE,
                                                                      buttons = c('excel')))

# ###tabelle modali percentuali KPI
# 
# output$tbw <- renderTable(tb() %>% 
# pivot_wider(names_from = "KPI", values_from = "valore"))
# 
# 
# ####radar plot modale####
# 
# radar <- reactive({
#  df<-data.frame(
#   Pubblicazioni = Tizsler$Pubblicazioni/Tizsler$FTED,
#   Progetti = Tizsler$`Progetti di Ricerca`/Tizsler$FTED, 
#   RFTE = Tizsler$`R/FTET`/((sum(Tizsler$RT)/sum(Tizsler$FTET))),
#   Esami = Tizsler$N.esami/sum(Tizsler$N.esami),
#   FTED = Tizsler$FTED/sum(Tizsler$FTED),
#   FTEC = Tizsler$FTEC/sum(Tizsler$FTEC),
#   FTET = Tizsler$FTET/sum(Tizsler$FTET),
#   RT = Tizsler$RT/sum(Tizsler$RT))
# rownames(df) <- Tizsler$Dipartimento
# df <- rbind(c(4,4,4,1,1,1,1,1) , rep(0,8) , df)
# })
# 
# output$radarIZSLER <-renderPlot({   
# colors_in=c( "red", "blue", "green", "black")
# radarchart( radar()  ,
#             #custom polygon
#             pcol=colors_in , plwd=1 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
#             #custom labels
#             #vlcex=0.8
# )
# legend(x=1.5, y=0.7, legend = rownames(radar()[-c(1,2),]), col = colors_in,  bty = "n", pch=16 , cex=0.8, pt.cex=1)
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #####DSA#####
# 
# ###value boxes dsa####
# es2 <- reactive(tizsler %>% 
#                  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                  select("N.esami"))
# 
# output$esami2 <- renderValueBox({
#   valueBox(prettyNum(es2(), big.mark = "."), "N. esami",  icon = icon("flask"),
#            color = "blue"
#   )
# })
# 
# ra2 <- reactive(tizsler %>% 
#                  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                  select("RA"))
# 
# output$ra2 <- renderValueBox({
#   valueBox(prettyNum(ra2(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# vp2 <- reactive(tizsler %>% 
#                  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                  select("RVP"))
# 
# output$vp2 <- renderValueBox({
#   valueBox(prettyNum(vp2(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# 
# ai2 <- reactive(tizsler %>% 
#                  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                  select("RAI"))
# 
# output$ai2 <- renderValueBox({
#   valueBox(prettyNum(ai2(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rt2 <- reactive(tizsler %>% 
#                  filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                  select("RT"))
# 
# output$rt2 <- renderValueBox({
#   valueBox(prettyNum(rt2(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rfte2 <- reactive(tizsler %>% 
#                    filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#                    select("R/FTET"))
# 
# output$rfte2 <- renderValueBox({
#   valueBox(prettyNum(rfte2(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# 
# output$IF2 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(IF == "IF" & Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
# })
# 
# 
# output$Int2 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(INT == "Int" & Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
# })
# 
# output$PR2 <- renderValueBox({
#   valueBox(
#     (  pr %>% 
#          filter(Dipartimento=="Dipartimento Sicurezza Alimentare") %>% 
#          summarise(n = nlevels(factor(Codice)))
#     ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
# })
# 
# ###tabella x reparti dsa####
# output$t2 <- renderUI({
#   border <- officer::fp_border()
#   flextable(
#     # (tdsa %>%
#     #    filter(Reparto != "Totale") %>% 
#     #    left_join(
#     #      (ricerca %>%
#     #         filter(IF == IF) %>%
#     #         count(Reparto, nr) %>%
#     #         group_by(Reparto) %>%
#     #         count(nr) %>%
#     #         summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
#     #    left_join(
#     #      (pr %>% 
#     #         group_by(Reparto) %>% 
#     #         summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
#     #         filter(!is.na(Reparto))),  by = "Reparto")
#     #  
#     # )
#     Tdsa
#     )%>%
#     theme_booktabs() %>%
#     color(i = 1, color = "blue", part = "header") %>%
#     bold( part = "header") %>%
#     fontsize(size=15) %>%
#     fontsize(part = "header", size = 15) %>%
#     line_spacing(space = 2.5) %>%
#     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
#     autofit() %>%
#     color(j= "R/FTET", color = "red", part = "all") %>%
#     color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
#     color(j= "Progetti di Ricerca", color = "red", part = "all") %>% 
#     vline(j= "RT", border = border, part = "all") %>%
#     footnote(i=1, j=3:10,
#              value = as_paragraph(
#                c("Full Time Equivalenti Dirigenza",
#                  "Full Time Equivalenti Comparto",
#                  "Full Time Equivalenti Totale",
#                  "Ricavo da Analisi",
#                  "Ricavo Vendita Prodotti",
#                  "Ricavo Attività Interna",
#                  "Ricavo Totale",
#                  "Ricavo per Full Equivalenti Totale")
#              ),
#              ref_symbols = c("a","b","c","d","e","f","g","h"),
#              part = "header", inline = T) %>%
#     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
#     htmltools_value()
# })
# 
# #### grafico benchmarking dsa####
# tb2 <- reactive({tdsa %>% 
#     filter(Reparto != "Totale") %>% 
#     mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
#            "RA" = round(100*(RA/sum(RA)),1), 
#            "FTED" = round(100*(FTED/sum(FTED)),1), 
#            "FTEC" = round(100*(FTEC/sum(FTEC)),1),
#            "RVP" =round(100*(RVP/sum(RVP)),1), 
#            "RAI" = round(100*(RAI/sum(RAI)), 1),
#            "RT" = round(100*(RT/sum(RT)),1),
#            "FTET" = round(100*(FTET/ sum(FTET)), 1) 
#            # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
#     ) %>% 
#     select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT") %>% 
#     pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
#     mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
# })
# 
# output$tbd2 <- renderPlot( 
#   
#   if(input$ind2 == "Reparto")
#   
#   {  
#   
#   ggplot(tb2(),  aes( 
#     x = KPI, 
#     y = valore, 
#     fill = KPI
#   )) + geom_col(width = 0.9, color = "black")+
#     coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
#     scale_fill_brewer(palette = "Blues")+
#     geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#     theme(legend.position = "blank",
#           panel.background= element_blank(),
#           plot.background = element_blank(), 
#           strip.text.x = element_text(size = 12, colour = "blue"), 
#           axis.text.x = element_text(size = 10, color = "black"))+
#     labs(x = "", y = "") 
#  
# }
# 
# else
# 
# {
#   
#   tb2() %>% 
#     mutate(Reparto = recode(Reparto, "REPARTO PRODUZIONE PRIMARIA" = "RPP", 
#                                  "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM", 
#                                  "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB", 
#                                  "REPARTO CONTROLLO ALIMENTI" = "RCA")) %>% 
#     ggplot(aes( 
#       x = Reparto, 
#       y = valore, 
#       fill = Reparto
#     )) + geom_col(width = 0.9, color = "black")+
#     coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
#     scale_fill_brewer(palette = "Blues")+
#     geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#     theme(legend.position = "blank",
#           panel.background= element_blank(),
#           plot.background = element_blank(), 
#           strip.text.x = element_text(size = 15, colour = "blue"), 
#           axis.text.x = element_text(size = 10, color = "black"))+
#     labs(x = "", y = "")
# }, bg = "transparent")
# 
# 
# paper2 <- reactive({
#   ricerca %>% filter(IF == "IF" & Dipartimento == "Dipartimento Sicurezza Alimentare" ) %>% 
#     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese, "IF" = impf) %>%
#     unique() %>% 
#     arrange(desc(IF))
# })
# 
# output$articoli2 <- renderDataTable(paper2(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                        paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# Cint2 <- reactive({
#   ricerca %>% filter(INT == "Int" & Dipartimento == "Dipartimento Sicurezza Alimentare" ) %>% 
#     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
#     unique()
# })
# 
# output$convegni2 <- renderDataTable(Cint2(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                        paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# 
# 
# 
# Prj2 <- reactive({
#   pr %>%
#     filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
#     summarise(Budget = sum(Budget), nUO = n()) %>% 
#     ungroup() %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
#     summarise(Durata = as.numeric(DataFine-DataInizio), 
#               R = as.numeric(date("2019-12-31")-date(DataInizio)), 
#               Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
#               Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
#     mutate(DataInizio = as.character(DataInizio), 
#            DataFine = as.character(DataFine)) %>% 
#     arrange(Realizzazione) %>% 
#     select(-R, -Durata)
#   
# })
# 
# output$projr2 <- renderDataTable(Prj2(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                      paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# ###tabelle modali percentuali KPI
# 
# output$tbw2 <- renderTable(tb2() %>% 
#                             pivot_wider(names_from = "KPI", values_from = "valore"))
# 
# ####radar plot modale####
# radar2 <- reactive({
#   df<-data.frame(
#     Pubblicazioni = Tdsa$Pubblicazioni/Tdsa$FTED,
#     Progetti = Tdsa$`Progetti di Ricerca`/Tdsa$FTED, 
#     RFTE = Tdsa$`R/FTET`/((sum(Tdsa$RT)/sum(Tdsa$FTET))),
#     Esami = Tdsa$N.esami/sum(Tdsa$N.esami),
#     FTED = Tdsa$FTED/sum(Tdsa$FTED),
#     FTEC = Tdsa$FTEC/sum(Tdsa$FTEC),
#     FTET = Tdsa$FTET/sum(Tdsa$FTET),
#     RT = Tdsa$RT/sum(Tdsa$RT))
#   rownames(df) <- Tdsa$Reparto
#   df <- rbind(c(4,5,4,1,1,1,1,1) , rep(0,8) , df)
# })
# 
# output$radarDSA <-renderPlot({   
#   colors_in=c( "red", "blue", "green", "black")
#   radarchart( radar2()  ,
#               #custom polygon
#               pcol=colors_in , plwd=1 , plty=1,
#               #custom the grid
#               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
#               #custom labels
#               #vlcex=0.8
#   )
#   legend(x=1.5, y=0.7, legend = rownames(radar2()[-c(1,2),]), col = colors_in,  bty = "n", pch=16 , cex=0.8, pt.cex=1)
# })
# 
# 
# 
# 
# 
# 
# 
# 
# ###DTSA####
# 
# ###value boxes dtsa####
# es3 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                   select("N.esami"))
# 
# output$esami3 <- renderValueBox({
#   valueBox(prettyNum(es3(), big.mark = "."), "N. esami",  icon = icon("flask"),
#            color = "blue"
#   )
# })
# 
# ra3 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                   select("RA"))
# 
# output$ra3 <- renderValueBox({
#   valueBox(prettyNum(ra3(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# vp3 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                   select("RVP"))
# 
# output$vp3 <- renderValueBox({
#   valueBox(prettyNum(vp3(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# 
# ai3 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                   select("RAI"))
# 
# output$ai3 <- renderValueBox({
#   valueBox(prettyNum(ai3(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rt3 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                   select("RT"))
# 
# output$rt3 <- renderValueBox({
#   valueBox(prettyNum(rt3(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rfte3 <- reactive(tizsler %>% 
#                     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#                     select("R/FTET"))
# 
# output$rfte3 <- renderValueBox({
#   valueBox(prettyNum(rfte3(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# # ric3 <- reactive({
# #   ricerca %>%
# #     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
# #     group_by(tipologia) %>% 
# #     count(nr) %>%
# #     summarise(n.articoli = n())
# # })
# 
# output$IF3 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(IF == "IF" & Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
# })
# 
# 
# output$Int3 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(INT == "Int" & Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
# })
# 
# output$PR3 <- renderValueBox({
#   valueBox(
#     (  pr %>% 
#          filter(Dipartimento=="Dipartimento Tutela e  Salute Animale") %>% 
#          summarise(n = nlevels(factor(Codice)))
#     ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
# })
# 
# # output$Naz3 <- renderValueBox({
# #   valueBox(
# #     (ricerca %>% 
# #        filter(NAZ == "Naz" & Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
# #        group_by(nr) %>% 
# #        count(nr) %>% 
# #        nrow()), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
# # })
# 
# 
# #### tabella x reparti dtsa######
# 
# output$t3 <- renderUI({
#   border <- officer::fp_border()
#   flextable(Tdtsa
#     # (tdtsa %>%
#     #    filter(Reparto != "Totale") %>% 
#     #    left_join(
#     #      (ricerca %>%
#     #         filter(IF == IF) %>%
#     #         count(Reparto, nr) %>%
#     #         group_by(Reparto) %>%
#     #         count(nr) %>%
#     #         summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
#     #    left_join(
#     #      (pr %>% 
#     #         group_by(Reparto) %>% 
#     #         summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
#     #         filter(!is.na(Reparto))),  by = "Reparto")
#     #  
#     )%>%
#     theme_booktabs() %>%
#     color(i = 1, color = "blue", part = "header") %>%
#     bold( part = "header") %>%
#     fontsize(size=15) %>%
#     fontsize(part = "header", size = 15) %>%
#     line_spacing(space = 2.5) %>%
#     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
#     autofit() %>%
#     color(j= "R/FTET", color = "red", part = "all") %>%
#     color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
#     color(j= "Progetti di Ricerca", color = "red", part = "all") %>% 
#     vline(j= "RT", border = border, part = "all") %>%
#     footnote(i=1, j=3:10,
#              value = as_paragraph(
#                c("Full Time Equivalenti Dirigenza",
#                  "Full Time Equivalenti Comparto",
#                  "Full Time Equivalenti Totale",
#                  "Ricavo da Analisi",
#                  "Ricavo Vendita Prodotti",
#                  "Ricavo Attività Interna",
#                  "Ricavo Totale",
#                  "Ricavo per Full Equivalenti Totale")
#              ),
#              ref_symbols = c("a","b","c","d","e","f","g","h"),
#              part = "header", inline = T) %>%
#     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
#     htmltools_value()
# })
# 
# 
# 
# 
# 
# 
# 
# #### grafico benchmarking dtsa####
# tb3 <- reactive({tdtsa %>% 
#     filter(Reparto != "Totale") %>% 
#     mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
#            "RA" = round(100*(RA/sum(RA)),1), 
#            "FTED" = round(100*(FTED/sum(FTED)),1), 
#            "FTEC" = round(100*(FTEC/sum(FTEC)),1),
#            "RVP" =round(100*(RVP/sum(RVP)),1), 
#            "RAI" = round(100*(RAI/sum(RAI)), 1),
#            "RT" = round(100*(RT/sum(RT)),1),
#            "FTET" = round(100*(FTET/ sum(FTET)), 1) 
#            # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
#     ) %>% 
#     select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
#     pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
#     mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT")))
# })
# 
# output$tbd3 <- renderPlot( 
#   
#   if(input$ind3 == "Reparto")
#     
#   {  
#     
#     ggplot(tb3(),  aes( 
#       x = KPI, 
#       y = valore, 
#       fill = KPI
#     )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 12, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "") 
#     
#   }
#   
#   else
#     
#   {
#     
#     tb3() %>% 
#       mutate(Reparto = recode(Reparto, "REPARTO VIROLOGIA" = "RVIR", 
#                               "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB", 
#                               "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB", 
#                               "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA")) %>% 
#       ggplot(aes( 
#         x = Reparto, 
#         y = valore, 
#         fill = Reparto
#       )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 15, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "")
#   }, bg = "transparent")
# 
# 
# ### tabelle modali pubblicazioni e convegni dsa####
# 
# # paper3 <- reactive({
# #   ricerca %>% filter(tipologia == "IF") %>% 
# #     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# 
# paper3 <- reactive({
#   ricerca %>% filter(IF == "IF" & Dipartimento == "Dipartimento Tutela e  Salute Animale" ) %>% 
#     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese, "IF" = impf) %>%
#     unique() %>% 
#     arrange(desc(IF))
# })
# 
# output$articoli3 <- renderDataTable(paper3(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                       paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# ###tabella modale convegni internazionali
# 
# # Cint3 <- reactive({
# #   ricerca %>% filter(tipologia == "Int") %>% 
# #     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# Cint3 <- reactive({
#   ricerca %>% filter(INT == "Int" & Dipartimento == "Dipartimento Tutela e  Salute Animale" ) %>% 
#     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
#     unique()
# })
# 
# 
# output$convegni3 <- renderDataTable(Cint3(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                       paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# 
# Prj3 <- reactive({
#   pr %>%
#     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
#     summarise(Budget = sum(Budget), nUO = n()) %>% 
#     ungroup() %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
#     summarise(Durata = as.numeric(DataFine-DataInizio), 
#               R = as.numeric(date("2019-12-31")-date(DataInizio)), 
#               Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
#               Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
#     mutate(DataInizio = as.character(DataInizio), 
#            DataFine = as.character(DataFine)) %>% 
#     arrange(Realizzazione) %>% 
#     select(-R, -Durata)
#   
# })
# 
# output$projr3 <- renderDataTable(Prj3(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                  extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                         paging = TRUE,autoWidth = TRUE,
#                                                                        buttons = c('excel')))
# 
# 
# ###tabella modale convegni Nazionali
# 
# # Cnaz3 <- reactive({
# #   ricerca %>% filter(tipologia == "Naz") %>% 
# #     filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# # Cnaz3 <- reactive({
# #   ricerca %>% filter(NAZ == "Naz" & Dipartimento == "Dipartimento Tutela e  Salute Animale" ) %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# # 
# # 
# # output$nazionali3 <- renderTable(Cnaz3())
# 
# 
# ###tabelle modali percentuali KPI
# 
# output$tbw3 <- renderTable(tb3() %>% 
#                             pivot_wider(names_from = "KPI", values_from = "valore"))
# 
# ####radar plot modale####
# 
# radar3 <- reactive({
#   df<-data.frame(
#     Pubblicazioni = Tdtsa$Pubblicazioni/Tdtsa$FTED,
#     Progetti = Tdtsa$`Progetti di Ricerca`/Tdtsa$FTED, 
#     RFTE = Tdtsa$`R/FTET`/((sum(Tdtsa$RT)/sum(Tdtsa$FTET))),
#     Esami = Tdtsa$N.esami/sum(Tdtsa$N.esami),
#     FTED = Tdtsa$FTED/sum(Tdtsa$FTED),
#     FTEC = Tdtsa$FTEC/sum(Tdtsa$FTEC),
#     FTET = Tdtsa$FTET/sum(Tdtsa$FTET),
#     RT = Tdtsa$RT/sum(Tdtsa$RT))
#   rownames(df) <- Tdtsa$Reparto
#   df <- rbind(c(4,7,4,1,1,1,1,1) , rep(0,8) , df)
# })
# 
# output$radarDTSA <-renderPlot({   
#   colors_in=c( "red", "blue", "green", "black")
#   radarchart( radar3()  ,
#               #custom polygon
#               pcol=colors_in , plwd=1 , plty=1,
#               #custom the grid
#               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
#               #custom labels
#               #vlcex=0.8
#   )
#   legend(x=1.5, y=0.7, legend = rownames(radar3()[-c(1,2),]), col = colors_in,  bty = "n", pch=16 , cex=0.8, pt.cex=1)
# })
# 
# 
# 
# ###ATLOMB####
# ###value boxes atlomb####
# es4 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                   select("N.esami"))
# 
# output$esami4 <- renderValueBox({
#   valueBox(prettyNum(es4(), big.mark = "."), "N. esami",  icon = icon("flask"),
#            color = "blue"
#   )
# })
# 
# ra4 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                   select("RA"))
# 
# output$ra4 <- renderValueBox({
#   valueBox(prettyNum(ra4(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# vp4 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                   select("RVP"))
# 
# output$vp4 <- renderValueBox({
#   valueBox(prettyNum(vp4(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# 
# ai4 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                   select("RAI"))
# 
# output$ai4 <- renderValueBox({
#   valueBox(prettyNum(ai4(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rt4 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                   select("RT"))
# 
# output$rt4 <- renderValueBox({
#   valueBox(prettyNum(rt4(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rfte4 <- reactive(tizsler %>% 
#                     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#                     select("R/FTET"))
# 
# output$rfte4 <- renderValueBox({
#   valueBox(prettyNum(rfte4(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# # ric4 <- reactive({
# #   ricerca %>%
# #     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
# #     group_by(tipologia) %>% 
# #     count(nr) %>%
# #     summarise(n.articoli = n())
# # })
# 
# output$IF4 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(IF == "IF" & Dipartimento == "Area Territoriale Lombardia") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
# })
# 
# 
# output$Int4 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(INT == "Int" & Dipartimento == "Area Territoriale Lombardia") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
# })
# 
# output$PR4 <- renderValueBox({
#   valueBox(
#     (  pr %>% 
#          filter(Dipartimento=="Area Territoriale Lombardia") %>% 
#          summarise(n = nlevels(factor(Codice)))
#     ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
# })
# 
# 
# 
# 
# # output$Naz4 <- renderValueBox({
# #   valueBox(
# #     (ricerca %>% 
# #        filter(NAZ == "Naz" & Dipartimento == "Area Territoriale Lombardia") %>% 
# #        group_by(nr) %>% 
# #        count(nr) %>% 
# #        nrow()), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
# # })
# 
# 
# #### tabella x reparti atlomb######
# 
# 
# 
# 
# output$t4 <- renderUI({
#   border <- officer::fp_border()
#   flextable(Tatlomb
#     # (tatlomb %>%
#     #    filter(Reparto != "Totale") %>% 
#     #    left_join(
#     #      (ricerca %>%
#     #         filter(IF == IF) %>%
#     #         count(Reparto, nr) %>%
#     #         group_by(Reparto) %>%
#     #         count(nr) %>%
#     #         summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
#     #    left_join(
#     #      (pr %>% 
#     #         group_by(Reparto) %>% 
#     #         summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
#     #         filter(!is.na(Reparto))),  by = "Reparto")
#     #  
#     # )
#     )%>%
#     theme_booktabs() %>%
#     color(i = 1, color = "blue", part = "header") %>%
#     bold( part = "header") %>%
#     fontsize(size=15) %>%
#     fontsize(part = "header", size = 15) %>%
#     line_spacing(space = 2.5) %>%
#     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
#     autofit() %>%
#     color(j= "R/FTET", color = "red", part = "all") %>%
#     color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
#     color(j= "Progetti di Ricerca", color = "red", part = "all") %>% 
#     vline(j= "RT", border = border, part = "all") %>%
#     footnote(i=1, j=3:10,
#              value = as_paragraph(
#                c("Full Time Equivalenti Dirigenza",
#                  "Full Time Equivalenti Comparto",
#                  "Full Time Equivalenti Totale",
#                  "Ricavo da Analisi",
#                  "Ricavo Vendita Prodotti",
#                  "Ricavo Attività Interna",
#                  "Ricavo Totale",
#                  "Ricavo per Full Equivalenti Totale")
#              ),
#              ref_symbols = c("a","b","c","d","e","f","g","h"),
#              part = "header", inline = T) %>%
#     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
#     htmltools_value()
# })
# 
# 
# 
# # output$t4 <- renderUI({
# #   border <- officer::fp_border()
# #   flextable(
# #     (tatlomb %>% 
# #        left_join(
# #          (ricerca %>% 
# #             filter(IF == IF & Dipartimento == "Area Territoriale Lombardia") %>%  
# #             count(Reparto, nr) %>% 
# #             group_by(Reparto) %>% 
# #             count(nr) %>% 
# #             summarise("Pubblicazioni" = sum(n)) %>% 
# #             bind_rows(data.frame("Pubblicazioni" =(ricerca %>% 
# #                                                      filter(IF == "IF" & Dipartimento == "Area Territoriale Lombardia") %>% 
# #                                                      group_by(nr) %>% 
# #                                                      count(nr) %>% 
# #                                                      select(nr) %>% 
# #                                                      nrow()))) %>% 
# #             replace_na(list(Reparto ="Totale"))), by = "Reparto")) %>% 
# #       filter(Reparto != "Totale")
# #   ) %>%
# #     theme_booktabs() %>% 
# #     color(i = 1, color = "blue", part = "header") %>% 
# #     bold( part = "header") %>% 
# #     fontsize(size=15) %>% 
# #     fontsize(part = "header", size = 15) %>% 
# #     line_spacing(space = 2.5) %>% 
# #     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
# #     autofit() %>% 
# #     color(j= "R/FTET", color = "red", part = "all") %>% 
# #     color(j= "Pubblicazioni",color = "red", part = "all" ) %>% 
# #     vline(j= "RT", border = border, part = "all") %>% 
# #     footnote(i=1, j=3:10,
# #              value = as_paragraph(
# #                c("Full Time Equivalenti Dirigenza",
# #                  "Full Time Equivalenti Comparto",
# #                  "Full Time Equivalenti Totale",
# #                  "Ricavo da Analisi",
# #                  "Ricavo Vendita Prodotti",
# #                  "Ricavo Attività Interna",
# #                  "Ricavo Totale",
# #                  "Ricavo per Full Equivalenti Totale")
# #              ),
# #              ref_symbols = c("a","b","c","d","e","f","g","h"),
# #              part = "header", inline = T) %>%
# #     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
# #     htmltools_value()
# # })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #### grafico benchmarking dtsa####
# tb4 <- reactive({tatlomb %>% 
#     filter(Reparto != "Totale") %>% 
#     mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
#            "RA" = round(100*(RA/sum(RA)),1), 
#            "FTED" = round(100*(FTED/sum(FTED)),1), 
#            "FTEC" = round(100*(FTEC/sum(FTEC)),1),
#            "RVP" =round(100*(RVP/sum(RVP)),1), 
#            "RAI" = round(100*(RAI/sum(RAI)), 1),
#            "RT" = round(100*(RT/sum(RT)),1),
#            "FTET" = round(100*(FTET/ sum(FTET)), 1) 
#            # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
#     ) %>% 
#     select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
#     pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
#     mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
# })
# 
# output$tbd4 <- renderPlot( 
#   
#   if(input$ind4 == "Reparto")
#     
#   {  
#     
#     ggplot(tb4(),  aes( 
#       x = KPI, 
#       y = valore, 
#       fill = KPI
#     )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 12, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "") 
#     
#   }
#   
#   else
#     
#   {
#     
#     tb4() %>% 
#       mutate(Reparto = recode(Reparto, "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN", 
#                               "SEDE TERRITORIALE DI BRESCIA" = "BS", 
#                               "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO", 
#                               "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI", 
#                               "SEDE TERRITORIALE DI PAVIA" = "PV")) %>% 
#       ggplot(aes( 
#         x = Reparto, 
#         y = valore, 
#         fill = Reparto
#       )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 15, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "")
#   }, bg = "transparent")
# 
# ### tabelle modali pubblicazioni e convegni atlomb####
# 
# # paper4 <- reactive({
# #   ricerca %>% filter(tipologia == "IF") %>% 
# #     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# 
# paper4 <- reactive({
#   ricerca %>% filter(IF == "IF" & Dipartimento == "Area Territoriale Lombardia" ) %>% 
#     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese, "IF" = impf) %>%
#     unique() %>% 
#     arrange(desc(IF))
# })
# 
# 
# output$articoli4 <- renderDataTable(paper4(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                        paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# ###tabella modale convegni internazionali
# 
# # Cint4 <- reactive({
# #   ricerca %>% filter(tipologia == "Int") %>% 
# #     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# Cint4 <- reactive({
#   ricerca %>% filter(INT == "Int" & Dipartimento == "Area Territoriale Lombardia" ) %>% 
#     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
#     unique()
# })
# 
# output$convegni4 <- renderDataTable(Cint4(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                       paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# Prj4 <- reactive({
#   pr %>%
#     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
#     summarise(Budget = sum(Budget), nUO = n()) %>% 
#     ungroup() %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
#     summarise(Durata = as.numeric(DataFine-DataInizio), 
#               R = as.numeric(date("2019-12-31")-date(DataInizio)), 
#               Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
#               Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
#     mutate(DataInizio = as.character(DataInizio), 
#            DataFine = as.character(DataFine)) %>% 
#     arrange(Realizzazione) %>% 
#     select(-R, -Durata)
#   
# })
# 
# output$projr4 <- renderDataTable(Prj4(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                  extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                         paging = TRUE,autoWidth = TRUE,
#                                                                        buttons = c('excel')))
# 
# 
# 
# 
# 
# 
# ###tabella modale convegni Nazionali
# 
# # Cnaz4 <- reactive({
# #   ricerca %>% filter(tipologia == "Naz") %>% 
# #     filter(Dipartimento == "Area Territoriale Lombardia") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# # Cnaz4 <- reactive({
# #   ricerca %>% filter(NAZ == "Naz" & Dipartimento == "Area Territoriale Lombardia" ) %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# # output$nazionali4 <- renderTable(Cnaz4())
# 
# 
# ###tabelle modali percentuali KPI
# 
# output$tbw4 <- renderTable(tb4() %>% 
#                             pivot_wider(names_from = "KPI", values_from = "valore"))
# 
# 
# ####radar plot modale####
# 
# radar4 <- reactive({
#   df<-data.frame(
#     Pubblicazioni = Tatlomb$Pubblicazioni/Tatlomb$FTED,
#     Progetti = Tatlomb$`Progetti di Ricerca`/Tatlomb$FTED, 
#     RFTE = Tatlomb$`R/FTET`/((sum(Tatlomb$RT)/sum(Tatlomb$FTET))),
#     Esami = Tatlomb$N.esami/sum(Tatlomb$N.esami),
#     FTED = Tatlomb$FTED/sum(Tatlomb$FTED),
#     FTEC = Tatlomb$FTEC/sum(Tatlomb$FTEC),
#     FTET = Tatlomb$FTET/sum(Tatlomb$FTET),
#     RT = Tatlomb$RT/sum(Tatlomb$RT))
#   rownames(df) <- Tatlomb$Reparto
#   df <- rbind(c(4,5.5,4,1,1,1,1,1) , rep(0,8) , df)
# })
# 
# output$radarATLOMB <-renderPlot({   
#   colors_in=c( "red", "blue", "green", "black", "brown")
#   radarchart( radar4()  ,
#               #custom polygon
#               pcol=colors_in , plwd=1 , plty=1,
#               #custom the grid
#               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
#               #custom labels
#               #vlcex=0.8
#   )
#   legend(x=1.5, y=0.7, legend = rownames(radar4()[-c(1,2),]), col = colors_in,  bty = "n", pch=16 , cex=0.8, pt.cex=1)
# })
# 
# 
# 
# 
# 
# 
# ###ATER####
# ###value boxes ater####
# es5 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                   select("N.esami"))
# 
# output$esami5 <- renderValueBox({
#   valueBox(prettyNum(es5(), big.mark = "."), "N. esami",  icon = icon("flask"),
#            color = "blue"
#   )
# })
# 
# ra5 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                   select("RA"))
# 
# output$ra5 <- renderValueBox({
#   valueBox(prettyNum(ra5(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# vp5<- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                   select("RVP"))
# 
# output$vp5 <- renderValueBox({
#   valueBox(prettyNum(vp5(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# 
# ai5 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                   select("RAI"))
# 
# output$ai5 <- renderValueBox({
#   valueBox(prettyNum(ai5(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rt5 <- reactive(tizsler %>% 
#                   filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                   select("RT"))
# 
# output$rt5 <- renderValueBox({
#   valueBox(prettyNum(rt5(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# rfte5 <- reactive(tizsler %>% 
#                     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#                     select("R/FTET"))
# 
# output$rfte5 <- renderValueBox({
#   valueBox(prettyNum(rfte5(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
#            color = "aqua"
#   )
# })
# 
# # ric5 <- reactive({
# #   ricerca %>%
# #     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #     group_by(tipologia) %>% 
# #     count(nr) %>%
# #     summarise(n.articoli = n())
# # })
# 
# output$IF5 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(IF == "IF" & Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
# })
# 
# 
# output$Int5 <- renderValueBox({
#   valueBox(
#     (ricerca %>% 
#        filter(INT == "Int" & Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#        group_by(nr) %>% 
#        count(nr) %>% 
#        nrow()), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
# })
# 
# output$PR5 <- renderValueBox({
#   valueBox(
#     (  pr %>% 
#          filter(Dipartimento=="Area Territoriale Emilia Romagna") %>% 
#          summarise(n = nlevels(factor(Codice)))
#     ), "Progetti di ricerca in corso ", icon = icon("user-graduate"), color = "light-blue")
# })
# 
# # output$Naz5 <- renderValueBox({
# #   valueBox(
# #     (ricerca %>% 
# #        filter(NAZ == "Naz" & Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #        group_by(nr) %>% 
# #        count(nr) %>% 
# #        nrow()), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
# # })
# 
# #### tabella x reparti ater######
# 
# output$t5 <- renderUI({
#   border <- officer::fp_border()
#   flextable(Tater
#     # (tater %>%
#     #    filter(Reparto != "Totale") %>% 
#     #    left_join(
#     #      (ricerca %>%
#     #         filter(IF == IF) %>%
#     #         count(Reparto, nr) %>%
#     #         group_by(Reparto) %>%
#     #         count(nr) %>%
#     #         summarise("Pubblicazioni" = sum(n))), by = "Reparto") %>% 
#     #    left_join(
#     #      (pr %>% 
#     #         group_by(Reparto) %>% 
#     #         summarise("Progetti di Ricerca"=nlevels(factor(Codice))) %>% 
#     #         filter(!is.na(Reparto))),  by = "Reparto")
#     #  
#     # )
#     )%>%
#     theme_booktabs() %>%
#     color(i = 1, color = "blue", part = "header") %>%
#     bold( part = "header") %>%
#     fontsize(size=15) %>%
#     fontsize(part = "header", size = 15) %>%
#     line_spacing(space = 2.5) %>%
#     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
#     autofit() %>%
#     color(j= "R/FTET", color = "red", part = "all") %>%
#     color(j= "Pubblicazioni",color = "red", part = "all" ) %>%
#     color(j= "Progetti di Ricerca", color = "red", part = "all") %>% 
#     vline(j= "RT", border = border, part = "all") %>%
#     footnote(i=1, j=3:10,
#              value = as_paragraph(
#                c("Full Time Equivalenti Dirigenza",
#                  "Full Time Equivalenti Comparto",
#                  "Full Time Equivalenti Totale",
#                  "Ricavo da Analisi",
#                  "Ricavo Vendita Prodotti",
#                  "Ricavo Attività Interna",
#                  "Ricavo Totale",
#                  "Ricavo per Full Equivalenti Totale")
#              ),
#              ref_symbols = c("a","b","c","d","e","f","g","h"),
#              part = "header", inline = T) %>%
#     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>%
#     htmltools_value()
# })
# 
# 
# # output$t5 <- renderUI({
# #   border <- officer::fp_border()
# #   flextable(
# #     (tater%>% 
# #        left_join(
# #          (ricerca %>% 
# #             filter(IF == IF & Dipartimento == "Area Territoriale Emilia Romagna") %>%  
# #             count(Reparto, nr) %>% 
# #             group_by(Reparto) %>% 
# #             count(nr) %>% 
# #             summarise("Pubblicazioni" = sum(n)) %>% 
# #             bind_rows(data.frame("Pubblicazioni" =(ricerca %>% 
# #                                                      filter(IF == "IF" & Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #                                                      group_by(nr) %>% 
# #                                                      count(nr) %>% 
# #                                                      select(nr) %>% 
# #                                                      nrow()))) %>% 
# #             replace_na(list(Reparto ="Totale"))), by = "Reparto")) %>% 
# #       filter(Reparto != "Totale")
# #   ) %>%
# #     theme_booktabs() %>% 
# #     color(i = 1, color = "blue", part = "header") %>% 
# #     bold( part = "header") %>% 
# #     fontsize(size=15) %>% 
# #     fontsize(part = "header", size = 15) %>% 
# #     line_spacing(space = 2.5) %>% 
# #     colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
# #     autofit() %>% 
# #     color(j= "R/FTET", color = "red", part = "all") %>% 
# #     color(j= "Pubblicazioni",color = "red", part = "all" ) %>% 
# #     vline(j= "RT", border = border, part = "all") %>% 
# #     footnote(i=1, j=3:10,
# #              value = as_paragraph(
# #                c("Full Time Equivalenti Dirigenza",
# #                  "Full Time Equivalenti Comparto",
# #                  "Full Time Equivalenti Totale",
# #                  "Ricavo da Analisi",
# #                  "Ricavo Vendita Prodotti",
# #                  "Ricavo Attività Interna",
# #                  "Ricavo Totale",
# #                  "Ricavo per Full Equivalenti Totale")
# #              ),
# #              ref_symbols = c("a","b","c","d","e","f","g","h"),
# #              part = "header", inline = T) %>%
# #     fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
# #     htmltools_value()
# # })
# 
# 
# 
# 
# 
# #### grafico benchmarking dtsa####
# tb5 <- reactive({tater %>% 
#     filter(Reparto != "Totale") %>% 
#     mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
#            "RA" = round(100*(RA/sum(RA)),1), 
#            "FTED" = round(100*(FTED/sum(FTED)),1), 
#            "FTEC" = round(100*(FTEC/sum(FTEC)),1),
#            "RVP" =round(100*(RVP/sum(RVP)),1), 
#            "RAI" = round(100*(RAI/sum(RAI)), 1),
#            "RT" = round(100*(RT/sum(RT)),1),
#            "FTET" = round(100*(FTET/ sum(FTET)), 1) 
#            # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
#     ) %>% 
#     select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
#     pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
#     mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
# })
# 
# output$tbd5 <- renderPlot( 
#   
#   if(input$ind5 == "Reparto")
#     
#   {  
#     
#     ggplot(tb5(),  aes( 
#       x = KPI, 
#       y = valore, 
#       fill = KPI
#     )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 12, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "") 
#     
#   }
#   
#   else
#     
#   {
#     
#     tb5() %>% 
#       mutate(Reparto = recode(Reparto, "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE", 
#                               "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA", 
#                               "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR", 
#                               "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE")) %>% 
#       ggplot(aes( 
#         x = Reparto, 
#         y = valore, 
#         fill = Reparto
#       )) + geom_col(width = 0.9, color = "black")+
#       coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
#       scale_fill_brewer(palette = "Blues")+
#       geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
#       theme(legend.position = "blank",
#             panel.background= element_blank(),
#             plot.background = element_blank(), 
#             strip.text.x = element_text(size = 15, colour = "blue"), 
#             axis.text.x = element_text(size = 10, color = "black"))+
#       labs(x = "", y = "")
#   }, bg = "transparent")
# 
# ### tabelle modali pubblicazioni e convegni ater####
# 
# # paper5 <- reactive({
# #   ricerca %>% filter(tipologia == "IF") %>% 
# #     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# 
# paper5 <- reactive({
#   ricerca %>% filter(IF == "IF" & Dipartimento == "Area Territoriale Emilia Romagna" ) %>% 
#     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese, "IF" = impf) %>%
#     unique() %>% 
#     arrange(desc(IF))
# })
# 
# output$articoli5 <- renderDataTable(paper5(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                       paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# ###tabella modale convegni internazionali
# 
# # Cint5 <- reactive({
# #   ricerca %>% filter(tipologia == "Int") %>% 
# #     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# Cint5 <- reactive({
#   ricerca %>% filter(INT == "Int" & Dipartimento == "Area Territoriale Emilia Romagna" ) %>% 
#     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
#     unique()
# })
# 
# output$convegni5 <- renderDataTable(Cint5(),server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                 extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                       paging = TRUE,autoWidth = TRUE,
#                                                                       buttons = c('excel')))
# 
# 
# 
# Prj5 <- reactive({
#   pr %>%
#     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>% 
#     summarise(Budget = sum(Budget), nUO = n()) %>% 
#     ungroup() %>% 
#     group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient, Budget, nUO) %>% 
#     summarise(Durata = as.numeric(DataFine-DataInizio), 
#               R = as.numeric(date("2019-12-31")-date(DataInizio)), 
#               Realizzazione = ifelse(R>Durata, 100, 100*(R/Durata)),
#               Realizzazione = paste(round(Realizzazione, 0), "%") )%>% 
#     mutate(DataInizio = as.character(DataInizio), 
#            DataFine = as.character(DataFine)) %>% 
#     arrange(Realizzazione) %>% 
#     select(-R, -Durata)
#   
# })
# 
# output$projr5 <- renderDataTable(Prj5(), server = FALSE, class = 'cell-border stripe', rownames=FALSE,
#                                  extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                                                         paging = TRUE,autoWidth = TRUE,
#                                                                        buttons = c('excel')))
# 
# 
# 
# 
# 
# 
# 
# ###tabella modale convegni Nazionali
# 
# # Cnaz5 <- reactive({
# #   ricerca %>% filter(tipologia == "Naz") %>% 
# #     filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
# #     select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
# #     unique()
# #   
# # })
# 
# # Cnaz5 <- reactive({
# #   ricerca %>% filter(NAZ == "Naz" & Dipartimento == "Area Territoriale Emilia Romagna" ) %>% 
# #     select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
# #     unique()
# # })
# # 
# # output$nazionali5 <- renderTable(Cnaz5())
# 
# 
# ###tabelle modali percentuali KPI
# 
# output$tbw5 <- renderTable(tb5() %>% 
#                             pivot_wider(names_from = "KPI", values_from = "valore"))
# 
# 
# 
# ####radar plot modale####
# 
# radar5 <- reactive({
#   df<-data.frame(
#     Pubblicazioni = Tater$Pubblicazioni/Tater$FTED,
#     Progetti = Tater$`Progetti di Ricerca`/Tater$FTED, 
#     RFTE = Tater$`R/FTET`/((sum(Tater$RT)/sum(Tater$FTET))),
#     Esami = Tater$N.esami/sum(Tater$N.esami),
#     FTED = Tater$FTED/sum(Tater$FTED),
#     FTEC = Tater$FTEC/sum(Tater$FTEC),
#     FTET = Tater$FTET/sum(Tater$FTET),
#     RT = Tater$RT/sum(Tater$RT))
#   rownames(df) <- Tater$Reparto
#   df <- rbind(c(4,8,4,1,1,1,1,1) , rep(0,8) , df)
# })
# 
# output$radarATER <-renderPlot({   
#   colors_in=c( "red", "blue", "green", "black")
#   radarchart( radar5()  ,
#               #custom polygon
#               pcol=colors_in , plwd=1 , plty=1,
#               #custom the grid
#               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
#               #custom labels
#               #vlcex=0.8
#   )
#   legend(x=1.5, y=0.7, legend = rownames(radar5()[-c(1,2),]), col = colors_in,  bty = "n", pch=16 , cex=0.8, pt.cex=1)
# })
# 
# 
# 
# 
# 
# 
# 
# 
# ###PROGRAMMAZIONE####
# #ftedipa####
# dtProg <- readRDS( here("programmazione", "shinyapp-in-sviluppo", "datiSB.rds"))
# 
# output$progFTEv <- renderDataTable(
#   datatable(
#     if(input$DC == "FTED")
#     { 
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         group_by( Dipartimento) %>% 
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>% 
#         group_by(Dipartimento, "Obiettivi Valorizzati" = Valorizzazione) %>% 
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Dipartimento", values_from = "FTEDp") %>%  
#         arrange(desc(`Obiettivi Valorizzati`)) 
#     
#     }
#     
#     else
#     { 
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         group_by( Dipartimento) %>% 
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>% 
#         group_by(Dipartimento, "Obiettivi Valorizzati" = Valorizzazione) %>% 
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Dipartimento", values_from = "FTECp") %>%  
#         arrange(desc(`Obiettivi Valorizzati`)) 
#       
#     }, 
#     class = 'cell-border stripe', rownames=FALSE, 
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#                                            ) 
#     
#   ) %>% formatPercentage(2:7, 2)
# 
#   )
# 
# 
# 
# 
# 
# output$progFTE <- renderDataTable(
# 
# datatable(   
# if(input$DC == "FTED")
#   {  
#     dtProg %>% 
#     group_by(obcod, Obiettivo, Valorizzazione, Dipartimento) %>% 
#     summarise(FTED = sum(FTED, na.rm = T), 
#               FTEC = sum(FTEC, na.rm = T)) %>% 
#     group_by( Dipartimento) %>% 
#     mutate(FTEDp = prop.table(FTED), 
#            FTECp = prop.table(FTEC) ) %>%  
#     #group_by(Dipartimento, Valorizzazione) %>% 
#     #summarise(FTEDp = sum(FTEDp)) %>% 
#     pivot_wider(id_cols = 1:4, 
#                 names_from = "Dipartimento", values_from = "FTEDp") %>% 
#     mutate(total = rowSums(across(where(is.numeric))))%>% 
#     filter(total > 0.00000000) %>% 
#     arrange(desc(Valorizzazione)) %>% 
#     select(-total) %>% 
#     column_to_rownames(var = "obcod")
#     
#   }
#   
#   else
#     
#   { dtProg %>% 
#       group_by(obcod, Obiettivo, Valorizzazione, Dipartimento) %>% 
#       summarise(FTED = sum(FTED, na.rm = T), 
#                 FTEC = sum(FTEC, na.rm = T)) %>% 
#       group_by( Dipartimento) %>% 
#       mutate(FTEDp = prop.table(FTED), 
#              FTECp = prop.table(FTEC) ) %>%  
#       #group_by(Dipartimento, Valorizzazione) %>% 
#       #summarise(FTEDp = sum(FTEDp)) %>% 
#       pivot_wider(id_cols = 1:4, 
#                   names_from = "Dipartimento", values_from = "FTECp") %>% 
#       mutate(total = rowSums(across(where(is.numeric))))%>% 
#       filter(total > 0.00000000) %>% 
#       arrange(desc(Valorizzazione)) %>% 
#       select(-total) %>% 
#       column_to_rownames(var = "obcod")  
#     
#   },
# 
#   # server = FALSE, 
#   class = 'cell-border stripe', rownames=FALSE,
#   extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                         paging = TRUE,autoWidth = TRUE,
#                                         buttons = c('excel')) 
#   
# ) %>% formatPercentage(3:8, 2)
# )
# 
# ##ftedsa####
# #dtProg <- readRDS( here("programmazione", "shinyapp-in-sviluppo", "datiSB.rds"))
# 
# output$dsaFTEv <- renderDataTable(
#   datatable(
#     if(input$DC2 == "FTED1")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
# 
#     }
# 
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
# 
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
# 
#   ) %>% formatPercentage(2:5, 2)
# 
# )
# 
# output$dsaFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC2 == "FTED1")
#     {  
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         #group_by(Dipartimento, Valorizzazione) %>% 
#         #summarise(FTEDp = sum(FTEDp)) %>% 
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:6, 2)
# )
# 
# 
# 
# ##ftedtsa####
# #dtProg <- readRDS( here("programmazione", "shinyapp-in-sviluppo", "datiSB.rds"))
# 
# output$dtsaFTEv <- renderDataTable(
#   datatable(
#     if(input$DC3 == "FTED2")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Tutela Salute Animale") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     }
#     
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Tutela Salute Animale") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
#     
#   ) %>% formatPercentage(2:5, 2)
#   
# )
# 
# output$dtsaFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC3 == "FTED2")
#     {  
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Dipartimento Tutela Salute Animale") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Dipartimento Tutela Salute Animale") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:6, 2)
# )
# 
# #fteater####
# output$aterFTEv <- renderDataTable(
#   datatable(
#     if(input$DC4 == "FTED3")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Area Territoriale Emilia Romagna") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     }
#     
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Area Territoriale Emilia Romagna") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
#     
#   ) %>% formatPercentage(2:5, 2)
#   
# )
# 
# output$aterFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC4 == "FTED3")
#     {  
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Area Territoriale Emilia Romagna") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Area Territoriale Emilia Romagna") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:6, 2)
# )
# 
# 
# 
# #fteatlom####
# output$atlomFTEv <- renderDataTable(
#   datatable(
#     if(input$DC5 == "FTED4")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Area Territoriale Lombardia") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     }
#     
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Area Territoriale Lombardia") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
#     
#   ) %>% formatPercentage(2:6, 2)
#   
# )
# 
# output$atlomFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC5 == "FTED4")
#     {  
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Area Territoriale Lombardia") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Area Territoriale Lombardia") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:7, 2)
# )
# 
# 
# 
# 
# ###ftedirsan####
# output$dirsanFTEv <- renderDataTable(
#   datatable(
#     if(input$DC6 == "FTED5")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Direzione Sanitaria") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     }
#     
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Direzione Sanitaria") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
#     
#   ) %>% formatPercentage(2:5, 2)
#   
# )
# 
# output$dirsanFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC6 == "FTED5")
#     {  
#       dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Direzione Sanitaria") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Direzione Sanitaria") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:7, 2)
# )
# 
# ###ftedipam####
# output$dipamFTEv <- renderDataTable(
#   datatable(
#     if(input$DC7 == "FTED6")
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Amministrativo") %>% 
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>% 
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTEDp = sum(FTEDp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTEDp") %>%
#         mutate_all(~replace(., is.nan(.), 0)) %>% 
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     }
#     
#     else
#     {
#       dtProg %>%
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>%
#         summarise(FTED = sum(FTED, na.rm = T),
#                   FTEC = sum(FTEC, na.rm = T)) %>%
#         filter(Dipartimento == "Dipartimento Amministrativo") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED),
#                FTECp = prop.table(FTEC) ) %>%
#         group_by(Reparto, "Obiettivi Valorizzati" = Valorizzazione) %>%
#         summarise(FTECp = sum(FTECp)) %>%
#         pivot_wider(names_from = "Reparto", values_from = "FTECp") %>%
#         mutate_all(~replace(., is.nan(.), 0)) %>% 
#         arrange(desc(`Obiettivi Valorizzati`))
#       
#     },
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="t", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE
#     )
#     
#   ) %>% formatPercentage(2:6, 2)
#   
# )
# 
# output$dipamFTE <- renderDataTable(
#   
#   datatable(   
#     if(input$DC7 == "FTED6")
#     {  
#       dtProg %>% 
#         replace_na(list(FTED= 0, FTEC = 0)) %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>%  
#         filter(Dipartimento == "Dipartimento Amministrativo") %>%  
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTEDp") %>%  
#         mutate_all(~replace(., is.nan(.), 0)) %>% 
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod") 
#       
#     }
#     
#     else
#       
#     { dtProg %>% 
#         group_by(obcod, Obiettivo, Valorizzazione, Dipartimento, Reparto) %>% 
#         summarise(FTED = sum(FTED, na.rm = T), 
#                   FTEC = sum(FTEC, na.rm = T)) %>% 
#         filter(Dipartimento == "Dipartimento Amministrativo") %>%
#         group_by(Reparto) %>%
#         mutate(FTEDp = prop.table(FTED), 
#                FTECp = prop.table(FTEC) ) %>%  
#         pivot_wider(id_cols = 1:5, 
#                     names_from = "Reparto", values_from = "FTECp") %>% 
#         mutate_all(~replace(., is.nan(.), 0)) %>%
#         mutate(total = rowSums(across(where(is.numeric))))%>% 
#         filter(total > 0.00000000) %>% 
#         arrange(desc(Valorizzazione)) %>% 
#         select(-total, -Dipartimento) %>% 
#         column_to_rownames(var = "obcod")  
#       
#     },
#     
#     # server = FALSE, 
#     class = 'cell-border stripe', rownames=FALSE,
#     extensions = 'Buttons',options = list(dom="Brftip", pageLength = 10,
#                                           paging = TRUE,autoWidth = TRUE,
#                                           buttons = c('excel')) 
#     
#   ) %>% formatPercentage(3:7, 2)
# )
# 
# 
# 
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # df <- reactive(
# #   
# #   data.frame(
# #     RT = input$rt,
# #     FTE = input$fte,
# #     rid = input$pc/100,
# #     vRT = input$Vrt/100,
# #     vFTE = input$Vfte/100
# #   ) %>% 
# #     mutate(FTEp = FTE-(FTE*rid),
# #            
# #            RFTEt = RT/FTE, 
# #            
# #            RFTEprog = RT/(FTEp), 
# #            
# #            VARrfte = 100*((RFTEprog-RFTEt)/RFTEt), 
# #            
# #            VarRT = RT+(RT*vRT),
# #            
# #            VarFT = FTEp+(FTEp*vFTE),
# #            
# #            RFTEr = VarRT/VarFT, 
# #            
# #            VARRFTEr = ifelse(rid == 0, "", 100*((RFTEr-RFTEt)/RFTEt)), 
# #            
# #            TN = 100, 
# #            
# #            RisN = ifelse(rid==0, "",
# #                          
# #                          (VARRFTEr)/VARrfte)
# #            
# #     )  
# # )
# # 
# # # output$tb <- renderTable(df() %>% 
# # #                            
# # #                            select("Ricavo Totale Previsto" = RT, "FTE disponibili" = FTE, 
# # #                                   
# # #                                   "FTE programmato" = FTEp, "Ricavo per FTE teorico" =RFTEt, 
# # #                                   
# # #                                   "Ricavo per FTE programmato" =RFTEprog, Target=VARrfte, "Target Normalizzato" = TN)
# # #                          
# # # )
# # # output$tb2 <- renderTable(df() %>%
# # # 
# # #                             select("Ricavo Totale" = VarRT, "FTE" =VarFT, "Ricavo per FTE" = RFTEr,
# # #                                    Risultato=VARRFTEr, "Risultato Normalizzato" = RisN)
# # # 
# # # )
# # 
# # output$rfteT <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(RFTEt= round(RFTEt, 2)) %>% 
# #                select(RFTEt)), "Ricavo per FTE teorico",  icon = icon("euro"),
# #             color = "blue"
# #   )
# # })
# # 
# # 
# # output$ftep <- renderValueBox({
# #   valueBox( (df() %>% 
# #                select(FTEp)), "FTE programmati per l'attività istituzionale",  icon = icon("flask"),
# #            color = "blue"
# #   )
# # })
# # 
# # 
# # output$rfteP <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(RFTEprog= round(RFTEprog, 2)) %>% 
# #                select(RFTEprog)), "Ricavo per FTE programmato",  icon = icon("euro"),
# #             color = "aqua"
# #   )
# # })
# # 
# # 
# # output$target <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(VARrfte= round(VARrfte, 2)) %>%
# #                select(VARrfte)), "Variazione % attesa del RFTE",  icon = icon("euro"),
# #             color = "red"
# #   )
# # })
# # 
# # 
# # output$rtot <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(VarRT= round(VarRT, 2)) %>%
# #                select(VarRT)), "Ricavo Totale ",  icon = icon("euro"),
# #             color = "blue"
# #   )
# # })
# # 
# # output$fteR <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(VarFT= round(VarFT, 2)) %>%
# #                select(VarFT)), "FTE erogati ",  icon = icon("flask"),
# #             color = "blue"
# #   )
# # })
# # options(scipen = 999)
# # output$rfteR <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(RFTEr= round(RFTEr, 2)) %>%
# #                select(RFTEr)), "Ricavo per FTE erogati ",  icon = icon("euro"),
# #             color = "blue"
# #   )
# # })
# # 
# # output$target2 <- renderValueBox({
# #   valueBox( (df() %>% 
# #                mutate(VARRFTEr= as.numeric(VARRFTEr)) %>% 
# #                mutate(VARRFTEr = round(VARRFTEr, 2)) %>%
# #                mutate(VARRFTEr= ifelse(is.na(VARRFTEr), 0, VARRFTEr)) %>% 
# #                select(VARRFTEr)), "Variazione % reale del RFTE ",  icon = icon("euro"),
# #             color = "red"
# #   )
# # })
# # 
# # # output$risn <- renderValueBox({
# # #   valueBox( (df() %>% 
# # #                mutate(RisN = as.numeric(RisN, 1)) %>% 
# # #                mutate(RisN = ifelse(is.na(RisN), 0, RisN)) %>% 
# # #                mutate(RisN = round(RisN, 1)) %>% 
# # #                select(RisN)), "Indicatore di verifica",  icon = icon("euro"),
# # #             color = "red"
# # #   )
 }
# # 
