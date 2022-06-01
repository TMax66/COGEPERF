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
       filter(articoliif == "IF") %>%
       group_by(NR) %>%
       count(NR) %>%
       select(NR) %>%
       nrow()),  "Articoli pubblicati su riviste peer-review con IF (scarica elenco)", icon = icon("book"), color = "light-blue")
})

output$Int <- renderValueBox({
  valueBox(
    (pubs() %>%
       filter(articoliif == "Int") %>%
       group_by(NR) %>%
       count(NR) %>%
       select(NR) %>%
       nrow()
    ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})


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
       filter(articoliif == "IF") %>%
       group_by(NR) %>%
       count(NR) %>%
       select(NR) %>%
       nrow()),  "Articoli pubblicati su riviste peer-review con IF (scarica elenco)", icon = icon("book"), color = "light-blue")
})


