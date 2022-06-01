Prjdip <- reactive({
  prdip() %>%
    group_by(CodIDIzler, Tipologia, DataInizio, DataFine, Descrizione, RespScient) %>%
    summarise(Budget = sum(Budget)) %>%
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