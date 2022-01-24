server<-function(input, output) {


output$parametri <- renderUI({
      req(input$CC)
      if(input$tabs == 1 | input$tabs == 2){
      selectInput("par", "Seleziona la categoria",
                  choices = c(
                    "Attività Complessiva",
                    "Attività Ufficiale",
                    "Attività Non Ufficiale"
                  ))
      } else {}
    })

output$tipocont <- renderUI({
  req(input$CC)
  radioButtons("tipoconteggio", "seleziona tipo di conteggio",
               c("Trimestrale",
                 "Progressivo" ))
})

output$tipocont2 <- renderUI({
  req(input$CC2)
  radioButtons("tipoconteggio2", "seleziona tipo di conteggio",
               c("Trimestrale",
                 "Progressivo" ))
})



  # output$butt1 <- renderUI({
  #   req(input$CC, input$par)
  #   div( align = "center", 
  #        downloadButton("down", "Scarica i dati in formato .tsv"))
  # })
  
  # output$butt2 <- renderUI({
  #   req(input$CC)
  #   div( align = "center", 
  #        downloadButton("down", "Scarica i dati in formato .tsv"))
  # })
  
##intestazioni pagine----
  titolo <- reactive(
    paste(input$CC,":",input$par)
  )
  
  output$titoloAtt <- renderText({
    req(input$par)
    titolo()} 
  )
  
  
  output$titoloRic <- renderText({
    req(input$par)
    paste(input$CC, ": Ricavi da", input$par)} 
  )
  
  output$titoloCosti <- renderText({
    req(input$CC)
    paste(input$CC, ": Costi complessivi" )} 
  )
  
  output$titoloCosti2 <- renderText({
    req(input$CC2)
    paste(input$CC2, ": Costi complessivi" )} 
  )
  
  output$titoloCosti3 <- renderText({
    req(input$CC3)
    paste(input$CC3, ": Costi complessivi" )} 
  )
  
#PRODUZIONE----

##Tabelle reattive----
dtAtt <- reactive (dtanalisi %>% filter(CDC == input$CC & Costi=="Ricavo") %>% 
                     group_by(CDC,  ANNO,  Quarter) %>% 
                     summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                               EUff = sum(AttUff, na.rm = TRUE), 
                               ENUff =sum(AttNUff, na.rm = TRUE), 
                               EPag = sum(AttPag, na.rm = TRUE), 
                               Egrat = sum(AttGrat, na.rm = TRUE), 
                               PI = sum(AI , na.rm = TRUE), 
                               Prodv = sum(VP, na.rm = TRUE)) %>% 
                     mutate(CumEsami = cumsum(N.Esami), 
                            CumEUff = cumsum(EUff), 
                            CumENUff = cumsum(ENUff), 
                            CumEPag = cumsum(EPag), 
                            CumEgrat = cumsum(Egrat), 
                            CumPI = cumsum(PI), 
                            CumProdv = cumsum(Prodv)) %>% 
                     ungroup() %>% 
                     arrange(Quarter) %>%  
                     mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
                            VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
                            VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
                            VarEPag = round((EPag/lag(EPag)-1)*100, 2),
                            VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
                            VarPI = round((PI/lag(PI)-1)*100, 2), 
                            VarProdv = round((Prodv/lag(Prodv)-1)*100, 2), 
                            VarCumEs = round((CumEsami/lag(CumEsami)-1)*100,2), 
                            VarCumEUff = round((CumEUff/lag(CumEUff)-1)*100,2), 
                            VarCumENUff = round((CumENUff/lag(CumENUff)-1)*100,2), 
                            VarCumEPag = round((CumEPag/lag(CumEPag)-1)*100,2),
                            VarCumEgrat = round((CumEgrat/lag(CumEgrat)-1)*100,2),
                            VarCumPI = round((CumPI/lag(CumPI)-1)*100,2),
                            VarCumProdv = round((CumProdv/lag(CumProdv)-1)*100,2)
                     )
           
)

  
dtT <- reactive(dtanalisi %>% filter(CDC == input$CC & Costi=="Ricavo") %>% 
                  group_by(CDC,  ANNO,  Quarter) %>% 
                  summarise(Ufficiali = sum(TUff, na.rm = T), 
                            NonUfficiali = sum(TNonUff, na.rm = T), 
                            Gratuiti = sum(TGratuito, na.rm = T),
                            Pagamento = sum(TPagamento, na.rm = T), 
                            Vprod = sum(TVP, na.rm= T), 
                            AI = sum(TAI, na.rm = T)) %>%
                  mutate(CumUff = cumsum(Ufficiali),
                         CumNonUff = cumsum(NonUfficiali), 
                         CumGrat = cumsum(Gratuiti), 
                         CumPag = cumsum(Pagamento), 
                         CumVprod = cumsum(Vprod), 
                         CumAI = cumsum(AI)) %>% 
                  rowwise() %>% 
                  mutate(TotRic = round(sum(Ufficiali, NonUfficiali, na.rm = T),2)) %>%
                  ungroup %>% 
                  mutate(CumTotRic = cumsum(TotRic)) %>% 
                  ungroup() %>% 
                  
                  arrange(Quarter) %>% 
                  mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
                         VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
                         VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
                         VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
                         VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
                         VarAI = round((AI/lag(AI)-1)*100,2), 
                         VarTot = round((TotRic/lag(TotRic)-1)*100, 2),
                         
                         VarCumUff = round((CumUff/lag(CumUff)-1)*100, 2), 
                         VarCumNonUff = round((CumNonUff/lag(CumNonUff)-1)*100, 2), 
                         VarCumGrat = round((CumGrat/lag(CumGrat)-1)*100, 2), 
                         VarCumPag = round((CumPag/lag(CumPag)-1)*100, 2), 
                         VarCumVprod = round((CumVprod/lag(CumVprod)-1)*100, 2), 
                         VarCumAI = round((CumAI/lag(CumAI)-1)*100, 2), 
                         VarCumTotRic = round((CumTotRic/lag(CumTotRic)-1)*100, 2) )
)
  
  
dtCostiT <- reactive(dtanalisi %>% filter(CDC== input$CC & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                       mutate(CumCosti = cumsum(Costi)) %>% 
                       ungroup () %>% 
                       arrange(Quarter) %>% 
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2, 
                              VarCumCosti = round((CumCosti/lag(CumCosti)-1)*100), 2) )


 


 
##Grafici----

##grafici attività analitica e produzione interna----

###Download tabella attività




# output$down  <- downloadHandler(
#    filename = function(){
#      paste0(input$CC, ".csv", sep = "")},
# 
#    content = function(file){
#     write.csv(dtanalisi, file, row.names = FALSE)
#    })
 
 
output$PLOT <- renderPlot({
  req(input$CC, input$par)
  
  if (input$par == "Attività Complessiva")
    {   
        if(input$tipoconteggio== "Trimestrale")
        {   
          Tplot(dtAtt(), "N.Esami", "VarEsami")#, euro = "")

        } else
        
        if(input$tipoconteggio == "Progressivo")
        {
          Tplot(dtAtt(), "CumEsami", "VarCumEs")#, euro = "")
        }

     }else
           
  if(input$par == "Attività Ufficiale") 
   {
    
        if(input$tipoconteggio == "Trimestrale")
        {  
          Tplot(dtAtt(), "EUff", "VarEUff")#, euro = "")
          
        } else
          
        if(input$tipoconteggio == "Progressivo")
        {
          Tplot(dtAtt(), "CumEUff", "VarCumEUff")#, euro = "")
        }

    }else

  if(input$par == "Attività Non Ufficiale")
   {
    
        if(input$tipoconteggio == "Trimestrale")
        { 
          Tplot(dtAtt(), "ENUff", "VarENUff")#, euro = "")
        } else
        
        if(input$tipoconteggio == "Progressivo")
        { 
        Tplot(dtAtt(), "CumENUff", "VarCumENUff")#, euro = "")
        }

    }

          }
)

##Tabella dettaglio prestazioni----

output$dtprestazioni <- renderUI({
  req(input$par)
  
  if (input$par == "Attività Complessiva")
  {
    if (input$tipoconteggio == "Trimestrale")
    { AC(CC = input$CC) } else
    if (input$tipoconteggio == "Progressivo")
    { AC2(CC = input$CC) }
  }
  else
    
    if(input$par == "Attività Ufficiale") 
      
    {
      if (input$tipoconteggio == "Trimestrale")
      { AU(CC = input$CC, Uff = "Ufficiale") } else
        if (input$tipoconteggio == "Progressivo")
        { AU2(CC = input$CC, Uff = "Ufficiale") }
  }
  else
    

    if(input$par == "Attività Non Ufficiale") 
      
      {
      
      if (input$tipoconteggio == "Trimestrale")
      { AU(CC = input$CC, Uff == "Non Ufficiale") } else
        if (input$tipoconteggio == "Progressivo")
        { AU2(CC = input$CC, Uff == "Non Ufficiale") }
      
    }
  
  
})



  
##Tabella dettaglio ricavi da prestazioni----
 
 
output$dtricprest <- renderUI({
   req(input$par)

  if (input$par == "Attività Complessiva") 
  {
    
    if(input$tipoconteggio == "Trimestrale")
    {RC(CC = input$CC)} else
    if(input$tipoconteggio == "Progressivo")
    {RC2(CC = input$CC)}
    
  }
   else
     if (input$par == "Attività Ufficiale")
  
     {
       
       if(input$tipoconteggio == "Trimestrale")
       {RUf(CC = input$CC)}else
       if(input$tipoconteggio == "Progressivo")
       {RUf2(CC = input$CC)}

     }
  else
    
    if (input$par == "Attività Non Ufficiale")
     
    {
      
      if(input$tipoconteggio == "Trimestrale")
      {RNUf(CC = input$CC)} else
      if(input$tipoconteggio == "Progressivo")
      {RNUf2(CC = input$CC)}
      
    }
  
 
})
 
 
##grafici ricavi----
###download dati ricavi----
 
# output$down <- downloadHandler(
#                        filename = function(){ 
#                        paste0(input$CC, "Totale Ricavi")}, 
#  
#                        content = function(file){
#                          vroom::vroom_write(dtT(), file)
#                        })
 




output$PLOT2 <- renderPlot({
  req(input$CC, input$par)
  
  if (input$par == "Attività Complessiva")
  {   
    if(input$tipoconteggio == "Trimestrale")
    {Tplot(dtT(), "TotRic", "VarTot" )} else
    if(input$tipoconteggio == "Progressivo")
    {Tplot(plotRC2(CC=input$CC), "CumTotRic", "VarCumTotRic" )}
    
  }else
    
    if(input$par == "Attività Ufficiale") 
  {
    if(input$tipoconteggio == "Trimestrale")
      {Tplot(dtT(), "Ufficiali", "VarUff" )} else
    if(input$tipoconteggio == "Progressivo")
    {Tplot(dtT(), "CumUff", "VarCumUff")}
  
}
  
  else
    
    if(input$par == "Attività Non Ufficiale")
      
    {
      
      if(input$tipoconteggio == "Trimestrale")
     {Tplot(dtT(), "NonUfficiali", "VarNUff" )} else
      if(input$tipoconteggio == "Progressivo")
      {Tplot(dtT(), "CumNonUff", "VarCumNonUff")}
      
    }

    }
)

##grafici costi----

output$PLOT3 <- renderPlot({
  req(input$CC)
  
  if(input$tipoconteggio == "Trimestrale")
  {Tplot(dtCostiT(), "Costi", "VarCosti")} else
  if(input$tipoconteggio == "Progressivo")
  {Tplot(dtCostiT(), "CumCosti", "VarCumCosti")}

})


##tabella dettaglio costi----

output$dettcosti <- renderUI({
req(input$CC)

  if(input$tipoconteggio == "Trimestrale")
  {CostiN(CC = input$CC)} else
  if(input$tipoconteggio == "Progressivo")
  {CostiP(CC=input$CC)}
})

#GESTIONE----

 

dtCostiTgest <- reactive(dtanalisi %>% filter(CDC== input$CC2 & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                         mutate(CumCosti = cumsum(Costi)) %>% 
                       ungroup() %>% 
                         arrange(Quarter) %>% 
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2, 
                              VarCumCosti = round((CumCosti/lag(CumCosti)-1)*100),2))  







##grafici costi----

output$PLOT4 <- renderPlot({
  req(input$CC2)
  if(input$tipoconteggio2 == "Trimestrale")
    {Tplot(dtCostiTgest(), "Costi", "VarCosti")}else
  if(input$tipoconteggio2 == "Progressivo")
  {Tplot(dtCostiTgest(), "CumCosti", "VarCumCosti")}
})

##tabella dettaglio costi----

output$dettcostigest <- renderUI({
  req(input$CC2)
  if(input$tipoconteggio2 == "Trimestrale")
  {CostigestN(CC = input$CC2)}else
  if(input$tipoconteggio2 == "Progressivo")
  {CostigestP(CC = input$CC2)}
 
})





##COSTI COMUNI----
# 
# 
# dtCostiCom <- reactive(dtanalisi %>% filter(CDC== input$CC3 & Costi=="Costo") %>% 
#                            group_by(CDC,  ANNO,  Quarter) %>% 
#                            summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
#                            ungroup() %>% 
#                            arrange(Quarter) %>% 
#                            mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2))
# 
# 
# ##grafici costi----
# 
# output$PLOT5 <- renderPlot({
#   req(input$CC3)
#   Tplot(dtCostiCom(), "Costi", "VarCosti")
#   
# })
# 
# ##tabella dettaglio costi----
# 
# output$dettcosticom <- renderUI({
#   req(input$CC3)
#   dtanalisi %>%  
#     filter(Costi== "Costo") %>% 
#     group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
#     
#     summarise(costidett = sum(Costo, na.rm = TRUE),
#     )  %>%  
#     filter(CDC == input$CC3  ) %>% 
#     group_by(ANNO, Quarter,Classe) %>%  
#     summarise(C = sum(costidett, na.rm=TRUE)) %>% 
#     mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
#     select(-ANNO, -Quarter) %>%  
#     pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
#     
#     left_join(  
#       
#       (dtanalisi %>% 
#          filter(Costi == "Costo") %>% 
#          group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
#          #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
#          summarise(costidett = sum(Costo, na.rm = TRUE),
#          )  %>%  
#          filter(CDC == input$CC3 ) %>% 
#          group_by(ANNO, Quarter, Classe) %>% 
#          summarise(C = sum(costidett, na.rm=TRUE)) %>% 
#          mutate(YQ = paste(ANNO, "-", Quarter)) %>%
#          select(-ANNO, -Quarter) %>% 
#          group_by(Classe) %>%
#          summarise(trend = spk_chr(C, type= "line", options =
#                                      list(paging = FALSE)))
#       )) %>% rename("Tipologia Costi" = Classe) %>% 
#     
#     format_table()  %>% 
#     htmltools::HTML() %>% 
#     div() %>% 
#     spk_add_deps()
# })
# 


## Tabella PIVOT----
output$pivot <- renderRpivotTable({
  rpivotTable( dtanalisi %>% 
                 select(Anno = ANNO,
                        Trimestre = TRIMESTRE,
                        Mese = MESE,
                        Dipartimento, Reparto, Laboratorio, "Centro di Costo" = CDC,
                        Fatturato, Tariffario, Costo,
                        "ANALISI" = Determinazioni,  
                        "Tipologia Analisi" = ClassAnalisi,
                        Categoria, Classificazione, Classe, Area, CodiceCDC),
              aggregatorName="Sum", vals = "",
              onRefresh = htmlwidgets::JS(
                "function(config) {
                        Shiny.onInputChange('mypivot', document.getElementById('pivot').innerHTML); 
                        }"))
})


pivot_tbl <- eventReactive(input$mypivot, {
  tryCatch({
    input$mypivot %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  }, error = function(e) {
    return()
  })
})



observe({
  if (is.data.frame(pivot_tbl()) && nrow(pivot_tbl()) > 0) {
    shinyjs::enable("download_pivot")
    shinyjs::enable("copy_pivot")
  } else {
    shinyjs::disable("download_pivot")
    shinyjs::disable("copy_pivot")
  }
})

output$download_pivot <- downloadHandler(
  filename = function() {
    if (input$format == "csv") {
      "pivot.csv"
    } else if (input$format == "excel") {
      "pivot.xlsx"
    }
  },
  content = function(file) {
    if (input$format == "csv") {
      write_csv(pivot_tbl(), path = file)
    } else if (input$format == "excel") {
      writexl::write_xlsx(as.data.frame(pivot_tbl(), path = file))
    }
  }
)

observeEvent(input$copy_pivot,  {
  clipr::write_clip(pivot_tbl(), object_type = "table")
})





output$download_pivot   <- downloadHandler(
   filename = function(){
     paste0("tabella", ".csv", sep = "")},

   content = function(file){
    write.csv(pivot_tbl(), file, row.names = FALSE)
   })




}
