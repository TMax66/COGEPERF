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
          ungroup() %>% 
           arrange(Quarter) %>%  
           mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
                  VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
                  VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
                  VarEPag = round((EPag/lag(EPag)-1)*100, 2),
                  VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
                  VarPI = round((PI/lag(PI)-1)*100, 2), 
                  VarProdv = round((Prodv/lag(Prodv)-1)*100, 2)
           ) 
           
)

  
dtT <- reactive(dtanalisi %>% filter(CDC == input$CC & Costi=="Ricavo") %>% 
    group_by(CDC,  ANNO,  Quarter) %>% 
    summarise(Ufficiali = sum(TUff, na.rm = T), 
              NonUfficiali = sum(TNonUff, na.rm = T), 
              Gratuiti = sum(TGratuito, na.rm = T),
              Pagamento = sum(TPagamento, na.rm = T), 
              Vprod = sum(TVP, na.rm= T), 
              AI = sum(TAI, na.rm = T)) %>% ungroup() %>% 
    ungroup %>% 
    arrange(Quarter) %>% 
    mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
           VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
           VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
           VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
           VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
           VarAI = round((AI/lag(AI)-1)*100,2)) %>% 
    rowwise() %>% 
    mutate(TotRic = round(sum(Ufficiali, NonUfficiali, na.rm = T),2)) %>% ungroup() %>% 
    mutate(VarTot = round((TotRic/lag(TotRic)-1)*100, 2))
)
  
  
dtCostiT <- reactive(dtanalisi %>% filter(CDC== input$CC & Costi=="Costo") %>% 
         group_by(CDC,  ANNO,  Quarter) %>% 
           summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
           ungroup %>% 
           arrange(Quarter) %>% 
           mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2)
                     )


 


 
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
    Tplot(dtAtt(), "N.Esami", "VarEsami")#, euro = "")
    
    }else
           
           if(input$par == "Attività Ufficiale") {
              Tplot(dtAtt(), "EUff", "VarEUff")#, euro = "")
           }


          else

            if(input$par == "Attività Non Ufficiale"){
               Tplot(dtAtt(), "ENUff", "VarENUff")#, euro = "")

            }


          # else
          # 
          #   if(input$par == "Produzione Interna"){
          #      Tplot(dtAtt(), "Prodv", "VarProdv", euro = "")
          # 
          #   }
            
          }

)

##tabella dettaglio prestazioni----

output$dtprestazioni <- renderUI({
  req(input$par)
  
  if (input$par == "Attività Complessiva")
  
  {
    dtanalisi %>%  
      filter(Costi== "Ricavo") %>% 
      group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
      filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
      summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                N_Num = sum(Numero, na.rm = TRUE), 
                S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
      filter(CDC == input$CC & Classe == "Prestazioni") %>% 
      group_by(ANNO, Quarter, Area) %>% 
      summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
      mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
      select(-ANNO, -Quarter) %>% 
      pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
      #rename(., "Prestazione" = Area) %>% 
      left_join(  
        
        (dtanalisi %>% 
           filter(Costi == "Ricavo") %>% 
           group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
           filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
           summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                     N_Num = sum(Numero, na.rm = TRUE), 
                     S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                     S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
           filter(CDC == input$CC & Classe == "Prestazioni") %>% 
           group_by(ANNO, Quarter, Area) %>% 
           summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
           mutate(YQ = paste(ANNO, "-", Quarter)) %>%
           select(-ANNO, -Quarter) %>% 
           group_by(Area) %>%
           summarise(trend = spk_chr(N, type= "line", options =
                                       list(paging = FALSE)))
        )) %>% rename("Prestazioni" = Area) %>% 
      
      format_table()  %>% 
      htmltools::HTML() %>% 
      div() %>% 
      spk_add_deps()
  }
  else
    
    if(input$par == "Attività Ufficiale") 
  
    {
      dtanalisi %>%  
        filter(Uff == "Ufficiale" & Costi == "Ricavo") %>% 
        group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
        filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
        summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                  N_Num = sum(Numero, na.rm = TRUE), 
                  S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                  S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
        filter(CDC == input$CC & Classe == "Prestazioni") %>% 
        group_by(ANNO, Quarter, Area) %>% 
        summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
        mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
        select(-ANNO, -Quarter) %>% 
        pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
        #rename(., "Prestazione" = Area) %>% 
        left_join(  
          
          (dtanalisi %>% 
             filter(Uff == "Ufficiale" & Costi == "Ricavo") %>% 
             group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
             filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
             summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                       N_Num = sum(Numero, na.rm = TRUE), 
                       S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                       S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
             filter(CDC == input$CC & Classe == "Prestazioni") %>% 
             group_by(ANNO, Quarter, Area) %>% 
             summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
             mutate(YQ = paste(ANNO, "-", Quarter)) %>%
             select(-ANNO, -Quarter) %>% 
             group_by(Area) %>%
             summarise(trend = spk_chr(N, type= "line", options =
                                         list(paging = FALSE)))
          )) %>% rename("Prestazioni" = Area) %>% 
        
        format_table()  %>% 
        htmltools::HTML() %>% 
        div() %>% 
        spk_add_deps()
      
      
      
    }
  else
    

    if(input$par == "Attività Non Ufficiale") {
      
      dtanalisi %>%  
        filter(Uff == "Non Ufficiale" & Costi== "Ricavo") %>% 
        group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio,CDC, Costi,ClassAnalisi, Classe, Area) %>% 
        filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
        summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                  N_Num = sum(Numero, na.rm = TRUE), 
                  S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                  S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
        filter(CDC == input$CC & Classe == "Prestazioni") %>% 
        group_by(ANNO, Quarter, Area) %>% 
        summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
        mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
        select(-ANNO, -Quarter) %>% 
        pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
        #rename(., "Prestazione" = Area) %>% 
        left_join(  
          
          (dtanalisi %>% 
             filter(Uff == "Non Ufficiale" & Costi== "Ricavo") %>% 
             group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
             filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
             summarise(N_Det = sum(Determinazioni, na.rm = TRUE),
                       N_Num = sum(Numero, na.rm = TRUE), 
                       S_Tariffa = sum(Tariffario, na.rm = TRUE), 
                       S_Fatturato = sum(Fatturato, na.rm = TRUE))  %>% 
             filter(CDC == input$CC & Classe == "Prestazioni") %>% 
             group_by(ANNO, Quarter, Area) %>% 
             summarise(N = sum(N_Det, na.rm=TRUE)) %>% 
             mutate(YQ = paste(ANNO, "-", Quarter)) %>%
             select(-ANNO, -Quarter) %>% 
             group_by(Area) %>%
             summarise(trend = spk_chr(N, type= "line", options =
                                         list(paging = FALSE)))
          )) %>% rename("Prestazioni" = Area) %>% 
        
        format_table()  %>% 
        htmltools::HTML() %>% 
        div() %>% 
        spk_add_deps()
      
    }
  
  
})



  
##tabella dettaglio ricavi da prestazioni----
 
 
output$dtricprest <- renderUI({
   req(input$par)

  if (input$par == "Attività Complessiva") 
  {
    dtanalisi %>%  
      filter(Costi== "Ricavo") %>% 
      filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
      rowwise() %>% 
      mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
      group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
      summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
      filter(CDC == input$CC & Classe == "Prestazioni") %>%  
      group_by(ANNO, Quarter, Area) %>% 
      summarise(N = sum(TRic, na.rm=TRUE)) %>% 
      mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
      select(-ANNO, -Quarter) %>% 
      pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
      #rename(., "Prestazione" = Area) %>% 
      left_join(  
        
        (dtanalisi %>% 
           filter(Costi == "Ricavo") %>% 
           filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
           rowwise() %>% 
           mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
           group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
           summarise(TRic= sum(TotRic, na.rm = TRUE)) %>% 
           filter(CDC == input$CC & Classe == "Prestazioni") %>%  
           group_by(ANNO, Quarter, Area) %>% 
           summarise(N = sum(TRic, na.rm=TRUE)) %>% 
           mutate(YQ = paste(ANNO, "-", Quarter)) %>%
           select(-ANNO, -Quarter) %>% 
           group_by(Area) %>%
           summarise(trend = spk_chr(N, type= "line", options =
                                       list(paging = FALSE)))
        )) %>% 
      
      rename("Prestazioni" = Area) %>% 
      
      format_table()  %>% 
      htmltools::HTML() %>% 
      div() %>% 
      spk_add_deps()
    
    
    
  }
   else
     if (input$par == "Attività Ufficiale")
  
     {
       dtanalisi %>%  
         filter(Costi == "Ricavo") %>% 
         filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
         # rowwise() %>% 
         # mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         summarise(TUff= sum(TUff, na.rm = TRUE)) %>% 
         filter(CDC == input$CC & Classe == "Prestazioni") %>%  
         group_by(ANNO, Quarter, Area) %>% 
         summarise(N = sum(TUff, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
         select(-ANNO, -Quarter) %>% 
         pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
         #rename(., "Prestazione" = Area) %>% 
         left_join(  
           (dtanalisi %>%  
            filter(Costi== "Ricavo") %>% 
                  filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
                  # rowwise() %>% 
                  # mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
                  group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC,ClassAnalisi, Classe, Area) %>% 
                  summarise(TUff= sum(TUff, na.rm = TRUE)) %>% 
                  filter(CDC == input$CC & Classe == "Prestazioni") %>%  
              group_by(ANNO, Quarter, Area) %>% 
              summarise(N = sum(TUff, na.rm=TRUE)) %>% 
              mutate(YQ = paste(ANNO, "-", Quarter)) %>%
              select(-ANNO, -Quarter) %>% 
              group_by(Area) %>%
              summarise(trend = spk_chr(N, type= "line", options =
                                          list(paging = FALSE)))
           )) %>% 
         
         rename("Prestazioni" = Area) %>% 
         
         format_table()  %>% 
         htmltools::HTML() %>% 
         div() %>% 
         spk_add_deps()

     }
  else
    
    if (input$par == "Attività Non Ufficiale")
      
     
    {
      dtanalisi %>%  
        filter(Costi== "Ricavo") %>% 
        filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
        # rowwise() %>% 
        # mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
        group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
        summarise(TNonUff= sum(TNonUff, na.rm = TRUE)) %>% 
        filter(CDC == input$CC & Classe == "Prestazioni") %>%  
        group_by(ANNO, Quarter, Area) %>% 
        summarise(N = sum(TNonUff, na.rm=TRUE)) %>% 
        mutate(YQ = paste(ANNO, "-", Quarter)) %>% ungroup() %>% 
        select(-ANNO, -Quarter) %>% 
        pivot_wider( names_from = YQ,  values_from = N, values_fill = 0) %>%   
        #rename(., "Prestazione" = Area) %>% 
        left_join(  
          (dtanalisi %>%  
             filter(Costi== "Ricavo") %>% 
             filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%
             # rowwise() %>% 
             # mutate(TotRic = sum(TUff, TNonUff, na.rm = T)) %>% ungroup %>% 
             group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
             summarise(TNonUff= sum(TNonUff, na.rm = TRUE)) %>% 
             filter(CDC == input$CC & Classe == "Prestazioni") %>%  
             group_by(ANNO, Quarter, Area) %>% 
             summarise(N = sum(TNonUff, na.rm=TRUE)) %>% 
             mutate(YQ = paste(ANNO, "-", Quarter)) %>%
             select(-ANNO, -Quarter) %>% 
             group_by(Area) %>%
             summarise(trend = spk_chr(N, type= "line", options =
                                         list(paging = FALSE)))
          )) %>% 
        
        rename("Prestazioni" = Area) %>% 
        
        format_table()  %>% 
        htmltools::HTML() %>% 
        div() %>% 
        spk_add_deps()
      
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
    Tplot(dtT(), "TotRic", "VarTot" )
    
  }else
    
    if(input$par == "Attività Ufficiale") {
      Tplot(dtT(), "Ufficiali", "VarUff" )
    }
  
  
  else
    
    if(input$par == "Attività Non Ufficiale"){
      Tplot(dtT(), "NonUfficiali", "VarNUff" )
      
    }
  
  
  else
    
    if(input$par == "Produzione Interna"){
      Tplot(dtT(), "Vprod", "VarVP" )
      
    }
  
}

)

##grafici costi----

output$PLOT3 <- renderPlot({
  req(input$CC)
  Tplot(dtCostiT(), "Costi", "VarCosti")

})

##tabella dettaglio costi----

output$dettcosti <- renderUI({
req(input$CC)
dtanalisi %>%  
  filter(Costi== "Costo") %>% 
  group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
  
  summarise(costidett = sum(Costo, na.rm = TRUE),
  )  %>%  
  filter(CDC == input$CC  ) %>% 
  group_by(ANNO, Quarter,Classe) %>%  
  summarise(C = sum(costidett, na.rm=TRUE)) %>% 
  mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
  select(-ANNO, -Quarter) %>%  
  pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
  
  left_join(  
    
    (dtanalisi %>% 
       filter(Costi == "Costo") %>% 
       group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
       #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
       summarise(costidett = sum(Costo, na.rm = TRUE),
       )  %>%  
       filter(CDC == input$CC ) %>% 
       group_by(ANNO, Quarter, Classe) %>% 
       summarise(C = sum(costidett, na.rm=TRUE)) %>% 
       mutate(YQ = paste(ANNO, "-", Quarter)) %>%
       select(-ANNO, -Quarter) %>% 
       group_by(Classe) %>%
       summarise(trend = spk_chr(C, type= "line", options =
                                   list(paging = FALSE)))
    )) %>% rename("Tipologia Costi" = Classe) %>% 
  
  format_table()  %>% 
  htmltools::HTML() %>% 
  div() %>% 
  spk_add_deps()
})

#GESTIONE----
dtCostiTgest <- reactive(dtanalisi %>% filter(CDC== input$CC2 & Costi=="Costo") %>% 
                       group_by(CDC,  ANNO,  Quarter) %>% 
                       summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                       ungroup() %>% 
                       mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2))


##grafici costi----

output$PLOT4 <- renderPlot({
  req(input$CC2)
  Tplot(dtCostiTgest(), "Costi", "VarCosti", euro="€")
  
})

##tabella dettaglio costi----

output$dettcostigest <- renderUI({
  req(input$CC2)
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == input$CC2  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
    select(-ANNO, -Quarter) %>%  
    pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
    
    left_join(  
      
      (dtanalisi %>% 
         filter(Costi == "Costo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(costidett = sum(Costo, na.rm = TRUE),
         )  %>%  
         filter(CDC == input$CC2 ) %>% 
         group_by(ANNO, Quarter, Classe) %>% 
         summarise(C = sum(costidett, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Classe) %>%
         summarise(trend = spk_chr(C, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Tipologia Costi" = Classe) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
})


#COSTI COMUNI----


dtCostiCom <- reactive(dtanalisi %>% filter(CDC== input$CC3 & Costi=="Costo") %>% 
                           group_by(CDC,  ANNO,  Quarter) %>% 
                           summarise(Costi = round(sum(Costo, na.rm = TRUE),2)) %>% 
                           ungroup() %>% 
                           mutate(VarCosti = round((Costi/lag(Costi)-1)*100),2))


##grafici costi----

output$PLOT5 <- renderPlot({
  req(input$CC3)
  Tplot(dtCostiCom(), "Costi", "VarCosti", euro="€")
  
})

##tabella dettaglio costi----

output$dettcosticom <- renderUI({
  req(input$CC3)
  dtanalisi %>%  
    filter(Costi== "Costo") %>% 
    group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
    
    summarise(costidett = sum(Costo, na.rm = TRUE),
    )  %>%  
    filter(CDC == input$CC3  ) %>% 
    group_by(ANNO, Quarter,Classe) %>%  
    summarise(C = sum(costidett, na.rm=TRUE)) %>% 
    mutate(YQ = paste(ANNO, "-", Quarter)) %>%  ungroup() %>% 
    select(-ANNO, -Quarter) %>%  
    pivot_wider( names_from = YQ,  values_from = C, values_fill = 0) %>%    
    
    left_join(  
      
      (dtanalisi %>% 
         filter(Costi == "Costo") %>% 
         group_by(ANNO, Quarter, Dipartimento, Reparto, Laboratorio, CDC, ClassAnalisi, Classe, Area) %>% 
         #filter(Classe %in% c("Prestazioni", "Vendite prodotti", "Ricavi da produzione")) %>%  
         summarise(costidett = sum(Costo, na.rm = TRUE),
         )  %>%  
         filter(CDC == input$CC3 ) %>% 
         group_by(ANNO, Quarter, Classe) %>% 
         summarise(C = sum(costidett, na.rm=TRUE)) %>% 
         mutate(YQ = paste(ANNO, "-", Quarter)) %>%
         select(-ANNO, -Quarter) %>% 
         group_by(Classe) %>%
         summarise(trend = spk_chr(C, type= "line", options =
                                     list(paging = FALSE)))
      )) %>% rename("Tipologia Costi" = Classe) %>% 
    
    format_table()  %>% 
    htmltools::HTML() %>% 
    div() %>% 
    spk_add_deps()
})



























#TABELLA PIVOT----

#  dtanalisi %>% 
# select(Anno = ANNO,
#                          Trimestre = TRIMESTRE,
#                          Mese = MESE,
#                          Dipartimento, Reparto, Laboratorio, "Centro di Costo" = CDC,
#                          Fatturato, Tariffario, Costo,
#                          "ANALISI" = Determinazioni, Numero,
#                          "Tipologia Analisi" = ClassAnalisi,
#                          Categoria, Classificazione, Classe, Area)

output$pivot <- renderRpivotTable({
  rpivotTable( dtanalisi %>% 
                 select(Anno = ANNO,
                        Trimestre = TRIMESTRE,
                        Mese = MESE,
                        Dipartimento, Reparto, Laboratorio, "Centro di Costo" = CDC,
                        Fatturato, Tariffario, Costo,
                        "ANALISI" = Determinazioni, Numero,
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
