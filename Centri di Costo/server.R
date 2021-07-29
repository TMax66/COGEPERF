server<-function(input, output) {
  
#Controlli della UI----
  # output$sel1 <- renderUI({  
  #   req(input$CC)
  #   selectInput("uff", "Ufficiale/Non Ufficiale", 
  #               choices = c("","Ufficiale", "Non Ufficiale"))  })
  # 
  # output$sel2 <- renderUI({  
  #   req(input$CC)
  # 
  #   selectInput("paga", "Gratuito/Pagamento", 
  #               choices = c("", "Gratuito", "Pagamento"))
  # })
    
    

    output$parametri <- renderUI({
      req(input$CC)
      selectInput("par", "Seleziona la categoria", 
                  choices = c(
                      
                    "Attività Complessiva", 
                    "Attività Ufficiale",
                    "Attività Non Ufficiale", 
                    "Produzione Interna"
                  ))
    })
    
    

  
  
  output$butt1 <- renderUI({
    req(input$CC)
    div( align = "center", 
         downloadButton("down", "Scarica i dati in formato .tsv"))
  })
  
  output$butt2 <- renderUI({
    req(input$CC)
    div( align = "center", 
         downloadButton("down", "Scarica i dati in formato .tsv"))
  })
  
#TABELLE REATTIVE----
dtAtt <- reactive (dtanalisi %>% filter(`Centro di Costo`== input$CC & `Costo o Ricavo`=="Ricavo") %>% 
         group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
             summarise(N.Esami = sum(Determinazioni, na.rm = TRUE), 
                     EUff = sum(AttUff, na.rm = TRUE), 
                     ENUff =sum(AttNUff, na.rm = TRUE), 
                     EPag = sum(AttPag, na.rm = TRUE), 
                     Egrat = sum(AttGrat, na.rm = TRUE), 
                     PI = sum(AI , na.rm = TRUE), 
                     Prodv = sum(VP, na.rm = TRUE)) %>% 
           ungroup() %>% 
           mutate(VarEsami = round((N.Esami/lag(N.Esami)-1)*100, 2), 
                  VarEUff = round((EUff/lag(EUff)-1)*100, 2), 
                  VarENUff = round((ENUff/lag(ENUff)-1)*100, 2), 
                  VarEPag = round((EPag/lag(EPag)-1)*100, 2),
                  VarEgrat = round((Egrat/lag(Egrat)-1)*100, 2),
                  VarPI = round((PI/lag(PI)-1)*100, 2), 
                  VarProdv = round((Prodv/lag(Prodv)-1)*100, 2), 
                  )
           
)
                    
  
  
  
 dtT <- reactive (dtanalisi %>% filter(`Centro di Costo`== input$CC & `Costo o Ricavo`=="Ricavo") %>% 
    group_by(`Centro di Costo`,  Anno,  Quarter) %>% 
    summarise(Ufficiali = sum(TUff, na.rm = T), 
              NonUfficiali = sum(TNonUff, na.rm = T), 
              Gratuiti = sum(TGratuito, na.rm = T),
              Pagamento = sum(TPagamento, na.rm = T), 
              Vprod = sum(TVP, na.rm= T), 
              AI = sum(TAI, na.rm = T)) %>% ungroup() %>% 
              
    mutate(VarUff = round((Ufficiali/lag(Ufficiali)-1)*100, 2), 
           VarNUff = round((NonUfficiali/lag(NonUfficiali)-1)*100, 2), 
           VarGrat = round((Gratuiti/lag(Gratuiti)-1)*100, 2), 
           VarPag = round((Pagamento/lag(Pagamento)-1)*100, 2), 
           VarVP = round((Vprod/lag(Vprod)-1)*100, 2), 
           VarAI = round((AI/lag(AI)-1)*100,2)) %>% 
    rowwise() %>% 
    mutate(TotRic = sum(Ufficiali, NonUfficiali, na.rm = T)) %>% ungroup() %>% 
    mutate(VarTot = round((TotRic/lag(TotRic)-1)*100, 2)) %>%   
    group_by(`Centro di Costo`,  Anno,  Quarter)
 )
  
  
# dt <- reactive(dtanalisi %>% 
#                    group_by(`Centro di Costo`, Pagamento, ClassAnalisi,Uff, Anno,  Quarter) %>% 
#                    summarise(Tariffato = round(sum(`A Tariffario`, na.rm = TRUE), 0), 
#                              Fatturato = round(sum(Fatturato, na.rm = TRUE), 0)) %>% 
#                    mutate(VarVal = round((Tariffato/lag(Tariffato) - 1) * 100, 2 ), 
#                           VarFatt= round((Fatturato/lag(Fatturato)-1)*100, 2)) %>% 
#                    filter(`Centro di Costo` == input$CC) %>% 
#                    pivot_longer(cols = 7:10, names_to = "Parametro", values_to = "metrica") %>% 
#                    to_be(Pagamento = input$paga) %>%  
#                    filter(Pagamento == input$paga & Uff == input$uff) %>%  
#                    pivot_wider(names_from = "Parametro", values_from = "metrica") %>% 
#                    data.frame()
#   )
  
  
  
  
lab <- reactive(   
   cc %>% 
      filter(`Centro di Costo`== input$CC) %>% 
      select(Laboratorio, CodCC ) %>% 
      data.frame())
  
  
  output$struttura <- renderText({
    req(input$CC)
    paste(lab()[1,1], "-", lab()[1,2])
  })
  
 titolo <- reactive(
  input$par
 )

 output$titoloAtt <- renderText({
   req(input$par)
   titolo()} 
 )
 
 output$titoloRic <- renderText({
   req(input$par)
   titolo()} 
 )
 
#GRAFICI----

##Grafici attività analitica e produzione interna----

###Download tabella attività----
output$down <- downloadHandler(
   filename = function(){ 
     paste0(input$CC, "Totale Attività")}, 
   
   content = function(file){
     vroom::vroom_write(dtAtt(), file)
   })
 
 
output$PLOT <- renderPlot({
  req(input$CC, input$par)
  
  if (input$par == "Attività Complessiva")
    {   
    Tplot(dtAtt(), "N.Esami", "VarEsami", euro = "")
    
    }else
           
           if(input$par == "Attività Ufficiale") {
              Tplot(dtAtt(), "EUff", "VarEUff", euro = "")
           }


          else

            if(input$par == "Attività Non Ufficiale"){
               Tplot(dtAtt(), "ENUff", "VarENUff", euro = "")

            }


          else

            if(input$par == "Produzione Interna"){
               Tplot(dtAtt(), "Prodv", "VarProdv", euro = "")

            }
            
          }

)
 
 
##Grafici ricavi----
###Download dati ricavi----
 
output$down <- downloadHandler(
                       filename = function(){ 
                       paste0(input$CC, "Totale Ricavi")}, 
 
                       content = function(file){
                         vroom::vroom_write(dtT(), file)
                       })
 




output$PLOT2 <- renderPlot({
  req(input$CC, input$par)
  
  if (input$par == "Attività Complessiva")
  {   
    Tplot(dtT(), "TotRic", "VarTot", euro = "€")
    
  }else
    
    if(input$par == "Attività Ufficiale") {
      Tplot(dtT(), "Ufficiali", "VarUff",euro = "€")
    }
  
  
  else
    
    if(input$par == "Attività Non Ufficiale"){
      Tplot(dtT(), "NonUfficiali", "VarNUff",euro = "€")
      
    }
  
  
  else
    
    if(input$par == "Produzione Interna"){
      Tplot(dtT(), "Vprod", "VarVP", euro = "")
      
    }
  
}

)
















 # output$plotT <-  renderPlot({ req(input$CC)
 #  Tplot(dtT(), "TotRic", "VarTot", euro = "€")})
 # 
 # output$plotUf <-  renderPlot({ 
 #   req(input$CC)
 #   Tplot(dtT(), "Ufficiali", "VarUff",euro = "€")
 # } )
 # 
 # output$plotNuf <-  renderPlot({ 
 #   req(input$CC)
 #   Tplot(dtT(), "NonUfficiali", "VarNUff",euro = "€")
 # } )
 # 
 # output$plotpag <-  renderPlot({ 
 #   req(input$CC)
 #   Tplot(dtT(), "Pagamento", "VarPag",euro = "€")
 # } )
 # 
 # output$plotgrat <-  renderPlot({ 
 #   req(input$CC)
 #   Tplot(dtT(), "Gratuiti", "VarGrat",euro = "€")
 # } )
 # 
 # 
 # 
 
 
 
  # output$plot1 <- renderPlot({
  #   req(input$CC, input$paga, input$uff)
  # 
  #   p1 <- ggplot(dt(),
  #                aes(y = dt()[,7], x = Quarter,  label=paste(as.character(dt()[,7]), "€")))+
  #     geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+
  #     geom_label(size = 5, aes(color = Anno == max(Anno)))+
  #     scale_color_manual(values = c("grey", "blue"), guide = "none") +
  #     facet_grid(~Anno, switch = "x", scales = "free")+
  #     geom_hline(yintercept = 0, size = 0.5)+
  #     labs(y = "", x = " ",
  #          title = paste( "Andamento trimestrale del", names(dt()[7])))+
  #     theme_ipsum_rc()+
  #     theme(panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  # 
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size = 15),
  # 
  #           strip.text.x = element_text(size = 18))
  # 
  # 
  # 
  #   p2 <- ggplot(dt(),
  #                aes(y = dt()[,8], x = Quarter,  label=paste(dt()[,8], "%")))+
  #     geom_line(aes(group = ClassAnalisi, color = Anno ==max(Anno)), size= 1.1,  )+
  #     geom_label(size = 5, aes(color = Anno == max(Anno)))+
  #     scale_color_manual(values = c("grey", "blue"), guide = "none") +
  #     facet_grid(~Anno, switch = "x", scales = "free")+
  #     geom_hline(yintercept = 0, size = 0.5)+
  #     labs(y = "" , x = " ",
  #          title = paste("Variazione % del", names(dt()[7]), "rispetto al trimestre precedente")
  #          )+
  #     theme_ipsum_rc()+
  #     theme(panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           axis.title.y = element_text(size= 18),
  #           #axis.text.y = element_text(size= 16),
  #           axis.text.y = element_blank(),
  # 
  #           axis.text.x = element_text(size = 15),
  #           strip.text.x = element_text(size = 18))
  # 
  #   p1/p2
  # 
  # })



}
