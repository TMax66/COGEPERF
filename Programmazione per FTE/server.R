server<-function(input, output) { 
  
  df <- reactive(
    
    data.frame(
      RT = input$rt,
      FTE = input$fte,
      rid = input$pc/100,
      vRT = input$Vrt/100,
      vFTE = input$Vfte/100
    ) %>%
      mutate(FTEp = FTE-(FTE*rid),
             
             RFTEt = RT/FTE,
             
             RFTEprog = RT/(FTEp),
             
             VARrfte = 100*((RFTEprog-RFTEt)/RFTEt),
             
             VarRT = RT+(RT*vRT),
             
             VarFT = FTEp+(FTEp*vFTE),
             
             RFTEr = VarRT/VarFT,
             
             VARRFTEr = ifelse(rid == 0, "", 100*((RFTEr-RFTEt)/RFTEt)),
             
             TN = 100,
             
             RisN = ifelse(rid==0, "",
                           
                           (VARRFTEr)/VARrfte)
             
      )
  )
  
  output$tb <- renderTable(df() %>%
                             
                             select("Ricavo Totale Previsto" = RT, "FTE disponibili" = FTE,
                                    
                                    "FTE programmato" = FTEp, "Ricavo per FTE teorico" =RFTEt,
                                    
                                    "Ricavo per FTE programmato" =RFTEprog, Target=VARrfte, "Target Normalizzato" = TN)
                           
  )
  output$tb2 <- renderTable(df() %>%
                              
                              select("Ricavo Totale" = VarRT, "FTE" =VarFT, "Ricavo per FTE" = RFTEr,
                                     Risultato=VARRFTEr, "Risultato Normalizzato" = RisN)
                            
  )
  
  output$rfteT <- renderValueBox({
    valueBox( (df() %>%
                 mutate(RFTEt= round(RFTEt, 2)) %>%
                 select(RFTEt)), "Ricavo per FTE teorico",  icon = icon("euro"),
              color = "blue"
    )
  })
  
  
  output$ftep <- renderValueBox({
    valueBox( (df() %>%
                 select(FTEp)), "FTE programmati per l'attività istituzionale",  icon = icon("flask"),
              color = "blue"
    )
  })
  
  
  output$rfteP <- renderValueBox({
    valueBox( (df() %>%
                 mutate(RFTEprog= round(RFTEprog, 2)) %>%
                 select(RFTEprog)), "Ricavo per FTE programmato",  icon = icon("euro"),
              color = "aqua"
    )
  })
  
  
  output$target <- renderValueBox({
    valueBox( (df() %>%
                 mutate(VARrfte= round(VARrfte, 2)) %>%
                 select(VARrfte)), "Variazione % attesa del RFTE",  icon = icon("euro"),
              color = "red"
    )
  })
  
  
  output$rtot <- renderValueBox({
    valueBox( (df() %>%
                 mutate(VarRT= round(VarRT, 2)) %>%
                 select(VarRT)), "Ricavo Totale ",  icon = icon("euro"),
              color = "blue"
    )
  })
  
  output$fteR <- renderValueBox({
    valueBox( (df() %>%
                 mutate(VarFT= round(VarFT, 2)) %>%
                 select(VarFT)), "FTE erogati ",  icon = icon("flask"),
              color = "blue"
    )
  })
  options(scipen = 999)
  output$rfteR <- renderValueBox({
    valueBox( (df() %>%
                 mutate(RFTEr= round(RFTEr, 2)) %>%
                 select(RFTEr)), "Ricavo per FTE erogati ",  icon = icon("euro"),
              color = "blue"
    )
  })
  
  output$target2 <- renderValueBox({
    valueBox( (df() %>%
                 mutate(VARRFTEr= as.numeric(VARRFTEr)) %>%
                 mutate(VARRFTEr = round(VARRFTEr, 2)) %>%
                 mutate(VARRFTEr= ifelse(is.na(VARRFTEr), 0, VARRFTEr)) %>%
                 select(VARRFTEr)), "Variazione % reale del RFTE ",  icon = icon("euro"),
              color = "red"
    )
  })
  
  output$risn <- renderValueBox({
    valueBox( (df() %>%
                 mutate(RisN = as.numeric(RisN, 1)) %>%
                 mutate(RisN = ifelse(is.na(RisN), 0, RisN)) %>%
                 mutate(RisN = round(RisN, 1)) %>%
                 select(RisN)), "Indicatore di verifica",  icon = icon("euro"),
              color = "red"
    )
  } )
  
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
