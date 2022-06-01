# Dipartimento----

##dati del 2022----
rfte22point <- reactive({
  dtmensili %>%
  select(Dipartimento, Anno, MESE, RT, FTET,FTp, RFTE, RFTEc) %>%  
  filter(Dipartimento == input$dip & Anno == 2022)
  })

##tabella----
tRFTEdipa <- reactive({
  dtmensili %>% 
    filter(Dipartimento == input$dip) %>%
    ungroup() %>% 
    mutate(FTETp = round(FTET*(FTp/100),1),
           FTp = round(FTp,1)) %>% 
    select(Anno, MESE, RT, FTET,FTp,FTETp, RFTE, RFTEc)
})


dip <- reactive({input$dip})


## grafici cumulati e mensili del RFTE----
  p1<-reactive({dtmensili%>% 
  filter(Dipartimento == input$dip & Anno == 2021) %>% 
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_point()+
  geom_line(group = 1, alpha = 0.5)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha = 0.3) +
  geom_point(data = rfte22point(), aes(x=MESE, y=RFTE), color="red") +
      geom_line(data = rfte22point(), aes(x=MESE, y=RFTE), color="red", lty=3)+
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_bw()
  })
  
  p2<-reactive({dtmensili%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = RFTEc)+
    geom_point()+
    geom_line(group = 1, alpha =0.5)+
    geom_ribbon(aes(ymin = lowc, ymax = RFTEc), fill = "grey70", alpha = 0.3) +
    geom_point(data = rfte22point(), aes(x=MESE, y=RFTEc), color="red") +
    geom_line(data = rfte22point(), aes(x=MESE, y=RFTEc), color="red", lty=3)+
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
})

P <-reactive({ p2()/p1()})

##grafici RT e FTE----

p3<-reactive({dtmensili%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = RT)+
    geom_point(alpha = 0.3)+
    geom_line(group = 1, alpha = 0.3)+
    geom_point(data = rfte22point(), aes(x=MESE, y=RT), color="red") +
    geom_line(data = rfte22point(), aes(x=MESE, y=RT), color="red", lty=3) +
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
})

p4<-reactive({dtmensili%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = FTET*(FTp/100))+
    labs(y = "FTET programmati per gli obiettivi valorizzati")+
    geom_point(alpha = 0.3)+
    geom_line(group = 1, alpha = 0.3)+
    geom_point(data = rfte22point(), aes(x=MESE, y = FTET*(FTp/100)), color="red") +
    geom_line(data = rfte22point(), aes(x=MESE, y = FTET*(FTp/100)), color="red", lty=3) +
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    theme_bw()
})

P2 <-reactive({ p3()/p4()})


 


#Reparti----

## dati del 2022----
rfte22pointR <-  reactive({
  dtmensiliR %>%
  select(Dipartimento,Reparto, Anno, MESE, RT, FTET, FTp, RFTE, RFTEc) %>%  
  filter(Dipartimento == input$dip & Anno == 2022)
})

## tabella----
tRFTErep <- reactive({
  dtmensiliR %>% 
    filter(Dipartimento == input$dip) %>%
    ungroup() %>% 
    mutate(FTETp = round(FTET*(FTp/100), 1),
           FTp = round(FTp,1)) %>% 
    select(Reparto,Anno, MESE, RT, FTET,FTp,FTETp, RFTE, RFTEc)
     
})

## grafici cumulati e mensili----
P3 <- reactive({dtmensiliR %>%
  filter(Dipartimento == input$dip & Anno == 2021) %>%
  ggplot()+
  aes(x = MESE, y = RFTE)+
  geom_point()+
  geom_line(group = 1, alpha = 0.3)+
  geom_ribbon(aes(ymin = low, ymax = RFTE), fill = "grey70", alpha = 0.3) +
  geom_point(data = rfte22pointR(), aes(x=MESE, y=RFTE), color="red") +
  geom_line(data = rfte22pointR(), aes(x=MESE, y=RFTE), color="red", lty=3) +
  scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  facet_wrap(Reparto~., scales = "free")+
  theme_bw()+
  theme(strip.text = element_text(size = 6.5, color = "blue"))
})

P4 <- reactive({dtmensiliR%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = RFTEc)+
    geom_point()+
    geom_line(group = 1, alpha =0.5)+
    geom_ribbon(aes(ymin = lowc, ymax = RFTEc), fill = "grey70", alpha = 0.3) +
    geom_point(data = rfte22point(), aes(x=MESE, y=RFTEc), color="red") +
    geom_line(data = rfte22point(), aes(x=MESE, y=RFTEc), color="red", lty=3)+
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    facet_wrap(Reparto~., scales = "free")+
    theme_bw()+
    theme(strip.text = element_text(size = 6.5, color = "blue"))
})

##grafici RT e FTE----

P5<-reactive({dtmensiliR%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = RT)+
    geom_point(alpha = 0.3)+
    geom_line(group = 1, alpha = 0.3)+
    geom_point(data = rfte22pointR(), aes(x=MESE, y=RT), color="red") +
    geom_line(data = rfte22pointR(), aes(x=MESE, y=RT), color="red", lty=3) +
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    facet_wrap(Reparto~., scales = "free")+
    theme_bw()+
    theme(strip.text = element_text(size = 6.5, color = "blue"))
})

P6<-reactive({dtmensiliR%>% 
    filter(Dipartimento == input$dip & Anno == 2021) %>% 
    ggplot()+
    aes(x = MESE, y = FTET*(FTp/100))+
    labs(y = "FTET programmati per gli obiettivi valorizzati")+
    geom_point(alpha = 0.3)+
    geom_line(group = 1, alpha = 0.3)+
    geom_point(data = rfte22pointR(), aes(x=MESE, y = FTET*(FTp/100)), color="red") +
    geom_line(data = rfte22pointR(), aes(x=MESE, y = FTET*(FTp/100)), color="red", lty=3) +
    scale_x_discrete(limit = c(1,2,3,4,5,6,7,8,9,10,11,12))+
    facet_wrap(Reparto~., scales = "free")+
    theme_bw()+
    theme(strip.text = element_text(size = 6.5, color = "blue"))
})




# Codici per la produzione del report in Excel----
output$download_excel <- downloadHandler(
  filename = function() {
    "RFTE.xlsx"
  },
  content = function(file) {
    wb <- createWorkbook()
    
    addWorksheet(wb, sheetName = "Info")
    addWorksheet(wb, sheetName = "Dipartimento")
    addWorksheet(wb, sheetName = "Reparti")
    
    #Cover----
    
    showGridLines(
      wb,
      sheet = 1,
      showGridLines = FALSE
    )
    
    writeData(
      wb, sheet = 1,
      
      c(dip(), 
        "", 
        "Monitoraggio del Ricavo Full Time Equivalente", 
        "", 
        "Ricavi totali (RT)",
        "I RT per ogni dipartimento sono calcolati a partire dai dati forniti dal datawerehouse DW_COGEP", 
        "sommando  l'attivita' valorizzata a tariffario,  quella fatturata, i ricavi dalla Vendita dei prodotti (VP) e dall'attivita' interna (AI).",
        "",
        "FTE", 
        "I FTE sono calcolati come il rapporto tra le ore complessivamente erogate dal personale di comparto e dirigenza", 
        "e le ore previste per un full time equivalente sulla base del contratto (36h settimanli per il comparto e 38 per la dirigenza)", 
        "",
        "FTE programmato", 
        "In fase di programmazione viene assegnata una % dei FTE disponibili a inizio anno alle attivita' istituzionali", 
        "dette anche valorizzate. Questa percentuale rappresenta il FTE programmato che viene utilizzata per il calcolo del RFTE", 
        "", 
        "Ricavo Full Time Equivalente (RFTE)", 
        "RFTE e' un indicatore di performance che tiene conto dei volumi di attivita' valorizzata e delle risorse a disposizione per la loro realizzazione", 
        "misurate come FTE. A partire dal 2022 l'RFTE viene assegnato a tutte le strutture che erogano attivita' analitiche come obiettivo di efficienza", 
        "Target:raggiungimento di almeno il 90% del valore di RFTE al 31/12/2021.", 
        "",
        "Monitoraggio", 
        "Per ogni dipartimento e per ogni struttura complessa o reparto", 
        "viene calcolato il RFTE mensile e cumulato ottenuto nel corso del 2021", 
        "Graficamente viene rappresentato sia l'andamento mensile che quello cumulato con l'aggiunta", 
        "di una banda posta sotto la linea di andamento, la cui ampiezza corrisponde al valore del RFTE del 2021 a cui viene sottrato il 10%", 
        "questa banda corrisponde all'area di raggiungimento del target previsto dall'obiettivo", 
        "I dati dell'andamento del 2021 sono riportati in rosso e vengono aggiornati ad ogni trimestre in occasione dell'aggiornamento", 
        "del data warehouse del controllo di gestione da parte dei Sistemi Informativi", 
        "I punti relativi al RFTE del 2022 che cadono nella banda grigia o superiormente alla linea di andamento del RFTE del 2021,indicano", 
        "il raggiungimento del target", 
        
        "", 
        "Alla rappresentazione grafica di cui sopra si aggiunge anche l'andamento mensile del RT e del FTE", 
        "In questo modo sara' possibile valutare quale dei due parametri incide principalmente sul valore di RFTE ottenuto.", 
        "", 
        "Ogni grafico e' accompagnato dalla tabella che riporta i dati utilizzati per la sua realizzazione",
        "I grafici sono inseriti nei diversi fogli del workbook come immagini e quindi non possono essere modificati"
        ), 
      startRow = 1, 
      startCol = 1
    )

    #Dipartimento sheet ----
    
    writeData(
      wb,
      sheet = 2,
      tRFTEdipa(),
      startRow = 1,
      startCol = 1
     
    )
    
    
    png("p11.png")
    print(P())
    dev.off()
    png("p12.png")
    print(P2())
    dev.off()
    
    insertImage(wb, "Dipartimento", "p11.png",  startCol = "J",  width = 5, height = 5)
    insertImage(wb, "Dipartimento", "p12.png",  startCol = "Q",  width = 5, height = 5)
    
    #Reparti sheet ----
    
    writeData(
      wb,
      sheet = 3,
      tRFTErep(),
      startRow = 1,
      startCol = 1
      
    )
    
    png("p13.png", width = 650)
    print(P3())
    dev.off()
    
    png("p14.png", width = 650)
    print(P4())
    dev.off()
    
    png("p15.png", width = 650)
    print(P5())
    dev.off()
    
    png("p16.png", width = 650)
    print(P6())
    dev.off()
    
    
    insertImage(wb, "Reparti", "p13.png",  startCol = "K",width = 7, height = 5)
    insertImage(wb, "Reparti", "p14.png",  startCol = "T",width = 7, height = 5)
    insertImage(wb, "Reparti", "p15.png",  startCol = "K",startRow = 27,width = 7, height = 5)
    insertImage(wb, "Reparti", "p16.png",  startCol = "T",startRow = 27,width = 7, height = 5)
  
    
    
    
    saveWorkbook(wb, file, overwrite = T)
    
    unlink(c(P(), P2(), P3(), P4(), P5(), P6()))
     
  }
)






































