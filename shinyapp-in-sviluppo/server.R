server <- function(input, output, session) { 

options(shiny.reactlog=TRUE)
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
  valueBox(prettyNum(ric(), big.mark = "." ), "Valorizzazioni da Tariffario",  icon = icon("euro"),
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


cst <- reactive(
  IZSLER() %>%
    summarise(costi = round((sum(costi)), 0)) %>% 
    select(costi))
output$Costi <- renderValueBox({
  valueBox(prettyNum(cst(), big.mark = "." ), "Costi complessivi",  icon = icon("euro"),
           color = "blue"
  )
})


cstfte <- reactive(
  IZSLER() %>%
    summarise(costifte = round((sum(costi)/ sum(FTE_T)), 0)) %>% 
    select(costifte))
output$costifte <- renderValueBox({
  valueBox(prettyNum(cstfte(), big.mark = "." ), "Costi per Full Time Equivalente",  icon = icon("euro"),
           color = "blue"
  )
})



roit <- reactive(
  IZSLER() %>%
    summarise(roi = round((sum(RT)/sum(costi)), 2)) %>% 
    select(roi))
output$roi <- renderValueBox({
  valueBox(prettyNum(roit(), big.mark = "." , decimal.mark = ","), "ROI",
           color = ifelse(roit() >= 1, "green", "red")
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


###tabella x dipartimenti####

output$t <- renderUI({
    border <- officer::fp_border()
    flextable(
      (IZSLER() %>%
         left_join(
           (pubs() %>%
              filter(articoliif == "IF") %>%
              count(Dipartimento, NR) %>%
              group_by(Dipartimento) %>%  
              count(NR) %>%
              summarise("Pubblicazioni" = sum(n))), by = "Dipartimento") %>%    
         left_join(
           (pr() %>%
              group_by(Dipartimento) %>%
              summarise("Progetti di Ricerca"=nlevels(factor(Codice)))
           ),  by = "Dipartimento" ))
      ) %>%  
      theme_booktabs() %>% 
      color(i = 1, color = "blue", part = "header") %>%
      bold( part = "header") %>%
      fontsize(size=15) %>%
      fontsize(part = "header", size = 15) %>%
      line_spacing(space = 2.5) %>% 
      autofit() %>%
      colformat_num(j = c( "ricavi", "VP", "AI", "costi",  "RT"), big.mark = ".", decimal.mark = ",", prefix = "€") %>%
      htmltools_value() 
      
})
  
     
 
     
      
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
  

 }

