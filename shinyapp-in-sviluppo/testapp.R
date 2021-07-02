
##Prova ValueBox----
ui <- fluidPage(
  
  sliderInput("Anno", "anno", min = 2019, max = 2021, value=2021), 
  
  selectInput("dip", "Dipartimento", choices = c("Direzione Sanitaria", "Dipartimento tutela e salute animale")), 
  hr(), 
  valueBoxOutput("esami"), 
 
  
  
)


server <- function(input, output, session)
{

tdiprep <- reactive(
    (tabIZSLER %>% 
       filter(Anno == input$Anno & Dipartimento == input$dip) %>% 
       rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
               "COSTI" = costi) %>%
       group_by(Reparto) %>%
       summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
       mutate(RT = (VALORE+VP+AI),
              FTE_T = round((FTED+FTED),1)) %>%
       arrange(desc(ANALISI)) %>%
       mutate("R-FTE" = round(RT/FTE_T,0), 
              "C-FTE" = round(COSTI/FTE_T, 0), 
              "ROI" = round(RT/COSTI, 2)) %>% 
       select(-FTED, -FTEC)))



output$esami <- renderValueBox(
  ValueBOX(tdiprep(), "RT", Variabile2 ="COSTI",   Titolo = "roi", colore = "red", colcond = "NO",  icona = "flask")
)
  
}


shinyApp(ui, server)


##grafico RFT/CFT

tabIZSLER %>% 
  mutate(FT = FTED+FTEC) %>% 
  select(Anno, Dipartimento, Reparto, Laboratorio, FT, totricavi, costi) %>%
  
  ggplot(aes(x = FT, y = costi))+
  geom_point()+
  facet_wrap(Dipartimento~Anno)


tabIZSLER %>% 
  mutate(RFT = totricavi/(FTED+FTEC), 
         CFT = costi/(FTED+FTEC), 
         FT = FTED+FTEC) %>% 
  select(Anno, Dipartimento, Reparto, Laboratorio, FT, RFT, CFT) %>% 
  pivot_longer(cols = 6:7, values_to = "valore", names_to = "metrica" ) %>%
  
  ggplot(aes(x = FT, y = valore, col = metrica))+
  geom_point()+
  facet_wrap(Dipartimento~Anno)
