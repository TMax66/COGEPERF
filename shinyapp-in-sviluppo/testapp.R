


## Prova SumBOx----

SumBOX <- function(dt, Variabile, Variabile2 = NULL){ 
  
  if(is.null(Variabile2)){    
    
    valore <- sum(dt[, Variabile])  
    
  } else {
    valore <- round(sum(dt[, Variabile]/sum(dt[, Variabile2])),2)
  }
  
}


tizsler <-  tabIZSLER %>%  
  rename( "ANALISI" = esami, "VALORE" = valore, "VP" = ricavovp, "AI" = valoreai, 
          "COSTI" = costi) %>%
  group_by(Anno, Dipartimento) %>%
  summarise_at(c("ANALISI", "VALORE",  "VP", "AI", "FTED", "FTEC","COSTI"), sum, na.rm = T) %>%
  mutate(RT = (VALORE+VP+AI),
         FTE_T = round((FTED+FTED),1)) %>%
  arrange(desc(ANALISI)) %>%
  mutate("R-FTE" = round(RT/FTE_T,0), 
         "C-FTE" = round(COSTI/FTE_T, 0), 
         "ROI" = round(RT/COSTI, 2)) %>% 
  select(-FTED, -FTEC)

theme <- bslib::bs_theme(version = 4)

ui <- fluidPage(
  
  theme = theme, 
  br(),
  sliderInput("Anno", "anno", min = 2019, max = 2021, value=2021), 
  
  
  hr(), 
  uiOutput("sumbox"), 
 
  
  
)


server <- function(input, output, session)
{

IZSLER <- reactive( tizsler %>% 
                        filter(Anno == 2019))
  
output$sumbox <- renderUI({
  
  fluidRow(
    box(
    summaryBox("N.analisi", prettyNum(SumBOX(IZSLER(), "ANALISI"), big.mark = ".", decimal.mark = ","), style= "info" ), 
    summaryBox("Valorizzazione da tariffario", prettyNum(SumBOX(IZSLER(), "VALORE"), big.mark = ".", decimal.mark = ","), style= "info" )
  )
  )
  
})
  

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
