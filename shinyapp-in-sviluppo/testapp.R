library(shiny)

tabIZSLER <- readRDS(file = here( "data", "processed", "TABELLA.rds"))

ValueBOX <- function(dt, Variabile, Titolo, colore, icona)
{ valore <- sum(dt[, Variabile])
valueBox(prettyNum(valore, big.mark = "."), Titolo, icon = icon(icona), color = colore)
}



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
  ValueBOX(tdiprep(), "ANALISI", Titolo = "N.Analisi", colore = "red", icona = "flask")
)
  
}


shinyApp(ui, server)