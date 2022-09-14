ui <- navbarPage(
  title = "IZSLER: attività analitica", 
  
 tabPanel(title = "Home", value = "home",
          fluidRow(
            h3("PIPPO"),
            
            uiOutput("aggdati")), 
          
          mainPanel(  
          tabsetPanel(
               tabPanel( 
            "Sanità Animale", 
            fluidPage(
             fluidRow(
                
             ) 
            )
            
            ), 
              tabPanel(
             "Alimenti Uomo"), 
              tabPanel(
             "Alimenti Zootecnici"
              )
              ))),
 tabPanel(title = "Dipartimento Tutela e Salute Animale", value = "tsa",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Sanità Animale"), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            ))), 
 tabPanel(title = "Dipartimento Sicurezza Alimentare", value = "sa",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Sanità Animale"), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            ))), 
 tabPanel(title = "Dipartimento Area Territoriale Lombardia", value = "lomb",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Sanità Animale", 
                fluidRow(
                  column(6,
                         plotOutput("clombY"))
                ), 
                fluidRow(
                  column(6, 
                         plotOutput("clombM"))
                ), 
                fluidRow(
                  column(6, 
                         plotOutput("clombW"))
                )
                
                ), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            ))),
 tabPanel(title = "Dipartimento Area Territoriale Emilia Romagna", value = "er",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Sanità Animale"), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            )))
)