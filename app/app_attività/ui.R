ui <- navbarPage(
  title = "IZSLER: attività analitica", 
  
 tabPanel(title = "IZSLER", value = "home",
          fluidRow(
            h3("Attività analitica"),
            
            uiOutput("aggdati")), 
          hr(), br(), 
          mainPanel(  
          tabsetPanel(
            tabPanel( 
              "Attività complessiva", 
              fluidPage(
                fluidRow(
                ) 
              )
            ), 
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
                "Attività complessiva", 
                fluidPage(
                  fluidRow(
                    
                  ) 
                )
                
              ),
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
                "Attività complessiva", 
                fluidPage(
                  fluidRow(
                    
                  ) 
                )
                
              ),
              tabPanel( 
                "Sanità Animale"), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            ))), 
 # Lombardia----
 #_____________________________________________________________________________
 tabPanel(title = "Dipartimento Area Territoriale Lombardia", value = "lomb",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Attività complessiva", 
                  fluidRow(column(9, 
                    plotOutput("clombTotY"))
                  ), 
                fluidRow(column(9, 
                 plotOutput("clombTotM"))
                ),
                fluidRow(column(9, 
                plotOutput("clombTotW"))
                ),
              ),
              tabPanel( 
                "Sanità Animale", 
                fluidRow(
                  column(9,
                         plotOutput("salombY"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("salombM"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("salombW"))
                )
                
                ), 
              tabPanel(
                "Alimenti Uomo",
                fluidRow(
                  column(9,
                         plotOutput("aulombY"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("aulombM"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("aulombW"))
                )), 
              tabPanel(
                "Alimenti Zootecnici",
                fluidRow(
                  column(9,
                         plotOutput("azlombY"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("azlombM"))
                ), 
                fluidRow(
                  column(9, 
                         plotOutput("azlombW"))
                )
              )
            ))),
 tabPanel(title = "Dipartimento Area Territoriale Emilia Romagna", value = "er",
          mainPanel(  
            tabsetPanel(
              tabPanel( 
                "Attività complessiva", 
                fluidPage(
                  fluidRow(
                    
                  ) 
                )
                
              ),
              tabPanel( 
                "Sanità Animale"), 
              tabPanel(
                "Alimenti Uomo"), 
              tabPanel(
                "Alimenti Zootecnici"
              )
            )))
)