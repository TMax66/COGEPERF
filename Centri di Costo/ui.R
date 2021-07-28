ui<-navbarPage("CONTROLLO DI GESTIONE - CENTRI DI COSTO",
    theme = shinytheme("cerulean"),

    tabPanel("Attività Analitica e Produzione",
             sidebarLayout(
                 sidebarPanel(
                     h4(textOutput("struttura")),
                     
                     selectInput("CC", "Seleziona il Centro di Costo", 
                                 choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))),
                     uiOutput("sel1"),
                     uiOutput("sel2")
                 ),
               mainPanel(
                   tabsetPanel(
                       tabPanel("Attività", 
                                fluidRow(
                                    column(12,
                                      plotOutput("plotEs", height = "400px"))
                                ), 
                                hr(),
                                fluidRow(
                                    uiOutput("butt1")
                                    
                                ), 
                                
                                
                                
                                
                                
                                ), 
                       tabPanel("Ricavi", 
                                fluidRow(
                                    
                                    column(12,  
                                    plotOutput("plotT", height="400px"), 
                                    plotOutput("plotUf", height="400px"), 
                                    plotOutput("plotNuf", height="400px"), 
                                    plotOutput("plotpag", height="400px"), 
                                    plotOutput("plotgrat", height="400px"),
                                    h2(textOutput("Titolo")),
                                    plotOutput("plot1", height="400px"))
                                )
                   )

                   )
               )
               )),
              
 
    
    tabPanel("Costi",
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             )
    ), 
                   
    tabPanel("Indicatori",
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             )
    ), 
    
    tabPanel("....",
             sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
             )
    )     
)
              

