ui<-navbarPage("CONTROLLO DI GESTIONE - CENTRI DI COSTO",
    theme = shinytheme("cerulean"),

    tabPanel("Attività-Ricavi-Costi",
             sidebarLayout(
                 sidebarPanel(width = 3,
                     h4(textOutput("struttura")),
                     
                     selectInput("CC", "Seleziona il Centro di Costo", 
                                 choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))),
                     uiOutput("sel1"),
                     uiOutput("sel2"), 
                     uiOutput("parametri")
                 ),
               mainPanel(
                   tabsetPanel(
                       tabPanel("Attività", 
                                fluidRow(
                                    column(12,
                                      h2(textOutput("titoloAtt")),
                                      plotOutput("PLOT",height = "400px" ))
                                ), 
                                # hr(),
                                # fluidRow(
                                #     uiOutput("butt1")
                                #     
                                # ), 
                                hr(),
                                fluidRow(
                                    column(11,
                                           
                                    htmlOutput("dtprestazioni")
                                    )
                                )
                                ), 
                       tabPanel("Ricavi", 
                                fluidRow(
                                    
                                    column(12,  
                                           h2(textOutput("titoloRic")),
                                           plotOutput("PLOT2",height = "400px" ))
                                     
                                )
                                # ,
                                # hr(),
                                # fluidRow(
                                #     uiOutput("butt2")
                                #     
                                # ), 
                   ), 
                   tabPanel("Costi")

                   )
               )
               )),
              
 
    
    # tabPanel("Costi",
    #          sidebarLayout(
    #            sidebarPanel(),
    #            mainPanel()
    #          )
    # ), 
                   
    # tabPanel("Indicatori",
    #          sidebarLayout(
    #            sidebarPanel(),
    #            mainPanel()
    #          )
    # ), 
    
    tabPanel("Tools",
             sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
             )
    )     
)
              

