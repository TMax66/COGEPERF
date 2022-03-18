ui<-navbarPage("CONTROLLO DI GESTIONE - CENTRI DI COSTO  (Versione TEST)",
   # theme = shinytheme("cerulean"),

    tabPanel("Produzione",
             sidebarLayout(
                 sidebarPanel(width = 3,
                     selectInput("CC", "Seleziona il Centro di Costo", 
                                # choices = c("", as.character(unique(factor(dtanalisi$CDC))), 
                                  choices = c("", produzione          
                                             )),
                     uiOutput("parametri"), 
                     uiOutput("tipocont")
                     # radioButtons("tipoconteggio", "seleziona tipo di conteggio", 
                     #              c("Trimestrale",
                     #                "Progressivo" ))
                 ),
               mainPanel(
                   tabsetPanel(id = "tabs",
                       tabPanel("Attività", value = 1,
                                fluidRow(
                                    column(12,
                                      h2(textOutput("titoloAtt")),
                                      plotOutput("PLOT",height = "500px" ))
                                ), 
                                hr(),
                                fluidRow(
                                    column(11,
                                           
                                    htmlOutput("dtprestazioni")
                                    )
                                )
                              
                                
                                ), 
                       tabPanel("Ricavi", value = 2,
                                fluidRow(
                                    
                                    column(12,  
                                           h2(textOutput("titoloRic")),
                                           plotOutput("PLOT2",height = "500px" ))
                                     
                                ), 
                                hr(), 
                                fluidRow(
                                    column(11, 
                                    htmlOutput("dtricprest")
                                ))
                                # ,
                                # hr(),
                                # fluidRow(
                                #     uiOutput("butt2")
                                #     
                                # ), 
                   ), 
                   tabPanel("Costi", value = 3,
                            fluidRow(
                                column(12,  
                                       h2(textOutput("titoloCosti")),
                                       plotOutput("PLOT3",height = "500px" ))
                                
                            ), 
                            hr(), 
                            fluidRow(
                                column(11, 
                                       htmlOutput("dettcosti")
                                ))
               )
               ) 
    )), 
    
      
), 

tabPanel("Gestione", 
         
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("CC2", "Seleziona il Centro di Costo", 
                                    choices = c("", gestionale          
                                    )),
                        uiOutput("tipocont2")
                       
                        
                        # radioButtons("tipoconteggio2", "seleziona tipo di conteggio", 
                        #              c("Trimestrale",
                        #                "Progressivo" ))
           ),
           mainPanel( 
             tabsetPanel(id = "tabsgest",
                         tabPanel("Costi", value = 4,
                                  fluidRow(
                                    column(12,  
                                           h2(textOutput("titoloCosti2")),
                                           plotOutput("PLOT4",height = "500px" ))
                                    
                                  ), 
                                  hr(), 
                                  fluidRow(
                                    column(11, 
                                           htmlOutput("dettcostigest")
                                    ))
                         )
             
                         
                         
                         
                         )
             
             
             
             
             
           ))
 )
, 
# tabPanel("Costi Comuni", 
#          
#          sidebarLayout(
#            sidebarPanel(width = 3,
#                         selectInput("CC3", "Seleziona il Centro di Costo", 
#                                     choices = c("", cscomuni          
#                                     ))
#            ),
#            mainPanel( 
#              tabsetPanel(id = "tabscosticom",
#                          tabPanel("Costi", value = 4,
#                                   fluidRow(
#                                     column(12,  
#                                            h2(textOutput("titoloCosti4")),
#                                            plotOutput("PLOT5",height = "500px" ))
#                                     
#                                   ), 
#                                   hr(), 
#                                   fluidRow(
#                                     column(11, 
#                                            htmlOutput("dettcosticom")
#                                     ))
#                          )
#                          
#                          
#                          
#                          
#              )
#              
#              
#              
#              
#              
#            ))
#          
#          
#          
#          
#          
#          ), 



tabPanel("TABELLE PIVOT", 
         
         fluidPage(
           # radioButtons(inputId = "format", label = "Enter the format to download", 
           #              choices = c( "csv", "excel"), inline = FALSE, selected = "csv"),
           # downloadButton("download_pivot"),
           # actionButton("copy_pivot", "Copy"),
           fluidRow(rpivotTableOutput("pivot") %>% 
                      withSpinner(color="blue", type=8)))
         )
)
         
         
         
         
     

 


              

