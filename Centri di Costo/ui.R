ui<-navbarPage("CONTROLLO DI GESTIONE - CENTRI DI COSTO",
    theme = shinytheme("cerulean"),

    tabPanel("Attività-Ricavi-Costi",
             sidebarLayout(
                 sidebarPanel(width = 3,
                     selectInput("CC", "Seleziona il Centro di Costo", 
                                 choices = c("", as.character(unique(factor(dtanalisi$CDC))))),
                     uiOutput("parametri")
                 ),
               mainPanel(
                   tabsetPanel(id = "tabs",
                       tabPanel("Attività", value = 1,
                                fluidRow(
                                    column(12,
                                      h2(textOutput("titoloAtt")),
                                      plotOutput("PLOT",height = "400px" ))
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
                                           plotOutput("PLOT2",height = "400px" ))
                                     
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
                                       plotOutput("PLOT3",height = "400px" ))
                                
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
tabPanel("TABELLA PIVOT", 
         # fluidPage(
         #     fluidRow(
         #         downloadButton("download_pivot", label = "Excel")), 
         #     fluidRow(
         #         column(6,div(style="height:10px"),rpivotTableOutput("pivot")
         #         ))
         #     
         # )
         
         fluidPage(
           radioButtons(inputId = "format", label = "Enter the format to download", 
                        choices = c( "csv", "excel"), inline = FALSE, selected = "csv"),
           downloadButton("download_pivot"),
           actionButton("copy_pivot", "Copy"),
           fluidRow(rpivotTableOutput("pivot")))
         )
)
         
         
         
         
     

 


              

