ui<-navbarPage("CONTROLLO DI GESTIONE - CENTRI DI COSTO",
    theme = shinytheme("cerulean"),

    # tabPanel("Situazione Generale",
    #          sidebarLayout(
    #              sidebarPanel(
    #                  h4(textOutput("struttura")),
    # 
    #                  selectInput("CCx", "Seleziona il Centro di Costo",
    #                              choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`)))))
    # 
    #              ),
    #         mainPanel(
    # 
    #         )
    #          )
    # 
    # 
    # 
    #          ),


    
    
    
    
    tabPanel("Attività Analitica e Produzione",
             sidebarLayout(
                 sidebarPanel(
                     h4(textOutput("struttura")),
                     
                     selectInput("CC", "Seleziona il Centro di Costo", 
                                 choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))),
                     selectInput("uff", "Ufficiale/Non Ufficiale", 
                                 choices = c("","Ufficiale", "Non Ufficiale")), 
                     selectInput("paga", "Gratuito/Pagamento", 
                                 choices = c("", "Gratuito", "Pagamento"))
                     
                 ),
               mainPanel(
                   fluidRow(
                       h2(textOutput("Titolo")), 
                       column(9, offset = -4, 
                       plotOutput("plot1", height="800px"))
                   ))
               )
             ), 
 
    
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
             

