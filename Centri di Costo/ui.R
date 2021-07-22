ui<-navbarPage("CENTRI DI COSTO",
    theme = shinytheme("cerulean"),
           
    tabPanel("Attività Analitica e Produzione",
             sidebarLayout(
               sidebarPanel(
                   
                   selectInput("CC", "Seleziona il Centro di Costo", 
                               choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))), 
                   selectInput("paga", "Tipologia Attività", 
                               choices = c("", as.character(unique(factor(dtanalisi$Pagamento))))),
                   radioButtons("uff", "", choices = c("Ufficiale", "Non Ufficiale")) 
               ),
               mainPanel(
                   fluidRow(
                       h2(textOutput("Titolo")), 
                       column(8, offset = -4, 
                       plotOutput("plot1", height="1000px"))
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
             

