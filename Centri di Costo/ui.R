ui<-navbarPage("CENTRI DI COSTO",
    theme = shinytheme("cerulean"),
           
    tabPanel("Attività Analitica e Produzione",
             sidebarLayout(
               sidebarPanel(
                   
                   selectInput("CC", "Seleziona il Centro di Costo", 
                               choices = c("", as.character(unique(factor(dtanalisi$`Centro di Costo`))))), 
                   selectInput("paga", "Tipologia Attività", 
                               choices = c("", as.character(unique(factor(dtanalisi$Pagamento))))), 
               ),
               mainPanel(
                   fluidRow(
                       plotOutput("varchange")
                   )
               )
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
             

