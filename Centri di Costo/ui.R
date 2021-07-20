ui<-navbarPage("CENTRI DI COSTO",
    theme = shinytheme("cerulean"),
           
    tabPanel("Attività Analitica e Produzione",
             sidebarLayout(
               sidebarPanel(
                   
                   selectInput("CC", "Seleziona il Centro di Costo", 
                               choices = unique(factor(dt$`Centro di Costo`))), 
                   selectInput("paga", "Tipologia Attività", 
                               choices = unique(factor(dt$Pagamento))), 
                   selectInput("par", "Seleziona il Parametro", 
                               choices = c("Valorizzato", "Fatturato", "Variazione"))
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
             

