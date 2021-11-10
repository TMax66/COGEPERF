ui<-navbarPage("IZSLER: Gestione Centralizzata Delle Richieste dell'Utenza",
    theme = shinytheme("cerulean"),
           
    tabPanel("Dati",
             fluidRow(
               br(), 
               downloadButton("downloadData", "Scarica i dati"),
                 DTOutput("dati")
             )
             ), 
    
    
    tabPanel("Pivot Table",
              
                 fluidPage(
                     # radioButtons(inputId = "format", label = "Enter the format to download", 
                     #              choices = c( "csv", "excel"), inline = FALSE, selected = "csv"),
                     # downloadButton("download_pivot"),
                     # actionButton("copy_pivot", "Copy"),
                     fluidRow(rpivotTableOutput("pivot") %>% 
                                  withSpinner(color="blue", type=8)))
               
             
             )
    # ), 
    #                
    # tabPanel("TAB2",
    #          sidebarLayout(
    #            sidebarPanel(),
    #            mainPanel()
    #          )
    # ), 
    # 
    # tabPanel("....",
    #          sidebarLayout(
    #              sidebarPanel(),
    #              mainPanel()
    #          )
    # )     
)         
             

