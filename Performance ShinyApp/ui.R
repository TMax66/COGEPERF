ui <- dashboardPage(
  
      dashboardHeader(title = "Controllo di gestione e performances", titleWidth = 500),
#Sidebar
      dashboardSidebar(
        width = 500,
            sidebarMenu(id = "menu", 
            menuItem("Quadro Generale Dipartimenti", tabName = "izsler", icon = icon("globe")), 
            conditionalPanel(
              condition = "input.menu == 'izsler' ", 
              sliderInput("anno", h3("Seleziona l'Anno"), min=2019, max = 2021, value = 2021)), 
           
             menuItem("Dipartimenti", tabName = "dipartimenti", icon = icon("sitemap")), 
             conditionalPanel(
              condition = "input.menu == 'dipartimenti' ", 
              radioButtons("dip", "Seleziona il Dipartimento",
                           choices =  c("DIPARTIMENTO TUTELA E SALUTE ANIMALE", 
                                        "DIPARTIMENTO SICUREZZA ALIMENTARE", 
                                        "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA", 
                                        "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA", 
                                        "DIREZIONE SANITARIA")), 
              br(), 
              sliderInput("anno2", h3("Seleziona l'Anno"), min=2019, max = 2021, value = 2021)), 
            menuItem("Report Costi-Ricavi per Centri di Costo",href = "http://rshiny.izsler.it/costiricavi",
                     newtab = FALSE, icon = icon("euro")
                     
                     ), 
            menuItem("Obiettivi di Performance", tabName = "Performance", icon = icon("table"))
            )),
      
      dashboardBody(
            tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")), 
            tags$head(tags$style(HTML('.modal-lg {width: 1500px;}'))),
            tags$head(tags$style(HTML(".main-sidebar { font-size: 25px; }"))), #change the font size to 20

         
#Quadro Generale----
        tabItems(
          tabItem( 
          tabName = "izsler",
          
          hr(),
           
           fluidRow(  
             column(12, 
               box(title = "Quadro Generale Dipartimenti", solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 12, 
                 valueBoxOutput("esami"),
                 valueBoxOutput("ricavi"),
                 valueBoxOutput("venprod"),
                 valueBoxOutput("attint"),
                 valueBoxOutput("rictot"),
                 valueBoxOutput("RFTE"),
                 valueBoxOutput("Costi"),
                 
                 div(id='clickdiv0',
                            valueBoxOutput("IF")),
                        bsModal("P", "Pubblicazioni IF", "clickdiv0",dataTableOutput("articoli"), size = "large"),

                        # div(id='clickdiv1',
                        #     valueBoxOutput("Int")),
                        # bsModal("CI", "Partecipazione a convegni internazionali", "clickdiv1", dataTableOutput("convegni"), size = "large"),

                        div(id='clickdiv2',
                          valueBoxOutput("PR")),
                        bsModal("Prj", "Progetti di ricerca in corso", "clickdiv2", dataTableOutput("projr"), size = "large"))
               )), 
         
          fluidRow(
             box(title = textOutput("year"),  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12,
            tableOutput("t")
             )
          ),
          fluidRow(
            column(1, 
                   div(style = "font-size: 25px",
                   radioButtons("ind", "", 
                                c("IP" = "IP","Dipartimento" = "Dipartimento" )))),
            
            
            column(11, 
                   div(id = 'clickdiv00',
                       plotOutput("tbd"))
                   # bsModal("TW", "Distribuzione percentuale degli Indicatori di performance  tra i Dipartimenti",  
                   #         'clickdiv00', tableOutput("tbw"))
                   )
            
          ), 
          fluidRow(
           
            column(1, 
                   div(style = "font-size: 25px",
                   radioButtons("kpi", "",
                                c("Prestazioni",
                                "Valorizzazione", 
                                 "VP","AI","RT", "FTED", 
                                "FTEC","FTET")
            ))), 
            column(11, 
                   plotOutput("ptrend"))
          )
          
          ), 
#Dipartimenti----
        tabItem(
          tabName = "dipartimenti", 
             
             hr(), 
             fluidRow(
               column(12,
                      box(title = h3(textOutput("dipa")), solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 12,
                          valueBoxOutput("esamidip"),
                          valueBoxOutput("ricavidip"),
                          valueBoxOutput("venproddip"),
                          valueBoxOutput("attintdip"),
                          valueBoxOutput("rictotdip"),
                          valueBoxOutput("RFTEdip"),
                          valueBoxOutput("Costidip"),
                         div(id='clickdiv3',
                              valueBoxOutput("IFdip")),
                          bsModal("P1", "Pubblicazioni IF", "clickdiv3",dataTableOutput("articolidip"), size = "large"),

                          # #div(id='clickdiv4',
                          #     valueBoxOutput("Intdip"), #),
                          # #bsModal("CI1", "Partecipazione a convegni internazionali", "clickdiv4", dataTableOutput("convegni"), size = "large"),

                          div(id='clickdiv5',
                              valueBoxOutput("PRdip")),
                          bsModal("Prj1", "Progetti di ricerca in corso", "clickdiv5", dataTableOutput("projrep"), size = "large"))
               )
               
               
               
               
             ),

             
             fluidRow(
                 
                column(12,
                box(title = h3(textOutput("dipa2")),  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12, 
                    tableOutput("tr"))
                )), 
              
            fluidRow(
            column(1, 
                   div(style = "font-size: 25px",
                   radioButtons("ind2", "", 
                                c( "IP" = "IP","Reparto" = "Reparto")))),
            
            column(11, 
                   div(id = 'clickdiv001',
                       plotOutput("tbd2"))
          
          
          
          
               )), 
          
          fluidRow(
            
            column(1, 
                   div(style = "font-size: 25px",
                   radioButtons("kpi2", "",
                                c("Prestazioni",
                                  "Valorizzazione", 
                                  "VP","AI","RT", "FTED", 
                                  "FTEC","FTET")
                   ))), 
            column(11, 
                   plotOutput("ptrendRep"))
            
            
          )
          
          
          
          ), 
tabItem(
  tabName = "Report Costi-Ricavi per Centri di Costo"
),
# Performance----
tabItem(
  tabName = "Performance",
  fluidPage(
    fluidRow( align = "center", 
    
    h1("Grado di raggiungimento degli obiettivi di performance organizzativa 
       ( aggiornamento al 30/06/2021)")),
    # fluidRow(
    #   valueBoxOutput("perfdg")
    # ), 
    br(), br(),
    fluidRow(   
    column(6,  
           plotOutput("pltArea",  width = "1500px",
                      height = "1500px")
          ), 
    column(6,
           box(width =12,
            tableOutput("AreaDip")
           )
           )
  
  
))
)


        )
      )
)






# tabItem(
#   tabName = "Performance", 
#   fluidPage( 
#     tabBox(title = "", width = 12, 
#            tabPanel("Performance Organizzativa", 
#                     fluidRow(
#                       column(4,
#                              box(width = 6,
#                                  title = "Performance Organizzativa dell'Ente", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("shine"),
#                                  tags$div(id="perfIZSLER", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "perfIZSLER")
#                              )), 
#                       
#                       column( 4, 
#                               box( width = 6,
#                                    title = "Direzione Generale", 
#                                    status = "primary", solidHeader = TRUE,
#                                    loadEChartsLibrary(), 
#                                    loadEChartsTheme("london"),
#                                    tags$div(id="dirgen", style="width:100%;height:400px;"),
#                                    deliverChart(div_id = "dirgen")
#                               )), 
#                       column( 4, 
#                               box(  width = 6,
#                                     title = "Direzione Sanitaria", 
#                                     status = "primary", solidHeader = TRUE,
#                                     loadEChartsLibrary(), 
#                                     loadEChartsTheme("london"),
#                                     tags$div(id="dirsan", style="width:100%;height:400px;"),
#                                     deliverChart(div_id = "dirsan")
#                               )
#                               
#                       )
#                     ), 
#                     fluidRow(
#                       
#                       column(2.5, 
#                              box(width = 2,
#                                  title = "Dipartimento Amministrativo", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("london"),
#                                  tags$div(id="dipamm", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "dipamm")
#                              )
#                       ), 
#                       column(2.5, 
#                              box(width = 2,
#                                  title = "Dipartimento Tutela e Salute animale", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("london"),
#                                  tags$div(id="dipTSA", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "dipTSA")
#                              )
#                       ), 
#                       
#                       column(2.5, 
#                              box(width = 2,
#                                  title = "Dipartimento Sicurezza Alimentare", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("london"),
#                                  tags$div(id="dipSA", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "dipSA")
#                              )
#                       ), 
#                       column(2.5, 
#                              box(width = 2,
#                                  title = "Area Territoriale Lombardia", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("london"),
#                                  tags$div(id="LOMB", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "LOMB")
#                              )
#                       ), 
#                       column(2.5, 
#                              box(width = 2,
#                                  title = "Area Territoriale Emilia Romagna", 
#                                  status = "primary", solidHeader = TRUE,
#                                  loadEChartsLibrary(), 
#                                  loadEChartsTheme("london"),
#                                  tags$div(id="EMR", style="width:100%;height:400px;"),
#                                  deliverChart(div_id = "EMR")
#                              )
#                       )
#                       
#                       
#                     )
#                     
#                     
#                     
#                     
#                     
#                     
#                     
#                     
#                     ), 
# tabPanel(" Aree "), 
# tabPanel("Obiettivi"), 
# tabPanel("Azioni"), 
# tabPanel("Indicatori")
#            )
#     )
#   
#   
#  
#  
# 
#   
#   )


          
          
   
 
 

 
 
 
          
      


































