ui <- dashboardPage(
      dashboardHeader(title = "Controllo di gestione e performances", titleWidth = 400),
      
      dashboardSidebar(
        width = 300,
            sidebarMenu( 
            menuItem("Quadro Generale Dipartimenti", tabName = "izsler", icon = icon("globe")), 
            menuItem("Dipartimenti", tabName = "dipartimenti", icon = icon("sitemap")))
        ),
      
      dashboardBody(
              tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 12pt; }")), 
            tags$head(tags$style(HTML('
                                .modal-lg {
                                width: 1500px;
                                }
                              '))),
        tabItems(
          tabItem( 
          tabName = "izsler",
          
          hr(),
           
           fluidRow(  
             column(12, 
               box(title = "KPI", solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 12, 
                 valueBoxOutput("esami"),
                 valueBoxOutput("ricavi"),
                 valueBoxOutput("venprod"),
                 valueBoxOutput("attint"),
                 valueBoxOutput("rictot"),
                 valueBoxOutput("RFTE"),
                 valueBoxOutput("Costi"),
                 valueBoxOutput("costifte"),
                 valueBoxOutput("roi"), 
                 div(id='clickdiv0',
                            valueBoxOutput("IF")),
                        bsModal("P", "Pubblicazioni IF", "clickdiv0",dataTableOutput("articoli"), size = "large"),

                        div(id='clickdiv1',
                            valueBoxOutput("Int")),
                        bsModal("CI", "Partecipazione a convegni internazionali", "clickdiv1", dataTableOutput("convegni"), size = "large"),

                        div(id='clickdiv2',
                          valueBoxOutput("PR")),
                        bsModal("Prj", "Progetti di ricerca in corso", "clickdiv2", dataTableOutput("projr"), size = "large"))
               )), 
         
          fluidRow(
            column(4), 
            column(4, 
                   sliderInput("anno", h2("Seleziona l'Anno"), min=2019, max = 2021, value = 2021)), 
            column(4)),
          fluidRow(
             box(title = textOutput("year"),  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12,
            tableOutput("t")
             )
          )
          ), 
         
        tabItem(
          tabName = "dipartimenti", 
             fluidRow(
               column(2, sliderInput("anno2", "Seleziona l'Anno", min=2019, max = 2021, value = 2021)), 
              
               column(10,  radioButtons("dip", "Seleziona il Dipartimento",
                                       choices = unique(factor(tabIZSLER$Dipartimento)), inline=T)
             ), 
             hr(), 
             fluidRow(
               column(12,
                      box(title = "KPI-Dipartimenti", solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 12,
                          valueBoxOutput("esamidip"),
                          valueBoxOutput("ricavidip"),
                          valueBoxOutput("venproddip"),
                          valueBoxOutput("attintdip"),
                          valueBoxOutput("rictotdip"),
                          valueBoxOutput("RFTEdip"),
                          valueBoxOutput("Costidip"),
                          valueBoxOutput("costiftedip"),
                          valueBoxOutput("roidip"),

                         # div(id='clickdiv3',
                              valueBoxOutput("IFdip"), #),
                          #bsModal("P1", "Pubblicazioni IF", "clickdiv3",dataTableOutput("articoli"), size = "large"),

                          #div(id='clickdiv4',
                              valueBoxOutput("Intdip"), #),
                          #bsModal("CI1", "Partecipazione a convegni internazionali", "clickdiv4", dataTableOutput("convegni"), size = "large"),

                          #div(id='clickdiv5',
                              valueBoxOutput("PRdip")#),
                         # bsModal("Prj1", "Progetti di ricerca in corso", "clickdiv5", dataTableOutput("projr"), size = "large"))
               )
               
               
               
               
             )), 
             
             fluidRow(
                 
                column(12,
                box(title = "",  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12, 
                    tableOutput("tr"))
                )) 
               )
             )
            )
          )
          
          
        )
 
 

 
 
 
          
      





































# ui <- dashboardPage(
#   dashboardHeader(title = "Controllo di gestione e performances", titleWidth = 400),
#  
#   dashboardSidebar(
#   width = 300,
#   sidebarMenu( 
#   menuItem("Quadro Generale Dipartimenti", tabName = "izsler", icon = icon("globe")), 
#   menuItem("Dipartimenti", tabName = "dipartimenti", icon = icon("sitemap")), 
#   #     # menuItem("Dipartimento Tutela e Salute Animale", tabName = "dsa", icon = icon("sitemap")),
#   #     # menuItem("Area Territoriale Lombardia", tabName = "lomb", icon = icon("sitemap")),
#   #     # menuItem("Area Territoriale Emilia Romagna", tabName = "emil", icon = icon("sitemap")),
#   #     hr(), 
#   #     br(), 
#   #     
#   #     selectInput("dip", "Seleziona il Dipartimento", choices = unique(factor(tabIZSLER$Dipartimento)), 
#   #                 selected = ""), 
#   sliderInput("anno", "Seleziona l'Anno", min=2019, max = 2021, value= 2021)
#   #     #menuItem("Note", tabName = "help", icon = icon("book")), 
#   #     #menuItem("Programmazione 2021", "tabName"= "pr21"),
#   #     hr(),
#   #     br()
#   #   )
#   ),
#   dashboardBody(
#     tags$head(tags$style(HTML('
#                         .modal-lg {
#                         width: 1500px;
#                         }
#                       '))),
# tabItems(
# #####IZSLER#####
#   tabItem( tabName = "izsler",
#    fluidRow(
#      valueBoxOutput("esami"),
#      valueBoxOutput("ricavi"),
#      valueBoxOutput("venprod"),
#      valueBoxOutput("attint"),
#      valueBoxOutput("rictot"),
#      valueBoxOutput("RFTE"),
#      valueBoxOutput("Costi"),
#      valueBoxOutput("costifte"),
#      valueBoxOutput("roi"),
# 
#    div(id='clickdiv0',
#        valueBoxOutput("IF")),
#    bsModal("P", "Pubblicazioni IF", "clickdiv0",dataTableOutput("articoli"), size = "large"),
# 
#    div(id='clickdiv1',
#        valueBoxOutput("Int")),
#    bsModal("CI", "Partecipazione a convegni internazionali", "clickdiv1", dataTableOutput("convegni"), size = "large"),
# 
#    div(id='clickdiv2',
#      valueBoxOutput("PR")),
#    bsModal("Prj", "Progetti di ricerca in corso", "clickdiv2", dataTableOutput("projr"), size = "large"),
# 
#    ),
#     
#     #br(),
#   
#     fluidRow(  
#     # #   div(id='radar1',
#     tableOutput("t"),
#    # tableOutput("tr")
#     ),
#     # bsModal("R1", "IZSLER: profilo indicatori di performances", "radar1", plotOutput("radarIZSLER"), size = "large")),
# 
#    # br(),
#     #fluidRow(
#     # column(1, 
#     # radioButtons("ind", "", 
#     #             c("IP" = "IP","Dipartimento" = "Dipartimento" ))),
#     # 
#     # 
#     # column(11, 
#     #          div(id = 'clickdiv00',
#     #              plotOutput("tbd")),
#     #              bsModal("TW", "Distribuzione percentuale degli Indicatori di performance  tra i Dipartimenti",  'clickdiv00', tableOutput("tbw")))
#     #  
#     ), 
# tabItem("dipartimenti",  
# 
# Sidebar(
#            width = 300,
#            selectInput("dip", "Seleziona il Dipartimento", choices = unique(factor(tabIZSLER$Dipartimento)), selected = ""), 
#            sliderInput("anno", "Seleziona l'Anno", min=2019, max = 2021, value= 2021)
# 
# ), 
# 
#  
#   fluidRow(
#    tableOutput("tr")
#   )
# 
# )
# )
# )
# )
# )#, 
# ####Dipartimento Sicurezza Alimentare####
#   # tabItem(tabName = "dsalim", 
#      #  fluidRow(
# #         valueBoxOutput("esami2"),
# #         valueBoxOutput("ra2"),
# #         valueBoxOutput("vp2"),
# #         valueBoxOutput("ai2"),
# #         valueBoxOutput("rt2"),
# #         valueBoxOutput("rfte2"),
# #       
# #       div(id='clickdiv3',
# #           valueBoxOutput("IF2")),
# #       bsModal("P2", "Pubblicazioni IF", "clickdiv3",dataTableOutput("articoli2"), size = "large"),
# #       
# #       div(id='clickdiv4',
# #           valueBoxOutput("Int2")),
# #       bsModal("CI2", "Partecipazione a convegni internazionali", "clickdiv4", dataTableOutput("convegni2"), size = "large"),
# #       
# #       div(id='clickdiv5',
# #           valueBoxOutput("PR2")),
# #       bsModal("Prj2", "Progetti di Ricerca", "clickdiv5", dataTableOutput("projr2"), size = "large"),
# #       
# #       # div(id='clickdiv5',
# #       #     valueBoxOutput("Naz2")),
# #       # bsModal("CN2", "Partecipazione a convegni nazionali", "clickdiv5", tableOutput("nazionali2"), size = "large"),
# #       # 
# #       
# #       ),
# #       
# #       br(),
# #       
# #       fluidRow( 
# #         div(id='radar2',  
# #tableOutput("tr")),
# #         bsModal("R2", "Dipartimento Sicurezza Alimentare: profilo indicatori di performances", "radar2", plotOutput("radarDSA"), size = "large")), 
# #       
# #       br(),
# #       fluidRow(
# #         column(1, 
# #                radioButtons("ind2", "", 
# #                             c( "IP" = "IP","Reparto" = "Reparto"))),
# #         
# #         column(11, 
# #                div(id = 'clickdiv01',
# #                    plotOutput("tbd2")),
# #                bsModal("TW2", "Distribuzione percentuale degli Indicatori di performance  tra i Reparti",  'clickdiv01', tableOutput("tbw2")))
# #       )
# #)
# # 
# # ####Dipartimento Tutela Salute Animale#####
# #   tabItem(tabName = "dsa", 
# #           fluidRow(
# #             valueBoxOutput("esami3"),
# #             valueBoxOutput("ra3"),
# #             valueBoxOutput("vp3"),
# #             valueBoxOutput("ai3"),
# #             valueBoxOutput("rt3"),
# #             valueBoxOutput("rfte3"),
# #             
# #             div(id='clickdiv6',
# #                 valueBoxOutput("IF3")),
# #             bsModal("P3", "Pubblicazioni IF", "clickdiv6",dataTableOutput("articoli3"), size = "large"),
# #             
# #             div(id='clickdiv7',
# #                 valueBoxOutput("Int3")),
# #             bsModal("CI3", "Partecipazione a convegni internazionali", "clickdiv7", dataTableOutput("convegni3"), size = "large"),
# #             
# #             div(id='clickdiv8',
# #                 valueBoxOutput("PR3")),
# #             bsModal("Prj3", "Progetti di ricerca", "clickdiv8", dataTableOutput("projr3"), size = "large"),
# #           
# #             
# #             # div(id='clickdiv8',
# #             #     valueBoxOutput("Naz3")),
# #             # bsModal("CN3", "Partecipazione a convegni nazionali", "clickdiv8", tableOutput("nazionali3"), size = "large"),
# #             ),
# #           
# #           br(),
# #           
# #           fluidRow( 
# #             div(id='radar3',  
# #                 tableOutput("t3")),
# #             bsModal("R3", "Dipartimento Tutela Salute Animale: profilo indicatori di performances", "radar3", plotOutput("radarDTSA"), size = "large")), 
# #           
# #           
# #           
# #           
# #           
# #           
# #           br(),
# #           fluidRow(
# #             column(1, 
# #                    radioButtons("ind3", "", 
# #                                 c( "IP" = "IP","Reparto" = "Reparto"))),
# #             
# #             column(11, 
# #                    div(id = 'clickdiv02',
# #                        plotOutput("tbd3")),
# #                    bsModal("TW3", "Distribuzione percentuale degli Indicatori di performance  tra i Reparti",  'clickdiv02', tableOutput("tbw3")))
# #             
# #             
# #           )
# #   ), 
# #           
# # ###Area Territoriale Lombardia####
# #   tabItem(tabName = "lomb", 
# #                   fluidRow(
# #                     valueBoxOutput("esami4"),
# #                     valueBoxOutput("ra4"),
# #                     valueBoxOutput("vp4"),
# #                     valueBoxOutput("ai4"),
# #                     valueBoxOutput("rt4"),
# #                     valueBoxOutput("rfte4"),
# #                     
# #                     div(id='clickdiv9',
# #                         valueBoxOutput("IF4")),
# #                     bsModal("P4", "Pubblicazioni IF", "clickdiv9",dataTableOutput("articoli4"), size = "large"),
# #                     
# #                     div(id='clickdiv10',
# #                         valueBoxOutput("Int4")),
# #                     bsModal("CI4", "Partecipazione a convegni internazionali", "clickdiv10", dataTableOutput("convegni4"), size = "large"),
# #                     
# #                     div(id='clickdiv11',
# #                         valueBoxOutput("PR4")),
# #                     bsModal("Prj4", "Progetti di ricerca", "clickdiv11", dataTableOutput("projr4"), size = "large"),
# #                     
# #                     
# #                     
# #                     # div(id='clickdiv11',
# #                     #     valueBoxOutput("Naz4")),
# #                     # bsModal("CN4", "Partecipazione a convegni nazionali", "clickdiv11", tableOutput("nazionali4"), size = "large"),
# # 
# #                   ),
# #           br(),
# #           
# #           fluidRow( 
# #             div(id='radar4',  
# #                 tableOutput("t4")),
# #             bsModal("R4", "Area Territoriale Lombardia: profilo indicatori di performances", "radar4", plotOutput("radarATLOMB"), size = "large")), 
# #           
# #           
# #           br(),
# #           fluidRow(
# #             column(1, 
# #                    radioButtons("ind4", "", 
# #                                 c( "IP" = "IP","Reparto" = "Reparto"))),
# #             
# #             column(11, 
# #                    div(id = 'clickdiv03',
# #                        plotOutput("tbd4")),
# #                    bsModal("TW4", "Distribuzione percentuale degli Indicatori di performance  tra i Reparti",  'clickdiv03', tableOutput("tbw4")))
# #             
# #             
# #             
# #           )
# # 
# #           ), 
# # ###Area Territoriale Emilia Romagna#####
# #   tabItem(tabName = "emil", 
# #           fluidRow(
# #             valueBoxOutput("esami5"),
# #             valueBoxOutput("ra5"),
# #             valueBoxOutput("vp5"),
# #             valueBoxOutput("ai5"),
# #             valueBoxOutput("rt5"),
# #             valueBoxOutput("rfte5"),
# #             
# #             div(id='clickdiv12',
# #                 valueBoxOutput("IF5")),
# #             bsModal("P5", "Pubblicazioni IF", "clickdiv12", dataTableOutput("articoli5"), size = "large"),
# #             
# #             div(id='clickdiv13',
# #                 valueBoxOutput("Int5")),
# #             bsModal("CI5", "Partecipazione a convegni internazionali", "clickdiv13", dataTableOutput("convegni5"), size = "large"),
# #             
# #             div(id='clickdiv14',
# #                 valueBoxOutput("PR5")),
# #             bsModal("Prj5", "Progetti di ricerca", "clickdiv14", dataTableOutput("projr5"), size = "large"),
# #             
# #             
# #             # div(id='clickdiv14',
# #             #     valueBoxOutput("Naz5")),
# #             # bsModal("CN5", "Partecipazione a convegni nazionali", "clickdiv14", tableOutput("nazionali5"), size = "large"),
# #             # 
# #           ),
# #           br(),
# #           fluidRow( 
# #             div(id='radar5',  
# #                 tableOutput("t5")),
# #             bsModal("R5", "Area Territoriale Emilia Romagna: profilo indicatori di performances", "radar5", plotOutput("radarATER"), size = "large")), 
# #           
# #           
# #           br(),
# #           fluidRow(
# #             column(1, 
# #                    radioButtons("ind5", "", 
# #                                 c( "IP" = "IP","Reparto" = "Reparto"))),
# #             column(11, 
# #                    div(id = 'clickdiv04',
# #                        plotOutput("tbd5")), 
# #                   bsModal("TW5", "Distribuzione percentuale degli Indicatori di performance  tra i Reparti",'clickdiv04',tableOutput("tbw5") )
# #           )
# #           )
# #           
# #           
# #           
# #   
# # ) 
# #  , 
# # 
# # ##Note#### 
# # tabItem(tabName = "help",
# #         includeHTML("note.html")
# #         ), 
# # 
# # ##Programmazione#
# # # tabItem(tabName = "progr", 
# # #         
# # #         wellPanel(
# # #           fluidRow( 
# # #             column(3,  
# # #                    h3("Programmazione"),            
# # #                    numericInput("rt", "Ricavo Totale previsto",  value = "1000000"), 
# # #                    br(), 
# # #                    numericInput("fte", "Full Time Equivalenti disponibili ",  value = "100"), 
# # #                    br(), 
# # #                    # numericInput("ftet", "Full Time Equivalenti teorico",  value = ""), 
# # #                    # br(),
# # #                    sliderInput("pc", "percentuale FTE allocata agli obiettivi", min=0, max= 50,  value = "0")), 
# # #             
# # #             column(9, 
# # #                    valueBoxOutput("rfteT"),
# # #                    valueBoxOutput("ftep"), 
# # #                    valueBoxOutput("rfteP"), 
# # #                    valueBoxOutput("target")
# # #                   
# # #                    # tableOutput("tb")  
# # #                    
# # #                    
# # #                    
# # #             ))), 
# # #         br(),br(),br(),
# # #         
# # #         wellPanel(
# # #           fluidRow(
# # #             column(3, 
# # #                    h3("Verifica"),
# # #                    sliderInput("Vrt", "Variazione percentuale del Ricavo Totale previsto", min=-50, max= 50,  value = 0),
# # #                    br(), 
# # #                    sliderInput("Vfte", "Variazione percentuale del FTE programmato ", min=-50, max= 50,  value = 0)),
# # #             
# # #             column(9, 
# # #                    valueBoxOutput("rtot"),
# # #                    valueBoxOutput("fteR"), 
# # #                    valueBoxOutput("rfteR"),
# # #                    valueBoxOutput("target2")#, 
# # #                    #valueBoxOutput("risn")
# # #                    
# # #                    
# # #                    # tableOutput("tb2") 
# # #             )))
# # #         
# # # )
# # 
# # # tabItem(tabName = "progr21", 
# # #         
# # #         h3(
# # #           HTML("<center>", "Distribuzione % Full Time Equivalenti per Dipartimento","</center>")
# # #         ),
# # #         #h3("Distribuzione % dei FTE per Dipartimento"), 
# # # 
# # #         radioButtons("DC", "", 
# # #                      c("FTE-Dirigenza" = "FTED","FTE-Comparto" = "FTEC" )), 
# # #         hr(), 
# # #         dataTableOutput("progFTEv"), 
# # #         hr(),
# # #         dataTableOutput("progFTE")
# # # 
# # # ),
# # 
# # # , 
# # # 
# # # tabItem(tabName = "dsaprogr21", 
# # #         
# # #         h3(
# # #           HTML("<center>", "Distribuzione % Full Time Equivalenti per Dipartimento","</center>")
# # #         ),
# # # 
# # #         
# # #         radioButtons("DC", "", 
# # #                      c("FTE-Dirigenza" = "FTED","FTE-Comparto" = "FTEC" )), 
# # #         hr(), 
# # #         dataTableOutput("progFTEv"), 
# # #         hr(),
# # #         dataTableOutput("progFTE")
# # #         
# # # )
# # ####PROGRAMMAZIONE####
# # tabItem(tabName = "pr21", 
# #         fluidRow(
# #           tabBox(
# #             title = "Programmazione 2021",
# #             # The id lets us use input$tabset1 on the server to find the current tab
# #             id = "tabset1", width = "400px", height = "100%",
# #             tabPanel("IZSLER", 
# #                      h3(
# #                        HTML("<center>", "IZSLER: Distribuzione % Full Time Equivalenti per Dipartimento","</center>")
# #                      ),
# #                      
# #                      radioButtons("DC", "", 
# #                                   c("FTE-Dirigenza" = "FTED","FTE-Comparto" = "FTEC" )), 
# #                      hr(), 
# #                      dataTableOutput("progFTEv"), 
# #                      hr(),
# #                      dataTableOutput("progFTE")),
# #             
# #             tabPanel("DIRSAN", 
# #                      h3(
# #                        HTML("<center>", "DIREZIONE SANITARIA: Distribuzione % Full Time Equivalenti ","</center>")
# #                      ),
# #                      
# #                      radioButtons("DC6", "",
# #                                   c("FTE-Dirigenza" = "FTED5","FTE-Comparto" = "FTEC5" )),
# #                      hr(),
# #                      dataTableOutput("dirsanFTEv"), 
# #                      hr(),
# #                      dataTableOutput("dirsanFTE")), 
# #             
# #             tabPanel("DIPAMM", 
# #                      h3(
# #                        HTML("<center>", "DIPARTIMENTO AMMINISTRATIVO: Distribuzione % Full Time Equivalenti ","</center>")
# #                      ),
# #                      
# #                      radioButtons("DC7", "",
# #                                   c("FTE-Dirigenza" = "FTED6","FTE-Comparto" = "FTEC6" )),
# #                      hr(),
# #                      dataTableOutput("dipamFTEv"), 
# #                      hr(),
# #                      dataTableOutput("dipamFTE")), 
# #             
# #             
# #             
# #             tabPanel("DSA",
# #                      h3(
# #                        HTML("<center>", "DIPARTIMENTO SICUREZZA ALIMENTARE: Distribuzione % Full Time Equivalenti per Reparto","</center>")
# #                      ),
# # 
# #                      radioButtons("DC2", "",
# #                                   c("FTE-Dirigenza" = "FTED1","FTE-Comparto" = "FTEC1" )),
# #                      hr(),
# #                      dataTableOutput("dsaFTEv"), 
# #                      hr(),
# #                      dataTableOutput("dsaFTE")),
# #             tabPanel("DTSA", 
# #                      h3(
# #                        HTML("<center>", "DIPARTIMENTO TUTELA SALUTE ANIMALE: Distribuzione % Full Time Equivalenti per Reparto","</center>")
# #                      ),
# #                      
# #                      radioButtons("DC3", "",
# #                                   c("FTE-Dirigenza" = "FTED2","FTE-Comparto" = "FTEC2" )),
# #                      hr(),
# #                      dataTableOutput("dtsaFTEv"), 
# #                      hr(),
# #                      dataTableOutput("dtsaFTE")),
# #            tabPanel("ATLOMB", 
# #                     h3(
# #                       HTML("<center>", "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA: Distribuzione % Full Time Equivalenti per Reparto","</center>")
# #                     ),
# #                     
# #                     radioButtons("DC5", "",
# #                                  c("FTE-Dirigenza" = "FTED4","FTE-Comparto" = "FTEC4" )),
# #                     hr(),
# #                     dataTableOutput("atlomFTEv"), 
# #                     hr(),
# #                     dataTableOutput("atlomFTE")), 
# #            
# #             tabPanel("ATER", 
# #                      h3(
# #                        HTML("<center>", "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA: Distribuzione % Full Time Equivalenti per Reparto","</center>")
# #                      ),
# #                      
# #                      radioButtons("DC4", "",
# #                                   c("FTE-Dirigenza" = "FTED3","FTE-Comparto" = "FTEC3" )),
# #                      hr(),
# #                      dataTableOutput("aterFTEv"), 
# #                      hr(),
# #                      dataTableOutput("aterFTE"))
# #            
# #           
# #            
# #            
# #            
# #            
# #           )
# #         
# #         
# #         ))
# # 
# # 
# 
# #)))
# 
