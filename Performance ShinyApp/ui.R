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
                           choices =  c("Dipartimento tutela e salute animale", 
                                        "Dipartimento sicurezza alimentare", 
                                        "Dipartimento area territoriale Lombardia", 
                                        "Dipartimento area territoriale Emilia Romagna", 
                                        "Direzione Sanitaria")), 
              br(), 
              sliderInput("anno2", h3("Seleziona l'Anno"), min=2019, max = 2021, value = 2021)), 
            menuItem("Report Costi-Ricavi per Centri di Costo", tabName = "Centri di Costo", icon = icon("euro")), 
            menuItem("Obiettivi di Performance", tabName = "Performance", icon = icon("table"))
            )),
      
      dashboardBody(
            tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")), 
            tags$head(tags$style(HTML('.modal-lg {width: 1500px;}'))),
            tags$head(tags$style(HTML(".main-sidebar { font-size: 25px; }"))), #change the font size to 20

            
        #     tags$head(tags$style(HTML('
        # # /* logo */
        # # .skin-blue .main-header .logo {
        # #                       background-color: #f4b943;
        # #                       }
        # # 
        # # /* logo when hovered */
        # # .skin-blue .main-header .logo:hover {
        # #                       background-color: #f4b943;
        # #                       }
        # # 
        # # /* navbar (rest of the header) */
        # # .skin-blue .main-header .navbar {
        # #                       background-color: #f4b943;
        # #                       }        
        # 
        # /* main sidebar */
        # .skin-blue .main-sidebar {
        #                       background-color: #f4b943;
        #                       }
        # 
        # /* active selected tab in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        #                       background-color: #ff0000;
        #                       }
        # 
        # /* other links in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        #                       background-color: #00ff00;
        #                       color: #000000;
        #                       }
        # 
        # /* other links in the sidebarmenu when hovered */
        #  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        #                       background-color: #ff69b4;
        #                       }
        # /* toggle button when hovered  */                    
        #  .skin-blue .main-header .navbar .sidebar-toggle:hover{
        #                       background-color: #ff69b4;
        #                       }
        #                       '))), 

            
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
             box(title = textOutput("year"),  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12,
            tableOutput("t")
             )
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
                          valueBoxOutput("costiftedip"),
                          valueBoxOutput("roidip"),

                         # div(id='clickdiv3',
                              valueBoxOutput("IFdip"), #),
                          #bsModal("P1", "Pubblicazioni IF", "clickdiv3",dataTableOutput("articoli"), size = "large"),

                          #div(id='clickdiv4',
                              valueBoxOutput("Intdip"), #),
                          #bsModal("CI1", "Partecipazione a convegni internazionali", "clickdiv4", dataTableOutput("convegni"), size = "large"),

                          div(id='clickdiv5',
                              valueBoxOutput("PRdip")),
                          bsModal("Prj1", "Progetti di ricerca in corso", "clickdiv5", dataTableOutput("projrep"), size = "large"))
               )
               
               
               
               
             ),

             
             fluidRow(
                 
                column(12,
                box(title = h3(textOutput("dipa2")),  solidHeader = TRUE, collapsible = TRUE,status = "primary", width = 12, 
                    tableOutput("tr"))
                )) 
               ), 
tabItem(
  tabName = "Report Costi-Ricavi per Centri di Costo"
), 
tabItem(
  tabName = "Performance", 
  fluidRow(
    column(6, offset = 3, 
          loadEChartsLibrary(), 
          tags$div(id="test", style="width:50%;height:400px;"),
          deliverChart(div_id = "test")
          )
  )
)
             )
            )
          )

          
          
   
 
 

 
 
 
          
      


































