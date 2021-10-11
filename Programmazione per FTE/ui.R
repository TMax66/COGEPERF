ui<- fluidPage( 
        theme = shinytheme("cerulean"),
    
    wellPanel(
                 fluidRow(
                     column(2,
                            h3("Programmazione"),
                            numericInput("rt", "Ricavo Totale previsto",  value = "1000000"),
                            br(),
                            numericInput("fte", "Full Time Equivalenti disponibili ",  value = "100"),
                            br(),
                            # numericInput("ftet", "Full Time Equivalenti teorico",  value = ""),
                            # br(),
                            sliderInput("pc", "percentuale FTE allocata agli obiettivi", min=0, max= 50,  value = "0")),
                     
                     column(10,
                            # valueBoxOutput("rfteT"),
                            # valueBoxOutput("ftep"),
                            # valueBoxOutput("rfteP"),
                            # valueBoxOutput("target"),
                            # br(), br(), br(), br(), hr() 
                            tableOutput("tb")
                            
                            
                            
                     ))),
    br(),br(),br(),
    
    wellPanel(
        fluidRow(
            column(2,
                   h3("Simulazione"),
                   sliderInput("Vrt", "Variazione percentuale del Ricavo Totale previsto", min=-50, max= 50,  value = 0),
                   br(),
                   sliderInput("Vfte", "Variazione percentuale del FTE programmato ", min=-50, max= 50,  value = 0)),
            
            column(10,
                   # valueBoxOutput("rtot"),
                   # valueBoxOutput("fteR"),
                   # valueBoxOutput("rfteR"),
                   # valueBoxOutput("target2")#,
                   # # valueBoxOutput("risn"),
                   
                   #br(),br(),br()
                    tableOutput("tb2")
            )))
    
    )
             
             
             
             
             
  

