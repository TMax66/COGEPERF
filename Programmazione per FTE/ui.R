ui<- fluidPage( 
        theme = shinytheme("cerulean"),
    
    wellPanel(
                 fluidRow(
                     column(3,
                            h3("Programmazione"),
                            numericInput("rt", "Ricavo Totale previsto",  value = "1000000"),
                            br(),
                            numericInput("fte", "Full Time Equivalenti disponibili ",  value = "100"),
                            br(),
                            # numericInput("ftet", "Full Time Equivalenti teorico",  value = ""),
                            # br(),
                            sliderInput("pc", "percentuale FTE allocata agli obiettivi", min=0, max= 50,  value = "0")),
                     
                     column(9,
                            valueBoxOutput("rfteT"),
                            valueBoxOutput("ftep"),
                            valueBoxOutput("rfteP"),
                            valueBoxOutput("target"),
                            
                            tableOutput("tb")
                            
                            
                            
                     ))),
    br(),br(),br(),
    
    wellPanel(
        fluidRow(
            column(3,
                   h3("Verifica"),
                   sliderInput("Vrt", "Variazione percentuale del Ricavo Totale previsto", min=-50, max= 50,  value = 0),
                   br(),
                   sliderInput("Vfte", "Variazione percentuale del FTE programmato ", min=-50, max= 50,  value = 0)),
            
            column(9,
                   valueBoxOutput("rtot"),
                   valueBoxOutput("fteR"),
                   valueBoxOutput("rfteR"),
                   valueBoxOutput("target2"),#,
                   #valueBoxOutput("risn")
                   
                   
                    tableOutput("tb2")
            )))
    
    )
             
             
             
             
             
  

