server <- function(input, output, session) {
  # data aggiornamento dei dati-----
  output$aggdati <- renderUI({
    paste0("Dati aggiornati al:", format(as.Date(substr(max(conf$Data_Accettazione, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"))
  })
  
  source("server_code/grafici.R", local = TRUE)
  
}