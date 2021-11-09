server<-function(input, output, session) { 
  
# tabella dati----  
output$dati <- renderDataTable( 
    dt,
    server = TRUE, 
    rownames = FALSE,filter = 'top',
    options = list(
      dom = 'Bfrtip')
      
     )

output$downloadData <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dt, file)
  }
)


#plot----

# dtP <- dt %>% 
#   mutate(dtacc = as.character(dtreg)) %>% 
#   group_by(dtreg) %>% 
#   mutate(Nconf = sum(n())) %>%  
#   #filter(esami > 0) %>%
#   mutate(sett = rollmean(Nconf, k = 30, fill = NA) )



# tabella pivot 
  
output$pivot <- renderRpivotTable({
  rpivotTable( dt,   
                 #select( ),
               aggregatorName="Count", vals = "",
               onRefresh = htmlwidgets::JS(
                 "function(config) {
                        Shiny.onInputChange('pivot', document.getElementById('pivot').innerHTML); 
                        }"))
})
  
  












}


