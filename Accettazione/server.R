server<-function(input, output, session) { 
  
# tabella dati----  
output$dati <- renderDataTable({  
    datatable(acc,
    rownames = FALSE,filter = 'top',
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('excel'))
      )
     })

# tabella pivot 
  
output$pivot <- renderRpivotTable({
  rpivotTable( acc,   
                 #select( ),
               aggregatorName="Sum", vals = "",
               onRefresh = htmlwidgets::JS(
                 "function(config) {
                        Shiny.onInputChange('pivot', document.getElementById('pivot').innerHTML); 
                        }"))
})
  
  












}


