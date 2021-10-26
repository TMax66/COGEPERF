server<-function(input, output, session) { 
  
# tabella dati----  
output$dati <- renderDataTable({  
    datatable(dt,
    rownames = FALSE,filter = 'top',
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('excel'))
      )
     })

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


