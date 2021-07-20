server<-function(input, output) {
  
  dt <- reactive(dtanalisi %>% 
    group_by(`Centro di Costo`, Pagamento, ClassAnalisi, Quarter) %>% 
    summarise(Valorizzato = sum(`A Tariffario`, na.rm = TRUE), 
              Fatturato = sum(Fatturato, na.rm = TRUE)) %>% 
    mutate(Variazione = round((Valorizzato/lag(Valorizzato) - 1) * 100, 2 ))
  )
  
 
  
  output$varchange <- renderPlot(
    
    dt() %>%
      filter(`Centro di Costo` == input$CC & Pagamento == input$paga) %>% 
      select(y = input$par, Quarter) %>% 
      ggplot(
        aes(y = y, x = Quarter,  label=y))+
      geom_line(aes(group = ClassAnalisi), size= 1.1, col = "darkgrey")+ 
      geom_label(size = 4, col = "blue")+
      facet_wrap(.~ClassAnalisi, ncol = 1)+
      geom_hline(yintercept = 0, size = 0.5)+
      #labs(y = "Variazione %", x = "Anno.Trimestre")+
      theme_ipsum_rc()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  )
  
  
  
}
